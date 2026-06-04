library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(redivis)

# copy of all code chunks in process_dataset workflow
ds <- "pilot-uniandes-co-rural"
dataset_spec <- list(list(name = ds, version = "current"))

# participants
participants <- process_participants(dataset_spec)

# runs
runs <- process_runs(dataset_spec,
                     remove_incomplete_runs = TRUE,
                     remove_invalid_runs = FALSE)

run_data <- runs |>
  mutate(validation_msg_run = validation_msg_run |> replace_na(""),
         straightlining = str_detect(validation_msg_run, "straightlining")) |>
  # filter to runs completed and no straightlining
  filter(completed & !straightlining) |>
  select(-straightlining) |>
  # filter out task version with known bug
  filter(!(task_version == "1.0.0-beta.19" &
             task_id %in% c("matrix-reasoning", "mental-rotation", "theory-of-mind"))) |>
  # take each user's first run of each task in each administration
  group_by(user_id, task_id, variant_id, administration_id) |>
  arrange(time_started) |>
  slice(1) |>
  ungroup() |>
  left_join(participants |> select(user_id, site, dataset), by = "user_id") |>
  relocate(site, dataset, .after = redivis_source)


# trials

trials <- process_trials(dataset_spec,
                         remove_incomplete_runs = TRUE,
                         remove_invalid_runs = FALSE,
                         remove_invalid_trials = FALSE)

trial_data <- trials |>
  semi_join(run_data) |>
  mutate(validation_msg_trial = validation_msg_trial |> replace_na(""),
         slow_rt = rt_numeric > 30000,
         fast_rt = str_detect(validation_msg_trial, "fast")) |>
  filter(task_id == "same-different-selection" | is.na(rt_numeric) | !slow_rt,
         !fast_rt) |>
  select(-slow_rt, -fast_rt, -valid_trial, -validation_msg_trial)

trial_data_coded <- trial_data |>
  left_join(participants |> select(user_id, site, dataset), by = "user_id") |>
  relocate(site, dataset, .after = redivis_source) |>
  recode_trials()

# scoring

scoring_table <- fetch_scoring_table()
registry_dir <- fetch_registry_dir()

task_runs <- run_data |>
  nest(runs = -c(task_id, dataset))

task_trials <- trial_data_coded |>
  filter(!is.na(item_task), !is.na(dataset)) |>
  nest(trials = -c(task_id, item_task, dataset)) |>
  left_join(task_runs)

safe_score <- safely(rlevante:::score)
task_scores <- task_trials |>
  mutate(score_output = pmap(list(item_task, dataset, trials, runs),
                             partial(safe_score, scoring_table = scoring_table,
                                     registry_dir = registry_dir)),
         scores = map(score_output, \(s) s$result))

scores <- task_scores |>
  mutate(scores = map2(scores, runs,
                       \(sc, ru) if (is.null(sc)) select(ru, run_id) else sc)) |>
  select(item_task, scores) |>
  unnest(scores)

runs_scored <- run_data |>
  inner_join(scores) |>
  mutate(score = round(score, 2), score_se = round(score_se, 2)) |>
  select(redivis_source, site, dataset, run_id, user_id, age, administration_id,
         task_id, task_version, task_code = item_task, language, adaptive, num_attempted,
         score, score_se, score_type, scoring_model, registry_version,
         max_incorrect, max_time, sequential_stimulus, corpus)

# surveys

surveys <- process_surveys(dataset_spec)
survey_data <- if (is.null(surveys)) surveys else link_surveys(surveys, participants)

# outputs

out_ds <- redivis$organization("levante")$dataset(paste0(ds, "-processed"))
if (!out_ds$exists()) out_ds$create()
# increment version to "next" if necessary
out_ds_next <- out_ds$create_next_version(if_not_exists = TRUE)

# given dataset, table name, and data, uploads data to table in dataset
sync_table <- \(out_ds, table_name, df) {
  if (is.null(df)) return()
  out_table <- out_ds$table(table_name)
  if (!out_table$exists()) out_table$create()
  out_table$update(upload_merge_strategy = "replace")
  out_table$upload(table_name)$create(df, if_not_exists = FALSE, replace_on_conflict = TRUE)
}

# sync trial_data and runs_scored
sync_table(out_ds_next, "trials", trial_data_coded)
sync_table(out_ds_next, "scores", runs_scored)
sync_table(out_ds_next, "participants", participants)
sync_table(out_ds_next, "surveys", survey_data)
if (!is.null(survey_data)) sync_table(out_ds_next, "surveys", survey_data)

out_ds_next$release()
