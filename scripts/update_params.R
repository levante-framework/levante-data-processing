library(rlevante)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(forcats)
library(rairtable)

registry_table <- fetch_registry_table()

# extract item params from model record
# n.b. works for multigroup scalar (not configural/metric) and singlegroup
mod_coefs <- \(mod_rec, item_sep = "-") {
  n_resp <- colSums(!is.na(mod_rec@data)) |>
    enframe(name = "item", value = "n_responses")
  model_vals(mod_rec) |>
    as_tibble() |>
    filter(group != "GROUP", item != "GROUP") |>
    select(group, item, name, value) |>
    pivot_wider(names_from = name, values_from = value) |>
    left_join(n_resp) |>
    mutate(item = str_remove(item, glue("{item_sep}[0-9]+$"))) |>
    group_by(group, item, d, a1) |>
    summarise(n_responses = sum(n_responses), .groups = "drop") |>
    select(item_uid = item, n_responses, d, a1) |>
    distinct() |>
    mutate(difficulty = -d / a1) |>
    arrange(difficulty)
}

# default model spec -- rasch scalar from all item multigroup
mod_spec_standard <- list(model_set = "multigroup_site",
                          subset = "all_items",
                          itemtype = "rasch",
                          nfact = "f1",
                          invariance = "scalar")
# 2pl model spec -- 2pl scalar from all item multigroup
mod_spec_2pl <- list(model_set = "multigroup_site",
                     subset = "all_items",
                     itemtype = "2pl",
                     nfact = "f1",
                     invariance = "scalar")

# by-language model spec
mod_spec_lang <- \(lang) list(model_set = "by_language",
                              subset = lang,
                              itemtype = "rasch",
                              nfact = "f1",
                              invariance = NA)

# prep_task_params("vocab", mod_spec_lang("en"))

# extract and format item params given a task and model spec
prep_task_params <- \(item_task, mod_spec) {

  # retrieve model record
  mod_spec$item_task <- item_task
  mod_filename <- model_spec_filename(mod_spec)
  mod_row <- registry_table |> filter(file_name == mod_filename)
  mod_rec <- get_registry_file(mod_filename, registry_table)

  # extract params and add registry info
  mod_params <- mod_coefs(mod_rec)
  mod_params |>
    select(item_uid, difficulty, discrimination = a1, n_responses) |>
    mutate(registry_version = mod_row$redivis_source |> str_extract("(?<=:)v.*?$"),
           itemtype = mod_spec$itemtype,
           groups = list(mod_rec@group_names),
           n_runs = length(mod_rec@runs),
           file_id = mod_row$file_id,
           added_at = mod_row$added_at)
}

# iterate over languages, extract and format params for each
prep_task_lang_params <- \(item_task, mod_spec_fun) {
  langs <- registry_table |>
    filter(str_detect(file_name, glue("^{item_task}/by_language"))) |>
    pull(file_name) |>
    str_extract(glue("(?<={item_task}/by_language/).*?(?=/)")) |>
    unique()
  langs |> set_names() |>
    map(\(lang) prep_task_params(item_task, mod_spec_fun(lang))) |>
    list_rbind(names_to = "language")
}

# example: new params for multigroup task
# params_mrot <- prep_task_params("mrot", mod_spec_standard)

# example: new params for by language task
# params_vocab <- prep_task_lang_params("vocab", mod_spec_lang)

corpus_tasks <- list(
  "hf" = mod_spec_standard,
  "math" = mod_spec_standard,
  "matrix" = mod_spec_standard,
  "mrot" = mod_spec_2pl,
  "sds" = mod_spec_2pl,
  "tom" = mod_spec_lang,
  "trog" = mod_spec_standard,
  "vocab" = mod_spec_lang
)

params <- imap(corpus_tasks, \(mod_spec, task) {
  prep_fun <- if (class(mod_spec) == "function") prep_task_lang_params else prep_task_params
  prep_fun(task, mod_spec)
}) |>
  list_rbind(names_to = "item_task") |>
  relocate(language, .after = item_task)

# airtable table target
param_table <- airtable(table = "parameters", base = "appe2p0S3xk4DL2qc")

# upload params for all corpus tasks to airtable
insert_records(params, param_table, typecast = TRUE)
