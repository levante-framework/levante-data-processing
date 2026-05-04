library(tidyverse)
library(rlevante)

scoring_dataset <- redivis$organization("levante")$dataset("levante_metadata_scoring:e97h:next")

scoring_models_table <- scoring_dataset$table("scoring_models:t416")
scoring_table <- scoring_models_table$to_tibble()

registry_dir <- scoring_dataset$table("model_registry:rqwv")$to_directory()

mod_basename <- spec[c("item_task", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
scoring_specs <- scoring_table |>
  select(item_task, model_set, subset, itemtype, nfact, invariance) |> as.list() |> transpose()

scoring_mods <- scoring_specs |>
  # map(rlevante:::model_spec_filename)
  map(\(spec) rlevante:::get_model_record(spec, registry_dir))

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

scoring_params <- map(scoring_mods, mod_coefs)

mutate(registry_version = mod_row$redivis_source |> str_extract("(?<=:)v.*?$"),
       itemtype = mod_spec$itemtype,
       groups = list(mod_rec@group_names),
       n_runs = length(mod_rec@runs),
       file_id = mod_row$file_id,
       added_at = mod_row$added_at)

scoring_table$params <- scoring_params
scoring_params <- scoring_table |>
  unnest(params) |>
  select(task_id, item_task, item_uid, difficulty, discrimination = a1, n_responses,
         model_set, subset, itemtype, nfact, invariance) |>
  mutate(across(c(difficulty, discrimination), \(x) round(x, 2))) |>
  distinct() |>
  arrange(item_uid)

params_table <- registry_dataset$table("item_parameters:4cvk")
params_table$update(upload_merge_strategy = "replace")
params_table$upload("scoring_params")$create(scoring_params, if_not_exists = FALSE, rename_on_conflict = TRUE)

