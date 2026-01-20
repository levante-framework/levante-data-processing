library(rlevante)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(forcats)
library(rairtable)

registry_table <- fetch_registry_table()

mod_coefs <- \(mod_rec, item_sep = "-") {
  model_vals(mod_rec) |>
    as_tibble() |>
    filter(group != "GROUP", item != "GROUP") |>
    select(group, item, name, value) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(item = str_remove(item, glue("{item_sep}[0-9]+$"))) |>
    select(item_uid = item, d, a1) |>
    distinct() |>
    mutate(difficulty = -d / a1) |>
    arrange(difficulty)
}

mod_spec_standard <- list(model_set = "multigroup_site",
                          subset = "all_items",
                          itemtype = "rasch",
                          nfact = "f1",
                          invariance = "scalar")
mod_spec_2pl <- list(model_set = "multigroup_site",
                     subset = "all_items",
                     itemtype = "2pl",
                     nfact = "f1",
                     invariance = "scalar")

prep_params <- \(item_task, mod_spec) {
  mod_spec$item_task <- item_task
  mod_filename <- model_spec_filename(mod_spec)
  mod_row <- registry_table |> filter(file_name == mod_filename)
  mod_rec <- get_registry_file(mod_filename, registry_table)

  mod_params <- mod_coefs(mod_rec)

  mod_params |>
    select(item_uid, difficulty, discrimination = a1) |>
    mutate(registry_version = mod_row$redivis_source |> str_extract("(?<=:)v.*?$"),
           itemtype = mod_spec$itemtype,
           groups = list(mod_rec@group_names),
           n_runs = length(mod_rec@runs),
           file_id = mod_row$file_id,
           added_at = mod_row$added_at)
}

param_table <- airtable(table = "parameters", base = "appe2p0S3xk4DL2qc")

cat_tasks <- c("math", "matrix", "mrot", "sds", "tom", "trog", "vocab")
walk(cat_tasks, \(task) {
  params <- prep_params(task, mod_spec_standard)
  insert_records(params, param_table)
})
