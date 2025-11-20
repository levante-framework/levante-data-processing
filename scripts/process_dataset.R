library(purrr)
library(redivis)
library(stringr)

# get list of references to all "site" datasets
# TODO: switch to -raw suffix after renaming
get_raw_datasets <- \() {
  all_datasets <- redivis$organization("levante")$list_datasets()
  site_prefixes <- c("pilot", "partner")
  is_raw_dataset <- \(ds) (str_extract(ds$name, "^[A-z]*(?=-)") %in% site_prefixes) & (str_extract(ds$name, "(?<=-)[A-z]*$") != "processed")
  all_datasets |> keep(is_raw_dataset) |> keep(is_raw_dataset)
}

update_workflow_source <- \(wf, raw_dataset) {
  message(glue("Updating workflow raw dataset to {raw_dataset$name}"))
  # get reference to workflow datasource that aren't metadata
  wf_raw_datasource <- wf$list_datasources() |>
    discard(\(ds) str_detect(ds$properties$sourceDataset$name, "metadata")) |>
    pluck(1)

  # TODO: fails if dataset is missing table or other mapping failure
  wf_raw_datasource$update(source_data = raw_dataset)
  return(wf)
}

update_workflow_metadata <- \(wf) {
  message("Updating workflow metadata datasets")
  # get reference to workflow datasources that are metadata
  wf_metadata <- wf$list_datasources() |>
    keep(\(ds) str_detect(ds$properties$sourceDataset$name, "metadata"))

  # update version of metadata datasources to current
  wf_metadata |> walk(\(ds) ds$update(version = "current"))
  return(wf)
}

run_workflow_nodes <- \(wf) {
  message("Running workflow")
  # get list of summary workflow transforms
  transforms <- wf$list_transforms()
  transform_list <- set_names(transforms, map(transforms, \(tr) tr$scoped_reference))

  # get list of summary workflow notebooks
  notebooks <- wf$list_notebooks()
  notebook_list <- set_names(notebooks, map(notebooks, \(tr) tr$scoped_reference))

  # define list of notebooks and transforms to run, in order
  # TODO: could this be done programmatically?
  wf_nodes <- list(notebook_list$`runs:sz8q`,
                   transform_list$`participants:n0c2`,
                   notebook_list$`trials:046p`,
                   notebook_list$`scoring:km04`,
                   transform_list$`runs_scored:zbhg`,
                   notebook_list$`outputs:b0wn`)
  # run nodes
  walk(wf_nodes, \(node) {
    message(glue("Running workflow node {node$name}"))
    node$run()
  })
  return(wf)
}

process_raw_dataset <- \(wf, raw_dataset) {
  wf |>
    update_workflow_source(raw_dataset) |>
    update_workflow_metadata() |>
    run_workflow_nodes()
}

# get reference to process_dataset workflow
wf <- redivis$organization("levante")$workflow("process_dataset:zr0v")

raw_datasets <- get_raw_datasets()
# process_raw_dataset(wf, raw_datasets[[3]])
# walk(raw_datasets, \(ds) process_raw_dataset(wf, ds))

# scoring_table <- get_scoring_table()
#
# get_dataset_tasks <- \(ds) {
#   ds$table("tasks")$to_tibble() |> filter(task_id != "schema_row") |> mutate(dataset = ds$name, .before = everything())
# }
#
# dataset_tasks <- raw_datasets |> map(get_dataset_tasks) |> list_rbind()
#
# irt_tasks <- c("memory-game",
#                "same-different-selection",
#                "egma-math",
#                "hearts-and-flowers",
#                "trog",
#                "theory-of-mind",
#                "vocab",
#                "matrix-reasoning",
#                "mental-rotation")
#
# dataset_tasks |>
#   filter(task_id %in% irt_tasks) |>
#   mutate(dataset = dataset |> str_replace_all("-", "_")) |>
#   anti_join(scoring_table)
