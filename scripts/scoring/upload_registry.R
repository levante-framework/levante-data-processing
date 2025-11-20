library(redivis)
library(purrr)
library(stringr)
library(glue)

registry_table <- redivis$organization("levante")$dataset("levante-metadata-scoring:e97h", version = "next")$table("model_registry:rqwv")

regdir <- "02_scoring_outputs/model_registry"
regfiles <- list(name = list.files(regdir, recursive = TRUE),
                 path = list.files(regdir, recursive = TRUE, full.names = TRUE)) |>
  transpose()

upload_registry <- \(regfiles) {
  registry_table$add_files(files = regfiles)
}

upload_registry_task <- \(regfiles, task) {
  task_files <- regfiles |> keep(\(rf) str_detect(rf$name, glue("^{task}/")))
  registry_table$add_files(files = task_files)
}
