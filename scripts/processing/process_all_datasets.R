library(redivis)
library(glue)
library(purrr)

datasets <- c(
  "levante_data_example_raw:bm7r",
  "partner_mpib_de_main:7n7w",
  "partner_sparklab_us_downex:4n9e",
  "pilot_langcog_us_downex:a6kb",
  "pilot_uniandes_co_bogota:3j4z",
  "pilot_mpieva_de_main:6c0n",
  "pilot_uniandes_co_rural:66d2",
  "pilot_western_ca_main:97mt"
)

# connect to processing workflow and processing notebook
wf <- redivis$user("levante")$workflow("process_dataset:zr0v")

# update all workflow datasources to current version
wf_ds <- wf$list_datasources()
walk(wf_ds, \(ds) ds$get())
walk(wf_ds, \(ds) ds$update(version = "current"))

# extract workflow datasource that's *not* metadata (i.e. actual dataset)
ds_names <- wf_ds |> map(\(ds) ds$properties$sourceDataset$name)
wf_data <- wf_ds[[which(!str_detect(ds_names, "metadata"))]]

process_dataset <- \(ds_name) {
  message(glue("Processing dataset {ds_name}..."))
  # replace datasource with dataset from given name
  wf_data$update(source_dataset = paste0("levante.", ds_name))
  # run processing notebook
  nb <- wf$notebook("process_dataset")
  nb$run()
}

# process each dataset
walk(datasets, process_dataset)

# run all transforms in combine data workflow (stack processed tables)
wf_stack <- redivis$user("levante")$workflow("combine_data:w841")
wf_trans <- wf_stack$list_transforms()
walk(wf_trans, \(tr) tr$run())
