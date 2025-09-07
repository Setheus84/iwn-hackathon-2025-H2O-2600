## IWN 2025 Hackathon
## Extract, Transform and Load the SmartH2O data

## Libraries
library(stringr)
library(dplyr)
library(readr)

## List and extract zip files
zip_files <- list.files(pattern = "zip$", recursive = TRUE, path = "raw-data", full.names = TRUE)

lapply(zip_files, function(z) {
  unzip(z, exdir = dirname(z))
})

## Load and transform consumption data
consumption_files <- list.files(pattern = "consumptions.csv",
                                recursive = TRUE)

meter_reads <- lapply(consumption_files, function(c) {
  town <- dirname(c)
  df <- read_csv(c) |> 
    select(-index) |> 
    mutate(town,
           meter_reading = str_remove(meter_reading, ","),
           meter_reading = as.numeric(meter_reading),
           smart_meter_id = sprintf("%03d", smart_meter_id),
           smart_meter_id = paste0(str_sub(town, 1, 1),
                                   smart_meter_id)) |> 
    rename(timestamp = tst) |> 
    arrange(smart_meter_id, timestamp)
}) |> bind_rows()


## Meta data
## List all files
household_files <- list.files(pattern = "households.csv",
                             recursive = TRUE)

## Load and transform meta data
households <- lapply(household_files, function(h) {
  town <- dirname(h)
  df <- read_csv(h) |>
    select(-index) |>
    mutate(smart_meter_id = sprintf("%03d", smart_meter_id),
           smart_meter_id = paste0(str_sub(town, 1, 1),
                                   smart_meter_id))
}) |> bind_rows()

households_meta <- select(households, 1:13) |> 
  distinct(smart_meter_id, .keep_all = TRUE) |>
  mutate(across(2:13, ~ str_replace(., "\\,", "\\."))) |> 
  mutate(across(c(2:6, 8, 10:13), as.numeric)) |> 
  mutate(residency_type = str_remove(residency_type, "NULL|\\\\N"),
         residency_type = if_else(residency_type == "", NA, residency_type),
         environmental_attitude = str_remove(environmental_attitude, "environmental "),
         environmental_attitude = str_remove(environmental_attitude, "NULL|\\\\N"),
         environmental_attitude = if_else(environmental_attitude == "", NA, environmental_attitude)) |> 
  filter(rowSums(is.na(across(-1))) < 12)

appliance_rating <- households[, c(1, 14:17)] |> 
  distinct(smart_meter_id, .keep_all = TRUE) |> 
  rename(appliance = name) |> 
  filter(appliance != "NULL" & appliance != "\\N") |> 
  mutate(appliance = str_replace(appliance, "_", " "),
         appliance = str_to_title(appliance),
         efficiency = if_else(efficiency == "NULL", NA, efficiency),
         ecomode = as.numeric(ecomode),
         timer = as.numeric(timer))

households_meta <- households_meta |> 
  full_join(appliance_rating)

appliances <- households[, c(1, 18:19)] |> 
  rename(appliance = name.1) |> 
  filter(appliance != "NULL" & appliance != "\\N") |> 
  mutate(appliance = str_replace_all(appliance, "_", " "),
         appliance = str_remove(appliance, " simple"),
         appliance = str_to_title(appliance),
         number = as.numeric(number)) |> 
  group_by(smart_meter_id, appliance) |> 
  distinct()

# Write data to disk
if (!dir.exists("clean_data")) {
  dir.create("clean_data")
}

write_csv(meter_reads, "clean_data/meter_reads.csv")
write_csv(households_meta, "clean_data/houshold_meta_data.csv")
write_csv(appliances, "clean_data/appliances.csv")
