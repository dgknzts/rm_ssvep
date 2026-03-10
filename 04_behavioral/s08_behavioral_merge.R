library(dplyr)
library(readr)
source("_config.R")

base_dir <- file.path(project_root, "data", "behavioral")
subject_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
all_subjects_data <- data.frame()

for (folder in subject_folders) {
  subject_code <- basename(folder)
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

  subject_data <- lapply(csv_files, function(file) {
    data <- read_csv(file)
    data <- data %>%
      select(stimStartTrigger, amount_response.keys, session, gender, age) %>%
      mutate(
        subject_code = subject_code,
        session = as.character(session)
      ) %>%
      na.omit()
    return(data)
  })

  subject_data_combined <- bind_rows(subject_data)
  all_subjects_data <- bind_rows(all_subjects_data, subject_data_combined)
}

# Demographics summary
unique_subjects <- all_subjects_data %>%
  distinct(subject_code, age, gender)

mean_age <- unique_subjects %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE))

gender_count <- unique_subjects %>%
  count(gender)

print(mean_age)
print(gender_count)

write.csv(all_subjects_data, behavioral_csv, row.names = FALSE)
