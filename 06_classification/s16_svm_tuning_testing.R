library(tidyverse)
library(caret)
library(kernlab)
library(pROC)
library(ROSE)
library(doParallel)
library(foreach)

source("_config.R")

# Load and verify required packages
load_required_packages <- function() {
  required_packages <- c("tidyverse", "caret", "kernlab", "pROC", "ROSE", 
                        "doParallel", "foreach", "themis")
  
  missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
  
  if(length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), 
               "\nPlease install them using: install.packages(c('", 
               paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  # Load packages
  lapply(required_packages, library, character.only = TRUE)
}

# Safe execution wrapper
safe_execute <- function(expr, error_msg = "An error occurred") {
  tryCatch({
    expr
  }, error = function(e) {
    cat("\n[ERROR]", error_msg, "\n")
    cat("Details:", e$message, "\n")
    stop(e)
  })
}

# Initialize script
cat("=== SVM Binary Classification ===\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

safe_execute(load_required_packages(), "Failed to load required packages")

# Setup parallel processing
setup_parallel <- function(max_cores = detectCores() - 1) {
  n_cores <- min(detectCores() - 1, max_cores)
  
  if(n_cores > 1) {
    cat("Setting up parallel processing with", n_cores, "cores...\n")
    
    tryCatch({
      cl <- makeCluster(n_cores, outfile = "parallel_log.txt")
      registerDoParallel(cl)
      
      cat("Cluster created. Testing parallel backend...\n")

      test_result <- tryCatch({
        foreach(i = 1:2, .combine = 'c') %dopar% { Sys.getpid() }
      }, error = function(e) {
        cat("Parallel test error:", e$message, "\n")
        return(NULL)
      })
      
      if(is.null(test_result) || length(test_result) != 2) {
        cat("Parallel processing test failed. Falling back to sequential mode.\n")
        stopCluster(cl)
        registerDoSEQ()
        return(list(cluster = NULL, enabled = FALSE, cores = 1))
      }
      
      cat("Parallel processing initialized with", n_cores, "cores\n\n")
      return(list(cluster = cl, enabled = TRUE, cores = n_cores))
      
    }, error = function(e) {
      cat("Failed to setup parallel processing:", e$message, "\n")
      cat("Falling back to sequential mode.\n\n")
      return(list(cluster = NULL, enabled = FALSE, cores = 1))
    })
    
  } else {
    cat("Running in sequential mode (single core)\n\n")
    return(list(cluster = NULL, enabled = FALSE, cores = 1))
  }
}

parallel_setup <- setup_parallel()

# Setup directories
setup_directories <- function() {
  data_path <- data_dir
  output_path <- results_dir

  if(!dir.exists(data_path)) {
    stop("Data directory not found: ", data_path, ". Please check _config.R.")
  }

  if(!dir.exists(output_path)) {
    cat("Creating output directory:", output_path, "\n")
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  cat("Data directory:", data_path, "\n")
  cat("Output directory:", output_path, "\n\n")

  return(list(data_path = data_path, output_path = output_path))
}

directories <- safe_execute(setup_directories(), "Failed to setup directories")

# Check for existing output files (backup before overwriting)
check_existing_files <- function(output_dir) {
  output_files <- c(
    "binary_classification_summary.csv",
    "binary_final_models.rds",
    "binary_test_performance.rds",
    "binary_data_splits.rds",
    "binary_grid_search_results_FUND.csv",
    "binary_grid_search_results_IM.csv",
    "binary_grid_search_results_ALL.csv"
  )
  
  output_file_paths <- file.path(output_dir, output_files)
  existing_files <- output_files[file.exists(output_file_paths)]
  
  if(length(existing_files) > 0) {
    cat("WARNING: The following output files already exist in", output_dir, ":\n")
    cat(paste("-", existing_files), sep = "\n")
    
    response <- readline(prompt = "Do you want to overwrite them? (yes/no): ")
    if(tolower(response) != "yes") {
      stop("Script aborted to prevent overwriting existing files.")
    }
    
    backup_dir <- file.path(output_dir, paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(backup_dir, showWarnings = FALSE)
    
    for(file in existing_files) {
      file.copy(file.path(output_dir, file), file.path(backup_dir, file))
    }
    cat("Existing files backed up to:", backup_dir, "\n\n")
  }
}

if(interactive()) {
  check_existing_files(directories$output_path)
}

# Load data
load_and_validate_data <- function() {
  data_file <- file.path(data_dir, "FFT_Results_regular_wide_RawFFT.csv")

  if(!file.exists(data_file)) {
    stop(paste("Data file not found:", data_file))
  }

  cat("Loading data from:", data_file, "\n")
  data <- safe_execute(
    read.csv(data_file),
    paste("Failed to read data file:", data_file)
  )
  
  cat("Data loaded:", nrow(data), "x", ncol(data), "\n")

  required_cols <- c("EventType", "Subject", "Trial")
  missing_cols <- setdiff(required_cols, names(data))
  
  if(length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(data)
}

data <- load_and_validate_data()

# Data preprocessing
preprocess_data <- function(data) {
  cat("\nPreprocessing data...\n")
  
  # Extract numeric event type from string (e.g., "S2" -> 2)
  data$EventType <- safe_execute(
    as.numeric(substr(as.character(data$EventType), 2, 2)),
    "Failed to extract EventType"
  )

  available_classes <- unique(data$EventType)
  cat("Available classes:", paste(available_classes, collapse = ", "), "\n")
  
  if(!all(c(2, 3) %in% available_classes)) {
    stop("Classes 2 and 3 not found in the data")
  }
  
  # Binary classification: Resp 2 vs Resp 3
  data <- data %>% filter(EventType %in% c(2, 3))
  cat("Filtered for classes 2 and 3:", nrow(data), "rows\n")
  
  if(nrow(data) == 0) {
    stop("No data remaining after filtering for classes 2 and 3")
  }
  
  data$EventType <- factor(data$EventType, levels = c(2, 3))
  levels(data$EventType) <- c("Class_2", "Class_3")
  
  class_dist <- table(data$EventType)
  cat("\nClass distribution:\n")
  print(class_dist)
  
  if(any(class_dist < 10)) {
    warning("Very few samples in one or more classes. Results may be unreliable.")
  }
  
  return(data)
}

data <- preprocess_data(data)

# Create feature datasets (FUND, IM, ALL)
create_feature_datasets <- function(data) {
  cat("\nCreating feature datasets...\n")
  
  id_cols <- c("Subject", "Trial", "EventType")
  
  fund_cols <- grep("_(4\\.8|6(\\.0)?|7\\.5)$", names(data), value = TRUE)
  im_cols <- grep("_(10\\.8|12\\.3|13\\.5|18\\.3)$", names(data), value = TRUE)
  
  cat("Found", length(fund_cols), "fundamental frequency columns\n")
  cat("Found", length(im_cols), "intermodulation frequency columns\n")
  
  if(length(fund_cols) == 0 || length(im_cols) == 0) {
    stop("No frequency columns found. Check column naming pattern.")
  }
  
  data_fund <- data %>% select(all_of(id_cols), all_of(fund_cols)) %>% drop_na()
  data_im <- data %>% select(all_of(id_cols), all_of(im_cols)) %>% drop_na()
  data_all <- data %>% select(all_of(id_cols), all_of(c(fund_cols, im_cols))) %>% drop_na()
  
  cat("\nDataset sizes (after NA removal):\n")
  cat("- FUND:", nrow(data_fund), "rows,", ncol(data_fund) - 4, "features\n")
  cat("- IM:", nrow(data_im), "rows,", ncol(data_im) - 4, "features\n")
  cat("- ALL:", nrow(data_all), "rows,", ncol(data_all) - 4, "features\n")
  
  min_samples <- 50
  if(any(c(nrow(data_fund), nrow(data_im), nrow(data_all)) < min_samples)) {
    warning(paste("Very few samples remaining after NA removal.",
                  "Consider checking data quality."))
  }
  
  return(list(FUND = data_fund, IM = data_im, ALL = data_all))
}

datasets <- create_feature_datasets(data)

# Hyperparameter grid
sigma_values <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5)
C_values <- c(0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000)
tune_grid <- expand.grid(sigma = sigma_values, C = C_values)
cat("\nHyperparameter grid size:", nrow(tune_grid), "combinations\n")

# Stratified train/validation/test split
create_stratified_splits <- function(data, train_prop = 0.6, val_prop = 0.2) {
  set.seed(123)
  
  model_data <- data %>% select(-Subject, -Trial)
  
  min_class_size <- min(table(model_data$EventType))
  if(min_class_size < 10) {
    warning("Very small class size detected. Stratification may fail.")
  }
  
  train_index <- createDataPartition(model_data$EventType, p = train_prop, list = FALSE)
  train_data <- model_data[train_index, ]
  temp_data <- model_data[-train_index, ]
  
  val_prop_adjusted <- val_prop / (1 - train_prop)
  val_index <- createDataPartition(temp_data$EventType, p = val_prop_adjusted, list = FALSE)
  val_data <- temp_data[val_index, ]
  test_data <- temp_data[-val_index, ]
  
  splits <- list(train = train_data, validation = val_data, test = test_data)
  
  for(split_name in names(splits)) {
    split_data <- splits[[split_name]]
    if(nrow(split_data) == 0) {
      stop(paste("Empty", split_name, "set created. Check data size."))
    }
    if(length(unique(split_data$EventType)) < 2) {
      stop(paste(split_name, "set missing one or more classes."))
    }
  }
  
  return(splits)
}

# Custom summary: ROC + accuracy
customSummary <- function(data, lev = NULL, model = NULL) {
  two_class_results <- twoClassSummary(data, lev, model)
  accuracy <- mean(data$pred == data$obs)
  c(two_class_results, Accuracy = accuracy)
}

# Train controls
trControl_tune <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = customSummary,
  sampling = "smote",
  savePredictions = "final",
  allowParallel = parallel_setup$enabled
)

trControl_final <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = customSummary,
  allowParallel = parallel_setup$enabled
)

# Process a single dataset: tune, validate, train final, evaluate on test
process_dataset <- function(ds_name, dataset, tune_grid, trControl_tune, trControl_final, output_dir) {
  cat("\n[", ds_name, "] Processing...\n", sep = "")

  splits <- safe_execute(
    create_stratified_splits(dataset),
    paste("Failed to create splits for", ds_name)
  )
  
  cat("[", ds_name, "] Train:", nrow(splits$train), 
      "| Val:", nrow(splits$validation), 
      "| Test:", nrow(splits$test), "\n", sep = "")
  
  cat("[", ds_name, "] Tuning hyperparameters...\n", sep = "")
  
  set.seed(123)
  tune_model <- safe_execute(
    train(
      EventType ~ .,
      data = splits$train,
      method = "svmRadial",
      preProcess = c("center", "scale"),
      trControl = trControl_tune,
      tuneGrid = tune_grid,
      metric = "ROC"
    ),
    paste("Failed to tune model for", ds_name)
  )
  
  best_params <- tune_model$bestTune
  cat("[", ds_name, "] Best parameters: sigma =", best_params$sigma, 
      ", C =", best_params$C, "\n", sep = "")
  
  val_pred <- predict(tune_model, newdata = splits$validation, type = "prob")
  val_roc <- roc(splits$validation$EventType, val_pred$Class_3, quiet = TRUE)
  val_auc <- auc(val_roc)
  
  cat("[", ds_name, "] Validation AUC:", round(val_auc, 4), "\n", sep = "")
  
  # Final model on train+validation, evaluate on test
  train_val_data <- rbind(splits$train, splits$validation)
  
  set.seed(123)
  final_model <- safe_execute(
    train(
      EventType ~ .,
      data = train_val_data,
      method = "svmRadial",
      preProcess = c("center", "scale"),
      trControl = trControl_final,
      tuneGrid = best_params,
      metric = "ROC"
    ),
    paste("Failed to train final model for", ds_name)
  )
  
  test_pred_prob <- predict(final_model, newdata = splits$test, type = "prob")
  test_pred_class <- predict(final_model, newdata = splits$test)
  
  test_roc <- roc(splits$test$EventType, test_pred_prob$Class_3, quiet = TRUE)
  test_auc <- auc(test_roc)
  test_accuracy <- mean(test_pred_class == splits$test$EventType)
  
  cm <- confusionMatrix(test_pred_class, splits$test$EventType, positive = "Class_3")
  test_sensitivity <- cm$byClass["Sensitivity"]
  test_specificity <- cm$byClass["Specificity"]
  
  cat("[", ds_name, "] Test AUC:", round(test_auc, 4), 
      "| Accuracy:", round(test_accuracy, 4), "\n", sep = "")
  
  result_summary <- tibble(
    dataset = ds_name,
    best_sigma = best_params$sigma,
    best_C = best_params$C,
    cv_auc = max(tune_model$results$ROC),
    val_auc = as.numeric(val_auc),
    test_auc = as.numeric(test_auc),
    test_accuracy = test_accuracy,
    test_sensitivity = as.numeric(test_sensitivity),
    test_specificity = as.numeric(test_specificity),
    train_size = nrow(splits$train),
    val_size = nrow(splits$validation),
    test_size = nrow(splits$test)
  )
  
  test_perf <- list(
    predictions = test_pred_prob,
    true_labels = splits$test$EventType,
    roc_curve = test_roc
  )
  
  grid_results <- tune_model$results %>%
    arrange(desc(ROC)) %>%
    mutate(dataset = ds_name)
  
  safe_execute(
    write.csv(grid_results, 
              file = file.path(output_dir, paste0("binary_grid_search_results_", ds_name, ".csv")), 
              row.names = FALSE),
    paste("Failed to save grid search results for", ds_name)
  )
  
  cat("[", ds_name, "] Done\n", sep = "")
  
  return(list(
    summary = result_summary,
    model = final_model,
    performance = test_perf,
    splits = list(
      train_val_combined = train_val_data,
      test_data = splits$test,
      original_splits = splits
    )
  ))
}

# --- Main processing ---
cat("\n=== Processing all feature sets ===\n")
start_time <- Sys.time()

results_summary <- list()
final_models <- list()
test_performance <- list()
data_splits <- list()

if(parallel_setup$enabled) {
  cat("Running in parallel mode with", parallel_setup$cores, "cores\n")
  
  clusterEvalQ(parallel_setup$cluster, {
    library(caret)
    library(kernlab)
    library(pROC)
    library(ROSE)
    library(themis)
    library(dplyr)
    library(tibble)
  })
  
  clusterExport(parallel_setup$cluster, 
                c("create_stratified_splits", "tune_grid", 
                  "trControl_tune", "trControl_final", "safe_execute", "directories", "customSummary"),
                envir = environment())
  
  parallel_results <- foreach(
    ds_name = names(datasets),
    dataset = datasets,
    .packages = c("caret", "kernlab", "pROC", "ROSE", "themis", "dplyr", "tibble"),
    .errorhandling = "stop"
  ) %dopar% {
    process_dataset(ds_name, dataset, tune_grid, trControl_tune, trControl_final, directories$output_path)
  }
  
  for(i in seq_along(parallel_results)) {
    ds_name <- names(datasets)[i]
    results_summary[[ds_name]] <- parallel_results[[i]]$summary
    final_models[[ds_name]] <- parallel_results[[i]]$model
    test_performance[[ds_name]] <- parallel_results[[i]]$performance
    data_splits[[ds_name]] <- parallel_results[[i]]$splits
  }
  
} else {
  cat("Running in sequential mode\n")
  
  for(ds_name in names(datasets)) {
    result <- safe_execute(
      process_dataset(ds_name, datasets[[ds_name]], tune_grid, 
                     trControl_tune, trControl_final, directories$output_path),
      paste("Failed to process dataset", ds_name)
    )
    
    results_summary[[ds_name]] <- result$summary
    final_models[[ds_name]] <- result$model
    test_performance[[ds_name]] <- result$performance
    data_splits[[ds_name]] <- result$splits
  }
}

end_time <- Sys.time()
total_time <- end_time - start_time

cat("\n=== Processing completed ===\n")
cat("Total time:", format(total_time), "\n")

# --- Save results ---
cat("\nSaving results to:", directories$output_path, "\n")
final_summary <- bind_rows(results_summary)
safe_execute(
  write.csv(final_summary, file = file.path(directories$output_path, "binary_classification_summary.csv"), row.names = FALSE),
  "Failed to save summary results"
)

safe_execute(
  saveRDS(final_models, file = file.path(directories$output_path, "binary_final_models.rds")),
  "Failed to save models"
)

safe_execute(
  saveRDS(test_performance, file = file.path(directories$output_path, "binary_test_performance.rds")),
  "Failed to save test performance"
)

safe_execute(
  saveRDS(data_splits, file = file.path(directories$output_path, "binary_data_splits.rds")),
  "Failed to save data splits"
)

cat("\n=== Results ===\n")
print(final_summary)

# Cleanup
if(parallel_setup$enabled) {
  stopCluster(parallel_setup$cluster)
  registerDoSEQ()
}

rm(data, datasets, data_splits, test_performance)
gc()

cat("\n=== Done ===\n")
cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")