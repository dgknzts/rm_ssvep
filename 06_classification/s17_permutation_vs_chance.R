library(tidyverse)
library(caret)
library(kernlab)
library(pROC)
library(doParallel)
library(foreach)

source("_config.R")

# Setup parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Load saved classification results
summary_data <- read.csv(file.path(results_dir, "binary_classification_summary.csv"))
data_splits <- readRDS(file.path(results_dir, "binary_data_splits.rds"))

# Number of permutations
n_perm <- 200

# Training control for permutations
trControl_perm <- trainControl(
  method = "none", 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

# Function to safely convert factors to proper levels
fix_factor_levels <- function(data, target_var) {
  if (is.factor(data[[target_var]])) {
    levels(data[[target_var]]) <- make.names(levels(data[[target_var]]))
  } else {
    data[[target_var]] <- factor(data[[target_var]])
    levels(data[[target_var]]) <- make.names(levels(data[[target_var]]))
  }
  return(data)
}

# Function to run permutation test for a single dataset
run_permutation_test <- function(dataset_name, n_perm) {
  cat("Processing", dataset_name, "dataset...\n")
  
  # Get data splits
  train_val_data <- data_splits[[dataset_name]]$train_val_combined
  test_data <- data_splits[[dataset_name]]$test_data
  
  # Fix factor levels
  train_val_data <- fix_factor_levels(train_val_data, "EventType")
  test_data <- fix_factor_levels(test_data, "EventType")
  
  # Get original performance
  original_auc <- summary_data[summary_data$dataset == dataset_name, "test_auc"]
  original_accuracy <- summary_data[summary_data$dataset == dataset_name, "test_accuracy"]
  
  # Get best parameters
  best_params <- data.frame(
    sigma = summary_data[summary_data$dataset == dataset_name, "best_sigma"],
    C = summary_data[summary_data$dataset == dataset_name, "best_C"]
  )
  
  # Export data to workers
  clusterExport(cl, c("train_val_data", "test_data", "best_params", "trControl_perm", "fix_factor_levels"),
                envir = environment())
  
  # Run permutations in parallel
  perm_results <- foreach(
    i = 1:n_perm,
    .combine = rbind,
    .packages = c("caret", "kernlab", "pROC", "dplyr"),
    .errorhandling = "remove"
  ) %dopar% {
    
    tryCatch({
      # Create permuted training data
      set.seed(123 + i)
      perm_train_data <- train_val_data
      
      # Permute labels while maintaining factor structure
      event_levels <- levels(perm_train_data$EventType)
      perm_train_data$EventType <- factor(
        sample(as.character(perm_train_data$EventType)), 
        levels = event_levels
      )
      
      # Train model on permuted data
      perm_model <- train(
        EventType ~ .,
        data = perm_train_data,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        trControl = trControl_perm,
        tuneGrid = best_params,
        metric = "ROC"
      )
      
      # Test on original test set
      test_pred_prob <- predict(perm_model, newdata = test_data, type = "prob")
      test_pred_class <- predict(perm_model, newdata = test_data)
      
      # Calculate metrics
      perm_roc <- roc(test_data$EventType, test_pred_prob[, event_levels[2]], quiet = TRUE)
      perm_auc <- as.numeric(auc(perm_roc))
      perm_accuracy <- mean(test_pred_class == test_data$EventType)
      
      c(auc = perm_auc, accuracy = perm_accuracy)
      
    }, error = function(e) {
      cat("Error in permutation", i, ":", e$message, "\n")
      c(auc = NA, accuracy = NA)
    })
  }
  
  # Remove any failed runs
  perm_results <- perm_results[complete.cases(perm_results), ]
  
  if (nrow(perm_results) < n_perm * 0.8) {
    warning(paste("Only", nrow(perm_results), "out of", n_perm, "permutations succeeded for", dataset_name))
  }
  
  # Extract results
  perm_auc <- perm_results[, "auc"]
  perm_accuracy <- perm_results[, "accuracy"]
  
  # Calculate statistics
  auc_p_value <- mean(perm_auc >= original_auc, na.rm = TRUE)
  accuracy_p_value <- mean(perm_accuracy >= original_accuracy, na.rm = TRUE)
  
  auc_95th <- quantile(perm_auc, 0.95, na.rm = TRUE)
  auc_99th <- quantile(perm_auc, 0.99, na.rm = TRUE)
  acc_95th <- quantile(perm_accuracy, 0.95, na.rm = TRUE)
  acc_99th <- quantile(perm_accuracy, 0.99, na.rm = TRUE)
  
  # Determine significance
  auc_sig <- case_when(
    original_auc > auc_99th ~ "p < 0.01 ***",
    original_auc > auc_95th ~ "p < 0.05 **",
    TRUE ~ "n.s."
  )
  
  acc_sig <- case_when(
    original_accuracy > acc_99th ~ "p < 0.01 ***",
    original_accuracy > acc_95th ~ "p < 0.05 **", 
    TRUE ~ "n.s."
  )
  
  cat("Completed", dataset_name, "- AUC p-value:", round(auc_p_value, 4), 
      "Accuracy p-value:", round(accuracy_p_value, 4), "\n")
  
  return(list(
    original_auc = original_auc,
    original_accuracy = original_accuracy,
    auc_null = perm_auc,
    accuracy_null = perm_accuracy,
    auc_p_value = auc_p_value,
    accuracy_p_value = accuracy_p_value,
    auc_significance = auc_sig,
    accuracy_significance = acc_sig,
    n_successful = nrow(perm_results)
  ))
}

# Run permutation tests for all datasets
datasets <- c("FUND", "IM", "ALL")
permutation_results <- list()

for (dataset_name in datasets) {
  permutation_results[[dataset_name]] <- run_permutation_test(dataset_name, n_perm)
}

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Save results
saveRDS(permutation_results, file.path(results_dir, "parallel_permutation_results.rds"))

# Print summary
cat("\n=== Permutation Test Results ===\n")
for (dataset_name in datasets) {
  result <- permutation_results[[dataset_name]]
  cat("\n", dataset_name, "Dataset:\n")
  cat("  Original AUC:", round(result$original_auc, 4), 
      "- Significance:", result$auc_significance, "\n")
  cat("  Original Accuracy:", round(result$original_accuracy, 4), 
      "- Significance:", result$accuracy_significance, "\n")
  cat("  Successful permutations:", result$n_successful, "out of", n_perm, "\n")
}

cat("\nPermutation testing completed successfully!\n")