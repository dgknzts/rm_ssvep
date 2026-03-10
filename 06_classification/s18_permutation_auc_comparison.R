library(tidyverse)
library(pROC)
library(caret)

source("_config.R")

# Load your existing results
data_splits <- readRDS(file.path(results_dir, "binary_data_splits.rds"))
final_models <- readRDS(file.path(results_dir, "binary_final_models.rds"))

# Permutation test function
permutation_auc_test <- function(model1_probs, model2_probs, true_labels, 
                                 model1_name = "Model1", model2_name = "Model2",
                                 n_perm = 1000, seed = 123) {
  
  set.seed(seed)
  
  # Calculate observed AUCs and difference
  obs_auc1 <- as.numeric(auc(roc(true_labels, model1_probs, quiet = TRUE)))
  obs_auc2 <- as.numeric(auc(roc(true_labels, model2_probs, quiet = TRUE)))
  obs_diff <- obs_auc1 - obs_auc2
  
  cat("Comparing", model1_name, "vs", model2_name, "\n")
  cat("Observed AUCs:", model1_name, "=", round(obs_auc1, 4), 
      "|", model2_name, "=", round(obs_auc2, 4), "\n")
  cat("Observed difference:", round(obs_diff, 4), "\n")
  cat("Running", n_perm, "permutations...\n")
  
  # Generate permutation distribution
  perm_diffs <- numeric(n_perm)
  
  for (i in  1:n_perm) {
    # Shuffle the true labels
    perm_labels <- sample(true_labels)
    
    # Calculate AUCs with permuted labels
    perm_auc1 <- as.numeric(auc(roc(perm_labels, model1_probs, quiet = TRUE)))
    perm_auc2 <- as.numeric(auc(roc(perm_labels, model2_probs, quiet = TRUE)))
    perm_diffs[i] <- perm_auc1 - perm_auc2
    
    if (i %% 200 == 0) cat("Completed", i, "permutations\n")
  }
  
  # Calculate p-value (two-tailed)
  p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
  
  # Calculate confidence interval for the difference
  ci_lower <- quantile(perm_diffs, 0.025)
  ci_upper <- quantile(perm_diffs, 0.975)
  
  # Determine significance
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  
  cat("P-value:", round(p_value, 6), "(", significance, ")\n")
  cat("95% CI for null difference: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n\n")
  
  return(list(
    comparison = paste(model1_name, "vs", model2_name),
    observed_auc1 = obs_auc1,
    observed_auc2 = obs_auc2,
    observed_difference = obs_diff,
    p_value = p_value,
    significance = significance,
    null_distribution = perm_diffs,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_permutations = n_perm
  ))
}

# Get test data and model predictions for each dataset
datasets <- c("FUND", "IM", "ALL")
test_predictions <- list()
test_labels <- list()

for (dataset in datasets) {
  test_data <- data_splits[[dataset]]$test_data
  model <- final_models[[dataset]]
  
  # Get predicted probabilities for Class_3
  pred_probs <- predict(model, newdata = test_data, type = "prob")$Class_3
  
  test_predictions[[dataset]] <- pred_probs
  test_labels[[dataset]] <- test_data$EventType
  
  cat("Dataset:", dataset, "| Test size:", nrow(test_data), 
      "| Class distribution:", table(test_data$EventType), "\n")
}


# FUND vs IM
perm_fund_im <- permutation_auc_test(
  test_predictions[["FUND"]], 
  test_predictions[["IM"]], 
  test_labels[["FUND"]],  # Using FUND labels (should be same as IM and ALL)
  "FUND", "IM", 
  n_perm = 1000
)

# FUND vs ALL
perm_fund_all <- permutation_auc_test(
  test_predictions[["FUND"]], 
  test_predictions[["ALL"]], 
  test_labels[["FUND"]],
  "FUND", "ALL", 
  n_perm = 1000
)

# IM vs ALL
perm_im_all <- permutation_auc_test(
  test_predictions[["IM"]], 
  test_predictions[["ALL"]], 
  test_labels[["IM"]],
  "IM", "ALL", 
  n_perm = 1000
)

# Collect all results
permutation_results <- list(
  FUND_vs_IM = perm_fund_im,
  FUND_vs_ALL = perm_fund_all,
  IM_vs_ALL = perm_im_all
)

# Create summary table
summary_table <- data.frame(
  Comparison = c("FUND vs IM", "FUND vs ALL", "IM vs ALL"),
  AUC_Diff = c(perm_fund_im$observed_difference, 
               perm_fund_all$observed_difference, 
               perm_im_all$observed_difference),
  P_Value = c(perm_fund_im$p_value, 
              perm_fund_all$p_value, 
              perm_im_all$p_value),
  Significance = c(perm_fund_im$significance, 
                   perm_fund_all$significance, 
                   perm_im_all$significance),
  CI_Lower = c(perm_fund_im$ci_lower, 
               perm_fund_all$ci_lower, 
               perm_im_all$ci_lower),
  CI_Upper = c(perm_fund_im$ci_upper, 
               perm_fund_all$ci_upper, 
               perm_im_all$ci_upper)
)

# Apply multiple testing correction
summary_table$P_Value_Bonferroni <- p.adjust(summary_table$P_Value, method = "bonferroni")
summary_table$P_Value_FDR <- p.adjust(summary_table$P_Value, method = "BH")

# Add corrected significance
summary_table$Sig_Bonferroni <- case_when(
  summary_table$P_Value_Bonferroni < 0.001 ~ "***",
  summary_table$P_Value_Bonferroni < 0.01 ~ "**",
  summary_table$P_Value_Bonferroni < 0.05 ~ "*",
  TRUE ~ "ns"
)

summary_table$Sig_FDR <- case_when(
  summary_table$P_Value_FDR < 0.001 ~ "***",
  summary_table$P_Value_FDR < 0.01 ~ "**",
  summary_table$P_Value_FDR < 0.05 ~ "*",
  TRUE ~ "ns"
)

# Print summary
cat("=== SUMMARY TABLE ===\n")
print(summary_table)

# Create datasets for plotting
null_distributions <- data.frame(
  FUND_vs_IM = perm_fund_im$null_distribution,
  FUND_vs_ALL = perm_fund_all$null_distribution,
  IM_vs_ALL = perm_im_all$null_distribution
)

observed_differences <- data.frame(
  Comparison = c("FUND_vs_IM", "FUND_vs_ALL", "IM_vs_ALL"),
  Observed_Difference = c(perm_fund_im$observed_difference,
                          perm_fund_all$observed_difference,
                          perm_im_all$observed_difference),
  P_Value = c(perm_fund_im$p_value,
              perm_fund_all$p_value,
              perm_im_all$p_value),
  Significance = c(perm_fund_im$significance,
                   perm_fund_all$significance,
                   perm_im_all$significance)
)

# Save results
write.csv(summary_table, file.path(results_dir, "AUC_permutation_test_summary.csv"), row.names = FALSE)
write.csv(null_distributions, file.path(results_dir, "AUC_permutation_null_distributions.csv"), row.names = FALSE)
write.csv(observed_differences, file.path(results_dir, "AUC_permutation_observed_differences.csv"), row.names = FALSE)
saveRDS(permutation_results, file.path(results_dir, "AUC_permutation_test_detailed.rds"))
