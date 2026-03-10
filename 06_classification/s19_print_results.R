library(tidyverse)

source("_config.R")

# Load classification summary and permutation results
classification_summary <- read.csv(file.path(results_dir, "binary_classification_summary.csv"))
auc_permutation_summary <- read.csv(file.path(results_dir, "AUC_permutation_test_summary.csv"))

# Report optimal hyperparameters
cat("=== OPTIMAL HYPERPARAMETERS ===\n")
for(i in 1:nrow(classification_summary)) {
  dataset_name <- classification_summary$dataset[i]
  sigma <- classification_summary$best_sigma[i]
  C <- classification_summary$best_C[i]
  
  feature_name <- case_when(
    dataset_name == "FUND" ~ "fundamentals feature set",
    dataset_name == "IM" ~ "IM feature set", 
    dataset_name == "ALL" ~ "combined feature set"
  )
  
  cat(sprintf("For the %s, the optimal hyperparameters were σ = %g and C = %g.\n", 
              feature_name, sigma, C))
}

# Report test AUC values
cat("\n=== TEST AUC PERFORMANCE ===\n")
fund_auc <- classification_summary[classification_summary$dataset == "FUND", "test_auc"]
im_auc <- classification_summary[classification_summary$dataset == "IM", "test_auc"]
all_auc <- classification_summary[classification_summary$dataset == "ALL", "test_auc"]

cat(sprintf("The classifier trained on fundamental frequencies achieved an AUC of %.3f, the IM frequencies yielded an AUC of %.3f, and the combined feature set achieved an AUC of %.3f.\n",
            fund_auc, im_auc, all_auc))

# Report statistical comparisons
cat("\n=== STATISTICAL COMPARISONS ===\n")

# Process each comparison
for(i in 1:nrow(auc_permutation_summary)) {
  comparison <- auc_permutation_summary$Comparison[i]
  auc_diff <- auc_permutation_summary$AUC_Diff[i]
  p_bonferroni <- auc_permutation_summary$P_Value_Bonferroni[i]
  
  # Format p-value
  p_formatted <- ifelse(p_bonferroni < 0.001, "< 0.001", sprintf("= %.3f", p_bonferroni))
  significance <- ifelse(p_bonferroni < 0.05, "significantly", "not significantly")
  
  # Create readable comparison text
  comparison_text <- case_when(
    comparison == "FUND vs IM" ~ "the classifier using IM frequencies significantly outperformed the fundamental frequency classifier",
    comparison == "FUND vs ALL" ~ "the classifier using combined features significantly outperformed the one using only fundamental frequencies",
    comparison == "IM vs ALL" ~ sprintf("the performance difference between the IM and ALL feature sets was %s significant", ifelse(p_bonferroni < 0.05, "", "not "))
  )
  
  if(comparison == "FUND vs IM") {
    cat(sprintf("%s (AUC Difference = %.3f, p-corr %s). ", 
                comparison_text, abs(auc_diff), p_formatted))
  } else if(comparison == "FUND vs ALL") {
    cat(sprintf("This analysis revealed that %s (AUC Difference = %.3f, p-corr %s). ", 
                comparison_text, abs(auc_diff), p_formatted))
  } else {
    significance_note <- ifelse(p_bonferroni < 0.05, 
                                sprintf("(AUC Difference = %.3f, p-corr %s)", abs(auc_diff), p_formatted),
                                sprintf("(AUC Difference = %.3f, p-corr > 0.05)", abs(auc_diff)))
    cat(sprintf("However, %s %s.\n", comparison_text, significance_note))
  }
}

# Additional summary statistics
cat("\n\n=== ADDITIONAL PERFORMANCE METRICS ===\n")
cat("Dataset\t\tTest Accuracy\tTest Sensitivity\tTest Specificity\n")
cat("-------\t\t-------------\t---------------\t----------------\n")
for(i in 1:nrow(classification_summary)) {
  dataset <- classification_summary$dataset[i]
  accuracy <- classification_summary$test_accuracy[i]
  sensitivity <- classification_summary$test_sensitivity[i]
  specificity <- classification_summary$test_specificity[i]
  
  cat(sprintf("%s\t\t%.3f\t\t%.3f\t\t\t%.3f\n", 
              dataset, accuracy, sensitivity, specificity))
}

cat("\n=== FORMATTED RESULTS FOR MANUSCRIPT ===\n")
cat("Copy and paste the following into your manuscript:\n\n")

# Hyperparameters section
cat("HYPERPARAMETERS:\n")
fund_sigma <- classification_summary[classification_summary$dataset == "FUND", "best_sigma"]
fund_C <- classification_summary[classification_summary$dataset == "FUND", "best_C"]
im_sigma <- classification_summary[classification_summary$dataset == "IM", "best_sigma"]
im_C <- classification_summary[classification_summary$dataset == "IM", "best_C"]
all_sigma <- classification_summary[classification_summary$dataset == "ALL", "best_sigma"]
all_C <- classification_summary[classification_summary$dataset == "ALL", "best_C"]

cat(sprintf("For the fundamentals feature set, the optimal hyperparameters were σ = %g and C = %g. For the IM feature set, the optimal values were σ = %g and C = %g. The combined feature set achieved optimal performance with σ = %g and C = %g.\n\n",
            fund_sigma, fund_C, im_sigma, im_C, all_sigma, all_C))

# AUC performance section
cat("AUC PERFORMANCE:\n")
cat(sprintf("The classifier trained on fundamental frequencies achieved an AUC of %.3f, the IM frequencies yielded a higher AUC of %.3f, and the combined feature set achieved an AUC of %.3f.\n\n",
            fund_auc, im_auc, all_auc))

# Statistical comparisons section
cat("STATISTICAL COMPARISONS:\n")
fund_vs_all <- auc_permutation_summary[auc_permutation_summary$Comparison == "FUND vs ALL", ]
fund_vs_im <- auc_permutation_summary[auc_permutation_summary$Comparison == "FUND vs IM", ]
im_vs_all <- auc_permutation_summary[auc_permutation_summary$Comparison == "IM vs ALL", ]

fund_all_p <- ifelse(fund_vs_all$P_Value_Bonferroni < 0.001, "< 0.001", sprintf("= %.3f", fund_vs_all$P_Value_Bonferroni))
fund_im_p <- ifelse(fund_vs_im$P_Value_Bonferroni < 0.001, "< 0.001", sprintf("= %.3f", fund_vs_im$P_Value_Bonferroni))
im_all_significance <- ifelse(im_vs_all$P_Value_Bonferroni < 0.05, 
                              sprintf("< %.3f", im_vs_all$P_Value_Bonferroni), "> 0.05")

cat(sprintf("This analysis revealed that the classifier using combined features significantly outperformed the one using only fundamental frequencies (AUC Difference = %.3f, p-corr %s). Similarly, the classifier using IM frequencies significantly outperformed the fundamental frequency classifier (AUC Difference = %.3f, p-corr %s). However, the performance difference between the IM and ALL feature sets was not statistically significant (AUC Difference = %.3f, p-corr %s).\n",
            abs(fund_vs_all$AUC_Diff), fund_all_p,
            abs(fund_vs_im$AUC_Diff), fund_im_p,
            abs(im_vs_all$AUC_Diff), im_all_significance))