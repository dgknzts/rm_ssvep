library(tidyverse)
library(ggplot2)

source("_config.R")

# Standalone fallback: define shared constants if not running from master
if(!exists("theme_classification")) {
  FONT_FAMILY     <- "Arial"
  BASE_SIZE        <- 18
  AXIS_TITLE_SIZE  <- 20
  AXIS_TEXT_SIZE   <- 16
  FACET_STRIP_SIZE <- 18
  LEGEND_TITLE_SIZE <- 16
  LEGEND_TEXT_SIZE  <- 15
  ANNOTATION_SIZE  <- 5.5
  LABEL_FUND <- "Fund"; LABEL_IM <- "IMs"; LABEL_COMB <- "Comb"
  PALETTE_3 <- c("#d73027", "#f46d43", "#74add1")

  theme_classification <- theme_classic(base_size = BASE_SIZE) +
    theme(
      text             = element_text(family = FONT_FAMILY, color = "black"),
      axis.title       = element_text(face = "bold", family = FONT_FAMILY, size = AXIS_TITLE_SIZE),
      axis.text        = element_text(family = FONT_FAMILY, color = "black", size = AXIS_TEXT_SIZE),
      strip.text       = element_text(face = "bold", family = FONT_FAMILY, size = FACET_STRIP_SIZE,
                                      color = "black", margin = margin(5, 5, 5, 5)),
      strip.background = element_rect(fill = "gray95", color = "gray60", linewidth = 0.8),
      legend.title     = element_text(face = "bold", family = FONT_FAMILY, size = LEGEND_TITLE_SIZE),
      legend.text      = element_text(family = FONT_FAMILY, size = LEGEND_TEXT_SIZE),
      panel.border     = element_rect(color = "gray60", fill = NA, linewidth = 0.8),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

# Load permutation against chance results
permutation_results <- readRDS(file.path(results_dir, "parallel_permutation_results.rds"))

# Load original classification summary
classification_summary <- read.csv(file.path(results_dir, "binary_classification_summary.csv"))

# Convert permutation results to tidy data frame
perm_df <- bind_rows(lapply(names(permutation_results), function(ds) {
  data.frame(
    Dataset = ds,
    AUC = permutation_results[[ds]]$auc_null
  )
}))

# Set proper ordering for Dataset factor
desired_order <- c("FUND", "IM", "ALL")
perm_df$Dataset <- factor(perm_df$Dataset, levels = desired_order)

# Create original results data frame with significance
orig_results <- data.frame(
  Dataset = classification_summary$dataset,
  AUC = classification_summary$test_auc
) %>%
  left_join(
    bind_rows(lapply(names(permutation_results), function(ds) {
      data.frame(
        Dataset = ds,
        p_value = permutation_results[[ds]]$auc_p_value,
        significance = permutation_results[[ds]]$auc_significance
      )
    })), by = "Dataset"
  ) %>%
  mutate(
    Dataset = factor(Dataset, levels = desired_order),
    sig_symbol = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ "\u2020",
      TRUE ~ "ns"
    ),
    point_color = case_when(
      p_value < 0.001 ~ "#d73027",
      p_value < 0.01 ~ "#f46d43",
      p_value < 0.05 ~ "#fdae61",
      p_value < 0.1 ~ "#fee08b",
      TRUE ~ "#d9d9d9"
    ),
    dataset_label = case_when(
      Dataset == "FUND" ~ LABEL_FUND,
      Dataset == "IM" ~ LABEL_IM,
      Dataset == "ALL" ~ LABEL_COMB
    )
  )

# Calculate summary statistics for annotations
summary_stats <- perm_df %>%
  group_by(Dataset) %>%
  summarise(
    median_auc = median(AUC),
    q75 = quantile(AUC, 0.75),
    max_auc = max(AUC),
    .groups = "drop"
  ) %>%
  left_join(orig_results, by = "Dataset") %>%
  mutate(
    annotation_y = pmax(max_auc, AUC) + 0.02,
    annotation_text = paste0(sig_symbol, "\np = ",
                           ifelse(p_value < 0.001, "< 0.001",
                                  sprintf("%.3f", p_value)))
  )

# Color palette
box_colors <- PALETTE_3

# Create boxplot with violin and significance indicators
p_boxplot <- ggplot(perm_df, aes(x = Dataset, y = AUC)) +
  # Horizontal line at chance level
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = "gray40", linewidth = 0.8, alpha = 0.8) +

  # Boxplots
  geom_boxplot(aes(fill = Dataset),
               outlier.shape = 21, outlier.size = 1.5, outlier.alpha = 0.6,
               alpha = 0.8, linewidth = 0.6, width = 0.6,
               outlier.fill = "white", outlier.stroke = 0.5) +

  # Violin overlay
  geom_violin(aes(fill = Dataset), alpha = 0.3, width = 0.8,
              scale = "width", trim = TRUE) +

  # Original results as points
  geom_point(data = orig_results,
             aes(x = Dataset, y = AUC, color = point_color),
             shape = 21, size = 4.5, stroke = 1.5, fill = "white") +

  # Significance annotations
  geom_text(data = summary_stats,
            aes(x = Dataset, y = annotation_y, label = annotation_text),
            size = ANNOTATION_SIZE, fontface = "bold", family = FONT_FAMILY,
            hjust = 0.5, vjust = 0, color = "black") +

  # Color scales
  scale_fill_manual(values = box_colors) +
  scale_color_identity() +

  # Axis formatting
  scale_y_continuous(
    limits = c(0.45, 0.85),
    breaks = seq(0.45, 0.85, 0.05),
    labels = function(x) sprintf("%.2f", x),
    expand = c(0.02, 0)
  ) +
  scale_x_discrete(labels = c("FUND" = LABEL_FUND,
                              "IM" = LABEL_IM,
                              "ALL" = LABEL_COMB)) +

  # Labels
  labs(
    x = "Feature Set",
    y = "Area Under the Curve (AUC)",
    title = "Permutation Test Against Chance",
    subtitle = "Null AUC distributions with observed performance overlaid",
    caption = "*** p < 0.001, ** p < 0.01, * p < 0.05 | Dashed line = chance (0.5)"
  ) +

  # Shared theme + panel-specific overrides
  theme_classification +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(hjust = 0.5, margin = margin(t = 5))
  )

print(p_boxplot)

# Save plot in multiple formats
ggsave(
  filename = file.path(fig_dir, "Permutation_AUC_Boxplot.png"),
  plot = p_boxplot,
  width = 7, height = 6, dpi = 300
)

# Summary statistics
cat("\n=== PERMUTATION TEST AGAINST CHANCE SUMMARY ===\n")
for (dataset_name in names(permutation_results)) {
  result <- permutation_results[[dataset_name]]
  sig_info <- orig_results %>% filter(Dataset == dataset_name)

  cat(sprintf("\n%s:\n", sig_info$dataset_label))
  cat(sprintf("  Observed AUC: %.4f\n", result$original_auc))
  cat(sprintf("  Significance: %s (p = %s)\n",
              sig_info$sig_symbol,
              ifelse(result$auc_p_value < 0.001, "< 0.001",
                     sprintf("%.3f", result$auc_p_value))))
  cat(sprintf("  Null range: %.4f - %.4f\n",
              min(result$auc_null), max(result$auc_null)))
  cat(sprintf("  Successful permutations: %d/200\n", result$n_successful))
}
