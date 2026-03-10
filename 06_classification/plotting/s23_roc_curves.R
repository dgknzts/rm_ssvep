library(tidyverse)
library(ggplot2)
library(pROC)

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

# Load classification results
summary_data <- read.csv(file.path(results_dir, "binary_classification_summary.csv"))
test_performance <- readRDS(file.path(results_dir, "binary_test_performance.rds"))

# Extract ROC curve data for each dataset
roc_data_list <- list()

for(dataset in names(test_performance)) {
  roc_obj <- test_performance[[dataset]]$roc_curve
  roc_coords <- coords(roc_obj, "all", ret = c("threshold", "specificity", "sensitivity"))

  roc_data_list[[dataset]] <- data.frame(
    dataset = dataset,
    specificity = roc_coords$specificity,
    sensitivity = roc_coords$sensitivity,
    threshold = roc_coords$threshold,
    fpr = 1 - roc_coords$specificity
  )
}

roc_data <- bind_rows(roc_data_list)

# Add AUC values
roc_data <- roc_data %>%
  left_join(
    summary_data %>% select(dataset, test_auc),
    by = "dataset"
  )

# Ensure proper ordering
desired_order <- c("FUND", "IM", "ALL")
roc_data$dataset <- factor(roc_data$dataset, levels = desired_order)

# Short legend labels
simple_labels <- c(LABEL_FUND, LABEL_IM, LABEL_COMB)
names(simple_labels) <- c("FUND", "IM", "ALL")

# Color palette
roc_colors <- PALETTE_3

# Create ROC plot
p_roc <- ggplot(roc_data, aes(x = fpr, y = sensitivity, color = dataset)) +
  # Chance diagonal
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray50", linewidth = 1, alpha = 0.8) +

  # Main ROC curves
  geom_line(linewidth = 1, alpha = 0.9) +

  # Optimal threshold points
  geom_point(data = roc_data %>%
               group_by(dataset) %>%
               slice_max(sensitivity + specificity, n = 1),
             aes(x = fpr, y = sensitivity, color = dataset),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +

  # Color scales
  scale_color_manual(
    values = roc_colors,
    labels = simple_labels,
    name = ""
  ) +

  # Axis formatting
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0.01, 0.01),
    breaks = seq(0, 1, 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0.01, 0.01),
    breaks = seq(0, 1, 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +

  # Labels
  labs(
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    title = "Receiver Operating Characteristic Curves",
    subtitle = "Classification performance across feature sets",
    caption = "Dashed line = chance | Points = optimal thresholds"
  ) +

  # Shared theme + panel-specific overrides
  theme_classification +
  theme(
    # Legend inside plot
    legend.position = c(0.75, 0.25),
    legend.title = element_blank(),
    legend.text = element_text(family = FONT_FAMILY, size = LEGEND_TEXT_SIZE),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.5),
    legend.margin = margin(6, 8, 6, 8),
    # Grid
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank()
  ) +

  # Legend styling
  guides(color = guide_legend(
    override.aes = list(linewidth = 2),
    nrow = 3,
    ncol = 1
  )) +

  # Square aspect ratio
  coord_fixed(ratio = 1)

# Display the plot
print(p_roc)

# Save plot in multiple formats
ggsave(
  filename = file.path(fig_dir, "ROC_curves.png"),
  plot = p_roc,
  width = 8, height = 8, dpi = 300
)

# Performance summary
cat("\n=== ROC CURVE ANALYSIS SUMMARY ===\n")
performance_summary <- summary_data %>%
  arrange(match(dataset, desired_order)) %>%
  mutate(
    dataset_pretty = case_when(
      dataset == "FUND" ~ LABEL_FUND,
      dataset == "IM" ~ LABEL_IM,
      dataset == "ALL" ~ LABEL_COMB,
      TRUE ~ as.character(dataset)
    ),
    performance = case_when(
      test_auc >= 0.9 ~ "Excellent",
      test_auc >= 0.8 ~ "Good",
      test_auc >= 0.7 ~ "Fair",
      test_auc >= 0.6 ~ "Poor",
      TRUE ~ "Fail"
    )
  )

for(i in 1:nrow(performance_summary)) {
  cat(sprintf("\n%s:\n", performance_summary$dataset_pretty[i]))
  cat(sprintf("  Test AUC: %.4f (%s)\n",
              performance_summary$test_auc[i],
              performance_summary$performance[i]))
}

best_model <- performance_summary %>% slice_max(test_auc, n = 1)
cat(sprintf("\nBest: %s (AUC = %.4f)\n", best_model$dataset_pretty[1], best_model$test_auc[1]))
