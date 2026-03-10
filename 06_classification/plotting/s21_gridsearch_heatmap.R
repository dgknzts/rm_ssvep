library(tidyverse)
library(ggplot2)
library(viridis)

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
  BEST_AUC_LABEL   <- 5.5
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

# Read in the binary grid search CSV files and add a column for the feature set
fund_results <- read.csv(file.path(results_dir, "binary_grid_search_results_FUND.csv")) %>% mutate(feature_set = "FUND")
im_results   <- read.csv(file.path(results_dir, "binary_grid_search_results_IM.csv"))   %>% mutate(feature_set = "IM")
all_results  <- read.csv(file.path(results_dir, "binary_grid_search_results_ALL.csv"))  %>% mutate(feature_set = "ALL")

# Combine all grid search results into one data frame
combined_results <- bind_rows(fund_results, im_results, all_results)

# Compute average ROC (AUC) per combination of sigma and C for each feature set
final_summary <- combined_results %>%
  group_by(feature_set, sigma, C) %>%
  summarize(avg_auc = mean(ROC), .groups = "drop")

# Compute the best (max) average AUC for each feature set
best_summary <- final_summary %>%
  group_by(feature_set) %>%
  filter(avg_auc == max(avg_auc)) %>%
  ungroup()

# Define the desired order for facets
desired_facet_order <- c("FUND", "IM", "ALL")

# Convert 'feature_set' to a factor with specified levels
final_summary <- final_summary %>%
  mutate(feature_set = factor(feature_set, levels = desired_facet_order))

best_summary <- best_summary %>%
  mutate(feature_set = factor(feature_set, levels = desired_facet_order))

# Heatmap with publication-ready styling
p_heatmap <- ggplot(final_summary, aes(x = factor(sigma), y = factor(C), fill = avg_auc)) +
  geom_tile(color = "white", linewidth = 0.8, alpha = 0.9) +
  # Overlay optimal parameters with highlighting
  geom_tile(data = best_summary, aes(x = factor(sigma), y = factor(C)),
            fill = NA, color = "#d73027", linewidth = 2.5, alpha = 1) +
  # Faceting with short labels
  facet_wrap(~ feature_set, ncol = 3,
             labeller = labeller(feature_set = c(
               "FUND" = LABEL_FUND,
               "IM"   = LABEL_IM,
               "ALL"  = LABEL_COMB
             ))) +
  # Color scale
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#762a83",
    midpoint = 0.6,
    name = "Average\nAUC",
    limits = c(0.4, 0.8),
    breaks = seq(0.4, 0.8, 0.1),
    labels = function(x) sprintf("%.1f", x),
    guide = guide_colorbar(
      title.position = "left",
      title.hjust = 0.5,
      barwidth = 12,
      barheight = 0.8,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  # Labels
  labs(
    x = expression(bold("RBF Kernel Parameter ("*sigma*")")),
    y = "Regularization\nParameter (C)",
    title = "Hyperparameter Optimization Results",
    subtitle = "Grid search performance across feature sets",
    caption = "Red boxes indicate optimal hyperparameter combinations"
  ) +
  # Only show key sigma values on x-axis to avoid crowding
  scale_x_discrete(
    breaks = c("1e-04", "0.001", "0.01", "0.1"),
    labels = c("1e-4", "0.001", "0.01", "0.1")
  ) +
  # Shared theme + panel-specific overrides
  theme_classification +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 8, b = 0)),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "bottom",
    legend.margin = margin(t = -8)
  )

# Display the heatmap
print(p_heatmap)

# Save plot in multiple formats
ggsave(
  filename = file.path(fig_dir, "GridSearch_heatmap.png"),
  plot = p_heatmap,
  width = 12, height = 5, dpi = 300
)

# Print optimization summary
cat("\n=== HYPERPARAMETER OPTIMIZATION SUMMARY ===\n")
for(fs in desired_facet_order) {
  best_params <- best_summary %>% filter(feature_set == fs)
  fs_name <- case_when(
    fs == "FUND" ~ "Fund",
    fs == "IM" ~ "IMs",
    fs == "ALL" ~ "Comb"
  )

  cat(sprintf("\n%s:\n", fs_name))
  cat(sprintf("  Optimal sigma: %g\n", best_params$sigma))
  cat(sprintf("  Optimal C: %g\n", best_params$C))
  cat(sprintf("  Best AUC: %.4f\n", best_params$avg_auc))
}
