library(tidyverse)
library(ggridges)
library(scales)

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

# Import the null distributions and observed differences
null_distributions <- read.csv(file.path(results_dir, "AUC_permutation_null_distributions.csv"))
observed_differences <- read.csv(file.path(results_dir, "AUC_permutation_observed_differences.csv"))

# Convert to long format for plotting
perm_diff_df <- null_distributions %>%
  pivot_longer(cols = everything(),
               names_to = "Comparison",
               values_to = "AUC_Diff") %>%
  mutate(Comparison = factor(Comparison, levels = c("FUND_vs_IM", "FUND_vs_ALL", "IM_vs_ALL")))

# Prepare observed differences with significance formatting
observed_diff_df <- observed_differences %>%
  mutate(
    Comparison = factor(Comparison, levels = c("FUND_vs_IM", "FUND_vs_ALL", "IM_vs_ALL")),
    Sig_Level = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      P_Value < 0.1 ~ "\u2020",
      TRUE ~ "ns"
    ),
    Sig_Category = case_when(
      P_Value < 0.001 ~ "p < 0.001",
      P_Value < 0.01 ~ "p < 0.01",
      P_Value < 0.05 ~ "p < 0.05",
      P_Value < 0.1 ~ "p < 0.1",
      TRUE ~ "p >= 0.1"
    ),
    P_Value_Text = case_when(
      P_Value < 0.001 ~ "p < 0.001",
      TRUE ~ sprintf("p = %.3f", P_Value)
    ),
    Point_Color = case_when(
      P_Value < 0.001 ~ "#d73027",
      P_Value < 0.01 ~ "#f46d43",
      P_Value < 0.05 ~ "#fdae61",
      P_Value < 0.1 ~ "#fee08b",
      TRUE ~ "black"
    ),
    # Short labels per advisor feedback
    Comparison_Label = case_when(
      Comparison == "FUND_vs_IM" ~ "Fund vs IMs",
      Comparison == "FUND_vs_ALL" ~ "Fund vs Comb",
      Comparison == "IM_vs_ALL" ~ "IMs vs Comb"
    )
  )

# Calculate quantiles for significance lines
quantiles_df <- perm_diff_df %>%
  group_by(Comparison) %>%
  summarise(
    p01 = quantile(AUC_Diff, 0.01),
    p99 = quantile(AUC_Diff, 0.99),
    median = median(AUC_Diff)
  )

# Merge for annotation positioning
annotation_df <- observed_diff_df %>%
  left_join(quantiles_df, by = "Comparison") %>%
  mutate(
    annotation_x = case_when(
      Observed_Difference > 0 ~ Observed_Difference + 0.005,
      TRUE ~ Observed_Difference - 0.005
    ),
    annotation_y = as.numeric(Comparison) - 0.2,
    annotation_text = sprintf("%s %s", P_Value_Text, Sig_Level)
  )

# Original teal/green color palette for ridge distributions
dist_colors <- c("#2c7fb8", "#7fcdbb", "#c7e9b4")

# Create ridge plot
ridge_plot <- ggplot(perm_diff_df, aes(x = AUC_Diff, y = Comparison)) +
  # Main density ridges
  geom_density_ridges(
    aes(fill = Comparison),
    alpha = 0.7,
    quantile_lines = TRUE,
    quantiles = c(0.025, 0.975),
    scale = 0.7,
    vline_color = "white",
    vline_size = 0.8,
    point_size = 0.3,
    point_alpha = 0.6,
    jittered_points = TRUE
  ) +
  # 99% CI lines at bottom of density plots
  geom_segment(
    data = quantiles_df,
    aes(x = p01, xend = p01, y = as.numeric(Comparison) + 0.05, yend = as.numeric(Comparison) - 0.3),
    color = "darkred", linewidth = 1.5, alpha = 0.9
  ) +
  geom_segment(
    data = quantiles_df,
    aes(x = p99, xend = p99, y = as.numeric(Comparison) + 0.05, yend = as.numeric(Comparison) - 0.3),
    color = "darkred", linewidth = 1.5, alpha = 0.9
  ) +
  # Observed differences as points
  geom_point(
    data = observed_diff_df,
    aes(x = Observed_Difference, y = as.numeric(Comparison) - 0.15, color = Point_Color),
    shape = 21, size = 6, stroke = 2, fill = "white"
  ) +
  # Significance annotations
  geom_text(
    data = annotation_df,
    aes(x = annotation_x, y = annotation_y, label = annotation_text),
    size = ANNOTATION_SIZE,
    fontface = "bold",
    family = FONT_FAMILY,
    hjust = ifelse(annotation_df$Observed_Difference > 0, 0, 1),
    vjust = 0.5,
    color = "black"
  ) +
  # Vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8, alpha = 0.8) +
  # Color scales
  scale_fill_manual(values = dist_colors) +
  scale_color_identity() +
  # Axis formatting
  scale_x_continuous(
    limits = c(-0.08, 0.06),
    labels = scales::label_number(accuracy = 0.01),
    expand = c(0.02, 0)
  ) +
  # Short y-axis labels per advisor feedback — on right side
  scale_y_discrete(
    labels = c("FUND_vs_IM"  = "Fund\nvs\nIMs",
               "FUND_vs_ALL" = "Fund\nvs\nComb",
               "IM_vs_ALL"   = "IMs\nvs\nComb"),
    position = "left"
  ) +
  # Labels
  labs(
    x = "AUC Difference",
    y = "Feature Set Comparison",
    title = "Statistical Significance of AUC Differences",
    subtitle = "Permutation test with 95% (white) and 99% (red) CIs",
    caption = "*** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant"
  ) +
  # Shared theme + panel-specific overrides
  theme_classification +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, lineheight = 0.8),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.8)
  )

print(ridge_plot)

ggsave(file.path(fig_dir, "Model_AUC_Diff_Density.png"),
       plot = ridge_plot, width = 8, height = 6, dpi = 300)

# Results table
results_table <- observed_diff_df %>%
  select(Comparison, Observed_Difference, P_Value, Sig_Level, Sig_Category) %>%
  mutate(
    Comparison_Pretty = case_when(
      Comparison == "FUND_vs_IM" ~ "Fund vs IMs",
      Comparison == "FUND_vs_ALL" ~ "Fund vs Comb",
      Comparison == "IM_vs_ALL" ~ "IMs vs Comb"
    ),
    AUC_Diff_Formatted = sprintf("%.4f", Observed_Difference),
    P_Value_Formatted = case_when(
      P_Value < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", P_Value)
    )
  ) %>%
  select(Comparison_Pretty, AUC_Diff_Formatted, P_Value_Formatted, Sig_Level, Sig_Category)

# Print results
cat("\n=== AUC DIFFERENCE ANALYSIS ===\n\n")
print(knitr::kable(results_table,
                   col.names = c("Comparison", "AUC Diff", "P-value", "Sig", "Category"),
                   align = "lcccl"))

cat("\n\n=== INTERPRETATION ===\n")
for(i in 1:nrow(observed_diff_df)) {
  comparison <- observed_diff_df$Comparison_Label[i]
  obs_diff <- observed_diff_df$Observed_Difference[i]
  p_val <- observed_diff_df$P_Value[i]
  sig_level <- observed_diff_df$Sig_Level[i]

  direction <- ifelse(obs_diff > 0, "outperformed", "underperformed")
  magnitude <- case_when(
    abs(obs_diff) > 0.05 ~ "substantially",
    abs(obs_diff) > 0.02 ~ "moderately",
    TRUE ~ "slightly"
  )

  cat(sprintf("%s:\n", comparison))
  cat(sprintf("  AUC Diff: %.4f (%s %s)\n", obs_diff, magnitude, direction))
  cat(sprintf("  Significance: %s (%s)\n", sig_level,
              ifelse(p_val < 0.05, "significant", "not significant")))
  cat("\n")
}
