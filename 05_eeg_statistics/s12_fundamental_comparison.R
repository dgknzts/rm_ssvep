# Libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
source("_config.R")

# Load ROI channels from previous script
load(file.path(data_dir, "roi_channels.RData"))

# Read data
data <- read.csv(file.path(data_dir, "pCond_SNR_data_regular.csv"))

# Frequencies of interest
f1 <- c(4.8)
f2 <- c(6)
f3 <- c(7.5)

# Prepare data
df <- data %>%
  # Categorize frequencies
  mutate(
    freq = case_when(
      Frequency %in% f1 ~ "f1",
      Frequency %in% f2 ~ "f2",
      Frequency %in% f3 ~ "f3",
      TRUE ~ "other"
    ),
    Side = ifelse(Side == 1, "Right", "Left")
  ) %>%
  # Filter only fundamental frequencies
  filter(freq %in% c("f1", "f2", "f3"))

# Filter by ROI channels for each side
df_filtered <- df %>%
  filter(
    (Side == "Right" & Channels %in% fundamental_channels_right) |
    (Side == "Left" & Channels %in% fundamental_channels_left)
  ) %>%
  mutate(
    Subject    = as.factor(Subject),
    Channels   = as.factor(Channels),
    Frequency  = as.factor(Frequency),
    Condition  = as.factor(Condition),
    Side       = as.factor(Side)
  ) %>%
  filter(!Condition == "Resp 1")

# Fit LMER model
model <- lmer(
  SNR ~ Condition * freq +
    (1 | Subject) +
    (1 | Subject:Channels),
  data = df_filtered
)

# Estimated marginal means with pairwise comparisons
pairwise_results <- emmeans(model, pairwise ~ freq | Condition, 
                            adjust = "tukey")

# Prepare contrast data
contrast_df <- as.data.frame(summary(pairwise_results$contrasts, infer = TRUE)) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# Prepare data for plotting
plot_data <- df_filtered %>%
  group_by(Subject, Condition, freq) %>%
  summarise(mean_SNR = mean(SNR, na.rm = TRUE), .groups = "drop") %>%
  group_by(Condition, freq) %>%
  summarise(
    overall_mean = mean(mean_SNR, na.rm = TRUE),
    se = sd(mean_SNR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---
# D) Plotting
# ---
# Build line_data for significant contrasts
line_data <- contrast_df %>%
  filter(significance != "") %>%
  mutate(
    # Manually specify the midpoint for each bracket
    midpoint = c(2, 2.5) # resp 2 contrasts
  )

line_data <- line_data %>%
  mutate(
    x_start = c(1.1, 2.1),
    x_end = c(2.9, 2.9),
    y_position = c(1, 0) * 0.25 + 5
  )

# Color palette
condition_colors <- c("Resp 2" = "#d73027", "Resp 3" = "#74add1")

p <- ggplot(plot_data, aes(x = freq, y = overall_mean, color = Condition)) +
  geom_point(
    size = 4.5, 
    position = position_dodge(width = 0.3),
    shape = 16,
    alpha = 0.9
  ) + 
  geom_errorbar(
    aes(ymin = overall_mean - se, ymax = overall_mean + se),
    width = 0.25, 
    position = position_dodge(width = 0.3),
    linewidth = 1,
    alpha = 0.8
  ) +
  
  # Significance annotations
  geom_text(
    data = line_data,
    aes(
      x = midpoint,
      y = y_position + 0.02,
      label = significance
    ),
    inherit.aes = FALSE,
    size = 5,
    color = "black",
    fontface = "bold",
    family = "Arial"
  ) +
  geom_segment(
    data = line_data,
    aes(
      x = x_start, xend = x_end,
      y = y_position, yend = y_position
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.8
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 18,
      family = "Arial", margin = margin(b = 15)
    ),
    
    axis.title.x = element_text(
      face = "bold", family = "Arial", size = 15,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      face = "bold", family = "Arial", size = 15,
      margin = margin(r = 10)
    ),
    axis.text = element_text(
      family = "Arial", color = "black", size = 13
    ),
    axis.text.x = element_text(
      family = "Arial", size = 13,
      margin = margin(t = 5)
    ),
    
    strip.text = element_text(
      face = "bold", family = "Arial", size = 15,
      color = "black", margin = margin(4, 4, 4, 4)
    ),
    strip.background = element_rect(
      fill = "gray95", color = "gray60", linewidth = 0.8
    ),
    
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.8),
    
    plot.margin = margin(15, 15, 15, 15)
  ) +
  
  scale_color_manual(values = condition_colors) +
  facet_wrap(~ factor(Condition),
             nrow = 1, strip.position = "top") +
  
  labs(
    x = "Fundamentals",
    y = "SNR"
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 8, 1),
    expand = expansion(mult = c(0, 0.15))
  )

print(p)

# Save
ggsave(
  filename = file.path(fig_dir, "fund_comparison_enhanced.png"),
  plot = p,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "fund_comparison_enhanced.tiff"),
  plot = p,
  width = 7,
  height = 4,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "fund_comparison_enhanced.svg"),
  plot = p,
  width = 8,
  height = 5,
  bg = "white"
)

# ---
# E) Format statistics for reporting
# ---
library(glue)

significant_contrasts_reporting <- contrast_df %>%
  filter(p.value < 0.05) %>%
  mutate(
    report_stats = sprintf(
      "b = %.3f, t(%.2f) = %.3f, p = %.3f, 95%% CI [%.3f, %.3f]",
      estimate, df, t.ratio, p.value, lower.CL, upper.CL
    ),
    p_value_formatted = case_when(
      p.value < 0.001 ~ "p < .001",
      p.value < 0.01  ~ sprintf("p = %.3f", p.value),
      p.value < 0.05  ~ sprintf("p = %.3f", p.value),
      TRUE            ~ sprintf("p = %.3f", p.value)
    ),
    report_stats_p_formatted = sprintf(
      "b = %.3f, t(%.2f) = %.3f, %s, 95%% CI [%.3f, %.3f]",
      estimate, df, t.ratio, p_value_formatted, lower.CL, upper.CL
    )
  ) %>%
  select(Condition, contrast, report_stats_p_formatted)

if (nrow(significant_contrasts_reporting) > 0) {
  cat("\n--- Fundamental frequency analysis results ---\n")
  for (i in 1:nrow(significant_contrasts_reporting)) {
    row <- significant_contrasts_reporting[i, ]
    cat(sprintf("Condition: %s | Comparison: %s\n", row$Condition, row$contrast))
    cat(sprintf("  Statistics: %s\n\n", row$report_stats_p_formatted))
  }
} else {
  cat("\nNo significant (p < 0.05) pairwise comparisons found.\n")
}
