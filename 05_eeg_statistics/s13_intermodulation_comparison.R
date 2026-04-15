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

# Frequencies of interest (update as needed)
f1 <- c(10.8)
f2 <- c(12.3)
f3 <- c(13.5)
f4 <- c(18.3)

# Prepare data
df <- data %>%
  # Categorize frequencies
  mutate(
    freq = case_when(
      Frequency %in% f1 ~ "1",
      Frequency %in% f2 ~ "2",
      Frequency %in% f3 ~ "3",
      Frequency %in% f4 ~ "4",
      TRUE ~ "other"
    ),
    Side = ifelse(Side == 1, "Right", "Left")
  ) %>%
  # Filter only specific frequencies
  filter(freq %in% c("1","2","3","4"))

# Frequency labels for IM components
freq_labels <- c("1" = "f1 + f2",
                 "2" = "f1 + f3",
                 "3" = "f2 + f3", 
                 "4" = "f1 + f2 + f3")

# Filter by ROI channels for each side
df_filtered <- df %>%
  filter(
    (Side == "Right" & Channels %in% ims_channels_right) |
    (Side == "Left" & Channels %in% ims_channels_left)
  ) %>%
  mutate(
    Subject    = as.factor(Subject),
    Channels   = as.factor(Channels),
    Frequency  = as.factor(Frequency),
    Condition  = as.factor(Condition),
    Side       = as.factor(Side),
    freq      = as.factor(freq)
  ) %>%
  filter(!Condition == "Resp 1")

# Set factor levels for Condition if you want a specific order (optional)
df_filtered$Condition <- factor(df_filtered$Condition, levels = c("Resp 2", "Resp 3"))

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

# Effect sizes and model fit
library(MuMIn)
eff_df <- as.data.frame(eff_size(pairwise_results, sigma = sigma(model), edf = df.residual(model)))
r2 <- r.squaredGLMM(model)
print(r2)

# Prepare contrast data
contrast_df <- as.data.frame(summary(pairwise_results$contrasts, infer = TRUE)) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    cohens_d = eff_df$effect.size
  )

plot_data_subject = df_filtered %>%
  group_by(Subject, Condition, freq) %>%
  summarise(mean_SNR = mean(SNR, na.rm = TRUE), .groups = "drop")

# Prepare data for plotting
plot_data <- plot_data_subject %>%
  group_by(Condition, freq) %>%
  summarise(
    overall_mean = mean(mean_SNR, na.rm = TRUE),
    se = sd(mean_SNR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Color palette
condition_colors <- c("Resp 2" = "#d73027", "Resp 3" = "#74add1")

# ---
# D) Plotting
# ---
# Build line_data for significant contrasts
line_data <- contrast_df %>%
  filter(significance != "") %>%
  mutate(
    freq_pair = case_when(
      grepl("1.*2", contrast) ~ "1-2",    # freq1 - freq2
      grepl("1.*3", contrast) ~ "1-3",    # freq1 - freq3  
      grepl("1.*4", contrast) ~ "1-4",    # freq1 - freq4
      grepl("2.*3", contrast) ~ "2-3",    # freq2 - freq3
      grepl("2.*4", contrast) ~ "2-4",    # freq2 - freq4
      grepl("3.*4", contrast) ~ "3-4",    # freq3 - freq4
      TRUE ~ "unknown"
    )
  ) %>%
  mutate(
    midpoint = case_when(
      freq_pair == "1-2" ~ 1.5,    # between pos 1 and 2
      freq_pair == "1-3" ~ 2.0,    # between pos 1 and 3
      freq_pair == "1-4" ~ 2.5,    # between pos 1 and 4
      freq_pair == "2-3" ~ 2.5,    # between pos 2 and 3
      freq_pair == "2-4" ~ 3.0,    # between pos 2 and 4
      freq_pair == "3-4" ~ 3.5,    # between pos 3 and 4
      TRUE ~ 2.5
    ),
    x_start = case_when(
      freq_pair == "1-2" ~ 1.1,
      freq_pair == "1-3" ~ 1.1,
      freq_pair == "1-4" ~ 1.1,
      freq_pair == "2-3" ~ 2.1,
      freq_pair == "2-4" ~ 2.1,
      freq_pair == "3-4" ~ 3.1,
      TRUE ~ 1.1
    ),
    x_end = case_when(
      freq_pair == "1-2" ~ 1.9,
      freq_pair == "1-3" ~ 2.9,
      freq_pair == "1-4" ~ 3.9,
      freq_pair == "2-3" ~ 2.9,
      freq_pair == "2-4" ~ 3.9,
      freq_pair == "3-4" ~ 3.9,
      TRUE ~ 2.9
    ),
    y_position = case_when(
      freq_pair == "1-2" ~ 4.8,    # short span
      freq_pair == "1-3" ~ 5.4,    # medium span
      freq_pair == "1-4" ~ 6.0,    # long span
      freq_pair == "2-3" ~ 4.8,    # short span
      freq_pair == "2-4" ~ 5.4,    # medium span
      freq_pair == "3-4" ~ 4.8,    # short span
      TRUE ~ 4.6
    )
  )

p <- ggplot(plot_data, aes(x = freq, y = overall_mean, color = Condition)) +
  geom_point(
    size = 4.5,
    position = position_dodge(width = 0.3),
    shape = 16,
    alpha = 0.9
  ) +
  geom_errorbar(
    aes(ymin = overall_mean - se, ymax = overall_mean + se),
    position = position_dodge(width = 0.3),
    width = 0.25,
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
      family = "Arial", size = 12,
      angle = 25, hjust = 1,
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
  facet_wrap(~ factor(Condition, labels = c("Resp 2", "Resp 3")),
             nrow = 1, strip.position = "top") +
  
  labs(
    x = "Intermodulations",
    y = "SNR"
  ) +
  
  scale_x_discrete(labels = freq_labels) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 8, 1),
    expand = expansion(mult = c(0, 0.18))
  )

print(p)

# Save
ggsave(
  filename = file.path(fig_dir, "im_comp_enhanced.png"),
  plot = p,
  width = 7,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "im_comp_enhanced.tiff"),
  plot = p,
  width = 7,
  height = 4,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "im_comp_enhanced.svg"),
  plot = p,
  width = 7,
  height = 4,
  bg = "white"
)

# ---
# E) Format statistics for reporting
# ---
library(stringr)

cat("\n--- Intermodulation frequency contrasts ---\n")
for (i in 1:nrow(contrast_df)) {
  r <- contrast_df[i, ]
  p_fmt <- ifelse(r$p.value < 0.001, "p < .001", sprintf("p = %.3f", r$p.value))
  cat(sprintf("%s | %s: b = %.3f, SE = %.3f, t(%.2f) = %.3f, %s, 95%% CI [%.3f, %.3f], d = %.2f\n",
              r$Condition, r$contrast, r$estimate, r$SE, r$df, r$t.ratio,
              p_fmt, r$lower.CL, r$upper.CL, r$cohens_d))
}

