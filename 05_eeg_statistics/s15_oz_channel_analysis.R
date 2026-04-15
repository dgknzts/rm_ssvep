# --- Libraries ---
library(dplyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(patchwork)
library(tidyverse)
source("_config.R")

# --- Global Settings ---
plot_font_family <- "Arial"

# --- Data Loading and Initial Preparation ---
data <- read.csv(file.path(data_dir, "pCond_SNR_data_regular.csv"))

data <- data %>%
  rename(Channel = Channels) %>%
  rename(ConditionGroup = Condition)

# --- Plot C: Line Plot Preparation (Adapted from previous steps) ---

# Define frequencies of interest for Line Plot
f1_line  <- c(4.8)
f2_line  <- c(6)
f3_line  <- c(7.5)
ims_line <- c(10.8, 12.3, 13.5, 18.3)
highlight_freqs_all_line <- sort(c(f1_line, f2_line, f3_line, ims_line))

posterior_roi_line <- c("Oz")

# Filter data for Line Plot
df_line <- data %>%
  filter(Channel %in% posterior_roi_line) %>%
  filter(!ConditionGroup == "Resp 1")

# Aggregate SNR Data for Line Plot
SNR_summary_line <- df_line %>%
  group_by(ConditionGroup, Frequency) %>%
  summarise(
    Mean  = mean(SNR, na.rm = TRUE),
    SD    = sd(SNR, na.rm = TRUE),
    N     = sum(!is.na(SNR)),
    Lower = Mean - 1.96 * SD / sqrt(N),
    Upper = Mean + 1.96 * SD / sqrt(N),
    .groups = 'drop'
  )

# Create labels for highlighted frequencies
highlight_freqs_in_range_line <- highlight_freqs_all_line[highlight_freqs_all_line >= 4.5 & highlight_freqs_all_line <= 18.5]
present_highlight_freqs_line <- intersect(highlight_freqs_in_range_line, unique(SNR_summary_line$Frequency))

# Generate labels dynamically
freq_labels_list_line <- setNames(
  lapply(present_highlight_freqs_line, function(freq) {
    if (freq %in% f1_line) return("f1")
    if (freq %in% f2_line) return("f2")
    if (freq %in% f3_line) return("f3")
    if (freq == 10.8) return("f1+f2")
    if (freq == 12.3) return("f1+f3")
    if (freq == 13.5) return("f2+f3")
    if (freq == 18.3) return("f1+f2+f3")
    return(as.character(freq))
  }),
  as.character(present_highlight_freqs_line)
)
freq_labels_ordered_line <- unlist(freq_labels_list_line[as.character(sort(present_highlight_freqs_line))])

# Create Line Plot (Plot C)
p_line <- ggplot(SNR_summary_line, aes(x = Frequency, y = Mean, color = ConditionGroup)) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.6, linetype = "dotted", alpha = 0.7) +
  geom_vline(
    xintercept = present_highlight_freqs_line, color = "grey75",
    linetype = "dashed", linewidth = 0.5, alpha = 0.9
  ) +
  geom_line(linewidth = 1.5, alpha = 0.8) +
  labs(
    y = "SNR"
  ) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, family = "Arial"),
    legend.background = element_rect(fill = alpha("white", 0.5), color = NA),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Arial"),
    axis.title.x = element_text(size = 16, margin = margin(t = 15), family = "Arial"),
    axis.title.y = element_text(size = 16, margin = margin(r = 15), family = "Arial"),
    axis.text.x = element_text(size = 14, family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6)
  ) +
  scale_y_continuous(breaks = seq(0, 3.5, by = 1)) +
  scale_x_continuous(
    breaks = sort(present_highlight_freqs_line),
    labels = freq_labels_ordered_line
  ) +
  coord_cartesian(
    ylim = c(0, 3.5),
    xlim = c(4.5, 18.5),
    clip = "on"
  ) +
  scale_color_grey(start = 0.65, end = 0.05)

# --- Plot A & B: Bar Plot Preparation ---

# Fundamental frequencies
f1_bar <- c(4.8)
f2_bar <- c(6)
f3_bar <- c(7.5)

# Prepare data for fundamentals (Plot A)
df_bar <- data %>%
  mutate(
    freq = case_when(
      Frequency %in% f1_bar ~ "f1",
      Frequency %in% f2_bar ~ "f2",
      Frequency %in% f3_bar ~ "f3",
      TRUE ~ "other"
    )
  ) %>%
  filter(freq %in% c("f1", "f2", "f3"))

df_filtered_bar <- df_bar %>%
  filter(Channel %in% c("Oz")) %>%
  mutate(
    Subject      = as.factor(Subject),
    Channel      = as.factor(Channel),
    freq         = factor(freq, levels = c("f1", "f2", "f3")),
    Condition    = as.factor(ConditionGroup)
  ) %>%
  filter(Condition %in% c("Resp 2", "Resp 3")) %>%
  filter(!is.na(freq))


# Intermodulation frequencies
imf1_bar <- c(10.8) # f1+f2
imf2_bar <- c(12.3) # f1+f3
imf3_bar <- c(13.5) # f2+f3
imf4_bar <- c(18.3) # f1+f2+f3

# Facet order
imf_order_bar <- c(
  "f1+f2",
  "f1+f3",
  "f2+f3",
  "f1+f2+f3"
)

# Prepare data for IMFs (Plot B)
df_imf_bar <- data %>%
  mutate(
    freq_imf_label = case_when(
      Frequency %in% imf1_bar ~ "f1+f2",
      Frequency %in% imf2_bar ~ "f1+f3",
      Frequency %in% imf3_bar ~ "f2+f3",
      Frequency %in% imf4_bar ~ "f1+f2+f3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(freq_imf_label))

df_imf_filtered_bar <- df_imf_bar %>%
  filter(Channel %in% c("Oz")) %>%
  mutate(
    Subject   = as.factor(Subject),
    Channel   = as.factor(Channel),
    freq_imf  = factor(freq_imf_label, levels = imf_order_bar),
    Condition = as.factor(ConditionGroup)
  ) %>%
  filter(Condition %in% c("Resp 2", "Resp 3")) %>%
  filter(!is.na(freq_imf))


# --- ANALYSIS 1: FUNDAMENTAL FREQUENCIES (Bar Plot A) ---
library(MuMIn)

model_fundamental <- lmer(
  SNR ~ Condition * freq + (1 | Subject),
  data = df_filtered_bar
)
pairwise_fundamental <- emmeans(model_fundamental, pairwise ~ Condition | freq, adjust = "tukey")
eff_fund_df <- as.data.frame(eff_size(pairwise_fundamental, sigma = sigma(model_fundamental), edf = df.residual(model_fundamental)))
print(r.squaredGLMM(model_fundamental))

contrast_df_fundamental_all <- as.data.frame(summary(pairwise_fundamental$contrasts, infer = TRUE)) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01  ~ "**", p.value < 0.05  ~ "*", TRUE ~ ""
    ),
    cohens_d = eff_fund_df$effect.size
  )

# Significant contrasts only (for plot annotations)
contrast_df_fundamental <- contrast_df_fundamental_all %>%
  filter(significance != "") %>%
  filter(grepl("Resp 2 - Resp 3", contrast) | grepl("Resp 3 - Resp 2", contrast))

# --- ANALYSIS 2: INTERMODULATION FREQUENCIES (Bar Plot B) ---
model_imf <- lmer(
  SNR ~ Condition * freq_imf + (1 | Subject),
  data = df_imf_filtered_bar
)
pairwise_imf <- emmeans(model_imf, pairwise ~ Condition | freq_imf, adjust = "tukey")
eff_imf_df <- as.data.frame(eff_size(pairwise_imf, sigma = sigma(model_imf), edf = df.residual(model_imf)))
print(r.squaredGLMM(model_imf))

contrast_df_imf_all <- as.data.frame(summary(pairwise_imf$contrasts, infer = TRUE)) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01  ~ "**", p.value < 0.05  ~ "*", TRUE ~ ""
    ),
    cohens_d = eff_imf_df$effect.size
  )

# Significant contrasts only (for plot annotations)
contrast_df_imf <- contrast_df_imf_all %>%
  filter(significance != "") %>%
  filter(grepl("Resp 2 - Resp 3", contrast) | grepl("Resp 3 - Resp 2", contrast))


# --- Plotting Preparation (Bar Plots A & B) ---
plot_data_bar <- df_filtered_bar %>%
  group_by(Subject, Condition, freq) %>%
  summarise(mean_SNR = mean(SNR, na.rm = TRUE), .groups = "drop") %>%
  group_by(Condition, freq) %>%
  summarise(
    overall_mean = mean(mean_SNR, na.rm = TRUE),
    se = sd(mean_SNR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

plot_data_imf_bar <- df_imf_filtered_bar %>%
  group_by(Subject, Condition, freq_imf) %>%
  summarise(mean_SNR = mean(SNR, na.rm = TRUE), .groups = "drop") %>%
  group_by(Condition, freq_imf) %>%
  summarise(
    overall_mean = mean(mean_SNR, na.rm = TRUE),
    se = sd(mean_SNR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

y_offset <- 0.1
sig_data_fundamental <- plot_data_bar %>%
  group_by(freq) %>%
  summarise(max_y = max(overall_mean + se, na.rm = TRUE), .groups = "drop") %>%
  left_join(contrast_df_fundamental, by = "freq") %>%
  filter(!is.na(significance)) %>%
  mutate(y_pos = max_y + y_offset, x_pos = 1.5)

sig_data_imf <- plot_data_imf_bar %>%
  group_by(freq_imf) %>%
  summarise(max_y = max(overall_mean + se, na.rm = TRUE), .groups = "drop") %>%
  left_join(contrast_df_imf, by = "freq_imf") %>%
  filter(!is.na(significance)) %>%
  mutate(y_pos = max_y + y_offset, x_pos = 1.5)


# --- Plotting fundamentals (Plot A) - With Color Coding ---
p <- ggplot(plot_data_bar, aes(x = freq, y = overall_mean, fill = Condition)) +
  geom_col(color = "black", position = position_dodge(width = 0.9), linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = overall_mean - se, ymax = overall_mean + se),
    width = 0.25, position = position_dodge(width = 0.9), linewidth = 0.7, color = "black"
  ) +
  geom_text(
    data = sig_data_fundamental, aes(x = freq, y = y_pos, label = significance),
    inherit.aes = FALSE, size = 10, family = plot_font_family, vjust = 0
  ) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, family = "Arial"),
    axis.title = element_text(size = 18, family = "Arial"),
    axis.text = element_text(size = 14, color = "black", family = "Arial"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6)
  ) +
  labs(x = "Frequency", y = "SNR") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_grey(start = 0.65, end = 0.05, labels = c("Resp 2", "Resp 3"))


# --- Plotting IMFs (Plot B) - With Color Coding ---
p_imf <- ggplot(plot_data_imf_bar, aes(x = freq_imf, y = overall_mean, fill = Condition)) +
  geom_col(color = "black", position = position_dodge(width = 0.9), linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = overall_mean - se, ymax = overall_mean + se),
    width = 0.25, position = position_dodge(width = 0.9), linewidth = 0.7, color = "black"
  ) +
  geom_text(
    data = sig_data_imf, aes(x = freq_imf, y = y_pos, label = significance),
    inherit.aes = FALSE, size = 10, family = plot_font_family, vjust = 0
  ) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, family = "Arial"),
    axis.title = element_text(size = 18, family = "Arial"),
    axis.text = element_text(size = 14, color = "black", family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6)
  ) +
  labs(x = "Frequency", y = "SNR") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_grey(start = 0.65, end = 0.05, labels = c("Resp 2", "Resp 3"))


# --- Combine Plots (A, B, C) ---
combined_plot_ABC <- p_line / (p + p_imf) +
  plot_annotation(
    tag_levels = 'A'
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 24, family = plot_font_family)
  )


# Print the combined plot
print(combined_plot_ABC)

# --- Save the combined plot ---
output_filename_base <- "Oz_Combined_SNR_Plots_Grayscale"
output_filename_tiff <- file.path(fig_dir, paste0(output_filename_base, ".tiff"))
output_filename_png <- file.path(fig_dir, paste0(output_filename_base, ".png"))


# Save as TIFF
ggsave(
  filename = output_filename_tiff,
  plot = combined_plot_ABC,
  width = 12, height = 9,
  dpi = 300,
  bg = "white",
  compression = "lzw"
)

# Save as PNG

ggsave(
  filename = output_filename_png,
  plot = combined_plot_ABC,
  width = 12, height = 7,
  dpi = 300,
  bg = "white"
)


# --- Formatting Statistics for Reporting ---

# --- Oz Fundamental contrasts ---
cat("\n--- Oz Fundamental contrasts ---\n")
for (i in 1:nrow(contrast_df_fundamental_all)) {
  r <- contrast_df_fundamental_all[i, ]
  p_fmt <- ifelse(r$p.value < 0.001, "p < .001", sprintf("p = %.3f", r$p.value))
  cat(sprintf("%s | %s: b = %.3f, SE = %.3f, t(%.2f) = %.3f, %s, 95%% CI [%.3f, %.3f], d = %.2f\n",
              r$freq, r$contrast, r$estimate, r$SE, r$df, r$t.ratio,
              p_fmt, r$lower.CL, r$upper.CL, r$cohens_d))
}

# --- Oz IM contrasts ---
cat("\n--- Oz IM contrasts ---\n")
for (i in 1:nrow(contrast_df_imf_all)) {
  r <- contrast_df_imf_all[i, ]
  p_fmt <- ifelse(r$p.value < 0.001, "p < .001", sprintf("p = %.3f", r$p.value))
  cat(sprintf("%s | %s: b = %.3f, SE = %.3f, t(%.2f) = %.3f, %s, 95%% CI [%.3f, %.3f], d = %.2f\n",
              r$freq_imf, r$contrast, r$estimate, r$SE, r$df, r$t.ratio,
              p_fmt, r$lower.CL, r$upper.CL, r$cohens_d))
}

