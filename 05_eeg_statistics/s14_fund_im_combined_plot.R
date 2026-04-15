# Combined fundamental + intermodulation plot
# ---
# A) Setup & data loading
# ---
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(patchwork)
library(glue)
library(stringr)
source("_config.R")

# Load ROI channel definitions
load(file.path(data_dir, "roi_channels.RData"))

master_data <- read.csv(file.path(data_dir, "pCond_SNR_data_regular.csv"))

# ---
# B) Shared plot elements
# ---
condition_colors <- c("Resp 2" = "#d73027", "Resp 3" = "#74add1")

# Shared theme for both plots
theme_master <- theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Arial", color = "black"),
    axis.title = element_text(face = "bold", size = 22),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 20, margin = margin(5, 5, 5, 5)),
    strip.background = element_rect(fill = "gray92", color = "gray20", linewidth = 0.8),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray20", fill = NA, linewidth = 1),
    plot.margin = margin(15, 15, 15, 15)
  )


# ---
# Part 1: Fundamental frequency plot
# ---
cat("\n--- Part 1: Fundamental frequencies ---\n")

# --- Data Prep ---
df_fund <- master_data %>%
  mutate(
    freq = case_when(
      Frequency %in% c(4.8) ~ "f1",
      Frequency %in% c(6.0) ~ "f2",
      Frequency %in% c(7.5) ~ "f3",
      TRUE ~ "other"
    ),
    Side = ifelse(Side == 1, "Right", "Left")
  ) %>%
  filter(freq %in% c("f1", "f2", "f3")) %>%
  filter(
    (Side == "Right" & Channels %in% fundamental_channels_right) |
      (Side == "Left" & Channels %in% fundamental_channels_left)
  ) %>%
  mutate(
    across(c(Subject, Channels, Frequency, Condition, Side), as.factor),
    freq = factor(freq, levels = c("f1", "f2", "f3"))
  ) %>%
  filter(!Condition == "Resp 1")

# --- Model & Contrasts ---
model_fund <- lmer(SNR ~ Condition * freq + (1 | Subject) + (1 | Subject:Channels), data = df_fund)
pairwise_results_fund <- emmeans(model_fund, pairwise ~ freq | Condition, adjust = "tukey")
contrast_df_fund <- as.data.frame(summary(pairwise_results_fund$contrasts, infer = TRUE)) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***", p.value < 0.01  ~ "**", p.value < 0.05  ~ "*", TRUE ~ ""
  ))

# --- Plotting Prep ---
plot_data_fund <- df_fund %>%
  group_by(Condition, freq) %>%
  summarise(
    overall_mean = mean(SNR, na.rm = TRUE),
    se = sd(SNR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Annotation placement
# Get max y-value per facet to position brackets
max_y_fund <- plot_data_fund %>%
  group_by(Condition) %>%
  summarise(max_val = max(overall_mean + se, na.rm = TRUE), .groups = "drop")

line_data_fund <- contrast_df_fund %>%
  filter(significance != "") %>%
  mutate(
    x_start = as.numeric(factor(str_extract(contrast, "^f[1-3]"), levels = c("f1", "f2", "f3"))),
    x_end = as.numeric(factor(str_extract(contrast, "f[1-3]$"), levels = c("f1", "f2", "f3")))
  ) %>%
  left_join(max_y_fund, by = "Condition") %>%
  group_by(Condition) %>%
  arrange(desc(x_end - x_start)) %>%
  mutate(y_level = row_number()) %>%
  ungroup() %>%
  mutate(
    y_position = max_val + (y_level * 0.7),
    midpoint = (x_start + x_end) / 2
  )

# --- Create Plot ---
p_fundamental <- ggplot(plot_data_fund, aes(x = freq, y = overall_mean, color = Condition)) +
  geom_errorbar(aes(ymin = overall_mean - se, ymax = overall_mean + se),
                width = 0.15, position = position_dodge(width = 0.3), linewidth = 1, alpha = 0.9) +
  geom_point(size = 4.5, position = position_dodge(width = 0.3), shape = 19) +
  geom_segment(data = line_data_fund, aes(x = x_start, xend = x_end, y = y_position, yend = y_position),
               inherit.aes = FALSE, color = "black", linewidth = 1) +
  geom_text(data = line_data_fund, aes(x = midpoint, y = y_position, label = significance),
            inherit.aes = FALSE, size = 8, color = "black", vjust = -0.5, fontface = "bold") +
  theme_master +
  scale_color_manual(values = condition_colors) +
  facet_wrap(~ Condition, nrow = 1,
             labeller = labeller(Condition = c("Resp 2" = "Response 2", "Resp 3" = "Response 3"))) +
  labs(x = "Fundamentals", y = "SNR") +
  scale_x_discrete(labels = c("f1" = expression(f[1]), "f2" = expression(f[2]), "f3" = expression(f[3]))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))


# ---
# Part 2: Intermodulation frequency plot
# ---
cat("\n--- Part 2: Intermodulation frequencies ---\n")

# --- Data Prep ---
df_im <- master_data %>%
  mutate(
    freq = case_when(
      Frequency %in% c(10.8) ~ "1", Frequency %in% c(12.3) ~ "2",
      Frequency %in% c(13.5) ~ "3", Frequency %in% c(18.3) ~ "4", TRUE ~ "other"
    ),
    Side = ifelse(Side == 1, "Right", "Left")
  ) %>%
  filter(freq %in% c("1", "2", "3", "4")) %>%
  filter(
    (Side == "Right" & Channels %in% ims_channels_right) |
      (Side == "Left" & Channels %in% ims_channels_left)
  ) %>%
  mutate(
    across(c(Subject, Channels, Frequency, Side), as.factor),
    Condition = factor(Condition, levels = c("Resp 2", "Resp 3")),
    freq = factor(freq, levels = c("1", "2", "3", "4"))
  ) %>%
  filter(!Condition == "Resp 1")

# --- Model & Contrasts ---
model_im <- lmer(SNR ~ Condition * freq + (1 | Subject) + (1 | Subject:Channels), data = df_im)
pairwise_results_im <- emmeans(model_im, pairwise ~ freq | Condition, adjust = "tukey")
contrast_df_im <- as.data.frame(summary(pairwise_results_im$contrasts, infer = TRUE)) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***", p.value < 0.01  ~ "**", p.value < 0.05  ~ "*", TRUE ~ ""
  ))

# --- Plotting Prep ---
plot_data_im <- df_im %>%
  group_by(Condition, freq) %>%
  summarise(overall_mean = mean(SNR, na.rm = TRUE), se = sd(SNR, na.rm = TRUE) / sqrt(n()), .groups = "drop")
freq_labels_im <- c("1" = expression(f[1] + f[2]), "2" = expression(f[1] + f[3]),
                    "3" = expression(f[2] + f[3]), "4" = expression(f[1] + f[2] + f[3]))

# Annotation placement
max_y_im <- plot_data_im %>%
  group_by(Condition) %>%
  summarise(max_val = max(overall_mean + se, na.rm = TRUE), .groups = "drop")

line_data_im <- contrast_df_im %>%
  filter(significance != "") %>%
  mutate(
    x_start = as.numeric(str_extract(contrast, "^[1-4]")),
    x_end = as.numeric(str_extract(contrast, "[1-4]$"))
  ) %>%
  left_join(max_y_im, by = "Condition") %>%
  group_by(Condition) %>%
  arrange(desc(x_end - x_start)) %>%
  mutate(y_level = row_number()) %>%
  ungroup() %>%
  mutate(
    y_position = max_val + (y_level * 1.2),
    midpoint = (x_start + x_end) / 2
  )

# --- Create Plot ---
p_intermodulation <- ggplot(plot_data_im, aes(x = freq, y = overall_mean, color = Condition)) +
  geom_errorbar(aes(ymin = overall_mean - se, ymax = overall_mean + se),
                width = 0.15, position = position_dodge(width = 0.3), linewidth = 1, alpha = 0.9) +
  geom_point(size = 4.5, position = position_dodge(width = 0.3), shape = 19) +
  geom_segment(data = line_data_im, aes(x = x_start, xend = x_end, y = y_position, yend = y_position),
               inherit.aes = FALSE, color = "black", linewidth = 1) +
  geom_text(data = line_data_im, aes(x = midpoint, y = y_position, label = significance),
            inherit.aes = FALSE, size = 8, color = "black", vjust = -0.5, fontface = "bold") +
  theme_master +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 16)) +
  scale_color_manual(values = condition_colors) +
  facet_wrap(~ Condition, nrow = 1,
             labeller = labeller(Condition = c("Resp 2" = "Response 2", "Resp 3" = "Response 3"))) +
  labs(x = "Intermodulations", y = "SNR") +
  scale_x_discrete(labels = freq_labels_im) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))


# ---
# Part 3: Combine plots
# ---
combined_plot <- p_fundamental / p_intermodulation

print(combined_plot)

# Save
ggsave(
  filename = file.path(fig_dir, "combined_frequency_comparison_vertical_final.png"),
  plot = combined_plot,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "combined_frequency_comparison_vertical_final.svg"),
  plot = combined_plot,
  width = 10,
  height = 12,
  bg = "white"
)

cat("\n--- Done ---\n")