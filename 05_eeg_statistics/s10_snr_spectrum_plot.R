library(ggplot2)
library(tidyverse)
library(ggrepel)
source("_config.R")

file <- "all_SNR_data_regular.csv"

# Stimulus frequencies (fundamentals + harmonics) and intermodulations
f1  <- c(4.8, 9.6, 14.4)
f2  <- c(6, 12, 18)
f3  <- c(7.5, 15, 22.5)
ims <- c(1.2, 1.5, 2.7, 10.8, 12.3, 13.5, 18.3)

df <- read.csv(file.path(data_dir, file))

# --- A) Assign color groups ---
df <- df %>%
  mutate(
    color_group = case_when(
      Frequency %in% f1  ~ "f1",
      Frequency %in% f2  ~ "f2",
      Frequency %in% f3  ~ "f3",
      Frequency %in% ims ~ "ims",
      TRUE ~ "other"
    )
  )

# --- B) Aggregate SNR by frequency ---
SNR_summary <- df %>%
  group_by(Frequency) %>%
  summarise(
    Mean  = mean(SNR, na.rm = TRUE),
    SD    = sd(SNR, na.rm = TRUE),
    N     = sum(!is.na(SNR)),
    Lower = Mean - 1.96 * SD / sqrt(N),
    Upper = Mean + 1.96 * SD / sqrt(N)
  ) %>%
  ungroup()

# --- C) Highlighted frequency data ---
SNR_summary_dot <- df %>%
  filter(color_group != "other") %>%
  group_by(Frequency) %>%
  summarise(
    color_group = first(color_group),
    Mean  = mean(SNR, na.rm = TRUE),
    SD    = sd(SNR, na.rm = TRUE),
    N     = sum(!is.na(SNR)),
    Lower = Mean - 1.96 * SD / sqrt(N),
    Upper = Mean + 1.96 * SD / sqrt(N)
  ) %>%
  ungroup() %>%
  mutate(
    plot_text = case_when(
      Frequency == 4.8  ~ "f₁",
      Frequency == 9.6  ~ "2f₁",
      Frequency == 14.4 ~ "3f₁",
      Frequency == 6    ~ "f₂",
      Frequency == 12   ~ "2f₂",
      Frequency == 18   ~ "3f₂",
      Frequency == 7.5  ~ "f₃",
      Frequency == 15   ~ "2f₃",
      Frequency == 22.5 ~ "3f₃",
      Frequency == 1.2  ~ "f₂-f₁",
      Frequency == 1.5  ~ "f₃-f₂",
      Frequency == 2.7  ~ "f₃-f₁",
      Frequency == 10.8 ~ "f₁+f₂",
      Frequency == 12.3 ~ "f₁+f₃",
      Frequency == 13.5 ~ "f₂+f₃",
      Frequency == 18.3 ~ "f₁+f₂+f₃",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Frequency) %>%
  mutate(
    # Two-tier label positions to avoid overlap
    label_y = case_when(
      plot_text %in% c("f₁", "2f₁", "3f₁", "f₂", "2f₂", "3f₂", "f₃", "2f₃", "3f₃") ~ 3.6,
      plot_text == "f₃-f₂" ~ 3.6,
      plot_text %in% c("f₂-f₁", "f₃-f₁", "f₁+f₂", "f₁+f₃", "f₂+f₃", "f₁+f₂+f₃") ~ 4.1,
      TRUE ~ 3.6
    )
  )

# --- D) Per-subject means for t-tests ---
t_test_data <- df %>%
  group_by(Frequency, Subject) %>%
  summarise(SNR = mean(SNR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    color_group = case_when(
      Frequency %in% f1  ~ "f1",
      Frequency %in% f2  ~ "f2",
      Frequency %in% f3  ~ "f3",
      Frequency %in% ims ~ "ims",
      TRUE ~ "other"
    )
  ) %>%
  filter(color_group != "other")

# --- E) One-sample t-tests against SNR = 1, FDR corrected ---
t_test_results <- t_test_data  %>%
  group_by(Frequency) %>%
  summarize({
    tt <- t.test(SNR, mu = 1)
    tibble(
      Mean = mean(SNR, na.rm = TRUE),
      SD = sd(SNR, na.rm = TRUE),
      t_statistic = tt$statistic,
      df = tt$parameter,
      t_test_p_value = tt$p.value,
      CI_lower = tt$conf.int[1],
      CI_upper = tt$conf.int[2],
      cohens_d = (mean(SNR, na.rm = TRUE) - 1) / sd(SNR, na.rm = TRUE)
    )
  }) %>%
  ungroup() %>%
  mutate(
    adjusted_p_value = p.adjust(t_test_p_value, method = "fdr"),
    Significance = case_when(
      adjusted_p_value < 0.001 ~ "***",
      adjusted_p_value < 0.01  ~ "**",
      adjusted_p_value < 0.05  ~ "*",
      TRUE ~ "-"
    ),
    color_group = case_when(
      Frequency %in% f1  ~ "f1",
      Frequency %in% f2  ~ "f2",
      Frequency %in% f3  ~ "f3",
      Frequency %in% ims ~ "ims",
      TRUE ~ "other"
    )
  ) %>%
  #filter(Significance != "-") %>%
  left_join(
    SNR_summary_dot %>% select(Frequency, label_y), 
    by = "Frequency"
  ) %>%
  mutate(y_pos = label_y + 0.18)

# --- F) Color palette ---
# Consistent with classification plot colors
color_palette <- c(
  "f1"  = "#d73027",  # Red - consistent with classification plots
  "f2"  = "#74add1",  # Blue - consistent with classification plots  
  "f3"  = "#4575b4",  # Darker blue for distinction
  "ims" = "#f46d43"   # Orange for intermodulations
)

# --- G) Plot ---
p <- ggplot(SNR_summary, aes(x = Frequency, y = Mean)) +
  geom_line(color = "grey60", linewidth = 1.5, alpha = 0.8) +

  geom_point(
    data = SNR_summary_dot,
    aes(x = Frequency, y = Mean, color = color_group),
    size = 5, shape = 18, show.legend = FALSE, alpha = 0.9
  ) +
  
  geom_segment(
    data = SNR_summary_dot,
    aes(x = Frequency, y = Mean, xend = Frequency, yend = label_y, color = color_group),
    linetype = "dotted",
    linewidth = 1,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_text(
    data = SNR_summary_dot,
    aes(x = Frequency, y = label_y, label = plot_text, color = color_group),
    size = 4.5, 
    show.legend = FALSE, 
    alpha = 1,
    vjust = 0.5,
    fontface = "bold",
    family = "Arial"
  ) +
  
  geom_text(
    data = t_test_results,
    aes(x = Frequency, y = y_pos, label = Significance),
    size = 5, vjust = 0.5, alpha = 1, 
    fontface = "bold", family = "Arial", color = "black"
  ) +
  
  scale_color_manual(values = color_palette) +

  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4), 
    expand = expansion(mult = c(0.05, 0.18)),
    labels = function(x) sprintf("%.0f", x)
  ) +
  scale_x_continuous(
    limits = c(0.5, 25), 
    expand = expansion(mult = c(0.02, 0.02)),
    breaks = seq(0, 25, 5)
  ) +
  
  coord_cartesian(ylim = c(0.5, 4.7)) +

  # SNR = 1 baseline (no signal above noise)
  geom_hline(
    yintercept = 1, 
    color = "#d73027", 
    linewidth = 1, 
    alpha = 0.8, 
    linetype = "dashed"
  ) +
  
  labs(
    x = "Frequency (Hz)", 
    y = "Signal-to-Noise Ratio (SNR)") +
  
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 18,
      family = "Arial", margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, family = "Arial", size = 13,
      color = "gray30", margin = margin(b = 12)
    ),
    plot.caption = element_text(
      hjust = 0.5, family = "Arial", size = 10,
      color = "gray50", margin = margin(t = 10)
    ),
    axis.title.x = element_text(
      face = "bold", family = "Arial", size = 15,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      face = "bold", family = "Arial", size = 15,
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      family = "Arial", size = 13, color = "black"
    ),
    axis.text.y = element_text(
      family = "Arial", size = 13, color = "black"
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.8),
    
    plot.margin = margin(15, 15, 15, 15)
  )

print(p)

# --- Save ---
ggsave(
  filename = file.path(fig_dir, "SNR_spectrum_enhanced.png"),
  plot = p, 
  width = 8, 
  height = 6, 
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "SNR_spectrum_enhanced.svg"),
  plot = p, 
  width = 12, 
  height = 8,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "SNR_spectrum_enhanced.tiff"),
  plot = p, 
  width = 12, 
  height = 8, 
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

# --- Significance summary ---
cat("\nFrequencies analyzed:", nrow(SNR_summary), "\n")
cat("Significant (FDR corrected):", nrow(t_test_results), "\n\n")

if(nrow(t_test_results) > 0) {
  significant_with_labels <- t_test_results %>%
    left_join(SNR_summary_dot %>% select(Frequency, plot_text), by = "Frequency") %>%
    arrange(Frequency)

  for(i in 1:nrow(significant_with_labels)) {
    row <- significant_with_labels[i,]
    cat(sprintf("%.1f Hz (%s): SNR = %.2f %s (p = %.4f, FDR p = %.4f)\n",
                row$Frequency, row$plot_text, row$Mean,
                row$Significance, row$t_test_p_value, row$adjusted_p_value))
  }
} else {
  cat("No significant frequency components found.\n")
}

# --- Table 1: Full t-test statistics for manuscript ---
table1 <- t_test_results %>%
  left_join(SNR_summary_dot %>% select(Frequency, plot_text), by = "Frequency") %>%
  arrange(Frequency) %>%
  select(Component = plot_text, Hz = Frequency, M = Mean, SD,
         t = t_statistic, df, p = t_test_p_value, pFDR = adjusted_p_value,
         CI_lower, CI_upper, d = cohens_d) %>%
  mutate(across(c(M, SD, t, d), ~round(., 2)),
         across(c(p, pFDR), ~round(., 4)))

cat("\n--- Table 1 ---\n")
print(as.data.frame(table1), row.names = FALSE)


