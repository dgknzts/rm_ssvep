library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
source("_config.R")

data <- read.csv(behavioral_csv)

# Deviation Score (DS): reported arcs minus 3 (veridical)
data <- data %>%
  mutate(DS = as.numeric(amount_response.keys) - 3)

# Mean DS per subject
subject_summary <- data %>%
  group_by(subject_code) %>%
  summarise(mean_DS = mean(DS, na.rm = TRUE), .groups = "drop")

overall_mean <- mean(subject_summary$mean_DS, na.rm = TRUE)
overall_sd <- sd(subject_summary$mean_DS, na.rm = TRUE)
n <- nrow(subject_summary)
df <- n - 1

# One-sample t-test: does mean DS differ from 0?
t_test_result <- t.test(subject_summary$mean_DS, mu = 0)

cohens_d <- overall_mean / overall_sd

cat("Overall Mean DS:", round(overall_mean, 2), "\n")
cat("Standard Deviation:", round(overall_sd, 2), "\n")
cat("Degrees of Freedom:", df, "\n")
cat("t(", df, ") =", round(t_test_result$statistic, 2),
    ", p =", round(t_test_result$p.value, 3), "\n")
cat("Cohen's d:", round(cohens_d, 2), "\n")
cat("95% CI:", round(t_test_result$conf.int[1], 2), "to",
    round(t_test_result$conf.int[2], 2), "\n")

plot <- ggplot(subject_summary, aes(x = factor(1), y = mean_DS)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_boxplot(width = 0.3, outlier.shape = NA, linewidth = 0.6, fill = "grey90") +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.6, color = "black") +
  labs(
    x = "",
    y = "Mean Deviation Score (Reported - 3)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.y = element_text(size = 16, margin = margin(r = 15), family = "Arial"),
    axis.text.y = element_text(size = 14, color = "black", family = "Arial"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(plot)

ggsave(
  filename = file.path(fig_dir, "deviation_score_boxplot.png"),
  plot = plot,
  width = 4, height = 6, dpi = 300, bg = "white"
)
