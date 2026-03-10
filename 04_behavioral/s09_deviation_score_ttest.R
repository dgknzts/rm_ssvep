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

cat("Overall Mean DS:", round(overall_mean, 2), "\n")
cat("Standard Deviation:", round(overall_sd, 2), "\n")
cat("Degrees of Freedom:", df, "\n")
cat("t(", df, ") =", round(t_test_result$statistic, 2),
    ", p =", round(t_test_result$p.value, 3), "\n")

plot <- ggplot(subject_summary, aes(x = factor(1), y = mean_DS)) +
  geom_boxplot(width = 0.2, outlier.shape = NA, size = 1.5) +
  geom_jitter(aes(color = subject_code), width = 0.05, size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Mean Deviation Score (DS) Across Subjects",
       x = "",
       y = "Mean DS (Reported - 3)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(plot)
