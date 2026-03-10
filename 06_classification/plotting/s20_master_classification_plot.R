# Master script: Classification figure
# Combines all classification plots into a single figure with consistent theming

# --- Setup and libraries ---
rm(list = ls()) # Start with clean environment

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggridges)
library(patchwork)

source("_config.R")

# --- Shared constants and theme ---
# Font sizes (scaled up per advisor feedback)
FONT_FAMILY       <- "Arial"
BASE_SIZE          <- 18
AXIS_TITLE_SIZE    <- 20
AXIS_TEXT_SIZE     <- 16
FACET_STRIP_SIZE   <- 18
LEGEND_TITLE_SIZE  <- 16
LEGEND_TEXT_SIZE   <- 15
ANNOTATION_SIZE    <- 5.5
TAG_SIZE           <- 18
BEST_AUC_LABEL     <- 5.5

# Abbreviated labels per advisor
LABEL_FUND <- "Fund"
LABEL_IM   <- "IMs"
LABEL_COMB <- "Comb"

# Color palette (consistent across all panels)
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

# --- Source individual plotting scripts ---
plotting_dir <- file.path(project_root, "06_classification", "plotting")

source(file.path(project_root, "06_classification", "plotting", "s21_gridsearch_heatmap.R"))
if(!exists("p_heatmap")) stop("p_heatmap not created")

source(file.path(project_root, "06_classification", "plotting", "s22_permutation_boxplot.R"))
if(!exists("p_boxplot")) stop("p_boxplot not created")

source(file.path(project_root, "06_classification", "plotting", "s23_roc_curves.R"))
if(!exists("p_roc")) stop("p_roc not created")

source(file.path(project_root, "06_classification", "plotting", "s24_auc_diff_ridge.R"))
if(!exists("ridge_plot")) stop("ridge_plot not created")

# --- Verify all plot objects ---
required_plots <- c("p_heatmap", "p_boxplot", "p_roc", "ridge_plot")
missing_plots <- required_plots[!required_plots %in% ls()]

if(length(missing_plots) > 0) {
  stop(paste("Missing plot objects:", paste(missing_plots, collapse = ", ")))
}

# --- Prepare panels for combination ---
p_heatmap_clean <- p_heatmap +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(5, 10, 5, 20),
    plot.tag.position = c(0, 1)
  )

p_boxplot_clean <- p_boxplot +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(10, 15, 15, 15),
    plot.tag.position = c(0, 1)
  )

p_roc_clean <- p_roc +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(10, 10, 15, 15)
  )

ridge_plot_clean <- ridge_plot +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(15, 5, 5, 15),
    plot.tag.position = c(0, 1)
  )

# --- Layout ---
# A (Heatmap, full width) / B (Permutation) + C (ROC) / D (Ridge, full width)

top_row <- p_heatmap_clean

middle_row <- (p_boxplot_clean | p_roc_clean) + 
  plot_layout(widths = c(1, 0.8))

bottom_row <- ridge_plot_clean

combined_plot <- top_row / middle_row / bottom_row +
  plot_layout(heights = c(1.5, 2, 2.5))

# --- Annotations ---
final_plot <- combined_plot +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(
        size = TAG_SIZE, face = "bold", family = FONT_FAMILY
      )
    )
  )

# --- Display and save ---
print(final_plot)

base_filename <- "comprehensive_classification_analysis"

ggsave(
  filename = file.path(fig_dir, paste0(base_filename, ".png")),
  plot = final_plot,
  width = 14, height = 16, dpi = 300, bg = "white"
)
