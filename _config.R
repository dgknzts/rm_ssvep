# _config.R - Shared path configuration for all R scripts
# =========================================================
# Adjust these paths to match your local setup before running any scripts.
# Set your working directory to the rm_ssvep/ folder, then source this file.

project_root <- getwd()  # Should be the rm_ssvep/ directory

# --- Input data paths ---
data_dir       <- file.path(project_root, "data", "spectral_csv")
behavioral_csv <- file.path(data_dir, "behavioral_data.csv")

# --- Output paths ---
results_dir    <- file.path(project_root, "06_classification", "results")
fig_dir        <- file.path(project_root, "figures")

# Create output directories if they don't exist
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
