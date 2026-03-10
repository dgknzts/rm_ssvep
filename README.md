# Integration Rather Than Suppression Drives Information Loss in Peripheral Vision

Oztas, D. N., L-Miao, L., Sayim, B., & Alp, N.

## Overview

When multiple identical objects are presented in the visual periphery, observers often report seeing fewer items than are actually present — a phenomenon known as **redundancy masking**. This study investigates whether this perceptual loss arises from neural **suppression** (reduced processing of individual items) or **integration** (merging of item representations).

We used **SSVEP (Steady-State Visual Evoked Potentials)** to frequency-tag three identical arcs, each flickering at a distinct frequency (f1 = 4.8 Hz, f2 = 6.0 Hz, f3 = 7.5 Hz). This allows us to track the neural representation of each item independently via EEG, while participants report how many arcs they perceive.

<img src="figures/readme/fig1_method.png" width="550">

**Figure 1.** Experimental paradigm. (A) Trial structure: fixation (1 s), stimulus presentation (10 s), response screen. (B) Gaze-contingent control with eye tracking. (C) Each arc was contrast-modulated at a unique frequency.

## Key Findings

### SSVEP Results

Fundamental frequency SNR was comparable across conditions (Response 2 vs. 3), suggesting individual item representations are **not suppressed** when fewer arcs are perceived. In contrast, intermodulation (IM) frequencies — neural signatures of nonlinear interaction between items — differed between conditions, pointing to **changes in integration**.

![Figure 2](figures/readme/fig2_results.png)

**Figure 2.** Left: Experimental conditions (Response 3 = veridical, Response 2 = redundancy masking). Center: Fundamental frequency SNR and ROI topographies,no significant condition differences. Right: Intermodulation component SNR,significant condition differences, indicating altered neural integration when fewer items are perceived.

### SVM Classification

Single-trial SVM classifiers trained on SSVEP features decoded the perceived number of arcs (Response 2 vs. 3). All three feature sets (fundamentals, IMs, combined) significantly outperformed chance. IM-based features outperformed fundamental-based features, consistent with integration as the mechanism underlying redundancy masking.

<img src="figures/readme/fig3b_permutation.png" width="550">

**Figure 3B.** Permutation test against chance. Null AUC distributions (200 permutations) with observed performance overlaid. All feature sets significantly above chance (p < 0.001).

<img src="figures/readme/fig3d_auc_ridge.png" width="550">

**Figure 3D.** AUC difference distributions (1000 permutations). Fundamentals significantly underperformed both IMs (p = 0.009) and combined (p = 0.003) feature sets. IMs vs. combined was not significant (p = 0.971).

## Execution Order

Intermediate CSV files are included in `data/spectral_csv/`, so the R pipeline can be run without MATLAB. Set your working directory to `rm_ssvep/` before running R scripts. MATLAB scripts require updating `project_root` and `eeglab_path` at the top of each file.

1. **Preprocessing** (MATLAB): `s01_preprocess_eeg.m`
2. **Spectral analysis** (MATLAB): `s02` - `s04` (FFT, SNR per condition, SNR spectrum)
3. **Topographic plots** (MATLAB): `s06`, `s07`
4. **Behavioral analysis** (R): `s08`, `s09`
5. **EEG statistics** (R): `s10` - `s15` (SNR spectrum, ROI selection, LME comparisons)
6. **Classification** (R): `s16` - `s20` (SVM training, permutation tests, plotting)

## Data Availability

Intermediate CSV files are included in `data/spectral_csv/` to allow running the R pipeline without MATLAB. Raw EEG data can be obtained from **[OSF](https://osf.io/q9b2f/?view_only=bef14d3ba40a4d7a85ee2584efe867dd)**. Place downloaded files in `../data/raweeg/` and `../data/behavioral/` relative to this repository before running the MATLAB scripts.

## Citation

If you use this code, please cite (preprint):

> Oztas, D. N., L-Miao, L., Sayim, B., & Alp, N. (2025). Redundancy masking and the compression of information in the brain. *bioRxiv*, 2025-05. https://doi.org/10.1101/2025.05.30.657088

## License

This project is licensed under the MIT License. See `LICENSE` for details.
