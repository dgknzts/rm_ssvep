% =========================================================================
% s07_roi_topoplots.m
%
% Generate ROI (Region of Interest) topoplots for the RM_SSVEP study.
% For both fundamental and intermodulation frequencies, this script:
%   1. Averages SNR across subjects and relevant frequencies per hemisphere
%   2. Identifies the top 6 channels (highest SNR) per hemisphere
%   3. Highlights those channels with red markers on the topoplot
%
% Requirements:
%   - EEGLAB must be on the MATLAB path (for the topoplot function)
%   - viridis.m colormap in the local utils/ folder
%   - SNR_all_Topo_regular.mat data file
%
% Outputs:
%   - Fundamental frequency topoplots: Right and Left hemispheres (PNG)
%   - Intermodulation frequency topoplots: Right and Left hemispheres (PNG)
%
% =========================================================================

%% Clear workspace
clear; clc; close all;

%% ---- USER-CONFIGURABLE PATHS ----
% Set the project root directory (change this to match your local setup)
project_root = '';  % UPDATE THIS: Set path to your project data directory

% Derived paths (no need to edit these)
data_dir   = fullfile(project_root, 'data', 'spectralData', 'topos');
fig_dir    = fullfile(project_root, 'figures');
utils_dir  = fullfile(fileparts(mfilename('fullpath')), 'utils');

% Ensure EEGLAB is on the path (user must have EEGLAB installed and added
% to the MATLAB path for the topoplot function to be available).
% Example: addpath(genpath('/path/to/eeglab2022.0'));

%% ---- Load Data and Colormap ----
% Load the SNR topoplot data
mat_file = fullfile(data_dir, 'SNR_all_Topo_regular.mat');
load(mat_file);

% Load the viridis colormap from the local utils folder
addpath(utils_dir);
run(fullfile(utils_dir, 'viridis.m'));  % Loads the 256x3 colormap variable "viridi"

% Create output directory if it does not exist
if ~exist(fig_dir, 'dir')
    mkdir(fig_dir);
end

%% ---- Topoplot Display Settings ----
gridscale_v = 128;
plotrad_v   = 0.6;

%% ---- Fundamental Frequency ROI Topoplots ----
% Define the fundamental frequencies (Hz)
upperFreqs = [4.8, 6, 7.5];

% Color limits for right and left hemisphere plots
fundCLim = {[1, 3.5], [1, 3.5]};

% Get indices for the fundamental frequencies in the SNR data
[~, fund_idx] = ismember(upperFreqs, SNR_all_Topo.frequencies);

% Compute mean SNR per hemisphere (averaged across subjects and frequencies)
SNR_right_avg = squeeze(mean(mean(SNR_all_Topo.data(1, :, :, fund_idx), 2), 4));
SNR_left_avg  = squeeze(mean(mean(SNR_all_Topo.data(2, :, :, fund_idx), 2), 4));

% Identify top 6 channels per hemisphere
[~, sort_idx_right] = sort(SNR_right_avg, 'descend');
best_channels_right = sort_idx_right(1:6);

[~, sort_idx_left] = sort(SNR_left_avg, 'descend');
best_channels_left = sort_idx_left(1:6);

%% Plot: Fundamental - Right Hemisphere
figure('Color', 'w');
topoplot(SNR_right_avg, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
         'gridscale', gridscale_v, 'plotrad', plotrad_v, ...
         'emarker2', {best_channels_right, 'o', 'r', 6, 2});
set(gca, 'CLim', fundCLim{1});
colormap(viridi);
cb = colorbar();
cb.Label.String   = 'SNR';
cb.Label.FontName = 'Arial';
set(cb, 'FontName', 'Arial', 'FontSize', 20);
set(gca, 'FontName', 'Arial');

out_file = fullfile(fig_dir, 'topo_Fundamental_Right.png');
print(gcf, out_file, '-dpng');
close(gcf);

%% Plot: Fundamental - Left Hemisphere
figure('Color', 'w');
topoplot(SNR_left_avg, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
         'gridscale', gridscale_v, 'plotrad', plotrad_v, ...
         'emarker2', {best_channels_left, 'o', 'r', 6, 2});
set(gca, 'CLim', fundCLim{2});
colormap(viridi);
cb = colorbar();
cb.Label.String   = 'SNR';
cb.Label.FontName = 'Arial';
set(cb, 'FontName', 'Arial', 'FontSize', 20);
set(gca, 'FontName', 'Arial');

out_file = fullfile(fig_dir, 'topo_Fundamental_Left.png');
print(gcf, out_file, '-dpng');
close(gcf);

%% ---- Intermodulation Frequency ROI Topoplots ----
% Define the intermodulation frequencies (Hz) and color limits
imFreqs = [10.8, 12.3, 13.5, 18.3];
imCLim  = {[1, 2.5], [1, 2.5]};

% Get indices for the IM frequencies
[~, im_idx] = ismember(imFreqs, SNR_all_Topo.frequencies);

% Compute mean SNR per hemisphere (averaged across subjects and IM frequencies)
SNR_right_avg_im = squeeze(mean(mean(SNR_all_Topo.data(1, :, :, im_idx), 2), 4));
SNR_left_avg_im  = squeeze(mean(mean(SNR_all_Topo.data(2, :, :, im_idx), 2), 4));

% Identify top 6 channels per hemisphere
[~, sort_idx_right_im] = sort(SNR_right_avg_im, 'descend');
best_channels_right_im = sort_idx_right_im(1:6);

[~, sort_idx_left_im] = sort(SNR_left_avg_im, 'descend');
best_channels_left_im = sort_idx_left_im(1:6);

%% Plot: Intermodulation - Right Hemisphere
figure('Color', 'w');
topoplot(SNR_right_avg_im, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
         'gridscale', gridscale_v, 'plotrad', plotrad_v, ...
         'emarker2', {best_channels_right_im, 'o', 'r', 6, 2});
set(gca, 'CLim', imCLim{1});
colormap(viridi);
cb = colorbar();
cb.Label.String   = 'SNR';
cb.Label.FontName = 'Arial';
set(cb, 'FontName', 'Arial', 'FontSize', 20);
set(gca, 'FontName', 'Arial');

out_file = fullfile(fig_dir, 'topo_IM_Right.png');
print(gcf, out_file, '-dpng');
close(gcf);

%% Plot: Intermodulation - Left Hemisphere
figure('Color', 'w');
topoplot(SNR_left_avg_im, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
         'gridscale', gridscale_v, 'plotrad', plotrad_v, ...
         'emarker2', {best_channels_left_im, 'o', 'r', 6, 2});
set(gca, 'CLim', imCLim{2});
colormap(viridi);
cb = colorbar();
cb.Label.String   = 'SNR';
cb.Label.FontName = 'Arial';
set(cb, 'FontName', 'Arial', 'FontSize', 20);
set(gca, 'FontName', 'Arial');

out_file = fullfile(fig_dir, 'topo_IM_Left.png');
print(gcf, out_file, '-dpng');
close(gcf);
