% =========================================================================
% s06_snr_topoplots.m
%
% Generate SNR topoplots for fundamental and intermodulation frequencies
% from the RM_SSVEP study. Produces individual topoplot images for each
% frequency of interest, averaged across left/right stimulus sides.
%
% Requirements:
%   - EEGLAB must be on the MATLAB path (for the topoplot function)
%   - viridis.m colormap in the local utils/ folder
%   - SNR_all_Topo_regular.mat data file
%
% Outputs:
%   - PNG topoplot images for each fundamental frequency (4.8, 6, 7.5 Hz)
%   - PNG topoplot images for each IM frequency (10.8, 12.3, 13.5, 18.3 Hz)
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

%% ---- Frequency Definitions and Color Limits ----

% Fundamental frequencies (Hz)
upperFreqs = [4.8, 6, 7.5];

% Intermodulation frequencies (Hz)
imFreqs = [10.8, 12.3, 13.5, 18.3];

% Color limits for each fundamental frequency
upperCLim = { [1, 3], [1, 3], [1, 4] };

% Color limits for each intermodulation frequency
imCLim = { [1, 2], [1, 2.2], [1, 3.5], [1, 1.5] };

%% ---- Topoplots for Fundamental Frequencies ----
for i = 1:length(upperFreqs)
    targetFreq = upperFreqs(i);

    % Find the index for the target frequency
    [~, freq_idx] = min(abs(SNR_all_Topo.frequencies - targetFreq));

    % Compute mean SNR across subjects for right and left sides, then average
    SNR_right_mean = squeeze(mean(SNR_all_Topo.data(1, :, :, freq_idx), 2));
    SNR_left_mean  = squeeze(mean(SNR_all_Topo.data(2, :, :, freq_idx), 2));
    SNR_combined   = (SNR_right_mean + SNR_left_mean) / 2;

    % Create and configure the topoplot
    figure('Color', 'w');
    topoplot(SNR_combined, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
             'gridscale', gridscale_v, 'plotrad', plotrad_v);
    set(gca, 'CLim', upperCLim{i});
    colormap(viridi);
    cb = colorbar();
    set(cb, 'FontSize', 24);
    title(sprintf('%.1f Hz', targetFreq), 'FontSize', 24);

    % Save and close
    out_file = fullfile(fig_dir, sprintf('topo_%.1f.png', targetFreq));
    print(gcf, out_file, '-dpng');
    close(gcf);
end

%% ---- Topoplots for Intermodulation Frequencies ----
for i = 1:length(imFreqs)
    targetFreq = imFreqs(i);
    [~, freq_idx] = min(abs(SNR_all_Topo.frequencies - targetFreq));

    SNR_right_mean = squeeze(mean(SNR_all_Topo.data(1, :, :, freq_idx), 2));
    SNR_left_mean  = squeeze(mean(SNR_all_Topo.data(2, :, :, freq_idx), 2));
    SNR_combined   = (SNR_right_mean + SNR_left_mean) / 2;

    figure('Color', 'w');
    topoplot(SNR_combined, SNR_all_Topo.chanlocs, 'electrodes', 'on', ...
             'gridscale', gridscale_v, 'plotrad', plotrad_v);
    set(gca, 'CLim', imCLim{i});
    colormap(viridi);
    cb = colorbar();
    set(cb, 'FontSize', 24);
    title(sprintf('%.1f Hz', targetFreq), 'FontSize', 24);

    out_file = fullfile(fig_dir, sprintf('topo_%.1f.png', targetFreq));
    print(gcf, out_file, '-dpng');
    close(gcf);
end
