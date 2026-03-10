%% s01_preprocess_eeg.m
% =========================================================================
% EEG Preprocessing Pipeline for SSVEP Experiment
% =========================================================================
% This script preprocesses raw BrainVision EEG data for a Steady-State
% Visual Evoked Potential (SSVEP) experiment. Processing steps include:
%   1. Bandpass filtering (0.5-40 Hz)
%   2. Resampling to 250 Hz
%   3. Notch filtering to remove 50 Hz line noise
%   4. Channel location assignment
%   5. Bad channel interpolation (subject-specific)
%   6. Average re-referencing (excluding VEOG)
%   7. Epoching around stimulus triggers
%   8. Behavioral trigger replacement from response logs
%
% Requirements:
%   - EEGLAB (tested with v2022.0)
%   - Custom utility functions in the 'utils' subfolder
%   - Raw EEG data in BrainVision format (.vhdr)
%   - Behavioral data folders for each subject
%
% Usage:
%   1. Set 'eeglab_path' to your local EEGLAB installation
%   2. Set 'project_root' to the root directory of the RM_SSVEP project
%   3. Run the script
% =========================================================================

%% Clock
tic;  % Start the timer

%% Setup
% --- Path Configuration ---

% UPDATE THIS: Set path to your EEGLAB installation
eeglab_path = '';  % e.g., '/path/to/eeglab2022.0/eeglab.m'
run(eeglab_path);

% Add custom utility functions to the path
utils_path = fullfile(fileparts(mfilename('fullpath')), 'utils');
addpath(genpath(utils_path));

% Project root directory (parent of the rm_ssvep code folder)
project_root = fullfile(fileparts(mfilename('fullpath')), '..', '..');

% Define paths relative to project root
raw_data_path        = fullfile(project_root, 'data', 'raweeg');
behavioral_data_path = fullfile(project_root, 'data', 'behavioral');
output_path          = fullfile(project_root, 'data', 'preprocessed', 'regular');

% Create output directory if it does not exist
if ~exist(output_path, 'dir')
    mkdir(output_path);
end

% Parameters
filter_band    = [0.5 40];  % Bandpass filter range (Hz)
notch_band     = [49 51];   % Notch filter range to remove 50 Hz line noise
epoch_range    = [-2 12];   % Epoch time window (seconds)
resample_rate  = 250;       % New sampling rate (Hz)

% Channel location file (ships with EEGLAB dipfit plugin)
chanloc_file = fullfile(fileparts(eeglab_path), 'plugins', 'dipfit', ...
    'standard_BEM', 'elec', 'standard_1005.elc');

epoch_triggers = {'S111', 'S222'};  % Stimulus onset triggers

% List raw data files (assumes filenames like "sbj_01.vhdr", "sbj_02.vhdr", etc.)
file_list    = dir(fullfile(raw_data_path, 'sbj_*.vhdr'));
num_subjects = length(file_list);

% Define subject-specific bad channels (each cell corresponds to one subject)
badChannels = { ...
    {'AF7','Fp1'};         ... % Subject 1
    {'AF7','Iz'};          ... % Subject 2
    {'AF7','AF3'};         ... % Subject 3
    {'CP1','AF7','AF3'};   ... % Subject 4
    {'AF7','AF8','Fp1'};   ... % Subject 5
    {'AF7','Fp2'};         ... % Subject 6
    {};                    ... % Subject 7
    {};                    ... % Subject 8
    {'Iz'};                ... % Subject 9
    {};                    ... % Subject 10
    {};                    ... % Subject 11
    {};                    ... % Subject 12
    {};                    ... % Subject 13
    {};                    ... % Subject 14
    {'TP7'};               ... % Subject 15
    {};                    ... % Subject 16
    {};                    ... % Subject 17
    {'AF4','AF7'};         ... % Subject 18
    {'C5','AF4'};          ... % Subject 19
    {'C5'};                ... % Subject 20
    {'C5'}                 ... % Subject 21
    };

% Start a parallel pool if one is not already running
if isempty(gcp('nocreate'))
    parpool('local');
end

%% Process Each Subject in Parallel
parfor subj = 1:num_subjects
    fprintf('=== Processing Subject %d ===\n', subj);

    %% (1) Load and Preprocess Raw Data
    % Load the raw BrainVision file
    raw_filename = file_list(subj).name;
    fprintf('Loading %s\n', raw_filename);
    EEG = pop_loadbv(raw_data_path, raw_filename);
    EEG.data = double(EEG.data);

    % Apply bandpass filter
    EEG = pop_eegfiltnew(EEG, 'locutoff', filter_band(1), 'hicutoff', filter_band(2));

    % Resample the data
    EEG = pop_resample(EEG, resample_rate);

    % Apply notch filter to remove 50 Hz line noise
    EEG = pop_eegfiltnew(EEG, 'locutoff', notch_band(1), 'hicutoff', notch_band(2), 'revfilt', 1);

    % Assign channel locations
    EEG = pop_chanedit(EEG, 'lookup', chanloc_file, 'rplurchanloc', 1);

    % Interpolate bad channels if defined for this subject
    current_bad = badChannels{subj};
    if ~isempty(current_bad)
        badIdx = [];
        for ch = 1:length(current_bad)
            idx = find(strcmp({EEG.chanlocs.labels}, current_bad{ch}));
            if ~isempty(idx)
                badIdx(end+1) = idx;  %#ok<SAGROW>
            end
        end
        if ~isempty(badIdx)
            EEG = pop_interp(EEG, badIdx, 'spherical');
        end
    end

    % Re-reference to average (excluding channel 64 - VEOG)
    EEG = pop_reref(EEG, [], 'exclude', 64);

    %% (2) Epoching and Trigger Adjustment Based on Behavioral Data
    % Epoch the data using stimulus onset triggers
    EEG = pop_epoch(EEG, epoch_triggers, epoch_range, 'epochinfo', 'yes');

    % Remove redundant trials (block error) for specific subjects 
    if subj == 8
        EEG = pop_select(EEG, 'notrial', 49:56);
    elseif subj == 10
        EEG = pop_select(EEG, 'notrial', 144:159);
    elseif subj == 18
        EEG = pop_select(EEG, 'notrial', 49);
    end

    % Load behavioral responses and extract new triggers
    % Custom function "new_extract_triggers" returns [combined_data, new_triggers]
    behav_dir = fullfile(behavioral_data_path, sprintf('sbj_%d_behavioral', subj));
    [~, new_triggers] = new_extract_triggers(behav_dir);

    % Keep only events corresponding to the original epoch triggers
    keep_ev  = ismember({EEG.event.type}, epoch_triggers);
    keep_ur  = ismember({EEG.urevent.type}, epoch_triggers);
    EEG.event   = EEG.event(keep_ev);
    EEG.urevent = EEG.urevent(keep_ur);

    % Replace event types with the behavioral response triggers
    for e = 1:length(new_triggers)
        if e <= length(EEG.event)
            EEG.event(e).type = new_triggers{e};
        end
        if e <= length(EEG.urevent)
            EEG.urevent(e).type = new_triggers{e};
        end
    end

    %% (3) Save the Processed File
    out_filename = sprintf('sbj_%02d.set', subj);
    EEG = pop_saveset(EEG, 'filename', out_filename, 'filepath', output_path);
    fprintf('Subject %d processing complete. Saved as %s\n\n', subj, out_filename);
end

elapsedTime = toc;  % Stop the timer
fprintf('Total processing time: %.2f seconds.\n', elapsedTime);
