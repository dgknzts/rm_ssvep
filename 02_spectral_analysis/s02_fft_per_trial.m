%% s02_fft_per_trial.m
% =========================================================================
% FFT Per Trial - Raw FFT Amplitude Extraction for Machine Learning
% =========================================================================
% This script computes the FFT for each trial of each subject and extracts
% raw FFT amplitudes at predefined target frequencies. The output is a
% wide-format CSV where each row is a trial and each column is a feature
% (channel x frequency combination). This format is suitable for ML
% classification pipelines.
%
% Input:  Preprocessed EEG .set files
% Output: Wide-format CSV with raw FFT amplitudes per trial
%
% Dependencies: EEGLAB
% =========================================================================

%% --- User Configuration (EDIT THESE PATHS) ---
% Root directory of the RM_SSVEP project
project_root = '';  % UPDATE THIS: Set path to your project data directory

% Path to EEGLAB installation
eeglab_path = '';  % UPDATE THIS: e.g., '/path/to/eeglab2022.0/eeglab.m'

%% --- Initialize EEGLAB ---
tic
run(eeglab_path);

%% --- Define Parameters ---
% Define target frequencies (Hz) for which raw FFT amplitudes will be saved.
target_freqs = [4.8, 6, 7.5, 9.6, 12, 15, 10.8, 12.3, 13.5, 18.3];
nTarget = length(target_freqs);

% Parameters for FFT calculation:
nChan      = 63;                % Number of channels (after removing VEOG)
srate      = 250;               % Sampling rate (Hz)
nyquist    = srate / 2;
resolution = 0.1;               % Desired frequency resolution (Hz)
time       = 0:1000/srate:10000; % Time vector in ms
pnts       = length(time);
nfft       = ceil(srate/resolution);  % Number of FFT points for desired resolution

% Frequency vector corresponding to the FFT (from 0 Hz to Nyquist)
frequencies = linspace(0, nyquist, (nyquist*10)+1);

% Set data and output paths
data_path   = fullfile(project_root, 'data', 'preprocessed', 'regular');
output_path = fullfile(project_root, 'data', 'spectralData', 'csv');

% Get list of preprocessed .set files
file_list = dir(fullfile(data_path, 'sbj_*.set'));
nSubj = length(file_list);

%% === Wide Format for Machine Learning (Using Raw FFT Amplitudes) ===
% In wide format each row corresponds to one trial and each column is a feature
% (channel_frequency raw FFT amplitude value). This format is easier to feed into ML workflows.

% First, compute the total number of trials across all subjects.
totalTrials = 0;
for subj = 1:nSubj
    EEG = pop_loadset('filename', file_list(subj).name, 'filepath', data_path);
    totalTrials = totalTrials + size(EEG.data, 3);
end

% Preallocate arrays to store trial-level info and feature vectors.
% trial_info will hold basic metadata: Subject, Trial, and EventType.
% features will store the feature vector (nChan*nTarget raw FFT amplitudes) for each trial.
trial_info = cell(totalTrials, 3);
features = NaN(totalTrials, nChan * nTarget);

trial_counter = 1;
for subj = 1:nSubj
    filename = file_list(subj).name;
    fprintf('Processing Subject (Wide Format Raw FFT): %s\n', filename);

    % Load the dataset for the current subject.
    EEG = pop_loadset('filename', filename, 'filepath', data_path);
    EEG.data = double(EEG.data);

    % Remove the VEOG channel.
    EEG = pop_select(EEG, 'nochannel', {'VEOG'});
    if subj == 1 % Save it only once
        chan_labels = {EEG.chanlocs.labels};
    end
    % Determine indices corresponding to the desired time window.
    start_idx = dsearchn(EEG.times', min(time));
    end_idx   = dsearchn(EEG.times', max(time));

    numTrials = size(EEG.data, 3);

    for trial = 1:numTrials
        % Extract data for the current trial and time window.
        trial_data = EEG.data(:, start_idx:end_idx, trial);

        % Compute FFT along the time dimension (2nd dimension) with nfft points.
        fft_result = fft(trial_data, nfft, 2);

        % Compute raw FFT amplitude (absolute value) and normalize.
        fft_amp = abs(fft_result) / (pnts/2);

        event_type = EEG.event(trial).type;

        % Build the feature vector: for each channel, extract the amplitude at each target frequency.
        feat_vector = zeros(1, nChan * nTarget);
        for iChan = 1:nChan
            for tf = 1:nTarget
                target_frequency = target_freqs(tf);
                % Find the FFT bin closest to the target frequency.
                target_idx = dsearchn(frequencies', target_frequency);
                % Extract the raw amplitude at the target frequency.
                amplitude_value = fft_amp(iChan, target_idx);
                % Save the amplitude in the feature vector.
                feat_vector((iChan-1)*nTarget + tf) = amplitude_value;
            end
        end

        % Record trial information and feature vector.
        trial_info{trial_counter, 1} = subj;
        trial_info{trial_counter, 2} = trial;
        trial_info{trial_counter, 3} = event_type;
        features(trial_counter, :) = feat_vector;

        trial_counter = trial_counter + 1;
    end
end

% Create variable (column) names for the features.
% Use the labels from the chanlocs file instead of generic 'Ch1', 'Ch2', etc.
feature_names = cell(1, nChan * nTarget);
for iChan = 1:nChan
    for tf = 1:nTarget
        % Create a feature name using the channel label and target frequency
        feature_names{(iChan-1)*nTarget + tf} = sprintf('%s_%.1f', chan_labels{iChan}, target_freqs(tf));
    end
end

% Combine the trial info and features into one table.
trial_info_table = cell2table(trial_info, 'VariableNames', {'Subject','Trial','EventType'});
features_table = array2table(features, 'VariableNames', feature_names);
wide_table = [trial_info_table, features_table];

% Save the wide-format table to a CSV file.
csv_filename_wide = fullfile(output_path, 'FFT_Results_regular_wide_RawFFT.csv');
writetable(wide_table, csv_filename_wide);
fprintf('Wide-format CSV (Raw FFT) exported to %s\n', csv_filename_wide);

toc
