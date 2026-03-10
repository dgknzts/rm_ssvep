%% s03_snr_per_condition.m
% =========================================================================
% SNR Per Condition - Signal-to-Noise Ratio Computation by Response Condition
% =========================================================================
% This script computes SNR for each subject, channel, condition,
% frequency, and side (left/right). Conditions are grouped by response type
% (Resp 1, 2, 3). Condition 100 triggers correspond to the left ROI
% (stimulus on the right), and condition 200 triggers correspond to the
% right ROI (stimulus on the left).
%
% Input:  Preprocessed EEG .set files
% Output: MAT file with SNR topographic data, long-format CSV
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

%% --- Parameters & Paths ---
nChan      = 63;             % Number of channels (after removing VEOG)
srate      = 250;            % Sampling rate (Hz)
nyquist    = srate / 2;
resolution = 0.1;            % Desired frequency resolution (Hz)
time       = 0:1000/srate:10000;  % Time vector (ms)
pnts       = length(time);
frequencies = linspace(0, nyquist, (nyquist*10)+1);  % e.g., 0:0.1:nyquist

% Parameters for SNR calculation
skipbins = 1;              % Number of bins to skip around the target frequency
numbins  = 8 + skipbins;   % Total number of bins used in SNR computation

% Combined conditions:
% condition_100: when stimulus 100 is presented on the right -> LEFT ROI
% condition_200: when stimulus 200 is presented on the left -> RIGHT ROI
condition_100 = { '111', '121', '131' };
condition_200 = { '211', '221', '231' };

% Data path
data_path = fullfile(project_root, 'data', 'preprocessed', 'regular');

% Output paths
topo_outpath = fullfile(project_root, 'data', 'spectralData', 'topos');
output_path  = fullfile(project_root, 'data', 'spectralData', 'csv');

% Get the list of preprocessed .set files in the selected data folder
file_list = dir(fullfile(data_path, 'sbj_*.set'));
nSubj = length(file_list);

%% --- Preallocate Storage for SNR ---
% Dimensions: [Side (2) x Subject x Channel x Condition x Frequency]
SNR_pSide_pSbj_pChannel_pCond_pFrequency = nan(2, nSubj, nChan, length(condition_100), length(frequencies));

%% --- Loop Over Subjects ---
for subj = 1:nSubj
    % Load subject data
    filename = file_list(subj).name;
    fprintf('Processing Subject: %s\n', filename);
    EEG = pop_loadset('filename', filename, 'filepath', data_path);
    EEG.data = double(EEG.data);
    EEG = pop_select(EEG, 'nochannel', {'VEOG'});  % Remove VEOG channel

    % Determine indices corresponding to the defined time vector
    start_idx = dsearchn(EEG.times', min(time));
    end_idx   = dsearchn(EEG.times', max(time));

    %% --- Process Conditions ---
    for cond = 1:length(condition_100)
        % Find trials matching each trigger
        event_indices_100 = find(strcmp({EEG.event.type}, condition_100{cond}));
        event_indices_200 = find(strcmp({EEG.event.type}, condition_200{cond}));

        % Skip if no trials found for either condition
        if isempty(event_indices_100) && isempty(event_indices_200)
            fprintf('No trials found for Subject %d, Condition %d\n', subj, cond);
            continue;
        end

        % left_and_right: { data for condition_100, data for condition_200 }
        left_and_right = { EEG.data(:,:,event_indices_100), EEG.data(:,:,event_indices_200) };

        % Loop over sides: 1 = left, 2 = right
        for iLeftRight = 1:length(left_and_right)
            data_temp = cell2mat(left_and_right(iLeftRight));
            nfft_chan = ceil(EEG.srate / resolution);
            % Compute the power spectrum from the trial-averaged data for each channel
            power_chan = abs( fft( mean(data_temp(:, start_idx:end_idx, :), 3), nfft_chan, 2 ) / (pnts/2) ).^2;

            % Loop over channels and frequencies
            for iChan = 1:nChan
                power_chan_temp = power_chan(iChan,:);
                for frex = numbins+1 : length(frequencies)-numbins-1
                    target_freq = frequencies(frex);
                    target_idx  = dsearchn(frequencies', target_freq);
                    numer = power_chan_temp(target_idx);
                    denom = mean(power_chan_temp([target_idx-numbins:target_idx-skipbins, ...
                                                   target_idx+skipbins:target_idx+numbins]));
                    snr = numer / denom;
                    SNR_pSide_pSbj_pChannel_pCond_pFrequency(iLeftRight, subj, iChan, cond, frex) = snr;
                end
            end
        end
    end  % Combined condition loop
end  % Subject loop

%% --- Trim Frequency Axis & Save MAT Files ---
num_frequencies_to_keep = 250;
SNR_pSide_pSbj_pChannel_pCond_pFrequency = SNR_pSide_pSbj_pChannel_pCond_pFrequency(:,:,:,:,1:num_frequencies_to_keep);
frequencies = frequencies(1:num_frequencies_to_keep);

SNR_all_Topo.data        = SNR_pSide_pSbj_pChannel_pCond_pFrequency;
SNR_all_Topo.frex        = frequencies;
SNR_all_Topo.chanlocs    = EEG.chanlocs;  % Assumed same across subjects
save_filename = fullfile(topo_outpath, 'SNR_pCond_Topo_regular.mat');
save(save_filename, 'SNR_all_Topo');
fprintf('SNR results saved to %s\n', save_filename);

% --- Export Results as CSV ---
conditions = {'Resp 1', 'Resp 2', 'Resp 3'};  % Names for the combined conditions
channel_labels = {EEG.chanlocs.labels};         % Channel labels (taken from the last subject)
[nSide, nSubj, nChan, nCond, nFreq] = size(SNR_pSide_pSbj_pChannel_pCond_pFrequency);
total_rows = nSide * nSubj * nChan * nCond * nFreq;
data_cell = cell(total_rows, 6);

row_idx = 1;
for iSide = 1:nSide
    for subj = 1:nSubj
        for iChan = 1:nChan
            for cond = 1:nCond
                for freq_idx = 1:nFreq
                    snr_value = SNR_pSide_pSbj_pChannel_pCond_pFrequency(iSide, subj, iChan, cond, freq_idx);
                    data_cell{row_idx, 1} = iSide;
                    data_cell{row_idx, 2} = subj;
                    data_cell{row_idx, 3} = channel_labels{iChan};
                    data_cell{row_idx, 4} = frequencies(freq_idx);
                    data_cell{row_idx, 5} = conditions{cond};
                    data_cell{row_idx, 6} = snr_value;
                    row_idx = row_idx + 1;
                end
            end
        end
    end
end

data_table = cell2table(data_cell, 'VariableNames', ...
    {'Side','Subject','Channels','Frequency','Condition','SNR'});
csv_filename = fullfile(output_path, 'pCond_SNR_data_regular.csv');
writetable(data_table, csv_filename);
fprintf('CSV exported to %s\n', csv_filename);

toc
% Example processing time: 185.867719 seconds.
