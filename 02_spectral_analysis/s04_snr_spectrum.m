%% s04_snr_spectrum.m
% =========================================================================
% SNR Spectrum - Overall SNR Across All Conditions (Left & Right)
% =========================================================================
% This script computes SNR for each subject, channel, and frequency across
% all trial conditions combined (no per-condition breakdown). Left and
% right conditions are computed separately. The output includes a MAT file
% with topographic SNR data and a long-format CSV for statistical analysis.
%
% Input:  Preprocessed EEG .set files
% Output: MAT file with overall SNR topographic data, long-format CSV
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
nChan      = 63;           % Number of channels after removing VEOG
srate      = 250;          % Sampling rate (Hz)
nyquist    = srate / 2;
resolution = 0.1;          % Desired frequency resolution (Hz)
time       = 0:1000/srate:10000;  % Time vector in ms
pnts       = length(time);
frequencies = linspace(0, nyquist, (nyquist*10)+1);  % e.g., 0:0.1:nyquist

% Parameters for SNR calculation
skipbins = 1;              % Number of bins to skip around the target frequency
numbins  = 8 + skipbins;    % Total number of bins used in SNR computation

% Define trial conditions (cell arrays of trigger strings)
% Left: 100-series (stimulus on right), Right: 200-series (stimulus on left)
conds = { {'111', '121', '131'}, {'211', '221', '231'} };

% Data path
data_path = fullfile(project_root, 'data', 'preprocessed', 'regular');

% Output paths for results
topo_outpath = fullfile(project_root, 'data', 'spectralData', 'topos');
output_path  = fullfile(project_root, 'data', 'spectralData', 'csv');

%% --- Get List of Processed Files ---
file_list = dir(fullfile(data_path, 'sbj_*.set'));
nSubj = length(file_list);

%% --- Preallocate SNR Storage ---
% SNR_left and SNR_right: dimensions [subject x channel x frequency]
SNR_left  = nan(nSubj, nChan, length(frequencies));
SNR_right = nan(nSubj, nChan, length(frequencies));

%% --- Loop Over Subjects ---
for subj = 1:nSubj
    % Load subject data
    filename = file_list(subj).name;
    fprintf('Processing subject: %s\n', filename);
    EEG = pop_loadset('filename', filename, 'filepath', data_path);
    EEG.data = double(EEG.data);

    % Remove the VEOG channel if present
    EEG = pop_select(EEG, 'nochannel', {'VEOG'});

    % Get channel labels for later export
    chanlocs = {EEG.chanlocs.labels};

    % Determine time indices corresponding to the defined time vector
    start_idx = dsearchn(EEG.times', min(time));
    end_idx   = dsearchn(EEG.times', max(time));

    %% --- Condition Selection ---
    % Left condition: triggers from conds{1}
    event_indices_left = [];
    for i = 1:length(conds{1})
        trigger = conds{1}{i};
        indices = find(strcmp({EEG.event.type}, trigger));
        event_indices_left = [event_indices_left, indices];  %#ok<AGROW>
    end
    % Average across trials for the left condition
    EEG_left_mean = squeeze(mean(EEG.data(:, :, event_indices_left), 3));

    % Right condition: triggers from conds{2}
    event_indices_right = [];
    for i = 1:length(conds{2})
        trigger = conds{2}{i};
        indices = find(strcmp({EEG.event.type}, trigger));
        event_indices_right = [event_indices_right, indices];  %#ok<AGROW>
    end
    EEG_right_mean = squeeze(mean(EEG.data(:, :, event_indices_right), 3));

    %% --- Compute SNR for Left Condition ---
    nfft = ceil(EEG.srate / resolution);
    for iChan = 1:nChan
        data_segment = EEG_left_mean(iChan, start_idx:end_idx);
        power = abs(fft(data_segment, nfft) / (pnts/2)).^2;
        for frex = numbins+1 : length(frequencies)-numbins-1
            numer = power(frex);
            denom = mean(power([frex-numbins:frex-skipbins, frex+skipbins:frex+numbins]));
            SNR_left(subj, iChan, frex) = numer / denom;
        end
    end

    %% --- Compute SNR for Right Condition ---
    for iChan = 1:nChan
        data_segment = EEG_right_mean(iChan, start_idx:end_idx);
        power = abs(fft(data_segment, nfft) / (pnts/2)).^2;
        for frex = numbins+1 : length(frequencies)-numbins-1
            numer = power(frex);
            denom = mean(power([frex-numbins:frex-skipbins, frex+skipbins:frex+numbins]));
            SNR_right(subj, iChan, frex) = numer / denom;
        end
    end
end

%% --- Combine Left and Right SNR ---
% Dimensions: [side x subject x channel x frequency]
SNR_pSide = nan(2, nSubj, nChan, length(frequencies));
SNR_pSide(1, :, :, :) = SNR_left;
SNR_pSide(2, :, :, :) = SNR_right;

% Optionally restrict to the first 250 frequency bins for further analysis
num_frequencies_to_keep = 250;
SNR_pSide = SNR_pSide(:,:,:,1:num_frequencies_to_keep);
freqs_trim = frequencies(1:num_frequencies_to_keep);

%% --- Save the SNR Results ---
SNR_all_Topo.data        = SNR_pSide;
SNR_all_Topo.frequencies = freqs_trim;
SNR_all_Topo.chanlocs    = EEG.chanlocs;  % from last subject (assumed same for all)
save_filename = fullfile(topo_outpath, 'SNR_all_Topo_regular.mat');
save(save_filename, 'SNR_all_Topo');
fprintf('SNR results saved to %s\n', save_filename);

%% --- Export Results as CSV ---
[nSide, nSubj, nChan, nFreq] = size(SNR_pSide);
total_rows = nSide * nSubj * nChan * nFreq;
% Columns: Side, Subject, Channel, Frequency, SNR
data_cell = cell(total_rows, 5);
chan_labels = {EEG.chanlocs.labels};
row_idx = 1;
for iSide = 1:nSide
    for subj = 1:nSubj
        for iChan = 1:nChan
            for freq_idx = 1:nFreq
                data_cell{row_idx, 1} = iSide;
                data_cell{row_idx, 2} = subj;
                data_cell{row_idx, 3} = chan_labels{iChan};
                data_cell{row_idx, 4} = freqs_trim(freq_idx);
                data_cell{row_idx, 5} = SNR_pSide(iSide, subj, iChan, freq_idx);
                row_idx = row_idx + 1;
            end
        end
    end
end

data_table = cell2table(data_cell, 'VariableNames', ...
    {'Side','Subject','Channel','Frequency','SNR'});
csv_filename = fullfile(output_path, 'all_SNR_data_regular.csv');
writetable(data_table, csv_filename);
fprintf('CSV exported to %s\n', csv_filename);

elapsedTime = toc;  % Stop the timer and get the elapsed time in seconds
fprintf('Total processing time: %.2f seconds.\n', elapsedTime);
