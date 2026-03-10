function [combinedData, new_triggers] = new_extract_triggers(directoryPath)
% new_extract_triggers - Read and merge behavioral CSV files, create new trigger codes.
%   directoryPath - Path to the directory containing the CSV files to merge

% Find all CSV files in the directory
csvFiles = dir(fullfile(directoryPath, '*.csv'));

if isempty(csvFiles)
    error('No CSV files found in the specified directory.');
end

% Sort files by modification date (oldest to newest)
[~, idx] = sort([csvFiles.datenum]);
csvFiles = csvFiles(idx);

% Column names to keep
columnsToKeep = {'stimStartTrigger', 'amount_response_keys', 'session'};

% Initialize an empty table
combinedData = table();

% Read each CSV file and process
for i = 1:length(csvFiles)
    % Build the full file path
    filePath = fullfile(directoryPath, csvFiles(i).name);

    % Read the file
    data = readtable(filePath);

    % Keep only the specified columns
    data = data(:, columnsToKeep);

    % Remove rows containing NaN
    data = rmmissing(data);

    % Append data to the combined table
    combinedData = [combinedData; data];
end

% Add row index column
combinedData.index = (1:height(combinedData))';

new_triggers = strings(height(combinedData), 1); % Initialize empty string array
for k = 1:height(combinedData)
    % Determine stimulus side from trigger code
    if combinedData.stimStartTrigger(k) == 111
        stim_value = 1;
    elseif combinedData.stimStartTrigger(k) == 222
        stim_value = 2;
    else
        error('Invalid stimStartTrigger value: %d', combinedData.stimStartTrigger(k));
    end

    % Create new trigger value: {stimulus_side}{amount_response}{1}
    new_triggers(k) = sprintf('%d%d1', ...
        stim_value, ...
        combinedData.amount_response_keys(k));
end

% Add the new trigger column to the table
combinedData.new_trigger = new_triggers;

% Verify the expected number of rows (192 trials per subject)
if height(combinedData) == 192
    disp('combinedData table has the correct number of rows (192).');
else
    disp(['Warning: combinedData table has an unexpected number of rows! Row count: ', num2str(height(combinedData))]);
end
