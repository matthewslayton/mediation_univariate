
clear all

cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts;
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/'
% I'm probably going to need functions from these here
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/Scripts/spm12');
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/function_files')

addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/;



% load IDs
itemIDs_tbl = readtable('itemIDs.xlsx'); % this has all 995 item IDs and labels

% BNA merged ROIs from shenyang
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ROI_BNA/';
allMasks = readtable('bnaMaskMeans.xlsx'); 
allMasksArr = table2array(allMasks);
maskArray_ID_data = allMasksArr(:,2:end);
numMasks = 102; 

subjInfo = struct('cIDs',[],'pIDs',[],'cmem',[],'pmem',[]);


% 19 subjects
subjectNum = {'002' '005' '006' '008' '009' '010' '011' '013' '014' '015' '016' '018' '019' '021' '022' '023' '024' '025' '026'};
subjectCounter = 1; % add 300 each time through loop so I can grab the 300 rows per subject

for subjects = 1:length(subjectNum)

    % Step 1: Grab the subject-specific rows 
    %subjectSpecificData = maskArray_ID_data(subjectCounter:subjectCounter+299,:);

    % Step 2: Cut PCs down to the 300 used in Encoding


    % **** instead of doing all of this, could get ID and TT num from the clustermeans table
    % currently activityPerTrialPerCustomCluster has ID but not TT. 
 
    all_betas_enc = dir(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Encoding_renamed/S',subjectNum{subjects},'/betas/*.nii'));
    all_IDs_enc = cell(numel(all_betas_enc),1);
    indices_TT = zeros(numel(all_betas_enc),1); % 1 for TT3 and 0 for anything else
    
    % get ID numbers used for the encoding trials
    for row = 1:numel(all_betas_enc)
    
        % get the item number. Could also get from custommasks table
        all_IDs_enc{row} = extractBetween(all_betas_enc(row).name,'Item', '_Enc');
    
        % is it TT3 (and should stay) or is it TT4, etc. 
        % not TT3 means there was a mismatch in covert naming (false alarm)
        TT_num_cell = extractBetween(all_betas_enc(row).name,'EncTT', '_');
        TT_num = cell2mat(TT_num_cell);
        if TT_num == '3'
            indices_TT(row) = 1;
        end
    end

%%%%%%%% break out from original code here

%%%% separte further into subsequently remembered and forgotten.
%%%%% What about the behavioral data from the fMRI subjects as well? 
%%%%% for that go to Shenyang's adjusted CMEM, go to predict_mem_6_matchShenyangAdjMemVal.m

% subjInfo is a struct that has mem, F, etc for all subjects, and remember
% they're cut to different lenghts. I want to go subject by subject, look
% at subjInfo.IDs to get the item ID for that trial. Then ask whether it
% was a hit (remembered) or a miss (forgotten)

%%% regular CMEM
% from predict_mem_2_enc_cmem.m and predict_mem_4_ret_cmem.m

    cmem_col = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'AC:AC');
    pmem_col = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'AD:AD');
    IDs = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'Z:Z');

    % process CMEM. Want 0 for miss (1 and 2), 1 for hit (3 and 4), and NaN
    % for catch trial to be removed later
    cmemorability = zeros(length(cmem_col),1);
    for number = 1:length(cmem_col)
        if cmem_col(number) == 1 %0 for miss
            cmemorability(number) = 0;
        elseif cmem_col(number) == 2 
            cmemorability(number) = 0;
        elseif cmem_col(number) == 3 %1 for hit
            cmemorability(number) = 1;
        elseif cmem_col(number) == 4  %3 for hit
            cmemorability(number) = 1;
        else
            cmemorability(number) = NaN; % maybe add NaNs so I can remove them later
        end
    end
    
    % IDs and memorability are still 330 at this point because catch trials
    % haven't been removed yet. Do isnan to get down to 300
    cmem_old = cmemorability(~isnan(cmemorability));
    cIDs_old = IDs(~isnan(cmemorability));

    % now IDs_old matches IDs_enc, so mem_old is in the correct order.
    % Now I have to remove the catch trials from mem_old like I did above

    for row = 1:numel(indices_TT)
        if indices_TT(row) == 0
            cmem_old(row) = NaN;  
            cIDs_old(row) = NaN;
        end 
    end

    % remove the rows with NaN
    cmem_old_short = cmem_old(~isnan(cmem_old));
    cIDs_old_short = cIDs_old(~isnan(cIDs_old));

    subjInfo(subjects).cIDs = cIDs_old_short;
    subjInfo(subjects).cmem = cmem_old_short;

    % process PMEM. Want 0 for miss (1 and 2), 1 for hit (3 and 4), and NaN
    % for catch trial to be removed later
    pmemorability = zeros(length(pmem_col),1);
    for number = 1:length(cmem_col)
        if pmem_col(number) == 1 %0 for miss
            pmemorability(number) = 0;
        elseif pmem_col(number) == 2 
            pmemorability(number) = 0;
        elseif pmem_col(number) == 3 %1 for hit
            pmemorability(number) = 1;
        elseif pmem_col(number) == 4  %3 for hit
            pmemorability(number) = 1;
        else
            pmemorability(number) = NaN; % maybe add NaNs so I can remove them later
        end
    end
    
    % IDs and memorability are still 330 at this point because catch trials
    % haven't been removed yet. Do isnan to get down to 300
    pmem_old = pmemorability(~isnan(pmemorability));
    pIDs_old = IDs(~isnan(pmemorability));

    % now IDs_old matches IDs_enc, so mem_old is in the correct order.
    % Now I have to remove the catch trials from mem_old like I did above

    for row = 1:numel(indices_TT)
        if indices_TT(row) == 0
            pmem_old(row) = NaN;  
            pIDs_old(row) = NaN;
        end 
    end

    % remove the rows with NaN
    pmem_old_short = pmem_old(~isnan(pmem_old));
    pIDs_old_short = pIDs_old(~isnan(pIDs_old));

    subjInfo(subjects).pIDs = pIDs_old_short;
    subjInfo(subjects).pmem = pmem_old_short;




    subjectCounter = subjectCounter + 300; % go to next subject's first row

end %subjects loop
    
%%%%% now I need to find the average mem value for each ID, but remember
%%%%% each subject won't have a value for every single ID

%% CMEM
% Initialize a map to store mem values for each ID
memMap = containers.Map('KeyType', 'double', 'ValueType', 'any');

% Loop through each subject's data
for i = 1:numel(subjInfo)
    currentSubject = subjInfo(i);
    IDs = currentSubject.cIDs;
    mem = currentSubject.cmem;

    % Loop through each ID in the current subject's data
    for j = 1:numel(IDs)
        currentID = IDs(j);
        currentMem = mem(j);

        % Check if the ID is already in the map
        if isKey(memMap, currentID)
            % If yes, append the mem value to the existing values
            memMap(currentID) = [memMap(currentID), currentMem];
        else
            % If no, create a new entry in the map
            memMap(currentID) = [currentMem];
        end
    end
end

% Convert cell array of keys to numeric array
allKeys = cell2mat(keys(memMap));

% Preallocate a cell array to store ID and corresponding average mem value
resultCell = cell(numel(allKeys), 2);

% Loop through each unique ID
uniqueIDs = unique(allKeys);
for i = 1:numel(uniqueIDs)
    currentID = uniqueIDs(i);
    memValues = memMap(currentID);
    averageMem = mean(memValues);
    
    % Store ID and average mem value in the result cell array
    resultCell{i, 1} = currentID;
    resultCell{i, 2} = averageMem;
end

% Now, resultCell contains ID and corresponding average mem value pairs
% Access resultCell{i, 1} for the ID and resultCell{i, 2} for the average mem value

set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white
% Convert result cell to numeric arrays
IDs = cell2mat(resultCell(2:end, 1));
avgMem = cell2mat(resultCell(2:end, 2));

% Scatter plot
scatter(IDs, avgMem, 'filled', 'MarkerFaceColor', [0.85 0.33 0.1]);

% Add labels to axes
xlabel('Image stimuli');
ylabel('Average memory performance');

% Add a line of best fit
coefficients = polyfit(IDs, avgMem, 1);
fitLine = polyval(coefficients, IDs);
hold on;
plot(IDs, fitLine, 'Color', [0 0.45 0.74], 'LineWidth', 2);

% Display correlation value
correlationValue = corr(IDs, avgMem);
annotation('textbox', [0.7, 0.8, 0.1, 0.1], 'String', sprintf('Correlation: %.2f', correlationValue), 'FitBoxToText', 'on', 'BackgroundColor', 'w');

% Customize the plot appearance
title('Average Conceptual Memory Performance');
grid on;
box on;

%% PMEM
figure
% Initialize a map to store mem values for each ID
memMap = containers.Map('KeyType', 'double', 'ValueType', 'any');

% Loop through each subject's data
for i = 1:numel(subjInfo)
    currentSubject = subjInfo(i);
    IDs = currentSubject.pIDs;
    mem = currentSubject.pmem;

    % Loop through each ID in the current subject's data
    for j = 1:numel(IDs)
        currentID = IDs(j);
        currentMem = mem(j);

        % Check if the ID is already in the map
        if isKey(memMap, currentID)
            % If yes, append the mem value to the existing values
            memMap(currentID) = [memMap(currentID), currentMem];
        else
            % If no, create a new entry in the map
            memMap(currentID) = [currentMem];
        end
    end
end

% Convert cell array of keys to numeric array
allKeys = cell2mat(keys(memMap));

% Preallocate a cell array to store ID and corresponding average mem value
resultCell = cell(numel(allKeys), 2);

% Loop through each unique ID
uniqueIDs = unique(allKeys);
for i = 1:numel(uniqueIDs)
    currentID = uniqueIDs(i);
    memValues = memMap(currentID);
    averageMem = mean(memValues);
    
    % Store ID and average mem value in the result cell array
    resultCell{i, 1} = currentID;
    resultCell{i, 2} = averageMem;
end

% Now, resultCell contains ID and corresponding average mem value pairs
% Access resultCell{i, 1} for the ID and resultCell{i, 2} for the average mem value

set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white
% Convert result cell to numeric arrays
IDs = cell2mat(resultCell(2:end, 1));
avgMem = cell2mat(resultCell(2:end, 2));

% Scatter plot
scatter(IDs, avgMem, 'filled', 'MarkerFaceColor', [0.85 0.33 0.1]);

% Add labels to axes
xlabel('Image stimuli');
ylabel('Average memory performance');

% Add a line of best fit
coefficients = polyfit(IDs, avgMem, 1);
fitLine = polyval(coefficients, IDs);
hold on;
plot(IDs, fitLine, 'Color', [0 0.45 0.74], 'LineWidth', 2);

% Display correlation value
correlationValue = corr(IDs, avgMem);
annotation('textbox', [0.7, 0.8, 0.1, 0.1], 'String', sprintf('Correlation: %.2f', correlationValue), 'FitBoxToText', 'on', 'BackgroundColor', 'w');

% Customize the plot appearance
title('Average Perceptual Memory Performance');
grid on;
box on;

%%%%% how about plotting factor 1 

data = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_IDs_mem_f01_forScatterPlot.xlsx')

