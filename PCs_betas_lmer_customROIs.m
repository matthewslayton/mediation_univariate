% This is a version of PCs_betas_mixedEffects_allSubj_complete.m
% both scripts will prepare activity per ROI per trial in a table to be
% used in R to do a mixed effect moddel. This one works with custom-made
% ROIs (which are made with activityPerTrialPerCustomCluster.m). The other
% uses pre-made activity per atlas ROI (from Cortney)

set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white

cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts;
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/'
% I'm probably going to need functions from these here
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/Scripts/spm12');
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/function_files')

%%%%%% load PC score or factor score (F) that has 995 rows for all the
%%%%%% items. This code will select the ones needed.
%%%%%% Remember this is different from the stimulus-presentation order
%%%%%% subsets that have 300 rows
%load all_score.mat %loads var 'score' <-- rename because this is a function

%%%%%%%%%%%%%%%%%%%%%%% one of two options
%%%%% load factors for FA_results (all 10 factors)
% % load twentyFactors.mat %loads var 'F', performed on 200 features
% % load tenFactors_500features.mat % done on entire feature matrix
% %load fortyFactors_200features.mat
% 
% % can load FAs on subsets of the feature matrix.
% % I have encycl, noTax, tax, fcn, vis
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FA_results/;
% 
% load 250feat_encycl_F.mat
% load 200feat_noTax_F.mat
% load 200feat_vis_F.mat
% load 150feat_fcn_F.mat
% load 140feat_tax_F.mat
% 
% numFactors = size(F,2); %for all of these it's 10 factors
% 

% look at nmf_test.m for nonnegative matrix factorization

%%%%%%%%%%%%%%%%%%%%%%% one of two options
%%%% load factors for Factanal_results (variable factors based on scree()in howManyFactors.R

%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/;

% v3 has mat files with the stimulus IDs, though now have been copied to v6
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v3/;
% most up-to-date. use this one
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v6/; %includes Hipp_A and Hipp_P

% v4 is with the thresholded mats but I ran NNMF with 8 facs (and 5 and 4
% for fcn and fcn-300). v5 is with most of the cols (or rows if the rows
% are smaller) as fac numbers, so like, 100.
%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v4/;
%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v5/; %NMF with as many facs as possible 

% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/;
% in this script the factor scores are called F

%%%%% all this stuff doesn't need to be done more than once, so I'm pasting
%%%%% it here before the subject loop
%currFac = 1; % 1 through 10. Set this value when I select these so I can make if statements lower down so there's less manual work for me

% load lex/con mem and vis/per mem and do the same thing with lmer as I did
% with Factors and PCs. These are from the MTurk norming study
% these come from mem_MTurk_betas_masks.m
load visMem_CR.mat
load lexMem_CR.mat

%%%%%%%%%%%%%%
%%% NOTE
%%%% Corrected Recognition is fine, but there's an additional issue
%%%% HR - FAR doesn't capture subject-specific behavior. For example, if 
%%%% HR=0.95 and FAR=0.45, then CR = 0.5. Trouble is, you'd get the same CR
%%%% if you did 0.8-0.3. Z-scores (which you get from d') are better
%%%% because the extreme values will be exaggerated. That means an HR of
%%%% 0.95 will have a higher value than an HR of 0.9, which you want.
%%%% The other issue is that we do a 4-point scale where 1 and 4 are more
%%%% confident and 2 and 3 are less confident. If we binarize them (where 3
%%%% and 4 are hits and 1 and 2 are misses), we don't capture a subject's 
%%%% bias. So, if they pick all 4s and 3s (they are a liberal chooser), then 
%%%% choosing 3 (not quite as confident) is better to count as a miss, not a hit. 
%%%% Shenyang's script has CMEM_adj, which adjusts the mem scores with
%%%% respect to each subject's answering behavior. 
%%%% The rmd script is R_0_prepare_data.Rmd and the values are in df_MEMadj.csv

% Summary: visMem_CR and lexMem_CR are no good. Instead load the corrected values

%%% the MTurk data memData.xlsx only has averages.
%%% the fMRI data has the real responses, and for that I need:
%%% predict_mem_4_ret_cmem.m and predict_mem_5_ret_pmem.m
%%% These scripts get the responses out of the Behav folder

%fixedMemData = readtable('df_MEMadj.csv');

% Note, the mem data has NaNs in it. I use NaNs later to remove catch trials, 
% so I have to change the existing NaNs to something else
for row = 1:length(lexMem_CR)
    if isnan(lexMem_CR(row))
        lexMem_CR(row) = 999;
    end
    if isnan(visMem_CR(row))
        visMem_CR(row) = 999;
    end
end
%     for row = 1:length(lexMem_CR)
%         if isnan(lexMem_CR(row))
%             lexMem_CR(row) = NaN;
%         end
%         if isnan(visMem_CR(row))
%             visMem_CR(row) = NaN;
%         end
%     end
% load IDs --- you need different reference tables for the 995 and 300
% types
itemIDs_tbl = readtable('itemIDs.xlsx'); % this has all 995 item IDs and labels
%itemIDs_330 = readtable('itemIDs_330.xlsx'); %% DON'T USE?
itemIDs_300 = readtable('itemIDs_300.xlsx');

% load the per ROI activity, which comes from
% activityPerTrialPerCustomCluster_cortney.m Does ROI and clusters

% BNA merged ROIs from shenyang
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ROI_BNA/';
allMasks = readtable('bnaMaskMeans.xlsx'); 
allMasksArr = table2array(allMasks);
maskArray_ID_data = allMasksArr(:,2:end);
numMasks = 102; 

% atlas masks and clusters
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ROI_clusters;
% don't forget to save csv output as xlsx
%customMasks = readtable('clustermeans_cleanMasks.xlsx'); % these are the vis/lex interaction term masks
%customMasks = readtable('atlasMaskMeans.xlsx'); % these are the vis/lex interaction term masks
% don't forget to go to line 188 and change numMasks
% could also combine these tables and do them all at once. % allMasks = customMasks + atlasMasks
%customMasksArray = table2array(customMasks);
%maskArray_ID_data = customMasksArray(:,2:end);

% allMasks = readtable('allMaskMeans.xlsx');
% allMasksArr = table2array(allMasks);
% maskArray_ID_data = allMasksArr(:,2:end);

%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/combined_masks;


for currFac = 1:4 %18 %end down on line ~2992

%currFac = 1;
    
%     if currFac == 1
%         load fac_encycl_F_jan23.mat %currFac = 1
%         F = fac_encycl; 
%         numFactors = size(F,2);
%     elseif currFac == 2
%         load fac_encycl_300_F_jan23.mat %currFac = 2, etc
%         F = fac_encycl_300; 
%         numFactors = size(F,2);
%     elseif currFac == 3
%         load fac_vis_F_jan23.mat
%         F = fac_vis; 
%         numFactors = size(F,2);
%     elseif currFac == 4
%         load fac_vis_300_F_jan23.mat
%         F = fac_vis_300; 
%         numFactors = size(F,2);
%     elseif currFac == 5
%         load fac_fcn_F_jan23.mat
%         F = fac_fcn; 
%         numFactors = size(F,2);
%     elseif currFac == 6
%         load fac_fcn_300_F_jan23.mat
%         F = fac_fcn_300; 
%         numFactors = size(F,2);
% 
%     end %end currFac if statement. the goal is to avoid having to manually highlight and run each of these
    
    if currFac == 1
        load W_encycl.mat %currFac = 1
        F = W_encycl(:,1:8); 
        numFactors = size(F,2);
%         load W_encycl_mult.mat %currFac = 1
%         F = W_encycl_mult;
%         load W_encycl_als.mat
%         F = W_encycl_als;
        

    elseif currFac == 2
        load W_encycl_300.mat %currFac = 2, etc
        F = W_encycl_300(:,1:8); 
        numFactors = size(F,2);
    elseif currFac == 3
        load W_vis.mat
        F = W_vis(:,1:8); 
        numFactors = size(F,2);
    elseif currFac == 4
        load W_vis_300.mat
        F = W_vis_300(:,1:8); 
        numFactors = size(F,2);
    elseif currFac == 5
        load W_fcn.mat
        F = W_fcn(:,1:5); 
        numFactors = size(F,2);
    elseif currFac == 6
        load W_fcn_300.mat
        F = W_fcn_300(:,1:4); 
        numFactors = size(F,2);

    elseif currFac == 7
        load W_all.mat
        F = W_all(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 8
        load W_all_300.mat
        F = W_all_300(:,1:8);
        numFactors = size(F,2);

        %%%%% indices_inOrder don't match. Need mat-specific row numbers
        % or is it just the odd-numbered ones because they're not 995?
    elseif currFac == 9
        load W_outside.mat
        F = W_outside(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 10
        load W_outside_300.mat
        F = W_outside_300(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 11
        load W_home.mat
        F = W_home(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 12
        load W_home_300.mat
        F = W_home_300(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 13
        load W_home_tool.mat
        F = W_home_tool(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 14
        load W_home_tool_300.mat
        F = W_home_tool_300(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 15
        load W_animal.mat
        F = W_animal(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 16
        load W_animal_300.mat
        F = W_animal_300(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 17
        load W_food.mat
        F = W_food(:,1:8);
        numFactors = size(F,2);

    elseif currFac == 18
        load W_food_300.mat
        F = W_food_300(:,1:8);
        numFactors = size(F,2);
    
    end 

    
    %%%% fall 2022
    % addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Factanal_results;
    % load fac_encycl_F.mat
    % % in this script the factor scores are called F
    % F = fac_encycl; %25 factors
    % numFactors = size(F,2);
    % load fac_tax_F.mat
    % F = fac_tax; %15 factors
    % numFactors = size(F,2);
    % load fac_vis_F.mat
    % F = fac_vis; %20 factors
    % numFactors = size(F,2);
    % load fac_fcn_F.mat
    % F = fac_fcn; %10 factors
    % numFactors = size(F,2);
    
    
   
    %% prepare betas per subject per trial per custom ROI
    
    %%%%% select one. either 'all' or remembered and forgotten
    % struct for each subject's score_subset and activity
    subjInfo = struct('Fval',[],'IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);
    
    subjInfo_remembered = struct('Fval',[],'IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);
    subjInfo_forgotten = struct('Fval',[],'IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);
    
    %%%% don't use
    %subjInfo = struct('PCval',[],'Fval',[],'IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);
    %memInfo = struct('cmem',[],'pmem',[],'cmem_items',[],'pmem_items',[]);
    
    % customMasks has values for the 300 trials
    % we have to cut any trial that isn't labelled with TT3. 
    % those are where the subject made some error
    
    % once we have the 260ish we take the subset of PCs and Factors
    % finally, we get the 260ish activity values from the 300
    
    % 19 subjects
    subjectNum = {'002' '005' '006' '008' '009' '010' '011' '013' '014' '015' '016' '018' '019' '021' '022' '023' '024' '025' '026'};
    subjectCounter = 1; % add 300 each time through loop so I can grab the 300 rows per subject
    % subject-specific data is on rows 2 to 301, 302 to 601, 602 to 901 etc.
    % my var cuts off the col names, so start on index 1
    
    % ~~~ % skip down to line 317 (after tables) for a repeat of the following loop.
    % I'd like to average the activity but because of the false alarms the
    % lengths are all different. I need to cut them to the length of the
    % shortest which is S021's 249
    
    % I need to use this code for compare_memorabilty.m
    %load pmem_combined.mat
    %load cmem_combined.mat 
    % load pmem_items.mat
    % load cmem_items.mat
    
    %%%% the factors, mem, etc are all based on the 995 items. We need to cut
    %%%% down to the 300 items used in the study
    
    for subjects = 1:length(subjectNum)
    
        % Step 1: Grab the subject-specific rows 
        subjectSpecificData = maskArray_ID_data(subjectCounter:subjectCounter+299,:);
    
        % Step 2: Cut PCs/factors down to the 300 used in Encoding
    
    
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
        end %end row loop
    
        % now I have the ID numbers that are used for the encoding trials of this
        % subject
        % I have to find the PC val etc that is on the row where that ID number is in
        % itemIDs_tbl. So, I want the row number
    
        indices_inOrder = zeros(numel(all_betas_enc),1);
        for idNumber = 1:numel(all_betas_enc)
            % all_IDs_enc is cell array of cells, so I have to peel them out
            % then convert the char you get out to a num to match the table items
            
            if currFac == 1 || currFac==3 || currFac==5 || currFac==7 %F has 995 rows %if ismember(currFac, [1, 3, 5, 7])
                index = find(itemIDs_tbl{:,2}==str2num(cell2mat(all_IDs_enc{idNumber}))); %find the four-digit ID num in the second col of the table
            %elseif mod(currFac,2) == 0 %F has 300 rows
            elseif currFac == 2 || currFac==4 || currFac==6 || currFac==8
                %%%% this isn't working
                %%%% I still need to know WHICH IDs are being presented for
                %%%% each trial. As written:
                % index = idNumber; % all_IDs_enc is 300 and so are the 300 fac scores
                %%%% I just get 1-300 for each subject, and then I take a
                %%%% subset of every other variable based on that. My F
                %%%% values don't match the same items so I can't run
                %%%% mediation! 
                %%%% I think I need to do the same procedure as the 995
                %%%% rows. I don't have to worry about items being 'out of
                %%%% bounds' because they won't be. I shouldn't have more
                %%%% than 300 items per subject selected from 300 total
                %%%% items.

                % i need to grab indices from a 300-specific table
                index = find(itemIDs_300{:,1}==str2num(cell2mat(all_IDs_enc{idNumber})));

            elseif currFac == 9 
                load outside_IDs.mat
                index = find(outside_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            elseif currFac == 10
                load outside_300_IDs.mat
                index = find(outside_300_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            elseif currFac==11
                load home_IDs.mat
                index = find(home_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));    
            elseif currFac == 12
                load home_300_IDs.mat
                index = find(home_300_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));   
            elseif currFac==13
                load home_tool_IDs.mat
                index = find(home_tool_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));    
            elseif currFac == 14
                load home_tool_300_IDs.mat
                index = find(home_tool_300_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));   
            elseif currFac==15
                load animal_IDs.mat
                index = find(animal_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            elseif currFac == 16
                load animal_300_IDs.mat
                index = find(animal_300_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            elseif currFac==17
                load food_IDs.mat
                index = find(food_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            elseif currFac == 18
                load food_300_IDs.mat
                index = find(food_300_IDs == str2num(cell2mat(all_IDs_enc{idNumber})));  
            end

            if isempty(index) %the item subset IDs won't match all of the trials, so fill with NaNs where the trial has an item from a different category
                indices_inOrder(idNumber) = NaN;
            else
                indices_inOrder(idNumber) = index;
            end
        end
    
        %%% save indices_inOrder to use with howManyFactors.R
        %save('indices_inOrder.mat','indices_inOrder')
    
        %score_subset = zeros(numel(all_betas_enc),994); 
        %factor_subset = zeros(numel(all_betas_enc),20); 
        factor_subset = zeros(numel(all_betas_enc),numFactors);
        lexMem_subset = zeros(numel(all_betas_enc),1);
        visMem_subset = zeros(numel(all_betas_enc),1);
       
        % this is for compare_memorability.m
        % I want only the col for a given iteration of the subjects loop
    %     cmem_subset = cmem_combined(:,subjects);
    %     pmem_subset = pmem_combined(:,subjects);
    %     cmem_items_subset = cmem_items(:,subjects);
    %     pmem_items_subset = pmem_items(:,subjects);
    
         if currFac == 1 || currFac==3 || currFac==5 || currFac==7 %F has 995 rows 
            % get the 300 subset for this subject 
            for row = 1:numel(all_betas_enc)
                if isnan(indices_inOrder(row))
                    factor_subset(row,:) = NaN(1,numFactors);
                    lexMem_subset(row,:) = NaN;
                    visMem_subset(row,:) = NaN;
                    subjectSpecificData(row,:) = NaN(1,size(subjectSpecificData,2));
                    continue 
                else
                    %score_subset(row,:) = score(indices_inOrder(row),:); % the entire row which is all the PCs
                    factor_subset(row,:) = F(indices_inOrder(row),:); % the entire row which is all the factors
                    lexMem_subset(row,:) = lexMem_CR(indices_inOrder(row),:);
                    visMem_subset(row,:) = visMem_CR(indices_inOrder(row),:);
                end
            end

         elseif currFac == 2 || currFac==4 || currFac==6 || currFac==8 %if we're running the 300's
            % we have to do something different. We're not cutting DOWN F,
            % we're re-ordering it

            for row = 1:numel(all_betas_enc)
                if isnan(indices_inOrder(row))
                    factor_subset(row,:) = NaN(1,numFactors);
                    lexMem_subset(row,:) = NaN;
                    visMem_subset(row,:) = NaN;
                    subjectSpecificData(row,:) = NaN(1,size(subjectSpecificData,2));
                    continue 
                else
                    %score_subset(row,:) = score(indices_inOrder(row),:); % the entire row which is all the PCs
                    factor_subset(row,:) = F(indices_inOrder(row),:); % the entire row which is all the factors
                    lexMem_subset(row,:) = lexMem_CR(indices_inOrder(row),:);
                    visMem_subset(row,:) = visMem_CR(indices_inOrder(row),:);
                end
            end

         end


    
        % Step 3: Remove any trial that isn't labelled with TT3
        % in the previous step I made indices_TT. If there's a 1 in the row, keep it
    
        %subject_numbers = customMasksArray(:,1);
        subject_numbers = allMasksArr(:,1);
        
        for row = 1:numel(indices_TT)
            if indices_TT(row) == 1
                %score_subset(row,:) = score_subset(row,:);
                factor_subset(row,:) = factor_subset(row,:);
                subjectSpecificData(row,:) = subjectSpecificData(row,:);
                subject_numbers(row) = subject_numbers(row);
                lexMem_subset(row,:) = lexMem_subset(row,:);
                visMem_subset(row,:) = visMem_subset(row,:);
    
                % this is for compare_memorability.m
    %             cmem_subset(row,:) = cmem_subset(row,:);
    %             pmem_subset(row,:) = pmem_subset(row,:);
    %             cmem_items_subset(row,:) = cmem_items_subset(row,:);
    %             pmem_items_subset(row,:) = pmem_items_subset(row,:);
            else 
               % score_subset(row,:) = NaN;
                factor_subset(row,:) = NaN;
                subjectSpecificData(row,:) = NaN;
                subject_numbers(row) = NaN;
                lexMem_subset(row,:) = NaN;
                visMem_subset(row,:) = NaN;
    
                 % this is for compare_memorability.m
    %             cmem_subset(row,:) = NaN;
    %             pmem_subset(row,:) = NaN;
    %             cmem_items_subset(row,:) = NaN;
    %             pmem_items_subset(row,:) = NaN
              
            end 
        end
    
        % remove the rows with NaN
        %score_subset(any(isnan(score_subset), 2), :) = [];
        factor_subset(any(isnan(factor_subset), 2), :) = [];
        lexMem_subset(any(isnan(lexMem_subset), 2), :) = [];
        visMem_subset(any(isnan(visMem_subset), 2), :) = [];
        subjectSpecificData(any(isnan(subjectSpecificData), 2), :) = [];
        subject_numbers(any(isnan(subject_numbers), 2), :) = [];
    %     cmem_subset(any(isnan(cmem_subset), 2), :) = [];
    %     pmem_subset(any(isnan(pmem_subset), 2), :) = [];
    %     cmem_items_subset(any(isnan(cmem_items_subset), 2), :) = [];
    %     pmem_items_subset(any(isnan(pmem_items_subset), 2), :) = [];
    
    %%%%%% for currFac = 10:18 need to skip this mem code for now
    %%%%%%%% break out from original code here
    
    %%%% separte further into subsequently remembered and forgotten.
    %%%%% What about the behavioral data from the fMRI subjects as well? 
    %%%%% for that go to Shenyang's adjusted CMEM, go to predict_mem_6_matchShenyangAdjMemVal.m
    
    % subjInfo is a struct that has mem, F, etc for all subjects, and remember
    % they're cut to different lenghts. I want to go subject by subject, look
    % at subjInfo.IDs to get the item ID for that trial. Then ask whether it
    % was a hit (remembered) or a miss (forgotten)
    
%     %%% regular CMEM
%     % from predict_mem_2_enc_cmem.m and predict_mem_4_ret_cmem.m
%     
%         cmem_col = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'AC:AC');
%         IDs = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'Z:Z');
%     
% %         IDs_360 = IDs;
% %         save('IDs_360.mat','IDs_360')
%         % process CMEM. Want 0 for miss (1 and 2), 1 for hit (3 and 4), and NaN
%         % for catch trial to be removed later
%         memorability = zeros(length(cmem_col),1);
%         for number = 1:length(cmem_col)
%             if cmem_col(number) == 1 %0 for miss
%                 memorability(number) = 0;
%             elseif cmem_col(number) == 2 
%                 memorability(number) = 0;
%             elseif cmem_col(number) == 3 %1 for hit
%                 memorability(number) = 1;
%             elseif cmem_col(number) == 4  %3 for hit
%                 memorability(number) = 1;
%             else
%                 memorability(number) = NaN; % maybe add NaNs so I can remove them later
%             end
%         end
% 
%         %%% I need to know whether a subject remembered a given item or not
% %         save(strcat('IDs_',num2str(subjects),'_330.mat'),'IDs')
% %         save(strcat('mem_',num2str(subjects),'_330.mat'),'memorability')
% 
% %         IDs_330 = IDs;
% %         save('IDs_330.mat','IDs_330')
% 
%         
%         % IDs and memorability are still 330 (360?) at this point because catch trials
%         % haven't been removed yet. Do isnan to get down to 300
%         mem_old = memorability(~isnan(memorability));
%         IDs_old = IDs(~isnan(memorability));
%     
%         % now IDs_old matches IDs_enc, so mem_old is in the correct order.
%         % Now I have to remove the catch trials from mem_old like I did above
%     
%         for row = 1:numel(indices_TT)
%             if indices_TT(row) == 0
%                 mem_old(row) = NaN;            
%               
%             end 
%         end
%     
%         % remove the rows with NaN
%         mem_old_short = mem_old(~isnan(mem_old));
% 
%     
%         % remove the zeros after
%        % score_remembered = zeros(length(mem_old_short),994);
%        % score_forgotten = zeros(length(mem_old_short),994);
%         factor_remembered = zeros(length(mem_old_short),size(F,2));
%         factor_forgotten = zeros(length(mem_old_short),size(F,2));
%         subjectSpecificData_remembered = zeros(length(mem_old_short),numMasks+1); 
%         subjectSpecificData_forgotten = zeros(length(mem_old_short),numMasks+1); 
%         lexMem_remembered = zeros(length(mem_old_short),1);
%         lexMem_forgotten = zeros(length(mem_old_short),1);
%         visMem_remembered = zeros(length(mem_old_short),1);
%         visMem_forgotten = zeros(length(mem_old_short),1);
%         subject_numbers_remembered = zeros(length(mem_old_short),1);
%         subject_numbers_forgotten = zeros(length(mem_old_short),1);
%     
%         for row = 1:numel(mem_old_short)
%             if mem_old_short(row) == 1
%                 %score_remembered(row,:) = score_subset(row,:);
%                 factor_remembered(row,:) = factor_subset(row,:);
%                 subjectSpecificData_remembered(row,:) = subjectSpecificData(row,:);
%                 lexMem_remembered(row,:) = lexMem_subset(row,:);
%                 visMem_remembered(row,:) = visMem_subset(row,:);
%                 subject_numbers_remembered(row) = subject_numbers_remembered(row);
%             elseif mem_old_short(row) == 0       
%                % score_forgotten(row,:) = score_subset(row,:);
%                 factor_forgotten(row,:) = factor_subset(row,:);
%                 subjectSpecificData_forgotten(row,:) = subjectSpecificData(row,:);
%                 lexMem_forgotten(row,:) = lexMem_subset(row,:);
%                 visMem_forgotten(row,:) = visMem_subset(row,:);
%                 subject_numbers_forgotten(row) = subject_numbers_forgotten(row);
%             end
%         end
%     
%         % remove zero rows
%         %score_remembered(all(~score_remembered,2),:) = [];
%        % score_forgotten(all(~score_forgotten,2),:) = [];
%        if currFac == 1 || currFac == 2 || currFac == 3 || currFac == 4
%             factor_remembered(all(~factor_remembered,2),:) = [];
%             factor_forgotten(all(~factor_forgotten,2),:) = [];
%             subjectSpecificData_remembered(all(~subjectSpecificData_remembered,2),:) = [];
%             subjectSpecificData_forgotten(all(~subjectSpecificData_forgotten,2),:) = [];
%             lexMem_remembered(all(~lexMem_remembered,2),:) = [];
%             lexMem_forgotten(all(~lexMem_forgotten,2),:) = [];  
%             visMem_remembered(all(~visMem_remembered,2),:) = [];
%             visMem_forgotten(all(~visMem_forgotten,2),:) = [];  
%             subject_numbers_remembered(all(~subject_numbers_remembered,2),:) = [];
%             subject_numbers_forgotten(all(~subject_numbers_forgotten,2),:) = [];
%         
%        elseif currFac == 5 || currFac == 6 %the numbers just aren't lining up. Try this
%             subjectSpecificData_remembered(all(~factor_remembered,2),:) = [];
%             subjectSpecificData_forgotten(all(~factor_forgotten,2),:) = [];
%             factor_remembered(all(~factor_remembered,2),:) = [];
%             factor_forgotten(all(~factor_forgotten,2),:) = [];
%             lexMem_remembered(all(~lexMem_remembered,2),:) = [];
%             lexMem_forgotten(all(~lexMem_forgotten,2),:) = [];  
%             visMem_remembered(all(~visMem_remembered,2),:) = [];
%             visMem_forgotten(all(~visMem_forgotten,2),:) = [];  
%             subject_numbers_remembered(all(~subject_numbers_remembered,2),:) = [];
%             subject_numbers_forgotten(all(~subject_numbers_forgotten,2),:) = [];
% 
%        end
% 
%         % add to struct
%        % subjInfo_remembered(subjects).PCval = score_remembered;
%         subjInfo_remembered(subjects).Fval = factor_remembered;
%         subjInfo_remembered(subjects).IDs = subjectSpecificData_remembered(:,1);
%         subjInfo_remembered(subjects).subjNum = subject_numbers_remembered;
%         subjInfo_remembered(subjects).activityVal = subjectSpecificData_remembered(:,2:end);
%         subjInfo_remembered(subjects).lexMem = lexMem_remembered;
%         subjInfo_remembered(subjects).visMem = visMem_remembered;
%     
%         %subjInfo_forgotten(subjects).PCval = score_forgotten;
%         subjInfo_forgotten(subjects).Fval = factor_forgotten;
%         subjInfo_forgotten(subjects).IDs = subjectSpecificData_forgotten(:,1);
%         subjInfo_forgotten(subjects).subjNum = subject_numbers_forgotten;
%         subjInfo_forgotten(subjects).activityVal = subjectSpecificData_forgotten(:,2:end);
%         subjInfo_forgotten(subjects).lexMem = lexMem_forgotten;
%         subjInfo_forgotten(subjects).visMem = visMem_forgotten;
%     
%     
%     
%     
%     %%%%%%% return to original code here
    
    
    
%         % store the PCs, factors, and activity in one big struct
%        % subjInfo(subjects).PCval = score_subset;

        %%% comment/uncomment here
        subjInfo(subjects).Fval = factor_subset;
        subjInfo(subjects).IDs = subjectSpecificData(:,1);
        subjInfo(subjects).subjNum = subject_numbers;
        subjInfo(subjects).activityVal = subjectSpecificData(:,2:end);
        subjInfo(subjects).lexMem = lexMem_subset;
        subjInfo(subjects).visMem = visMem_subset;
    % 
    %     memInfo(subjects).cmem = cmem_subset;
    %     memInfo(subjects).pmem = pmem_subset;
    %     memInfo(subjects).cmem_items = cmem_items_subset;
    %     memInfo(subjects).pmem_items = pmem_items_subset;
    
        subjectCounter = subjectCounter + 300; % go to next subject's first row
    end %indiv subject loop

% end % you can add this to end the currFac loop here so you don't run
% anything else. Do this for testing to see if I can/need to do mem
% separately
    %%%%%%%%%%%%%%%%%%%%% various save statements for different purposes
    %%%%%%%%%%%%%%%%%%%%% don't really lend themselves to if statements so
    %%%%%%%%%%%%%%%%%%%%% needs to be done manually
    % cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP'
    % save('subjInfo_allROIs.mat','subjInfo')
    % 
    % save('memInfo.mat','memInfo')
    % 
    % 
    % % to not use cd you can assign var to new var with entire pathname and save that
    % save('subjInfo_customROIs_clean.mat','subjInfo')
    % %save('subjInfo_customROIs_F500.mat','subjInfo')
    % %save('subjInfo_customROIs_F200_m40.mat','subjInfo')
    % save('subjInfo_memAtlasROIs.mat','subjInfo')
    
    
    %% make table to prepare for lmer
    % cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
    % load subjInfo_allROIs.mat %if you're starting from here can just load and skip the stuff above
    
    %load subjInfo_customROIs_clean.mat
    %load subjInfo_memAtlasROIs.mat
    % note, PCs_betas_mixedEffects_allSubj_complete.m loads the activity data
    % as cells. It starts as rows that we horzcat, needs to be transposed, and
    % then you get 5225x30, which is the number of ROIs
    % In this script, I loaded the activity data as doubles. It's all cols, so
    % we vertcat and we're done
    
   %%%% this is my normal/standard code for filling the all trials subjInfo struct
    %numMasks = 11; % all custom clusters = 14, but the ones I want = 11. 
    %numMasks = 6; % atlas ROIs
    %numMasks = 17;
    numMasks = 102; %BNA. There are 104 cols but first two are subject and item
    %allActivityMat = zeros(5225,numMasks);
    clear allActivityMat
    for ROI = 1:numMasks
        activityCol = vertcat(subjInfo(1).activityVal(:,ROI),subjInfo(2).activityVal(:,ROI), ...
            subjInfo(3).activityVal(:,ROI),subjInfo(4).activityVal(:,ROI),subjInfo(5).activityVal(:,ROI), ...
            subjInfo(6).activityVal(:,ROI),subjInfo(7).activityVal(:,ROI),subjInfo(8).activityVal(:,ROI), ...
            subjInfo(9).activityVal(:,ROI),subjInfo(10).activityVal(:,ROI),subjInfo(11).activityVal(:,ROI), ...
            subjInfo(12).activityVal(:,ROI),subjInfo(13).activityVal(:,ROI),subjInfo(14).activityVal(:,ROI), ...
            subjInfo(15).activityVal(:,ROI),subjInfo(16).activityVal(:,ROI),subjInfo(17).activityVal(:,ROI), ...
            subjInfo(18).activityVal(:,ROI),subjInfo(19).activityVal(:,ROI));
        allActivityMat(:,ROI) = activityCol;
    end

    %save('allActivityMat_jan23ROI.mat','allActivityMat')
    
    %oneROI_PCs = vertcat(subjInfo.PCval(1:10)); %<-can't do if you want a subset
    % oneROI_PCs = vertcat(subjInfo(1).PCval(:,1:10),subjInfo(2).PCval(:,1:10),subjInfo(3).PCval(:,1:10), ...
    %     subjInfo(4).PCval(:,1:10),subjInfo(5).PCval(:,1:10),subjInfo(6).PCval(:,1:10),subjInfo(7).PCval(:,1:10), ...
    %     subjInfo(8).PCval(:,1:10),subjInfo(9).PCval(:,1:10),subjInfo(10).PCval(:,1:10),subjInfo(11).PCval(:,1:10), ...
    %     subjInfo(12).PCval(:,1:10),subjInfo(13).PCval(:,1:10),subjInfo(14).PCval(:,1:10),subjInfo(15).PCval(:,1:10), ...
    %     subjInfo(16).PCval(:,1:10),subjInfo(17).PCval(:,1:10),subjInfo(18).PCval(:,1:10),subjInfo(19).PCval(:,1:10));
    
    oneROI_Fs = vertcat(subjInfo.Fval); %<- can do if you want it all
    % oneROI_Fs = vertcat(subjInfo(1).Fval(:,1:10),subjInfo(2).Fval(:,1:10),subjInfo(3).Fval(:,1:10), ...
    %     subjInfo(4).Fval(:,1:10),subjInfo(5).Fval(:,1:10),subjInfo(6).Fval(:,1:10),subjInfo(7).Fval(:,1:10), ...
    %     subjInfo(8).Fval(:,1:10),subjInfo(9).Fval(:,1:10),subjInfo(10).Fval(:,1:10),subjInfo(11).Fval(:,1:10), ...
    %     subjInfo(12).Fval(:,1:10),subjInfo(13).Fval(:,1:10),subjInfo(14).Fval(:,1:10),subjInfo(15).Fval(:,1:10), ...
    %     subjInfo(16).Fval(:,1:10),subjInfo(17).Fval(:,1:10),subjInfo(18).Fval(:,1:10),subjInfo(19).Fval(:,1:10));
    
    oneROI_lexMem = vertcat(subjInfo.lexMem);
    oneROI_visMem = vertcat(subjInfo.visMem);
    
    % I think I messed up the subject ID field, so just do it the old way
    % col of subject names
    subjCol = vertcat(repmat("S002",numel(subjInfo(1).Fval(:,1)),1),repmat("S005",numel(subjInfo(2).Fval(:,1)),1), ...
        repmat("S006",numel(subjInfo(3).Fval(:,1)),1),repmat("S008",numel(subjInfo(4).Fval(:,1)),1), ...
        repmat("S009",numel(subjInfo(5).Fval(:,1)),1),repmat("S010",numel(subjInfo(6).Fval(:,1)),1), ...
        repmat("S011",numel(subjInfo(7).Fval(:,1)),1),repmat("S013",numel(subjInfo(8).Fval(:,1)),1), ...
        repmat("S014",numel(subjInfo(9).Fval(:,1)),1),repmat("S015",numel(subjInfo(10).Fval(:,1)),1), ...
        repmat("S016",numel(subjInfo(11).Fval(:,1)),1),repmat("S018",numel(subjInfo(12).Fval(:,1)),1), ...
        repmat("S019",numel(subjInfo(13).Fval(:,1)),1),repmat("S021",numel(subjInfo(14).Fval(:,1)),1), ...
        repmat("S022",numel(subjInfo(15).Fval(:,1)),1),repmat("S023",numel(subjInfo(16).Fval(:,1)),1), ...
        repmat("S024",numel(subjInfo(17).Fval(:,1)),1),repmat("S025",numel(subjInfo(18).Fval(:,1)),1), ...
        repmat("S026",numel(subjInfo(19).Fval(:,1)),1));
    
    % col of ItemIDs
    ID_col = vertcat(subjInfo.IDs);
    % ID_col = vertcat(subjInfo(1).IDs,subjInfo(2).IDs,subjInfo(3).IDs,subjInfo(4).IDs, ...
    %     subjInfo(5).IDs,subjInfo(6).IDs,subjInfo(7).IDs,subjInfo(8).IDs,subjInfo(9).IDs, ...
    %     subjInfo(10).IDs,subjInfo(11).IDs,subjInfo(12).IDs,subjInfo(13).IDs,subjInfo(14).IDs,...
    %     subjInfo(15).IDs,subjInfo(16).IDs,subjInfo(17).IDs,subjInfo(18).IDs,subjInfo(19).IDs);

%     %%%% this is all for remembered and forgotten
%     clear allActivityMat_remembered
%     clear allActivityMat_forgotten
%     %%%%%%%%%
      
% %%%% ALL TRIALS (not remembered/forgotten)
%     %% Factors for variable number of factors jan 2023
    sz = [numel(ID_col) numFactors+4];
    if currFac == 1 %encycl numFactors = 8
        %sz = [numel(ID_col) numFactors+4]; %add the mem cols. If I end up doing additional NMF types, can add mem to all of these
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 2 %encycl_300 numFactors = 8
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 3 %vis numFactors = 8
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 4 %vis_300 numFactors = 8
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 5 %fcn numFactors = 5
        varTypes = ["string","string","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","visual_CR","lexical_CR"];
    elseif currFac == 6 %fcn_300 numFactors = 4
        varTypes = ["string","string","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","visual_CR","lexical_CR"];

    elseif currFac == 7 %all
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 8 %all 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 9 %outside
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 10 %outside 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 11 %home
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 12 %home 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 13 %home tool
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 14 %home tool 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 15 %animal
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 16 %animal 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 17 %food
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];
    elseif currFac == 18 %food 300
        varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
        varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","visual_CR","lexical_CR"];

    end %end currFac if statement for making a struct to hold the F values
%     
%     if currFac == 1 %encycl numFactors = 25
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
%     elseif currFac == 2 %encycl_300 numFactors = 18
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18"];
%     elseif currFac == 3 %vis numFactors = 25
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
%     elseif currFac == 4 %vis_300 numFactors = 18
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18"];
%     elseif currFac == 5 %fcn numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 6 %fcn_300 numFactors = 4
%         varTypes = ["string","string","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04"];
%     elseif currFac == 7 %noTax numFactors = 50
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
%             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
%             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
%     elseif currFac == 8 %noTax_300 numFactors = 30
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
%             "F26","F27","F28","F29","F30"];
%     elseif currFac == 9 %all numFactors = 50
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
%             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
%             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
%     elseif currFac == 10 %all_300 numFactors = 18
%         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
%             "double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
%             "F11","F12","F13","F14","F15","F16","F17","F18"];
%     end %end currFac if statement for making a struct to hold the F values
    
    allButROI_custom_F = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
    allButROI_custom_F.Subj = subjCol;
    allButROI_custom_F.ItemID = ID_col;
    allButROI_custom_F.visual_CR = oneROI_visMem;
    allButROI_custom_F.lexical_CR = oneROI_lexMem;
    Fs_tbl = array2table(oneROI_Fs); 
    allButROI_custom_F(:,3:numFactors+2) = Fs_tbl;
    
    
%     if currFac == 1
%         allButROI_encycl_fac = allButROI_custom_F;
%         save('allButROI_encycl_fac_jan23.mat', 'allButROI_encycl_fac')
%     elseif currFac == 2
%         allButROI_encycl_300_fac = allButROI_custom_F;
%         save('allButROI_encycl_300_fac_jan23.mat', 'allButROI_encycl_300_fac')
%     elseif currFac == 3
%         allButROI_vis_fac = allButROI_custom_F;
%         save('allButROI_vis_fac_jan23.mat', 'allButROI_vis_fac')
%     elseif currFac == 4
%         allButROI_vis_300_fac = allButROI_custom_F;
%         save('allButROI_vis_300_fac_jan23.mat', 'allButROI_vis_300_fac')
%     elseif currFac == 5
%         allButROI_fcn_fac = allButROI_custom_F;
%         save('allButROI_fcn_fac_jan23.mat', 'allButROI_fcn_fac')
%     elseif currFac == 6
%         allButROI_fcn_300_fac = allButROI_custom_F;
%         save('allButROI_fcn_300_fac_jan23.mat', 'allButROI_fcn_300_fac')
% %     elseif currFac == 7
% %         allButROI_noTax_fac = allButROI_custom_F;
% %         save('allButROI_noTax_fac_jan23.mat', 'allButROI_noTax_fac')
% %     elseif currFac == 8
% %         allButROI_noTax_300_fac = allButROI_custom_F;
% %         save('allButROI_noTax_300_fac_jan23.mat', 'allButROI_noTax_300_fac')
% %     elseif currFac == 9
% %         allButROI_all_fac = allButROI_custom_F;
% %         save('allButROI_all_fac_jan23.mat', 'allButROI_all_fac')
% %     elseif currFac == 10
% %         allButROI_all_300_fac = allButROI_custom_F;
% %         save('allButROI_all_300_fac_jan23.mat', 'allButROI_all_300_fac')
%     end %end currFac if statement

    % %%% Factors ALL TRIALS


    % can I change the 999's to NaNs in lex and vis here?
%     for row = 1:length(lexMem_CR)
%         if isnan(lexMem_CR(row))
%             lexMem_CR(row) = NaN;
%         end
%         if isnan(visMem_CR(row))
%             visMem_CR(row) = NaN;
%         end
%     end

    
    %%% new masks jan 2023

    %%%% UNILATERAL 1: get the data
    %%%% all trials. see below for remembered and forgotten
%     AvgROI = allActivityMat(:,1);
%     mask_AG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,2);
    mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,3);
    mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,4);
%     mask_ATL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,5);
    mask_ATL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,6);
    mask_ATL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     % cols 7,8,9 are amyg
%     % cols 10,11,12 are basal ganglia
%     AvgROI = allActivityMat(:,13);
%     mask_CG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,14);
%     mask_CG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,15);
%     mask_CG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,16);
%     mask_FFA = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,17);
%     mask_FFA_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,18);
%     mask_FFA_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,19);
%     mask_FuG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,20);
    mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,21);
    mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,22);
%     mask_Hipp = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,23);
    mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,24);
%     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,25);
%     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,26);
    mask_Hipp_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,27);
    mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,28);
%     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,29);
%     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,30);
    mask_Hipp_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,31);
%     mask_IFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,32);
    mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,33);
    mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,34);
%     mask_INS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,35);
%     mask_INS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,36);
%     mask_INS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,37);
%     mask_IPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,38);
%     mask_IPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,39);
%     mask_IPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,40);
%     mask_ITG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,41);
    mask_ITG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,42);
    mask_ITG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,43);
%     mask_LOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,44);
    mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,45);
    mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,46);
%     mask_MFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,47);
%     mask_MFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,48);
%     mask_MFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,49);
%     mask_MTG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,50);
%     mask_MTG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,51);
%     mask_MTG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,52);
%     mask_MVOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,53);
    mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,54);
    mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,55);
%     mask_OrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,56);
%     mask_OrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,57);
%     mask_OrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,58);
%     mask_PCL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,59);
%     mask_PCL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,60);
%     mask_PCL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,61);
%     mask_PHC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,62);
    mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,63);
    mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,64);
%     mask_Pcun = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,65);
    mask_Pcun_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,66);
    mask_Pcun_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,67);
%     mask_Perirhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,68);
    mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,69);
    mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,70);
%     mask_PhG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,71);
    mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,72);
    mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,73);
%     mask_PoG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,74);
    mask_PoG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,75);
    mask_PoG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,76);
%     mask_PrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,77);
    mask_PrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,78);
    mask_PrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,79);
%     mask_RSC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,80);
    mask_RSC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,81);
    mask_RSC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,82);
%     mask_Rhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,83);
    mask_Rhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,84);
    mask_Rhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,85);
%     mask_SFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,86);
%     mask_SFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,87);
%     mask_SFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,88);
%     mask_SMG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,89);
    mask_SMG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,90);
    mask_SMG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,91);
%     mask_SPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,92);
%     mask_SPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,93);
%     mask_SPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,94);
%     mask_STG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,95);
%     mask_STG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,96);
%     mask_STG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     % 97,98,99 thalamus
%     AvgROI = allActivityMat(:,100);
%     mask_pSTS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,101);
    mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,102);
    mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');

    if currFac == 1
       % currSheet = 'avgActivity_BNA_nnmf_encycl_additionalROIs.xlsx';
        currSheet = 'avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx';
%         currSheet = 'avgActivity_BNA_nnmf_encycl_mult.xlsx';
        %currSheet = 'avgActivity_BNA_nnmf_encycl_als.xlsx';
    elseif currFac == 2
        currSheet = 'avgActivity_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 3
        currSheet = 'avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx';
    elseif currFac == 4
        currSheet = 'avgActivity_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 5
        currSheet = 'avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx';
    elseif currFac == 6
        currSheet = 'avgActivity_BNA_nnmf_fcn_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 7
        currSheet = 'avgActivity_BNA_nnmf_all_additionalROIs_unilateral.xlsx';
    elseif currFac == 8
        currSheet = 'avgActivity_BNA_nnmf_all_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 9
        currSheet = 'avgActivity_BNA_nnmf_outside_additionalROIs_unilateral.xlsx';
    elseif currFac == 10
        currSheet = 'avgActivity_BNA_nnmf_outside_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 11
        currSheet = 'avgActivity_BNA_nnmf_home_additionalROIs_unilateral.xlsx';
    elseif currFac == 12
        currSheet = 'avgActivity_BNA_nnmf_home_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 13
        currSheet = 'avgActivity_BNA_nnmf_home_tool_additionalROIs_unilateral.xlsx';
    elseif currFac == 14
        currSheet = 'avgActivity_BNA_nnmf_home_tool_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 15
        currSheet = 'avgActivity_BNA_nnmf_animal_additionalROIs_unilateral.xlsx';
    elseif currFac == 16
        currSheet = 'avgActivity_BNA_nnmf_animal_300_additionalROIs_unilateral.xlsx';
    elseif currFac == 17
        currSheet = 'avgActivity_BNA_nnmf_food_additionalROIs_unilateral.xlsx';
    elseif currFac == 18
        currSheet = 'avgActivity_BNA_nnmf_food_300_additionalROIs_unilateral.xlsx'; 
    end %end currFac if statement

     % UNILATERAL 2: write the data
    writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
    writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
    writetable(mask_ATL_L,currSheet,'Sheet','mask_ATL_L') 
    writetable(mask_ATL_R,currSheet,'Sheet','mask_ATL_R') 
    writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
    writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
    writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
    writetable(mask_Hipp_L,currSheet,'Sheet','mask_Hipp_L') 
    writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
    writetable(mask_Hipp_R,currSheet,'Sheet','mask_Hipp_R') 
    writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
    writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
    writetable(mask_ITG_L,currSheet,'Sheet','mask_ITG_L') 
    writetable(mask_ITG_R,currSheet,'Sheet','mask_ITG_R') 
    writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
    writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
    writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
    writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
    writetable(mask_Pcun_L,currSheet,'Sheet','mask_Pcun_L') 
    writetable(mask_Pcun_R,currSheet,'Sheet','mask_Pcun_R') 
    writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
    writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
    writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
    writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
    writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
    writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
    writetable(mask_PoG_L,currSheet,'Sheet','mask_PoG_L') 
    writetable(mask_PoG_R,currSheet,'Sheet','mask_PoG_R') 
    writetable(mask_PrG_L,currSheet,'Sheet','mask_PrG_L') 
    writetable(mask_PrG_R,currSheet,'Sheet','mask_PrG_R') 
    writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
    writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 
    writetable(mask_Rhinal_L,currSheet,'Sheet','mask_Rhinal_L') 
    writetable(mask_Rhinal_R,currSheet,'Sheet','mask_Rhinal_R') 
    writetable(mask_RSC_L,currSheet,'Sheet','mask_RSC_L') 
    writetable(mask_RSC_R,currSheet,'Sheet','mask_RSC_R') 
    writetable(mask_SMG_L,currSheet,'Sheet','mask_SMG_L') 
    writetable(mask_SMG_R,currSheet,'Sheet','mask_SMG_R') 


    %%%% BILATERAL: get the data
    AvgROI = allActivityMat(:,1);
    mask_AG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,4);
    mask_ATL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,19);
    mask_FuG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,22);
    mask_Hipp = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,31);
    mask_IFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,40);
    mask_ITG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,43);
    mask_LOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,52);
    mask_MVOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,61);
    mask_PHC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,64);
    mask_Pcun = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,67);
    mask_Perirhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,70);
    mask_PhG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,73);
    mask_PoG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,76);
    mask_PrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,79);
    mask_RSC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,82);
    mask_Rhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,88);
    mask_SMG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
    AvgROI = allActivityMat(:,100);
    mask_pSTS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');


    
%     if currFac == 1
%         currSheet = 'avgActivity_BNA_F_encycl_jan23.xlsx';
%     elseif currFac == 2
%         currSheet = 'avgActivity_BNA_F_encycl_300_jan23.xlsx';
%     elseif currFac == 3
%         currSheet = 'avgActivity_BNA_F_vis_jan23.xlsx';
%     elseif currFac == 4
%         currSheet = 'avgActivity_BNA_F_vis_300_jan23.xlsx';
%     elseif currFac == 5
%         currSheet = 'avgActivity_BNA_F_fcn_jan23.xlsx';
%     elseif currFac == 6
%         currSheet = 'avgActivity_BNA_F_fcn_300_jan23.xlsx';
% %     elseif currFac == 7
% %         currSheet = 'avgActivity_BNA_F_noTax_jan23.xlsx';
% %     elseif currFac == 8
% %         currSheet = 'avgActivity_BNA_F_noTax_300_jan23.xlsx';
% %     elseif currFac == 9
% %         currSheet = 'avgActivity_BNA_F_all_jan23.xlsx';
% %     elseif currFac == 10
% %         currSheet = 'avgActivity_BNA_F_all_300_jan23.xlsx';
%     end %end currFac if statement

    if currFac == 1
       % currSheet = 'avgActivity_BNA_nnmf_encycl_additionalROIs.xlsx';
        currSheet = 'avgActivity_BNA_nnmf_encycl_additionalROIs_bilateral.xlsx';
%         currSheet = 'avgActivity_BNA_nnmf_encycl_mult.xlsx';
        %currSheet = 'avgActivity_BNA_nnmf_encycl_als.xlsx';
    elseif currFac == 2
        currSheet = 'avgActivity_BNA_nnmf_encycl_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 3
        currSheet = 'avgActivity_BNA_nnmf_vis_additionalROIs_bilateral.xlsx';
    elseif currFac == 4
        currSheet = 'avgActivity_BNA_nnmf_vis_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 5
        currSheet = 'avgActivity_BNA_nnmf_fcn_additionalROIs_bilateral.xlsx';
    elseif currFac == 6
        currSheet = 'avgActivity_BNA_nnmf_fcn_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 7
        currSheet = 'avgActivity_BNA_nnmf_all_additionalROIs_bilateral.xlsx';
    elseif currFac == 8
        currSheet = 'avgActivity_BNA_nnmf_all_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 9
        currSheet = 'avgActivity_BNA_nnmf_outside_additionalROIs_bilateral.xlsx';
    elseif currFac == 10
        currSheet = 'avgActivity_BNA_nnmf_outside_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 11
        currSheet = 'avgActivity_BNA_nnmf_home_additionalROIs_bilateral.xlsx';
    elseif currFac == 12
        currSheet = 'avgActivity_BNA_nnmf_home_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 13
        currSheet = 'avgActivity_BNA_nnmf_home_tool_additionalROIs_bilateral.xlsx';
    elseif currFac == 14
        currSheet = 'avgActivity_BNA_nnmf_home_tool_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 15
        currSheet = 'avgActivity_BNA_nnmf_animal_additionalROIs_bilateral.xlsx';
    elseif currFac == 16
        currSheet = 'avgActivity_BNA_nnmf_animal_300_additionalROIs_bilateral.xlsx';
    elseif currFac == 17
        currSheet = 'avgActivity_BNA_nnmf_food_additionalROIs_bilateral.xlsx';
    elseif currFac == 18
        currSheet = 'avgActivity_BNA_nnmf_food_300_additionalROIs_bilateral.xlsx'; 
    end %end currFac if statement

 %%%% heads up this takes a while
% BILATERAL 2: write the data
    writetable(mask_AG,currSheet,'Sheet','mask_AG') 
    writetable(mask_ATL,currSheet,'Sheet','mask_ATL') 
    writetable(mask_FuG,currSheet,'Sheet','mask_FuG') 
    writetable(mask_Hipp,currSheet,'Sheet','mask_Hipp') 
    writetable(mask_IFG,currSheet,'Sheet','mask_IFG') 
    writetable(mask_ITG,currSheet,'Sheet','mask_ITG') 
    writetable(mask_LOC,currSheet,'Sheet','mask_LOC') 
    writetable(mask_MVOC,currSheet,'Sheet','mask_MVOC') 
    writetable(mask_Pcun,currSheet,'Sheet','mask_Pcun') 
    writetable(mask_Perirhinal,currSheet,'Sheet','mask_Perirhinal') 
    writetable(mask_PHC,currSheet,'Sheet','mask_PHC')  
    writetable(mask_PhG,currSheet,'Sheet','mask_PhG') 
    writetable(mask_PoG,currSheet,'Sheet','mask_PoG') 
    writetable(mask_PrG,currSheet,'Sheet','mask_PrG') 
    writetable(mask_pSTS,currSheet,'Sheet','mask_pSTS') 
    writetable(mask_Rhinal,currSheet,'Sheet','mask_Rhinal') 
    writetable(mask_RSC,currSheet,'Sheet','mask_RSC') 
    writetable(mask_SMG,currSheet,'Sheet','mask_SMG') 


%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 

%     writetable(mask_AG,currSheet,'Sheet','mask_AG') 
%     writetable(mask_ATL,currSheet,'Sheet','mask_ATL') 
%     writetable(mask_CG,currSheet,'Sheet','mask_CG') 
%     writetable(mask_CG_L,currSheet,'Sheet','mask_CG_L') 
%     writetable(mask_CG_R,currSheet,'Sheet','mask_CG_R') 
%     writetable(mask_FFA,currSheet,'Sheet','mask_FFA') 
%     writetable(mask_FFA_L,currSheet,'Sheet','mask_FFA_L') 
%     writetable(mask_FFA_R,currSheet,'Sheet','mask_FFA_R') 
%     writetable(mask_IFG,currSheet,'Sheet','mask_IFG') 
%     writetable(mask_INS,currSheet,'Sheet','mask_INS') 
%     writetable(mask_INS_L,currSheet,'Sheet','mask_INS_L') 
%     writetable(mask_INS_R,currSheet,'Sheet','mask_INS_R') 
%     writetable(mask_IPL,currSheet,'Sheet','mask_IPL') 
%     writetable(mask_IPL_L,currSheet,'Sheet','mask_IPL_L') 
%     writetable(mask_IPL_R,currSheet,'Sheet','mask_IPL_R') 
%     writetable(mask_ITG,currSheet,'Sheet','mask_ITG') 
%     writetable(mask_LOC,currSheet,'Sheet','mask_LOC') 
%     writetable(mask_MFG,currSheet,'Sheet','mask_MFG') 
%     writetable(mask_MFG_L,currSheet,'Sheet','mask_MFG_L') 
%     writetable(mask_MFG_R,currSheet,'Sheet','mask_MFG_R') 
%     writetable(mask_MTG,currSheet,'Sheet','mask_MTG') 
%     writetable(mask_MTG_L,currSheet,'Sheet','mask_MTG_L') 
%     writetable(mask_MTG_R,currSheet,'Sheet','mask_MTG_R') 
%     writetable(mask_MVOC,currSheet,'Sheet','mask_MVOC') 
%     writetable(mask_OrG,currSheet,'Sheet','mask_OrG') 
%     writetable(mask_OrG_L,currSheet,'Sheet','mask_OrG_L') 
%     writetable(mask_OrG_R,currSheet,'Sheet','mask_OrG_R') 
%     writetable(mask_PCL,currSheet,'Sheet','mask_PCL') 
%     writetable(mask_PCL_L,currSheet,'Sheet','mask_PCL_L') 
%     writetable(mask_PCL_R,currSheet,'Sheet','mask_PCL_R') 
%     writetable(mask_PHC,currSheet,'Sheet','mask_PHC') 
%     writetable(mask_Pcun,currSheet,'Sheet','mask_Pcun') 
%     writetable(mask_Perirhinal,currSheet,'Sheet','mask_Perirhinal') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG,currSheet,'Sheet','mask_PhG') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_PoG,currSheet,'Sheet','mask_PoG') 
%     writetable(mask_PrG,currSheet,'Sheet','mask_PrG') 
%     writetable(mask_RSC,currSheet,'Sheet','mask_RSC') 
%     writetable(mask_Rhinal,currSheet,'Sheet','mask_Rhinal') 
%     writetable(mask_SFG,currSheet,'Sheet','mask_SFG') 
%     writetable(mask_SFG_L,currSheet,'Sheet','mask_SFG_L') 
%     writetable(mask_SFG_R,currSheet,'Sheet','mask_SFG_R') 
%     writetable(mask_SMG,currSheet,'Sheet','mask_SMG') 
%     writetable(mask_SPL,currSheet,'Sheet','mask_SPL') 
%     writetable(mask_SPL_L,currSheet,'Sheet','mask_SPL_L') 
%     writetable(mask_SPL_R,currSheet,'Sheet','mask_SPL_R') 
%     writetable(mask_STG,currSheet,'Sheet','mask_STG') 
%     writetable(mask_STG_L,currSheet,'Sheet','mask_STG_L') 
%     writetable(mask_STG_R,currSheet,'Sheet','mask_STG_R') 
%     writetable(mask_pSTS,currSheet,'Sheet','mask_pSTS') 
    

    %% PCs
    % sz = [5225 12]; 
    % varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
    % varNames = ["Subj","ItemID","PC01","PC02","PC03","PC04","PC05","PC06","PC07","PC08","PC09","PC10"];
    % allButROI_custom = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
    % 
    % allButROI_custom.Subj = subjCol;
    % allButROI_custom.ItemID = ID_col;
    % PCs_tbl = array2table(oneROI_PCs); 
    % allButROI_custom(:,3:12) = PCs_tbl;
    % 
    % cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
    % save('allActivityMat_clean.mat','allActivityMat')
    % save('allButROI_customClean.mat','allButROI_custom')
    
    %%%%%%%%%%%%%%%%%%%%%%% one of two options
    %% Factors for FA_results 10 factors
    % sz = [5225 12]; 
    % varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
    % varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10"];
    % 
    % 
    % allButROI_custom_F.Subj = subjCol;
    % allButROI_custom_F.ItemID = ID_col;
    % Fs_tbl = array2table(oneROI_Fs); 
    % allButROI_custom_F(:,3:12) = Fs_tbl;
    % 
    % cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
    % %save('allActivityMat.mat','allActivityMat')
    % %save('allButROI_custom_F.mat','allButROI_custom_F')
    % % save('allButROI_customClean_F500.mat','allButROI_custom_F')
    % 
    % allButROI_encycl = allButROI_custom_F;
    % save('allButROI_encycl.mat', 'allButROI_encycl')
    % 
    % allButROI_noTax = allButROI_custom_F;
    % save('allButROI_noTax.mat', 'allButROI_noTax')
    % 
    % allButROI_vis = allButROI_custom_F;
    % save('allButROI_vis.mat', 'allButROI_vis')
    % 
    % allButROI_fcn = allButROI_custom_F;
    % save('allButROI_fcn.mat', 'allButROI_fcn')
    % 
    % allButROI_tax = allButROI_custom_F;
    % save('allButROI_tax.mat', 'allButROI_tax')
    
    
 
    
    %% Factors for variable number of factors fall 2022
    % factanal, use scree plot to select number of factors
    % sz = [5225 numFactors+2];
    % 
    % if numFactors == 25 %encycl
    %     varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
    %         "double","double","double","double","double","double","double","double","double","double",...
    %         "double","double","double","double","double"];
    %     varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
    %         "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
    % 
    % elseif numFactors == 15 %tax
    %     varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
    %         "double","double","double","double","double"];
    %     varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
    %         "F11","F12","F13","F14","F15"];
    % 
    % elseif numFactors == 20 %vis
    %     varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
    %         "double","double","double","double","double","double","double","double","double","double"];
    %     varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
    %         "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20"];
    % 
    % elseif numFactors == 10 %fcn
    %     varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double"];
    %     varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10"];
    % 
    % end
    
    % allButROI_custom_F = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
    % allButROI_custom_F.Subj = subjCol;
    % allButROI_custom_F.ItemID = ID_col;
    % Fs_tbl = array2table(oneROI_Fs); 
    % allButROI_custom_F(:,3:numFactors+2) = Fs_tbl;
    
    %%%%% ok i don't RUN stuff in the STAMP_scripts. That just stores the
    %%%%% scripts. I run and store everything in STAMP. Also, don't cd around!
    %%%%% Just addpath().     
    % cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
    % 
    % if numFactors == 25 %encycl
    %     allButROI_encycl_fac = allButROI_custom_F;
    %     save('allButROI_encycl_fac.mat', 'allButROI_encycl_fac')
    % 
    % elseif numFactors == 15 %tax
    %     allButROI_tax_fac = allButROI_custom_F;
    %     save('allButROI_tax_fac.mat', 'allButROI_tax_fac')
    % 
    % elseif numFactors == 20 %vis
    %     allButROI_vis_fac = allButROI_custom_F;
    %     save('allButROI_vis_fac.mat', 'allButROI_vis_fac')
    % 
    % elseif numFactors == 10 %fcn
    %     allButROI_fcn_fac = allButROI_custom_F;
    %     save('allButROI_fcn_fac.mat', 'allButROI_fcn_fac')
    % 
    % end
    
%     %% Mem
%     % there are still 999 rows in lexMem and visMem we have to cut out those
%     % trials from allActivityMat as well as oneROI_lexMem and oneROI_visMem
%     % also, you should have run and included F in the struct as well. We're
%     % not going to include the F scores in the table we write to the
%     % spreadsheet, but we use it to write out the subjCol and ID_col
% 
%     %load allActivityMat_17.mat % 17 masks
%     %load allActivityMat_jan23ROI.mat %102 masks
% 
%     %%% need these from about 100 lines above. If it's already run they'll be
%     %%% in the workspace 
%     %oneROI_lexMem = vertcat(subjInfo.lexMem);
%     %oneROI_visMem = vertcat(subjInfo.visMem);
% 
%     
%     lex_index = find(oneROI_lexMem==999); % 1030 of these
%     vis_index = find(oneROI_visMem==999); % 110
%     combined_index = vertcat(lex_index,vis_index); % 1140
%     both_index = unique(combined_index); % 1047
%     
%     % cut down so you have the same length as mem for which we have non-NaN values
%     allActivityMat(both_index,:) = [];
%     oneROI_lexMem(both_index) = [];
%     oneROI_visMem(both_index) = [];
%     subjCol(both_index) = [];
%     ID_col(both_index) = [];
%     
%     allActivityMat_memShort = allActivityMat;
%     oneROI_lexMem_short = oneROI_lexMem;
%     oneROI_visMem_short = oneROI_visMem;
%     subjCol_short = subjCol;
%     ID_col_short = ID_col;
%     
%     % at this point the mem values should all be cleaned
%     % therefore, I can now make bothMem from the short versions of vis and lex
%     
%     normLex = oneROI_lexMem_short - repmat(mean(oneROI_lexMem_short),length(oneROI_lexMem_short),1);
%     normVis = oneROI_visMem_short - repmat(mean(oneROI_visMem_short),length(oneROI_visMem_short),1);
%     
%     bothMem = normVis .* normLex;
%     
%     
%     %sz = [4178 5]; 
%     sz = [length(subjCol) 5];
%     varTypes = ["string","string","double","double","double"];
%     varNames = ["Subj","ItemID","lexMem","visMem","bothMem"];
%     allButROI_memShort = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
%     
%     allButROI_memShort.Subj = subjCol_short;
%     allButROI_memShort.ItemID = ID_col_short;
%     allButROI_memShort.lexMem = oneROI_lexMem_short;
%     allButROI_memShort.visMem = oneROI_visMem_short;
%     allButROI_memShort.bothMem = bothMem;
%     
%  %   cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
% %    save('allActivityMat_memShort.mat','allActivityMat_memShort')
% %    save('allButROI_memShort.mat','allButROI_memShort')
%     save('allActivityMat_memShort_jan23.mat','allActivityMat_memShort')
%     save('allButROI_memShort_jan23.mat','allButROI_memShort')
    
    %%%% skip way down for making mem/BOLD table and writing to spreadsheet
    %%%% to end of currFac loop
    
    %% Make final data table for R 
    
    %%% wbatever allButROI_custom_F is for this time through the script, it
    %%% will work with the masks. There is an if statement below that will
    %%% label based on the feature category I'm running that time
    
    %%%%%%%%%%%%%%% load if you're starting from here
    % load allActivityMat_clean.mat
    % load allButROI_customClean.mat
    % load allButROI_customClean_F500.mat
    % load allButROI_memShort.mat
    % load allActivityMat_memShort.mat
    
    %allButROI_custom = allButROI_custom_F; % don't want to re-write code below, so just rename
    
    % spreadsheet for PCs and F
    % /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/avgActivity_customROIs_PC_F500.xlsx
    
    % spreadsheet for Mem
    % avgActivity_customROIs_mem.xlsx
    % 
    % %%% PCs
    % AvgROI = allActivityMat(:,1);
    % mask28_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,2);
    % mask29_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,3);
    % mask30_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,4);
    % mask31_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,5);
    % mask32_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % % AvgROI = allActivityMat(:,6);
    % % mask33_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,6);
    % mask34_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,7);
    % mask35_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % % AvgROI = allActivityMat(:,9);
    % % mask36_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,8);
    % mask37_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % % AvgROI = allActivityMat(:,11);
    % % mask38_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,9);
    % mask39_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,10);
    % mask40_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    % 
    % AvgROI = allActivityMat(:,11);
    % mask41_PC = addvars(allButROI_custom,AvgROI,'Before','Subj');
    

%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  %%% new masks jan 2023 remembered trials
% 
% %%%% remembered struct 
%     numMasks = 102; %BNA. There are 104 cols but first two are subject and item
%     clear allActivityMat_remembered
%     for ROI = 1:numMasks
%         activityCol = vertcat(subjInfo_remembered(1).activityVal(:,ROI),subjInfo_remembered(2).activityVal(:,ROI), ...
%             subjInfo_remembered(3).activityVal(:,ROI),subjInfo_remembered(4).activityVal(:,ROI),subjInfo_remembered(5).activityVal(:,ROI), ...
%             subjInfo_remembered(6).activityVal(:,ROI),subjInfo_remembered(7).activityVal(:,ROI),subjInfo_remembered(8).activityVal(:,ROI), ...
%             subjInfo_remembered(9).activityVal(:,ROI),subjInfo_remembered(10).activityVal(:,ROI),subjInfo_remembered(11).activityVal(:,ROI), ...
%             subjInfo_remembered(12).activityVal(:,ROI),subjInfo_remembered(13).activityVal(:,ROI),subjInfo_remembered(14).activityVal(:,ROI), ...
%             subjInfo_remembered(15).activityVal(:,ROI),subjInfo_remembered(16).activityVal(:,ROI),subjInfo_remembered(17).activityVal(:,ROI), ...
%             subjInfo_remembered(18).activityVal(:,ROI),subjInfo_remembered(19).activityVal(:,ROI));
%         allActivityMat_remembered(:,ROI) = activityCol;
%     end
% 
%     oneROI_Fs = vertcat(subjInfo_remembered.Fval); 
% 
%     oneROI_lexMem = vertcat(subjInfo_remembered.lexMem);
%     oneROI_visMem = vertcat(subjInfo_remembered.visMem);
% %     subjCol = vertcat(repmat("S002",numel(subjInfo_remembered(1).Fval(:,1)),1),repmat("S005",numel(subjInfo_remembered(2).Fval(:,1)),1), ...
% %         repmat("S006",numel(subjInfo_remembered(3).Fval(:,1)),1),repmat("S008",numel(subjInfo_remembered(4).Fval(:,1)),1), ...
% %         repmat("S009",numel(subjInfo_remembered(5).Fval(:,1)),1),repmat("S010",numel(subjInfo_remembered(6).Fval(:,1)),1), ...
% %         repmat("S011",numel(subjInfo_remembered(7).Fval(:,1)),1),repmat("S013",numel(subjInfo_remembered(8).Fval(:,1)),1), ...
% %         repmat("S014",numel(subjInfo_remembered(9).Fval(:,1)),1),repmat("S015",numel(subjInfo_remembered(10).Fval(:,1)),1), ...
% %         repmat("S016",numel(subjInfo_remembered(11).Fval(:,1)),1),repmat("S018",numel(subjInfo_remembered(12).Fval(:,1)),1), ...
% %         repmat("S019",numel(subjInfo_remembered(13).Fval(:,1)),1),repmat("S021",numel(subjInfo_remembered(14).Fval(:,1)),1), ...
% %         repmat("S022",numel(subjInfo_remembered(15).Fval(:,1)),1),repmat("S023",numel(subjInfo_remembered(16).Fval(:,1)),1), ...
% %         repmat("S024",numel(subjInfo_remembered(17).Fval(:,1)),1),repmat("S025",numel(subjInfo_remembered(18).Fval(:,1)),1), ...
% %         repmat("S026",numel(subjInfo_remembered(19).Fval(:,1)),1));
% 
%     subjCol = vertcat(repmat("S002",numel(subjInfo_remembered(1).Fval(:,1)),1),repmat("S005",numel(subjInfo_remembered(2).Fval(:,1)),1), ...
%         repmat("S006",numel(subjInfo_remembered(3).Fval(:,1)),1),repmat("S008",numel(subjInfo_remembered(4).Fval(:,1)),1), ...
%         repmat("S009",numel(subjInfo_remembered(5).Fval(:,1)),1),repmat("S010",numel(subjInfo_remembered(6).Fval(:,1)),1), ...
%         repmat("S011",numel(subjInfo_remembered(7).Fval(:,1)),1),repmat("S013",numel(subjInfo_remembered(8).Fval(:,1)),1), ...
%         repmat("S014",numel(subjInfo_remembered(9).Fval(:,1)),1),repmat("S015",numel(subjInfo_remembered(10).Fval(:,1)),1), ...
%         repmat("S016",numel(subjInfo_remembered(11).Fval(:,1)),1),repmat("S018",numel(subjInfo_remembered(12).Fval(:,1)),1), ...
%         repmat("S019",numel(subjInfo_remembered(13).Fval(:,1)),1),repmat("S021",numel(subjInfo_remembered(14).Fval(:,1)),1), ...
%         repmat("S022",numel(subjInfo_remembered(15).Fval(:,1)),1),repmat("S023",numel(subjInfo_remembered(16).Fval(:,1)),1), ...
%         repmat("S024",numel(subjInfo_remembered(17).Fval(:,1)),1),repmat("S025",numel(subjInfo_remembered(18).Fval(:,1)),1), ...
%         repmat("S026",numel(subjInfo_remembered(19).Fval(:,1)),1));
% 
%     ID_col = vertcat(subjInfo_remembered.IDs);
% 
%     sz = [numel(ID_col) numFactors+2];
%     
%     if currFac == 1 %encycl numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 2 %encycl_300 numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 3 %vis numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 4 %vis_300 numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 5 %fcn numFactors = 5
%         varTypes = ["string","string","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05"];
%     elseif currFac == 6 %fcn_300 numFactors = 4
%         varTypes = ["string","string","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04"];
% 
%     elseif currFac == 7 %all
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 8 %all 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 9 %outside
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 10 %outside 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 11 %home
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 12 %home 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 13 %home tool
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 14 %home tool 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 15 %animal
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 16 %animal 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 17 %food
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 18 %food 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
% 
%     end %end currFac if statement for making a struct to hold the F values
% 
% 
% 
% %     if currFac == 1 %encycl numFactors = 25
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
% %     elseif currFac == 2 %encycl_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     elseif currFac == 3 %vis numFactors = 25
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
% %     elseif currFac == 4 %vis_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     elseif currFac == 5 %fcn numFactors = 8
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
% %     elseif currFac == 6 %fcn_300 numFactors = 4
% %         varTypes = ["string","string","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04"];
% %     elseif currFac == 7 %noTax numFactors = 50
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
% %             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
% %     elseif currFac == 8 %noTax_300 numFactors = 30
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30"];
% %     elseif currFac == 9 %all numFactors = 50
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
% %             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
% %     elseif currFac == 10 %all_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     end %end currFac if statement for making a struct to hold the F values
%     
%     allButROI_custom_F = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
%     allButROI_custom_F.Subj = subjCol;
%     allButROI_custom_F.ItemID = ID_col;
%     Fs_tbl = array2table(oneROI_Fs); 
%     allButROI_custom_F(:,3:numFactors+2) = Fs_tbl;
% 
%     allActivityMat = allActivityMat_remembered;
% 
% 
%     AvgROI = allActivityMat(:,2);
%     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,3);
%     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,20);
%     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,21);
%     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,24);
%     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,25);
%     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,28);
%     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,29);
%     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,32);
%     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,33);
%     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,44);
%     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,45);
%     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,53);
%     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,54);
%     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,62);
%     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,63);
%     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,68);
%     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,69);
%     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,71);
%     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,72);
%     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,101);
%     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,102);
%     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% % 
% %     AvgROI = allActivityMat(:,1);
% %     mask_AG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,2);
% %     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,3);
% %     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,4);
% %     mask_ATL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,5);
% %     mask_ATL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,6);
% %     mask_ATL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     % cols 7,8,9 are amyg
% %     % cols 10,11,12 are basal ganglia
% %     AvgROI = allActivityMat(:,13);
% %     mask_CG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,14);
% %     mask_CG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,15);
% %     mask_CG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,16);
% %     mask_FFA = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,17);
% %     mask_FFA_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,18);
% %     mask_FFA_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,19);
% %     mask_FuG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,20);
% %     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,21);
% %     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,22);
% %     mask_Hipp = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,23);
% %     mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,24);
% %     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,25);
% %     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,26);
% %     mask_Hipp_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,27);
% %     mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,28);
% %     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,29);
% %     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,30);
% %     mask_Hipp_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,31);
% %     mask_IFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,32);
% %     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,33);
% %     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,34);
% %     mask_INS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,35);
% %     mask_INS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,36);
% %     mask_INS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,37);
% %     mask_IPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,38);
% %     mask_IPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,39);
% %     mask_IPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,40);
% %     mask_ITG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,41);
% %     mask_ITG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,42);
% %     mask_ITG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,43);
% %     mask_LOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,44);
% %     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,45);
% %     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,46);
% %     mask_MFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,47);
% %     mask_MFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,48);
% %     mask_MFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,49);
% %     mask_MTG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,50);
% %     mask_MTG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,51);
% %     mask_MTG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,52);
% %     mask_MVOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,53);
% %     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,54);
% %     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,55);
% %     mask_OrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,56);
% %     mask_OrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,57);
% %     mask_OrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,58);
% %     mask_PCL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,59);
% %     mask_PCL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,60);
% %     mask_PCL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,61);
% %     mask_PHC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,62);
% %     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,63);
% %     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,64);
% %     mask_Pcun = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,65);
% %     mask_Pcun_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,66);
% %     mask_Pcun_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,67);
% %     mask_Perirhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,68);
% %     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,69);
% %     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,70);
% %     mask_PhG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,71);
% %     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,72);
% %     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,73);
% %     mask_PoG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,74);
% %     mask_PoG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,75);
% %     mask_PoG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,76);
% %     mask_PrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,77);
% %     mask_PrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,78);
% %     mask_PrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,79);
% %     mask_RSC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,80);
% %     mask_RSC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,81);
% %     mask_RSC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,82);
% %     mask_Rhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,83);
% %     mask_Rhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,84);
% %     mask_Rhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,85);
% %     mask_SFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,86);
% %     mask_SFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,87);
% %     mask_SFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,88);
% %     mask_SMG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,89);
% %     mask_SMG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,90);
% %     mask_SMG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,91);
% %     mask_SPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,92);
% %     mask_SPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,93);
% %     mask_SPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,94);
% %     mask_STG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,95);
% %     mask_STG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,96);
% %     mask_STG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     % 97,98,99 thalamus
% %     AvgROI = allActivityMat(:,100);
% %     mask_pSTS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,101);
% %     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,102);
% %     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     
%     
% %     if currFac == 1
% %         currSheet = 'avgActivity_remembered_BNA_F_encycl_jan23.xlsx';
% %     elseif currFac == 2
% %         currSheet = 'avgActivity_remembered_BNA_F_encycl_300_jan23.xlsx';
% %     elseif currFac == 3
% %         currSheet = 'avgActivity_remembered_BNA_F_vis_jan23.xlsx';
% %     elseif currFac == 4
% %         currSheet = 'avgActivity_remembered_BNA_F_vis_300_jan23.xlsx';
% %     elseif currFac == 5
% %         currSheet = 'avgActivity_remembered_BNA_F_fcn_jan23.xlsx';
% %     elseif currFac == 6
% %         currSheet = 'avgActivity_remembered_BNA_F_fcn_300_jan23.xlsx';
% % %     elseif currFac == 7
% % %         currSheet = 'avgActivity_remembered_BNA_F_noTax_jan23.xlsx';
% % %     elseif currFac == 8
% % %         currSheet = 'avgActivity_remembered_BNA_F_noTax_300_jan23.xlsx';
% % %     elseif currFac == 9
% % %         currSheet = 'avgActivity_remembered_BNA_F_all_jan23.xlsx';
% % %     elseif currFac == 10
% % %         currSheet = 'avgActivity_remembered_BNA_F_all_300_jan23.xlsx';
% %     end %end currFac if statement
% 
%     if currFac == 1
%         currSheet = 'avgActivity_remembered_BNA_nnmf_encycl.xlsx';
%     elseif currFac == 2
%         currSheet = 'avgActivity_remembered_BNA_nnmf_encycl_300.xlsx';
%     elseif currFac == 3
%         currSheet = 'avgActivity_remembered_BNA_nnmf_vis.xlsx';
%     elseif currFac == 4
%         currSheet = 'avgActivity_remembered_BNA_nnmf_vis_300.xlsx';
%     elseif currFac == 5
%         currSheet = 'avgActivity_remembered_BNA_nnmf_fcn.xlsx';
%     elseif currFac == 6
%         currSheet = 'avgActivity_remembered_BNA_nnmf_fcn_300.xlsx';
%     elseif currFac == 7
%         currSheet = 'avgActivity_remembered_BNA_nnmf_all.xlsx';
%     elseif currFac == 8
%         currSheet = 'avgActivity_remembered_BNA_nnmf_all_300.xlsx';
%     elseif currFac == 9
%         currSheet = 'avgActivity_remembered_BNA_nnmf_outside.xlsx';
%     elseif currFac == 10
%         currSheet = 'avgActivity_remembered_BNA_nnmf_outside_300.xlsx';
%     elseif currFac == 11
%         currSheet = 'avgActivity_remembered_BNA_nnmf_home.xlsx';
%     elseif currFac == 12
%         currSheet = 'avgActivity_remembered_BNA_nnmf_home_300.xlsx';
%     elseif currFac == 13
%         currSheet = 'avgActivity_remembered_BNA_nnmf_home_tool.xlsx';
%     elseif currFac == 14
%         currSheet = 'avgActivity_remembered_BNA_nnmf_home_tool_300.xlsx';
%     elseif currFac == 15
%         currSheet = 'avgActivity_remembered_BNA_nnmf_animal.xlsx';
%     elseif currFac == 16
%         currSheet = 'avgActivity_remembered_BNA_nnmf_animal_300.xlsx';
%     elseif currFac == 17
%         currSheet = 'avgActivity_remembered_BNA_nnmf_food.xlsx';
%     elseif currFac == 18
%         currSheet = 'avgActivity_remembered_BNA_nnmf_food_300.xlsx';
%     end %end currFac if statement
% 
%     
%     
%     
%     %%%% heads up this takes a while
%     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
%     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
%     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
%     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
%     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
%     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
%     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
%     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
%     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
%     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 
% 
% %     
% %     writetable(mask_AG,currSheet,'Sheet','mask_AG') 
% %     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
% %     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
% %     writetable(mask_ATL,currSheet,'Sheet','mask_ATL') 
% %     writetable(mask_ATL_L,currSheet,'Sheet','mask_ATL_L') 
% %     writetable(mask_ATL_R,currSheet,'Sheet','mask_ATL_R') 
% %     writetable(mask_CG,currSheet,'Sheet','mask_CG') 
% %     writetable(mask_CG_L,currSheet,'Sheet','mask_CG_L') 
% %     writetable(mask_CG_R,currSheet,'Sheet','mask_CG_R') 
% %     writetable(mask_FFA,currSheet,'Sheet','mask_FFA') 
% %     writetable(mask_FFA_L,currSheet,'Sheet','mask_FFA_L') 
% %     writetable(mask_FFA_R,currSheet,'Sheet','mask_FFA_R') 
% %     writetable(mask_FuG,currSheet,'Sheet','mask_FuG') 
% %     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
% %     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
% %     writetable(mask_Hipp,currSheet,'Sheet','mask_Hipp') 
% %     writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
% %     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
% %     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
% %     writetable(mask_Hipp_L,currSheet,'Sheet','mask_Hipp_L') 
% %     writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
% %     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
% %     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
% %     writetable(mask_Hipp_R,currSheet,'Sheet','mask_Hipp_R') 
% %     writetable(mask_IFG,currSheet,'Sheet','mask_IFG') 
% %     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
% %     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
% %     writetable(mask_INS,currSheet,'Sheet','mask_INS') 
% %     writetable(mask_INS_L,currSheet,'Sheet','mask_INS_L') 
% %     writetable(mask_INS_R,currSheet,'Sheet','mask_INS_R') 
% %     writetable(mask_IPL,currSheet,'Sheet','mask_IPL') 
% %     writetable(mask_IPL_L,currSheet,'Sheet','mask_IPL_L') 
% %     writetable(mask_IPL_R,currSheet,'Sheet','mask_IPL_R') 
% %     writetable(mask_ITG,currSheet,'Sheet','mask_ITG') 
% %     writetable(mask_ITG_L,currSheet,'Sheet','mask_ITG_L') 
% %     writetable(mask_ITG_R,currSheet,'Sheet','mask_ITG_R') 
% %     writetable(mask_LOC,currSheet,'Sheet','mask_LOC') 
% %     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
% %     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
% %     writetable(mask_MFG,currSheet,'Sheet','mask_MFG') 
% %     writetable(mask_MFG_L,currSheet,'Sheet','mask_MFG_L') 
% %     writetable(mask_MFG_R,currSheet,'Sheet','mask_MFG_R') 
% %     writetable(mask_MTG,currSheet,'Sheet','mask_MTG') 
% %     writetable(mask_MTG_L,currSheet,'Sheet','mask_MTG_L') 
% %     writetable(mask_MTG_R,currSheet,'Sheet','mask_MTG_R') 
% %     writetable(mask_MVOC,currSheet,'Sheet','mask_MVOC') 
% %     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
% %     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
% %     writetable(mask_OrG,currSheet,'Sheet','mask_OrG') 
% %     writetable(mask_OrG_L,currSheet,'Sheet','mask_OrG_L') 
% %     writetable(mask_OrG_R,currSheet,'Sheet','mask_OrG_R') 
% %     writetable(mask_PCL,currSheet,'Sheet','mask_PCL') 
% %     writetable(mask_PCL_L,currSheet,'Sheet','mask_PCL_L') 
% %     writetable(mask_PCL_R,currSheet,'Sheet','mask_PCL_R') 
% %     writetable(mask_PHC,currSheet,'Sheet','mask_PHC') 
% %     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
% %     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
% %     writetable(mask_Pcun,currSheet,'Sheet','mask_Pcun') 
% %     writetable(mask_Pcun_L,currSheet,'Sheet','mask_Pcun_L') 
% %     writetable(mask_Pcun_R,currSheet,'Sheet','mask_Pcun_R') 
% %     writetable(mask_Perirhinal,currSheet,'Sheet','mask_Perirhinal') 
% %     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
% %     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
% %     writetable(mask_PhG,currSheet,'Sheet','mask_PhG') 
% %     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
% %     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
% %     writetable(mask_PoG,currSheet,'Sheet','mask_PoG') 
% %     writetable(mask_PoG_L,currSheet,'Sheet','mask_PoG_L') 
% %     writetable(mask_PoG_R,currSheet,'Sheet','mask_PoG_R') 
% %     writetable(mask_PrG,currSheet,'Sheet','mask_PrG') 
% %     writetable(mask_PrG_L,currSheet,'Sheet','mask_PrG_L') 
% %     writetable(mask_PrG_R,currSheet,'Sheet','mask_PrG_R') 
% %     writetable(mask_RSC,currSheet,'Sheet','mask_RSC') 
% %     writetable(mask_RSC_L,currSheet,'Sheet','mask_RSC_L') 
% %     writetable(mask_RSC_R,currSheet,'Sheet','mask_RSC_R') 
% %     writetable(mask_Rhinal,currSheet,'Sheet','mask_Rhinal') 
% %     writetable(mask_Rhinal_L,currSheet,'Sheet','mask_Rhinal_L') 
% %     writetable(mask_Rhinal_R,currSheet,'Sheet','mask_Rhinal_R') 
% %     writetable(mask_SFG,currSheet,'Sheet','mask_SFG') 
% %     writetable(mask_SFG_L,currSheet,'Sheet','mask_SFG_L') 
% %     writetable(mask_SFG_R,currSheet,'Sheet','mask_SFG_R') 
% %     writetable(mask_SMG,currSheet,'Sheet','mask_SMG') 
% %     writetable(mask_SMG_L,currSheet,'Sheet','mask_SMG_L') 
% %     writetable(mask_SMG_R,currSheet,'Sheet','mask_SMG_R') 
% %     writetable(mask_SPL,currSheet,'Sheet','mask_SPL') 
% %     writetable(mask_SPL_L,currSheet,'Sheet','mask_SPL_L') 
% %     writetable(mask_SPL_R,currSheet,'Sheet','mask_SPL_R') 
% %     writetable(mask_STG,currSheet,'Sheet','mask_STG') 
% %     writetable(mask_STG_L,currSheet,'Sheet','mask_STG_L') 
% %     writetable(mask_STG_R,currSheet,'Sheet','mask_STG_R') 
% %     writetable(mask_pSTS,currSheet,'Sheet','mask_pSTS') 
% %     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
% %     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 
% 
% 
% 
% %%%%% forgotten trials
%     clear allActivityMat_forgotten
%     for ROI = 1:numMasks
%         activityCol = vertcat(subjInfo_forgotten(1).activityVal(:,ROI),subjInfo_forgotten(2).activityVal(:,ROI), ...
%             subjInfo_forgotten(3).activityVal(:,ROI),subjInfo_forgotten(4).activityVal(:,ROI),subjInfo_forgotten(5).activityVal(:,ROI), ...
%             subjInfo_forgotten(6).activityVal(:,ROI),subjInfo_forgotten(7).activityVal(:,ROI),subjInfo_forgotten(8).activityVal(:,ROI), ...
%             subjInfo_forgotten(9).activityVal(:,ROI),subjInfo_forgotten(10).activityVal(:,ROI),subjInfo_forgotten(11).activityVal(:,ROI), ...
%             subjInfo_forgotten(12).activityVal(:,ROI),subjInfo_forgotten(13).activityVal(:,ROI),subjInfo_forgotten(14).activityVal(:,ROI), ...
%             subjInfo_forgotten(15).activityVal(:,ROI),subjInfo_forgotten(16).activityVal(:,ROI),subjInfo_forgotten(17).activityVal(:,ROI), ...
%             subjInfo_forgotten(18).activityVal(:,ROI),subjInfo_forgotten(19).activityVal(:,ROI));
%         allActivityMat_forgotten(:,ROI) = activityCol;
%     end
% 
%     oneROI_Fs = vertcat(subjInfo_forgotten.Fval); 
% 
%     oneROI_lexMem = vertcat(subjInfo_forgotten.lexMem);
%     oneROI_visMem = vertcat(subjInfo_forgotten.visMem);
%     subjCol = vertcat(repmat("S002",numel(subjInfo_forgotten(1).Fval(:,1)),1),repmat("S005",numel(subjInfo_forgotten(2).Fval(:,1)),1), ...
%         repmat("S006",numel(subjInfo_forgotten(3).Fval(:,1)),1),repmat("S008",numel(subjInfo_forgotten(4).Fval(:,1)),1), ...
%         repmat("S009",numel(subjInfo_forgotten(5).Fval(:,1)),1),repmat("S010",numel(subjInfo_forgotten(6).Fval(:,1)),1), ...
%         repmat("S011",numel(subjInfo_forgotten(7).Fval(:,1)),1),repmat("S013",numel(subjInfo_forgotten(8).Fval(:,1)),1), ...
%         repmat("S014",numel(subjInfo_forgotten(9).Fval(:,1)),1),repmat("S015",numel(subjInfo_forgotten(10).Fval(:,1)),1), ...
%         repmat("S016",numel(subjInfo_forgotten(11).Fval(:,1)),1),repmat("S018",numel(subjInfo_forgotten(12).Fval(:,1)),1), ...
%         repmat("S019",numel(subjInfo_forgotten(13).Fval(:,1)),1),repmat("S021",numel(subjInfo_forgotten(14).Fval(:,1)),1), ...
%         repmat("S022",numel(subjInfo_forgotten(15).Fval(:,1)),1),repmat("S023",numel(subjInfo_forgotten(16).Fval(:,1)),1), ...
%         repmat("S024",numel(subjInfo_forgotten(17).Fval(:,1)),1),repmat("S025",numel(subjInfo_forgotten(18).Fval(:,1)),1), ...
%         repmat("S026",numel(subjInfo_forgotten(19).Fval(:,1)),1));
% 
%     ID_col = vertcat(subjInfo_forgotten.IDs);
% 
%     sz = [numel(ID_col) numFactors+2];
%     
%     if currFac == 1 %encycl numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 2 %encycl_300 numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 3 %vis numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 4 %vis_300 numFactors = 8
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 5 %fcn numFactors = 5
%         varTypes = ["string","string","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05"];
%     elseif currFac == 6 %fcn_300 numFactors = 4
%         varTypes = ["string","string","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04"];
% 
%     elseif currFac == 7 %all
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 8 %all 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 9 %outside
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 10 %outside 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 11 %home
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 12 %home 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 13 %home tool
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 14 %home tool 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 15 %animal
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 16 %animal 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 17 %food
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
%     elseif currFac == 18 %food 300
%         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
%         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
% 
%     end %end currFac if statement for making a struct to hold the F values
% 
% %     if currFac == 1 %encycl numFactors = 25
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
% %     elseif currFac == 2 %encycl_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     elseif currFac == 3 %vis numFactors = 25
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25"];
% %     elseif currFac == 4 %vis_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     elseif currFac == 5 %fcn numFactors = 8
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08"];
% %     elseif currFac == 6 %fcn_300 numFactors = 4
% %         varTypes = ["string","string","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04"];
% %     elseif currFac == 7 %noTax numFactors = 50
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
% %             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
% %     elseif currFac == 8 %noTax_300 numFactors = 30
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30"];
% %     elseif currFac == 9 %all numFactors = 50
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25",...
% %             "F26","F27","F28","F29","F30","F31","F32","F33","F34","F35","F36","F37","F38","F39","F40",...
% %             "F41","F42","F43","F44","F45","F46","F47","F48","F49","F50"];
% %     elseif currFac == 10 %all_300 numFactors = 18
% %         varTypes = ["string","string","double","double","double","double","double","double","double","double","double","double",...
% %             "double","double","double","double","double","double","double","double"];
% %         varNames = ["Subj","ItemID","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10",...
% %             "F11","F12","F13","F14","F15","F16","F17","F18"];
% %     end %end currFac if statement for making a struct to hold the F values
% 
%     allButROI_custom_F = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
%     allButROI_custom_F.Subj = subjCol;
%     allButROI_custom_F.ItemID = ID_col;
%     Fs_tbl = array2table(oneROI_Fs); 
%     allButROI_custom_F(:,3:numFactors+2) = Fs_tbl;
% 
%     %%% new masks jan 2023 forgotten trials
%     allActivityMat = allActivityMat_forgotten;
% 
%     AvgROI = allActivityMat(:,2);
%     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,3);
%     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,20);
%     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,21);
%     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,24);
%     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,25);
%     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,28);
%     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,29);
%     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,32);
%     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,33);
%     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,44);
%     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,45);
%     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,53);
%     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,54);
%     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,62);
%     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,63);
%     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,68);
%     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,69);
%     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,71);
%     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,72);
%     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,101);
%     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,102);
%     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% % 
% %     AvgROI = allActivityMat(:,1);
% %     mask_AG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,2);
% %     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,3);
% %     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,4);
% %     mask_ATL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,5);
% %     mask_ATL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,6);
% %     mask_ATL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     % cols 7,8,9 are amyg
% %     % cols 10,11,12 are basal ganglia
% %     AvgROI = allActivityMat(:,13);
% %     mask_CG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,14);
% %     mask_CG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,15);
% %     mask_CG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,16);
% %     mask_FFA = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,17);
% %     mask_FFA_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,18);
% %     mask_FFA_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,19);
% %     mask_FuG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,20);
% %     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,21);
% %     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,22);
% %     mask_Hipp = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,23);
% %     mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,24);
% %     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,25);
% %     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,26);
% %     mask_Hipp_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,27);
% %     mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,28);
% %     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,29);
% %     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,30);
% %     mask_Hipp_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,31);
% %     mask_IFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,32);
% %     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,33);
% %     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,34);
% %     mask_INS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,35);
% %     mask_INS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,36);
% %     mask_INS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,37);
% %     mask_IPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,38);
% %     mask_IPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,39);
% %     mask_IPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,40);
% %     mask_ITG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,41);
% %     mask_ITG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,42);
% %     mask_ITG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,43);
% %     mask_LOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,44);
% %     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,45);
% %     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,46);
% %     mask_MFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,47);
% %     mask_MFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,48);
% %     mask_MFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,49);
% %     mask_MTG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,50);
% %     mask_MTG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,51);
% %     mask_MTG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,52);
% %     mask_MVOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,53);
% %     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,54);
% %     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,55);
% %     mask_OrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,56);
% %     mask_OrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,57);
% %     mask_OrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,58);
% %     mask_PCL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,59);
% %     mask_PCL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,60);
% %     mask_PCL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,61);
% %     mask_PHC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,62);
% %     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,63);
% %     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,64);
% %     mask_Pcun = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,65);
% %     mask_Pcun_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,66);
% %     mask_Pcun_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,67);
% %     mask_Perirhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,68);
% %     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,69);
% %     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,70);
% %     mask_PhG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,71);
% %     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,72);
% %     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,73);
% %     mask_PoG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,74);
% %     mask_PoG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,75);
% %     mask_PoG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,76);
% %     mask_PrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,77);
% %     mask_PrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,78);
% %     mask_PrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,79);
% %     mask_RSC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,80);
% %     mask_RSC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,81);
% %     mask_RSC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,82);
% %     mask_Rhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,83);
% %     mask_Rhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,84);
% %     mask_Rhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,85);
% %     mask_SFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,86);
% %     mask_SFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,87);
% %     mask_SFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,88);
% %     mask_SMG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,89);
% %     mask_SMG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,90);
% %     mask_SMG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,91);
% %     mask_SPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,92);
% %     mask_SPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,93);
% %     mask_SPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,94);
% %     mask_STG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,95);
% %     mask_STG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,96);
% %     mask_STG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     % 97,98,99 thalamus
% %     AvgROI = allActivityMat(:,100);
% %     mask_pSTS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,101);
% %     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% %     AvgROI = allActivityMat(:,102);
% %     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     
%     
% %     if currFac == 1
% %         currSheet = 'avgActivity_forgotten_BNA_F_encycl_jan23.xlsx';
% %     elseif currFac == 2
% %         currSheet = 'avgActivity_forgotten_BNA_F_encycl_300_jan23.xlsx';
% %     elseif currFac == 3
% %         currSheet = 'avgActivity_forgotten_BNA_F_vis_jan23.xlsx';
% %     elseif currFac == 4
% %         currSheet = 'avgActivity_forgotten_BNA_F_vis_300_jan23.xlsx';
% %     elseif currFac == 5
% %         currSheet = 'avgActivity_forgotten_BNA_F_fcn_jan23.xlsx';
% %     elseif currFac == 6
% %         currSheet = 'avgActivity_forgotten_BNA_F_fcn_300_jan23.xlsx';
% % %     elseif currFac == 7
% % %         currSheet = 'avgActivity_forgotten_BNA_F_noTax_jan23.xlsx';
% % %     elseif currFac == 8
% % %         currSheet = 'avgActivity_forgotten_BNA_F_noTax_300_jan23.xlsx';
% % %     elseif currFac == 9
% % %         currSheet = 'avgActivity_forgotten_BNA_F_all_jan23.xlsx';
% % %     elseif currFac == 10
% % %         currSheet = 'avgActivity_forgotten_BNA_F_all_300_jan23.xlsx';
% %     end %end currFac if statement
% 
%     if currFac == 1
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_encycl.xlsx';
%     elseif currFac == 2
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_encycl_300.xlsx';
%     elseif currFac == 3
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_vis.xlsx';
%     elseif currFac == 4
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_vis_300.xlsx';
%     elseif currFac == 5
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_fcn.xlsx';
%     elseif currFac == 6
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_fcn_300.xlsx';
%     elseif currFac == 7
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_all.xlsx';
%     elseif currFac == 8
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_all_300.xlsx';
%     elseif currFac == 9
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_outside.xlsx';
%     elseif currFac == 10
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_outside_300.xlsx';
%     elseif currFac == 11
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_home.xlsx';
%     elseif currFac == 12
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_home_300.xlsx';
%     elseif currFac == 13
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_home_tool.xlsx';
%     elseif currFac == 14
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_home_tool_300.xlsx';
%     elseif currFac == 15
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_animal.xlsx';
%     elseif currFac == 16
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_animal_300.xlsx';
%     elseif currFac == 17
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_food.xlsx';
%     elseif currFac == 18
%         currSheet = 'avgActivity_forgotten_BNA_nnmf_food_300.xlsx';
%     end %end currFac if statement
%     
%  %%%% heads up this takes a while
%     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
%     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
%     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
%     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
%     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
%     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
%     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
%     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
%     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
%     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 
 
%     writetable(mask_AG,currSheet,'Sheet','mask_AG') 
%     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
%     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
%     writetable(mask_ATL,currSheet,'Sheet','mask_ATL') 
%     writetable(mask_ATL_L,currSheet,'Sheet','mask_ATL_L') 
%     writetable(mask_ATL_R,currSheet,'Sheet','mask_ATL_R') 
%     writetable(mask_CG,currSheet,'Sheet','mask_CG') 
%     writetable(mask_CG_L,currSheet,'Sheet','mask_CG_L') 
%     writetable(mask_CG_R,currSheet,'Sheet','mask_CG_R') 
%     writetable(mask_FFA,currSheet,'Sheet','mask_FFA') 
%     writetable(mask_FFA_L,currSheet,'Sheet','mask_FFA_L') 
%     writetable(mask_FFA_R,currSheet,'Sheet','mask_FFA_R') 
%     writetable(mask_FuG,currSheet,'Sheet','mask_FuG') 
%     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
%     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
%     writetable(mask_Hipp,currSheet,'Sheet','mask_Hipp') 
%     writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_L,currSheet,'Sheet','mask_Hipp_L') 
%     writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_Hipp_R,currSheet,'Sheet','mask_Hipp_R') 
%     writetable(mask_IFG,currSheet,'Sheet','mask_IFG') 
%     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
%     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
%     writetable(mask_INS,currSheet,'Sheet','mask_INS') 
%     writetable(mask_INS_L,currSheet,'Sheet','mask_INS_L') 
%     writetable(mask_INS_R,currSheet,'Sheet','mask_INS_R') 
%     writetable(mask_IPL,currSheet,'Sheet','mask_IPL') 
%     writetable(mask_IPL_L,currSheet,'Sheet','mask_IPL_L') 
%     writetable(mask_IPL_R,currSheet,'Sheet','mask_IPL_R') 
%     writetable(mask_ITG,currSheet,'Sheet','mask_ITG') 
%     writetable(mask_ITG_L,currSheet,'Sheet','mask_ITG_L') 
%     writetable(mask_ITG_R,currSheet,'Sheet','mask_ITG_R') 
%     writetable(mask_LOC,currSheet,'Sheet','mask_LOC') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
%     writetable(mask_MFG,currSheet,'Sheet','mask_MFG') 
%     writetable(mask_MFG_L,currSheet,'Sheet','mask_MFG_L') 
%     writetable(mask_MFG_R,currSheet,'Sheet','mask_MFG_R') 
%     writetable(mask_MTG,currSheet,'Sheet','mask_MTG') 
%     writetable(mask_MTG_L,currSheet,'Sheet','mask_MTG_L') 
%     writetable(mask_MTG_R,currSheet,'Sheet','mask_MTG_R') 
%     writetable(mask_MVOC,currSheet,'Sheet','mask_MVOC') 
%     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
%     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
%     writetable(mask_OrG,currSheet,'Sheet','mask_OrG') 
%     writetable(mask_OrG_L,currSheet,'Sheet','mask_OrG_L') 
%     writetable(mask_OrG_R,currSheet,'Sheet','mask_OrG_R') 
%     writetable(mask_PCL,currSheet,'Sheet','mask_PCL') 
%     writetable(mask_PCL_L,currSheet,'Sheet','mask_PCL_L') 
%     writetable(mask_PCL_R,currSheet,'Sheet','mask_PCL_R') 
%     writetable(mask_PHC,currSheet,'Sheet','mask_PHC') 
%     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
%     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
%     writetable(mask_Pcun,currSheet,'Sheet','mask_Pcun') 
%     writetable(mask_Pcun_L,currSheet,'Sheet','mask_Pcun_L') 
%     writetable(mask_Pcun_R,currSheet,'Sheet','mask_Pcun_R') 
%     writetable(mask_Perirhinal,currSheet,'Sheet','mask_Perirhinal') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG,currSheet,'Sheet','mask_PhG') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_PoG,currSheet,'Sheet','mask_PoG') 
%     writetable(mask_PoG_L,currSheet,'Sheet','mask_PoG_L') 
%     writetable(mask_PoG_R,currSheet,'Sheet','mask_PoG_R') 
%     writetable(mask_PrG,currSheet,'Sheet','mask_PrG') 
%     writetable(mask_PrG_L,currSheet,'Sheet','mask_PrG_L') 
%     writetable(mask_PrG_R,currSheet,'Sheet','mask_PrG_R') 
%     writetable(mask_RSC,currSheet,'Sheet','mask_RSC') 
%     writetable(mask_RSC_L,currSheet,'Sheet','mask_RSC_L') 
%     writetable(mask_RSC_R,currSheet,'Sheet','mask_RSC_R') 
%     writetable(mask_Rhinal,currSheet,'Sheet','mask_Rhinal') 
%     writetable(mask_Rhinal_L,currSheet,'Sheet','mask_Rhinal_L') 
%     writetable(mask_Rhinal_R,currSheet,'Sheet','mask_Rhinal_R') 
%     writetable(mask_SFG,currSheet,'Sheet','mask_SFG') 
%     writetable(mask_SFG_L,currSheet,'Sheet','mask_SFG_L') 
%     writetable(mask_SFG_R,currSheet,'Sheet','mask_SFG_R') 
%     writetable(mask_SMG,currSheet,'Sheet','mask_SMG') 
%     writetable(mask_SMG_L,currSheet,'Sheet','mask_SMG_L') 
%     writetable(mask_SMG_R,currSheet,'Sheet','mask_SMG_R') 
%     writetable(mask_SPL,currSheet,'Sheet','mask_SPL') 
%     writetable(mask_SPL_L,currSheet,'Sheet','mask_SPL_L') 
%     writetable(mask_SPL_R,currSheet,'Sheet','mask_SPL_R') 
%     writetable(mask_STG,currSheet,'Sheet','mask_STG') 
%     writetable(mask_STG_L,currSheet,'Sheet','mask_STG_L') 
%     writetable(mask_STG_R,currSheet,'Sheet','mask_STG_R') 
%     writetable(mask_pSTS,currSheet,'Sheet','mask_pSTS') 
%     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
%     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 


%%%% mem and per-mask activity
% 
%     %%% don't want to re-type everything, so just re-name variables
%     allActivityMat = allActivityMat_memShort;
%     allButROI_custom_F = allButROI_memShort;
% 
% 
%     AvgROI = allActivityMat(:,2);
%     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,3);
%     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,20);
%     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,21);
%     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,24);
%     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,25);
%     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,28);
%     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,29);
%     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,32);
%     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,33);
%     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,44);
%     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,45);
%     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,53);
%     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,54);
%     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,62);
%     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,63);
%     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,68);
%     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,69);
%     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,71);
%     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,72);
%     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,101);
%     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,102);
%     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');

%     AvgROI = allActivityMat(:,1);
%     mask_AG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,2);
%     mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,3);
%     mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,4);
%     mask_ATL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,5);
%     mask_ATL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,6);
%     mask_ATL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     % cols 7,8,9 are amyg
%     % cols 10,11,12 are basal ganglia
%     AvgROI = allActivityMat(:,13);
%     mask_CG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,14);
%     mask_CG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,15);
%     mask_CG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,16);
%     mask_FFA = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,17);
%     mask_FFA_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,18);
%     mask_FFA_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,19);
%     mask_FuG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,20);
%     mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,21);
%     mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,22);
%     mask_Hipp = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,23);
%     mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,24);
%     mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,25);
%     mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,26);
%     mask_Hipp_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,27);
%     mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,28);
%     mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,29);
%     mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,30);
%     mask_Hipp_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,31);
%     mask_IFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,32);
%     mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,33);
%     mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,34);
%     mask_INS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,35);
%     mask_INS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,36);
%     mask_INS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,37);
%     mask_IPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,38);
%     mask_IPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,39);
%     mask_IPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,40);
%     mask_ITG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,41);
%     mask_ITG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,42);
%     mask_ITG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,43);
%     mask_LOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,44);
%     mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,45);
%     mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,46);
%     mask_MFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,47);
%     mask_MFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,48);
%     mask_MFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,49);
%     mask_MTG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,50);
%     mask_MTG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,51);
%     mask_MTG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,52);
%     mask_MVOC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,53);
%     mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,54);
%     mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,55);
%     mask_OrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,56);
%     mask_OrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,57);
%     mask_OrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,58);
%     mask_PCL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,59);
%     mask_PCL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,60);
%     mask_PCL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,61);
%     mask_PHC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,62);
%     mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,63);
%     mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,64);
%     mask_Pcun = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,65);
%     mask_Pcun_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,66);
%     mask_Pcun_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,67);
%     mask_Perirhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,68);
%     mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,69);
%     mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,70);
%     mask_PhG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,71);
%     mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,72);
%     mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,73);
%     mask_PoG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,74);
%     mask_PoG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,75);
%     mask_PoG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,76);
%     mask_PrG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,77);
%     mask_PrG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,78);
%     mask_PrG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,79);
%     mask_RSC = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,80);
%     mask_RSC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,81);
%     mask_RSC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,82);
%     mask_Rhinal = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,83);
%     mask_Rhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,84);
%     mask_Rhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,85);
%     mask_SFG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,86);
%     mask_SFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,87);
%     mask_SFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,88);
%     mask_SMG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,89);
%     mask_SMG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,90);
%     mask_SMG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,91);
%     mask_SPL = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,92);
%     mask_SPL_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,93);
%     mask_SPL_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,94);
%     mask_STG = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,95);
%     mask_STG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,96);
%     mask_STG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     % 97,98,99 thalamus
%     AvgROI = allActivityMat(:,100);
%     mask_pSTS = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,101);
%     mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
%     AvgROI = allActivityMat(:,102);
%     mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
 
%     currSheet = 'avgActivity_mem_jan23.xlsx';
% 
%     
%  %%%% heads up this takes a while
%     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
%     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
%     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
%     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
%     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
%     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
%     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
%     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
%     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
%     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 
%     
%     writetable(mask_AG,currSheet,'Sheet','mask_AG') 
%     writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
%     writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
%     writetable(mask_ATL,currSheet,'Sheet','mask_ATL') 
%     writetable(mask_ATL_L,currSheet,'Sheet','mask_ATL_L') 
%     writetable(mask_ATL_R,currSheet,'Sheet','mask_ATL_R') 
%     writetable(mask_CG,currSheet,'Sheet','mask_CG') 
%     writetable(mask_CG_L,currSheet,'Sheet','mask_CG_L') 
%     writetable(mask_CG_R,currSheet,'Sheet','mask_CG_R') 
%     writetable(mask_FFA,currSheet,'Sheet','mask_FFA') 
%     writetable(mask_FFA_L,currSheet,'Sheet','mask_FFA_L') 
%     writetable(mask_FFA_R,currSheet,'Sheet','mask_FFA_R') 
%     writetable(mask_FuG,currSheet,'Sheet','mask_FuG') 
%     writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
%     writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
%     writetable(mask_Hipp,currSheet,'Sheet','mask_Hipp') 
%     writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
%     writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
%     writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
%     writetable(mask_Hipp_L,currSheet,'Sheet','mask_Hipp_L') 
%     writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
%     writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
%     writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
%     writetable(mask_Hipp_R,currSheet,'Sheet','mask_Hipp_R') 
%     writetable(mask_IFG,currSheet,'Sheet','mask_IFG') 
%     writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
%     writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
%     writetable(mask_INS,currSheet,'Sheet','mask_INS') 
%     writetable(mask_INS_L,currSheet,'Sheet','mask_INS_L') 
%     writetable(mask_INS_R,currSheet,'Sheet','mask_INS_R') 
%     writetable(mask_IPL,currSheet,'Sheet','mask_IPL') 
%     writetable(mask_IPL_L,currSheet,'Sheet','mask_IPL_L') 
%     writetable(mask_IPL_R,currSheet,'Sheet','mask_IPL_R') 
%     writetable(mask_ITG,currSheet,'Sheet','mask_ITG') 
%     writetable(mask_ITG_L,currSheet,'Sheet','mask_ITG_L') 
%     writetable(mask_ITG_R,currSheet,'Sheet','mask_ITG_R') 
%     writetable(mask_LOC,currSheet,'Sheet','mask_LOC') 
%     writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
%     writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
%     writetable(mask_MFG,currSheet,'Sheet','mask_MFG') 
%     writetable(mask_MFG_L,currSheet,'Sheet','mask_MFG_L') 
%     writetable(mask_MFG_R,currSheet,'Sheet','mask_MFG_R') 
%     writetable(mask_MTG,currSheet,'Sheet','mask_MTG') 
%     writetable(mask_MTG_L,currSheet,'Sheet','mask_MTG_L') 
%     writetable(mask_MTG_R,currSheet,'Sheet','mask_MTG_R') 
%     writetable(mask_MVOC,currSheet,'Sheet','mask_MVOC') 
%     writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
%     writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
%     writetable(mask_OrG,currSheet,'Sheet','mask_OrG') 
%     writetable(mask_OrG_L,currSheet,'Sheet','mask_OrG_L') 
%     writetable(mask_OrG_R,currSheet,'Sheet','mask_OrG_R') 
%     writetable(mask_PCL,currSheet,'Sheet','mask_PCL') 
%     writetable(mask_PCL_L,currSheet,'Sheet','mask_PCL_L') 
%     writetable(mask_PCL_R,currSheet,'Sheet','mask_PCL_R') 
%     writetable(mask_PHC,currSheet,'Sheet','mask_PHC') 
%     writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
%     writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
%     writetable(mask_Pcun,currSheet,'Sheet','mask_Pcun') 
%     writetable(mask_Pcun_L,currSheet,'Sheet','mask_Pcun_L') 
%     writetable(mask_Pcun_R,currSheet,'Sheet','mask_Pcun_R') 
%     writetable(mask_Perirhinal,currSheet,'Sheet','mask_Perirhinal') 
%     writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
%     writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
%     writetable(mask_PhG,currSheet,'Sheet','mask_PhG') 
%     writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
%     writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
%     writetable(mask_PoG,currSheet,'Sheet','mask_PoG') 
%     writetable(mask_PoG_L,currSheet,'Sheet','mask_PoG_L') 
%     writetable(mask_PoG_R,currSheet,'Sheet','mask_PoG_R') 
%     writetable(mask_PrG,currSheet,'Sheet','mask_PrG') 
%     writetable(mask_PrG_L,currSheet,'Sheet','mask_PrG_L') 
%     writetable(mask_PrG_R,currSheet,'Sheet','mask_PrG_R') 
%     writetable(mask_RSC,currSheet,'Sheet','mask_RSC') 
%     writetable(mask_RSC_L,currSheet,'Sheet','mask_RSC_L') 
%     writetable(mask_RSC_R,currSheet,'Sheet','mask_RSC_R') 
%     writetable(mask_Rhinal,currSheet,'Sheet','mask_Rhinal') 
%     writetable(mask_Rhinal_L,currSheet,'Sheet','mask_Rhinal_L') 
%     writetable(mask_Rhinal_R,currSheet,'Sheet','mask_Rhinal_R') 
%     writetable(mask_SFG,currSheet,'Sheet','mask_SFG') 
%     writetable(mask_SFG_L,currSheet,'Sheet','mask_SFG_L') 
%     writetable(mask_SFG_R,currSheet,'Sheet','mask_SFG_R') 
%     writetable(mask_SMG,currSheet,'Sheet','mask_SMG') 
%     writetable(mask_SMG_L,currSheet,'Sheet','mask_SMG_L') 
%     writetable(mask_SMG_R,currSheet,'Sheet','mask_SMG_R') 
%     writetable(mask_SPL,currSheet,'Sheet','mask_SPL') 
%     writetable(mask_SPL_L,currSheet,'Sheet','mask_SPL_L') 
%     writetable(mask_SPL_R,currSheet,'Sheet','mask_SPL_R') 
%     writetable(mask_STG,currSheet,'Sheet','mask_STG') 
%     writetable(mask_STG_L,currSheet,'Sheet','mask_STG_L') 
%     writetable(mask_STG_R,currSheet,'Sheet','mask_STG_R') 
%     writetable(mask_pSTS,currSheet,'Sheet','mask_pSTS') 
%     writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
%     writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 



end % currFac loop for all ten factor scores



%%%%%%% MEM
%%% remember, when I run the loop above, the mem scores need to match
%%% everything else, so whether I'm running a particular subject or a
%%% particular set of F scores. Mem should be cut to 300 because those are
%%% the objects that the Ps actually see. However, I don't need to cut down
%%% to 271 or 293 or whatever because really I'm not trying to run mem WITH
%%% all the other variables, like F scores. I should do a separate loop
%%% down here

cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts;
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/'
% I'm probably going to need functions from these here
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/Scripts/spm12');
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/function_files')

addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/;

load visMem_CR.mat
load lexMem_CR.mat

%load visMem_HR.mat
%load lexMem_HR.mat

%%% to make it easier, just keep the 'CR' var name

%visMem_CR = visMem_HR;
%lexMem_CR = lexMem_HR;


% Note, the mem data has NaNs in it. I use NaNs later to remove catch trials, 
% so I have to change the existing NaNs to something else
for row = 1:length(lexMem_CR)
    if isnan(lexMem_CR(row))
        lexMem_CR(row) = 999;
    end
    if isnan(visMem_CR(row))
        visMem_CR(row) = 999;
    end
end

% load IDs
itemIDs_tbl = readtable('itemIDs.xlsx'); % this has all 995 item IDs and labels

% BNA merged ROIs from shenyang
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ROI_BNA/';
allMasks = readtable('bnaMaskMeans.xlsx'); 
allMasksArr = table2array(allMasks);
maskArray_ID_data = allMasksArr(:,2:end);
numMasks = 102; 

subjInfo = struct('IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);

subjInfo_remembered = struct('IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);
subjInfo_forgotten = struct('IDs',[],'subjNum',[],'activityVal',[],'lexMem',[],'visMem',[],'bothMem',[]);

% 19 subjects
subjectNum = {'002' '005' '006' '008' '009' '010' '011' '013' '014' '015' '016' '018' '019' '021' '022' '023' '024' '025' '026'};
subjectCounter = 1; % add 300 each time through loop so I can grab the 300 rows per subject

for subjects = 1:length(subjectNum)

    % Step 1: Grab the subject-specific rows 
    subjectSpecificData = maskArray_ID_data(subjectCounter:subjectCounter+299,:);

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

    indices_inOrder = zeros(numel(all_betas_enc),1);
    for idNumber = 1:numel(all_betas_enc)
        % all_IDs_enc is cell array of cells, so I have to peel them out
        % then convert the char you get out to a num to match the table items
        index = idNumber; % all_IDs_enc is 300 and so are the 300 fac scores
        indices_inOrder(idNumber) = index;
    end

    lexMem_subset = zeros(numel(all_betas_enc),1);
    visMem_subset = zeros(numel(all_betas_enc),1);

    for row = 1:numel(all_betas_enc)
        lexMem_subset(row,:) = lexMem_CR(indices_inOrder(row),:);
        visMem_subset(row,:) = visMem_CR(indices_inOrder(row),:);
    end

    % Step 3: Remove any trial that isn't labelled with TT3
    % in the previous step I made indices_TT. If there's a 1 in the row, keep it

    %subject_numbers = customMasksArray(:,1);
    subject_numbers = allMasksArr(:,1);
    
    for row = 1:numel(indices_TT)
        if indices_TT(row) == 1
            subjectSpecificData(row,:) = subjectSpecificData(row,:);
            subject_numbers(row) = subject_numbers(row);
            lexMem_subset(row,:) = lexMem_subset(row,:);
            visMem_subset(row,:) = visMem_subset(row,:);
        else 
            subjectSpecificData(row,:) = NaN;
            subject_numbers(row) = NaN;
            lexMem_subset(row,:) = NaN;
            visMem_subset(row,:) = NaN;
        end 
    end

    % remove the rows with NaN
    lexMem_subset(any(isnan(lexMem_subset), 2), :) = [];
    visMem_subset(any(isnan(visMem_subset), 2), :) = [];
    subjectSpecificData(any(isnan(subjectSpecificData), 2), :) = [];
    subject_numbers(any(isnan(subject_numbers), 2), :) = [];


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
    IDs = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newencS', subjectNum{subjects},'_final2.xlsx'),'Z:Z');

    % process CMEM. Want 0 for miss (1 and 2), 1 for hit (3 and 4), and NaN
    % for catch trial to be removed later
    memorability = zeros(length(cmem_col),1);
    for number = 1:length(cmem_col)
        if cmem_col(number) == 1 %0 for miss
            memorability(number) = 0;
        elseif cmem_col(number) == 2 
            memorability(number) = 0;
        elseif cmem_col(number) == 3 %1 for hit
            memorability(number) = 1;
        elseif cmem_col(number) == 4  %3 for hit
            memorability(number) = 1;
        else
            memorability(number) = NaN; % maybe add NaNs so I can remove them later
        end
    end
    
    % IDs and memorability are still 330 at this point because catch trials
    % haven't been removed yet. Do isnan to get down to 300
    mem_old = memorability(~isnan(memorability));
    IDs_old = IDs(~isnan(memorability));

    % now IDs_olc matches IDs_enc, so mem_old is in the correct order.
    % Now I have to remove the catch trials from mem_old like I did above

    for row = 1:numel(indices_TT)
        if indices_TT(row) == 0
            mem_old(row) = NaN;                    
        end 
    end

    % remove the rows with NaN
    mem_old_short = mem_old(~isnan(mem_old));

    subjectSpecificData_remembered = zeros(length(mem_old_short),numMasks+1); 
    subjectSpecificData_forgotten = zeros(length(mem_old_short),numMasks+1); 
    lexMem_remembered = zeros(length(mem_old_short),1);
    lexMem_forgotten = zeros(length(mem_old_short),1);
    visMem_remembered = zeros(length(mem_old_short),1);
    visMem_forgotten = zeros(length(mem_old_short),1);
    subject_numbers_remembered = zeros(length(mem_old_short),1);
    subject_numbers_forgotten = zeros(length(mem_old_short),1);

    for row = 1:numel(mem_old_short)
        if mem_old_short(row) == 1
            subjectSpecificData_remembered(row,:) = subjectSpecificData(row,:);
            lexMem_remembered(row,:) = lexMem_subset(row,:);
            visMem_remembered(row,:) = visMem_subset(row,:);
            subject_numbers_remembered(row) = subject_numbers_remembered(row);
        elseif mem_old_short(row) == 0       
            subjectSpecificData_forgotten(row,:) = subjectSpecificData(row,:);
            lexMem_forgotten(row,:) = lexMem_subset(row,:);
            visMem_forgotten(row,:) = visMem_subset(row,:);
            subject_numbers_forgotten(row) = subject_numbers_forgotten(row);
        end
    end

    lexMem_rem_withZeros = lexMem_remembered;
    lexMem_for_withZeros = lexMem_forgotten;
    visMem_rem_withZeros = visMem_remembered;
    visMem_for_withZeros = visMem_forgotten;

    % remove zero rows
    subjectSpecificData_remembered(all(~subjectSpecificData_remembered,2),:) = [];
    subjectSpecificData_forgotten(all(~subjectSpecificData_forgotten,2),:) = [];


   % lexMem_remembered(all(~lexMem_rem_withZeros,2),:) = [];
   % lexMem_forgotten(all(~lexMem_for_withZeros,2),:) = [];  
    lexMem_remembered(all(~visMem_rem_withZeros,2),:) = [];
    lexMem_forgotten(all(~visMem_for_withZeros,2),:) = [];  
    %visMem_remembered(all(~lexMem_rem_withZeros,2),:) = [];
    visMem_remembered(all(~visMem_rem_withZeros,2),:) = []; %make sure vis gets cut to the same length?
    visMem_forgotten(all(~visMem_for_withZeros,2),:) = [];  
    %visMem_forgotten(all(~lexMem_for_withZeros,2),:) = [];  
    subject_numbers_remembered(all(~subject_numbers_remembered,2),:) = [];
    subject_numbers_forgotten(all(~subject_numbers_forgotten,2),:) = [];

    % add to struct
    subjInfo_remembered(subjects).IDs = subjectSpecificData_remembered(:,1);
    subjInfo_remembered(subjects).subjNum = subject_numbers_remembered;
    subjInfo_remembered(subjects).activityVal = subjectSpecificData_remembered(:,2:end);
    subjInfo_remembered(subjects).lexMem = lexMem_remembered;
    subjInfo_remembered(subjects).visMem = visMem_remembered;

    subjInfo_forgotten(subjects).IDs = subjectSpecificData_forgotten(:,1);
    subjInfo_forgotten(subjects).subjNum = subject_numbers_forgotten;
    subjInfo_forgotten(subjects).activityVal = subjectSpecificData_forgotten(:,2:end);
    subjInfo_forgotten(subjects).lexMem = lexMem_forgotten;
    subjInfo_forgotten(subjects).visMem = visMem_forgotten;

%%%%%%% return to original code here
    subjInfo(subjects).IDs = subjectSpecificData(:,1);
    subjInfo(subjects).subjNum = subject_numbers;
    subjInfo(subjects).activityVal = subjectSpecificData(:,2:end);
    subjInfo(subjects).lexMem = lexMem_subset;
    subjInfo(subjects).visMem = visMem_subset;

    subjectCounter = subjectCounter + 300; % go to next subject's first row
end
    

%%%%%%%% all trials
numMasks = 102; %BNA. There are 104 cols but first two are subject and item
allActivityMat = zeros(5225,numMasks);
for ROI = 1:numMasks
    activityCol = vertcat(subjInfo(1).activityVal(:,ROI),subjInfo(2).activityVal(:,ROI), ...
        subjInfo(3).activityVal(:,ROI),subjInfo(4).activityVal(:,ROI),subjInfo(5).activityVal(:,ROI), ...
        subjInfo(6).activityVal(:,ROI),subjInfo(7).activityVal(:,ROI),subjInfo(8).activityVal(:,ROI), ...
        subjInfo(9).activityVal(:,ROI),subjInfo(10).activityVal(:,ROI),subjInfo(11).activityVal(:,ROI), ...
        subjInfo(12).activityVal(:,ROI),subjInfo(13).activityVal(:,ROI),subjInfo(14).activityVal(:,ROI), ...
        subjInfo(15).activityVal(:,ROI),subjInfo(16).activityVal(:,ROI),subjInfo(17).activityVal(:,ROI), ...
        subjInfo(18).activityVal(:,ROI),subjInfo(19).activityVal(:,ROI));
    allActivityMat(:,ROI) = activityCol;
end

oneROI_lexMem = vertcat(subjInfo.lexMem);
oneROI_visMem = vertcat(subjInfo.visMem);

subjCol = vertcat(repmat("S002",numel(subjInfo(1).lexMem(:,1)),1),repmat("S005",numel(subjInfo(2).lexMem(:,1)),1), ...
    repmat("S006",numel(subjInfo(3).lexMem(:,1)),1),repmat("S008",numel(subjInfo(4).lexMem(:,1)),1), ...
    repmat("S009",numel(subjInfo(5).lexMem(:,1)),1),repmat("S010",numel(subjInfo(6).lexMem(:,1)),1), ...
    repmat("S011",numel(subjInfo(7).lexMem(:,1)),1),repmat("S013",numel(subjInfo(8).lexMem(:,1)),1), ...
    repmat("S014",numel(subjInfo(9).lexMem(:,1)),1),repmat("S015",numel(subjInfo(10).lexMem(:,1)),1), ...
    repmat("S016",numel(subjInfo(11).lexMem(:,1)),1),repmat("S018",numel(subjInfo(12).lexMem(:,1)),1), ...
    repmat("S019",numel(subjInfo(13).lexMem(:,1)),1),repmat("S021",numel(subjInfo(14).lexMem(:,1)),1), ...
    repmat("S022",numel(subjInfo(15).lexMem(:,1)),1),repmat("S023",numel(subjInfo(16).lexMem(:,1)),1), ...
    repmat("S024",numel(subjInfo(17).lexMem(:,1)),1),repmat("S025",numel(subjInfo(18).lexMem(:,1)),1), ...
    repmat("S026",numel(subjInfo(19).lexMem(:,1)),1));

ID_col = vertcat(subjInfo.IDs);
    
clear lex_index
clear vis_index
clear combined_index
clear both_index
lex_index = find(oneROI_lexMem==999); % 966
vis_index = find(oneROI_visMem==999); % 73
combined_index = vertcat(lex_index,vis_index); % 1039
both_index = unique(combined_index); % 966
    
% cut down so you have the same length as mem for which we have non-NaN values
allActivityMat(both_index,:) = [];
oneROI_lexMem(both_index) = [];
oneROI_visMem(both_index) = [];
subjCol(both_index) = [];
ID_col(both_index) = [];
    
allActivityMat_memShort = allActivityMat;
oneROI_lexMem_short = oneROI_lexMem;
oneROI_visMem_short = oneROI_visMem;
subjCol_short = subjCol;
ID_col_short = ID_col;

% at this point the mem values should all be cleaned
% therefore, I can now make bothMem from the short versions of vis and lex

normLex = oneROI_lexMem_short - repmat(mean(oneROI_lexMem_short),length(oneROI_lexMem_short),1);
normVis = oneROI_visMem_short - repmat(mean(oneROI_visMem_short),length(oneROI_visMem_short),1);

bothMem = normVis .* normLex;
    
    
%sz = [4178 5]; 
sz = [length(subjCol) 5];
varTypes = ["string","string","double","double","double"];
varNames = ["Subj","ItemID","lexMem","visMem","bothMem"];
allButROI_memShort = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);

allButROI_memShort.Subj = subjCol_short;
allButROI_memShort.ItemID = ID_col_short;
allButROI_memShort.lexMem = oneROI_lexMem_short;
allButROI_memShort.visMem = oneROI_visMem_short;
allButROI_memShort.bothMem = bothMem;

save('allActivityMat_memShort_jan23.mat','allActivityMat_memShort')
save('allButROI_memShort_jan23.mat','allButROI_memShort')

allActivityMat = allActivityMat_memShort;
allButROI_custom_F = allButROI_memShort;

AvgROI = allActivityMat(:,2);
mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,3);
mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,20);
mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,21);
mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,23);
mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,24);
mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,25);
mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,27);
mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,28);
mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,29);
mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,32);
mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,33);
mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,44);
mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,45);
mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,53);
mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,54);
mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,62);
mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,63);
mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,68);
mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,69);
mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,71);
mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,72);
mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,101);
mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,102);
mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');

currSheet = 'avgActivity_BNA_mem_jan23.xlsx';

%%%% heads up this takes a while
writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 

%%%%%%%%%% remembered

numMasks = 102; %BNA. There are 104 cols but first two are subject and item
for ROI = 1:numMasks
    activityCol = vertcat(subjInfo_remembered(1).activityVal(:,ROI),subjInfo_remembered(2).activityVal(:,ROI), ...
        subjInfo_remembered(3).activityVal(:,ROI),subjInfo_remembered(4).activityVal(:,ROI),subjInfo_remembered(5).activityVal(:,ROI), ...
        subjInfo_remembered(6).activityVal(:,ROI),subjInfo_remembered(7).activityVal(:,ROI),subjInfo_remembered(8).activityVal(:,ROI), ...
        subjInfo_remembered(9).activityVal(:,ROI),subjInfo_remembered(10).activityVal(:,ROI),subjInfo_remembered(11).activityVal(:,ROI), ...
        subjInfo_remembered(12).activityVal(:,ROI),subjInfo_remembered(13).activityVal(:,ROI),subjInfo_remembered(14).activityVal(:,ROI), ...
        subjInfo_remembered(15).activityVal(:,ROI),subjInfo_remembered(16).activityVal(:,ROI),subjInfo_remembered(17).activityVal(:,ROI), ...
        subjInfo_remembered(18).activityVal(:,ROI),subjInfo_remembered(19).activityVal(:,ROI));
    allActivityMat_remembered(:,ROI) = activityCol;
end

clear oneROI_lexMem
clear oneROI_visMem
oneROI_lexMem = vertcat(subjInfo_remembered.lexMem); % 3736. too short. can cut down with vis zeros in above loop to address this
oneROI_visMem = vertcat(subjInfo_remembered.visMem); %3758, same as allActivityMat_remembered

subjCol = vertcat(repmat("S002",numel(subjInfo_remembered(1).activityVal(:,1)),1),repmat("S005",numel(subjInfo_remembered(2).activityVal(:,1)),1), ...
    repmat("S006",numel(subjInfo_remembered(3).activityVal(:,1)),1),repmat("S008",numel(subjInfo_remembered(4).activityVal(:,1)),1), ...
    repmat("S009",numel(subjInfo_remembered(5).activityVal(:,1)),1),repmat("S010",numel(subjInfo_remembered(6).activityVal(:,1)),1), ...
    repmat("S011",numel(subjInfo_remembered(7).activityVal(:,1)),1),repmat("S013",numel(subjInfo_remembered(8).activityVal(:,1)),1), ...
    repmat("S014",numel(subjInfo_remembered(9).activityVal(:,1)),1),repmat("S015",numel(subjInfo_remembered(10).activityVal(:,1)),1), ...
    repmat("S016",numel(subjInfo_remembered(11).activityVal(:,1)),1),repmat("S018",numel(subjInfo_remembered(12).activityVal(:,1)),1), ...
    repmat("S019",numel(subjInfo_remembered(13).activityVal(:,1)),1),repmat("S021",numel(subjInfo_remembered(14).activityVal(:,1)),1), ...
    repmat("S022",numel(subjInfo_remembered(15).activityVal(:,1)),1),repmat("S023",numel(subjInfo_remembered(16).activityVal(:,1)),1), ...
    repmat("S024",numel(subjInfo_remembered(17).activityVal(:,1)),1),repmat("S025",numel(subjInfo_remembered(18).activityVal(:,1)),1), ...
    repmat("S026",numel(subjInfo_remembered(19).activityVal(:,1)),1));

ID_col = vertcat(subjInfo_remembered.IDs);

clear lex_index
clear vis_index
clear combined_index
clear both_index
lex_index = find(oneROI_lexMem==999); % 690
vis_index = find(oneROI_visMem==999); % 56
combined_index = vertcat(lex_index,vis_index); % 746
both_index = unique(combined_index); % 728

allActivityMat = allActivityMat_remembered;
    
% cut down so you have the same length as mem for which we have non-NaN values
allActivityMat(both_index,:) = [];
oneROI_lexMem(both_index) = [];
oneROI_visMem(both_index) = []; 
subjCol(both_index) = [];
ID_col(both_index) = [];
    
allActivityMat_memShort = allActivityMat;
oneROI_lexMem_short = oneROI_lexMem;
oneROI_visMem_short = oneROI_visMem;
subjCol_short = subjCol;
ID_col_short = ID_col;

% at this point the mem values should all be cleaned
% therefore, I can now make bothMem from the short versions of vis and lex

%normLex = oneROI_lexMem_short - repmat(mean(oneROI_lexMem_short),length(oneROI_lexMem_short),1);
%normVis = oneROI_visMem_short - repmat(mean(oneROI_visMem_short),length(oneROI_visMem_short),1);

%bothMem = normVis .* normLex;
    
%sz = [4178 5]; 
sz = [length(subjCol) 4];
varTypes = ["string","string","double","double"];
varNames = ["Subj","ItemID","lexMem","visMem"];
allButROI_memShort = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);

allButROI_memShort.Subj = subjCol_short;
allButROI_memShort.ItemID = ID_col_short;
allButROI_memShort.lexMem = oneROI_lexMem_short;
allButROI_memShort.visMem = oneROI_visMem_short;
%allButROI_memShort.bothMem = bothMem;

save('allActivityMat_memShort_HR_remembered_jan23.mat','allActivityMat_memShort')
save('allButROI_memShort_HR_remembered_jan23.mat','allButROI_memShort')

allActivityMat = allActivityMat_memShort;
allButROI_custom_F = allButROI_memShort;

AvgROI = allActivityMat(:,2);
mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,3);
mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,20);
mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,21);
mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,23);
mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,24);
mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,25);
mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,27);
mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,28);
mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,29);
mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,32);
mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,33);
mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,44);
mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,45);
mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,53);
mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,54);
mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,62);
mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,63);
mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,68);
mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,69);
mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,71);
mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,72);
mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,101);
mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,102);
mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');

currSheet = 'avgActivity_BNA_mem_remembered_jan23.xlsx';

%%%% heads up this takes a while
writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R') 
writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 


%%%%%%%%%% forgotten

numMasks = 102; %BNA. There are 104 cols but first two are subject and item
for ROI = 1:numMasks
    activityCol = vertcat(subjInfo_forgotten(1).activityVal(:,ROI),subjInfo_forgotten(2).activityVal(:,ROI), ...
        subjInfo_forgotten(3).activityVal(:,ROI),subjInfo_forgotten(4).activityVal(:,ROI),subjInfo_forgotten(5).activityVal(:,ROI), ...
        subjInfo_forgotten(6).activityVal(:,ROI),subjInfo_forgotten(7).activityVal(:,ROI),subjInfo_forgotten(8).activityVal(:,ROI), ...
        subjInfo_forgotten(9).activityVal(:,ROI),subjInfo_forgotten(10).activityVal(:,ROI),subjInfo_forgotten(11).activityVal(:,ROI), ...
        subjInfo_forgotten(12).activityVal(:,ROI),subjInfo_forgotten(13).activityVal(:,ROI),subjInfo_forgotten(14).activityVal(:,ROI), ...
        subjInfo_forgotten(15).activityVal(:,ROI),subjInfo_forgotten(16).activityVal(:,ROI),subjInfo_forgotten(17).activityVal(:,ROI), ...
        subjInfo_forgotten(18).activityVal(:,ROI),subjInfo_forgotten(19).activityVal(:,ROI));
    allActivityMat_forgotten(:,ROI) = activityCol;
end

clear oneROI_lexMem
clear oneROI_visMem
oneROI_lexMem = vertcat(subjInfo_forgotten.lexMem);
oneROI_visMem = vertcat(subjInfo_forgotten.visMem);

subjCol = vertcat(repmat("S002",numel(subjInfo_forgotten(1).lexMem(:,1)),1),repmat("S005",numel(subjInfo_forgotten(2).lexMem(:,1)),1), ...
    repmat("S006",numel(subjInfo_forgotten(3).lexMem(:,1)),1),repmat("S008",numel(subjInfo_forgotten(4).lexMem(:,1)),1), ...
    repmat("S009",numel(subjInfo_forgotten(5).lexMem(:,1)),1),repmat("S010",numel(subjInfo_forgotten(6).lexMem(:,1)),1), ...
    repmat("S011",numel(subjInfo_forgotten(7).lexMem(:,1)),1),repmat("S013",numel(subjInfo_forgotten(8).lexMem(:,1)),1), ...
    repmat("S014",numel(subjInfo_forgotten(9).lexMem(:,1)),1),repmat("S015",numel(subjInfo_forgotten(10).lexMem(:,1)),1), ...
    repmat("S016",numel(subjInfo_forgotten(11).lexMem(:,1)),1),repmat("S018",numel(subjInfo_forgotten(12).lexMem(:,1)),1), ...
    repmat("S019",numel(subjInfo_forgotten(13).lexMem(:,1)),1),repmat("S021",numel(subjInfo_forgotten(14).lexMem(:,1)),1), ...
    repmat("S022",numel(subjInfo_forgotten(15).lexMem(:,1)),1),repmat("S023",numel(subjInfo_forgotten(16).lexMem(:,1)),1), ...
    repmat("S024",numel(subjInfo_forgotten(17).lexMem(:,1)),1),repmat("S025",numel(subjInfo_forgotten(18).lexMem(:,1)),1), ...
    repmat("S026",numel(subjInfo_forgotten(19).lexMem(:,1)),1));

ID_col = vertcat(subjInfo_forgotten.IDs);
    
allActivityMat = allActivityMat_forgotten;

clear lex_index
clear vis_index
clear combined_index
clear both_index
lex_index = find(oneROI_lexMem==999); % 1030 of these
vis_index = find(oneROI_visMem==999); % 110
combined_index = vertcat(lex_index,vis_index); % 1140
both_index = unique(combined_index); % 1047
    
% cut down so you have the same length as mem for which we have non-NaN values
allActivityMat(both_index,:) = [];
oneROI_lexMem(both_index) = [];
oneROI_visMem(both_index) = [];
subjCol(both_index) = [];
ID_col(both_index) = [];
    
allActivityMat_memShort = allActivityMat;
oneROI_lexMem_short = oneROI_lexMem;
oneROI_visMem_short = oneROI_visMem;
subjCol_short = subjCol;
ID_col_short = ID_col;

% at this point the mem values should all be cleaned
% therefore, I can now make bothMem from the short versions of vis and lex

% normLex = oneROI_lexMem_short - repmat(mean(oneROI_lexMem_short),length(oneROI_lexMem_short),1);
% normVis = oneROI_visMem_short - repmat(mean(oneROI_visMem_short),length(oneROI_visMem_short),1);
% 
% bothMem = normVis .* normLex;
    
%sz = [4178 5]; 
sz = [length(subjCol) 4];
varTypes = ["string","string","double","double"];
varNames = ["Subj","ItemID","lexMem","visMem"];
allButROI_memShort = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);

allButROI_memShort.Subj = subjCol_short;
allButROI_memShort.ItemID = ID_col_short;
allButROI_memShort.lexMem = oneROI_lexMem_short;
allButROI_memShort.visMem = oneROI_visMem_short;
%allButROI_memShort.bothMem = bothMem;

save('allActivityMat_memShort_HR_forgotten_jan23.mat','allActivityMat_memShort')
save('allButROI_memShort_HR_forgotten_jan23.mat','allButROI_memShort')

allActivityMat = allActivityMat_memShort;
allButROI_custom_F = allButROI_memShort;

AvgROI = allActivityMat(:,2);
mask_AG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,3);
mask_AG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,20);
mask_FuG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,21);
mask_FuG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,23);
mask_Hipp_A = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,24);
mask_Hipp_A_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,25);
mask_Hipp_A_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,27);
mask_Hipp_P = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,28);
mask_Hipp_P_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,29);
mask_Hipp_P_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,32);
mask_IFG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,33);
mask_IFG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,44);
mask_LOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,45);
mask_LOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,53);
mask_MVOC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,54);
mask_MVOC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,62);
mask_PHC_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,63);
mask_PHC_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,68);
mask_Perirhinal_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,69);
mask_Perirhinal_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,71);
mask_PhG_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,72);
mask_PhG_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,101);
mask_pSTS_L = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
AvgROI = allActivityMat(:,102);
mask_pSTS_R = addvars(allButROI_custom_F,AvgROI,'Before','Subj');

currSheet = 'avgActivity_BNA_mem_forgotten_jan23.xlsx';

%%%% heads up this takes a while
writetable(mask_AG_L,currSheet,'Sheet','mask_AG_L') 
writetable(mask_AG_R,currSheet,'Sheet','mask_AG_R') 
writetable(mask_FuG_L,currSheet,'Sheet','mask_FuG_L') 
writetable(mask_FuG_R,currSheet,'Sheet','mask_FuG_R')
writetable(mask_Hipp_A,currSheet,'Sheet','mask_Hipp_A') 
writetable(mask_Hipp_P,currSheet,'Sheet','mask_Hipp_P') 
writetable(mask_Hipp_A_L,currSheet,'Sheet','mask_Hipp_A_L') 
writetable(mask_Hipp_A_R,currSheet,'Sheet','mask_Hipp_A_R') 
writetable(mask_Hipp_P_L,currSheet,'Sheet','mask_Hipp_P_L') 
writetable(mask_Hipp_P_R,currSheet,'Sheet','mask_Hipp_P_R') 
writetable(mask_IFG_L,currSheet,'Sheet','mask_IFG_L') 
writetable(mask_IFG_R,currSheet,'Sheet','mask_IFG_R') 
writetable(mask_LOC_L,currSheet,'Sheet','mask_LOC_L') 
writetable(mask_LOC_R,currSheet,'Sheet','mask_LOC_R') 
writetable(mask_MVOC_L,currSheet,'Sheet','mask_MVOC_L') 
writetable(mask_MVOC_R,currSheet,'Sheet','mask_MVOC_R') 
writetable(mask_PHC_L,currSheet,'Sheet','mask_PHC_L') 
writetable(mask_PHC_R,currSheet,'Sheet','mask_PHC_R') 
writetable(mask_Perirhinal_L,currSheet,'Sheet','mask_Perirhinal_L') 
writetable(mask_Perirhinal_R,currSheet,'Sheet','mask_Perirhinal_R') 
writetable(mask_PhG_L,currSheet,'Sheet','mask_PhG_L') 
writetable(mask_PhG_R,currSheet,'Sheet','mask_PhG_R') 
writetable(mask_pSTS_L,currSheet,'Sheet','mask_pSTS_L') 
writetable(mask_pSTS_R,currSheet,'Sheet','mask_pSTS_R') 



% %%% old masks from before BNA masks
% AvgROI = allActivityMat(:,1);
% mask28_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,2);
% mask29_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,3);
% mask30_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,4);
% mask31_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,5);
% mask32_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% % AvgROI = allActivityMat(:,6);
% % mask33_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,6);
% mask34_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,7);
% mask35_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% % AvgROI = allActivityMat(:,9);
% % mask36_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,8);
% mask37_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% % AvgROI = allActivityMat(:,11);
% % mask38_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,9);
% mask39_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,10);
% mask40_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,11);
% mask41_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% % atlasMasks
% AvgROI = allActivityMat(:,12);
% L_HC_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,13);
% R_HC_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,14);
% L_PhG_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,15);
% R_PhG_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,16);
% L_Fus_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat(:,17);
% R_Fus_F = addvars(allButROI_custom_F,AvgROI,'Before','Subj');
% 
% 
% %%%%% don't copy and paste!!!!
% %%%% don't copy and paste the tables into excel, and don't even copy and
% %%%% paste the template. Matlab will make you a spreadsheet and add sheet
% %%%% labels
% 
% 
% if numFactors == 25 %encycl
%     currSheet = 'avgActivity_customROIs_F_fac_encycl.xlsx';
% 
% elseif numFactors == 15 %tax
%     currSheet = 'avgActivity_customROIs_F_fac_tax.xlsx';
% 
% elseif numFactors == 20 %vis
%     currSheet = 'avgActivity_customROIs_F_fac_vis.xlsx';
% 
% elseif numFactors == 10 %fcn
%     currSheet = 'avgActivity_customROIs_F_fac_fcn.xlsx';
% 
% end
% 
% 
% %%%% heads up this takes a while
% writetable(mask28_F,currSheet,'Sheet','mask28') 
% writetable(mask29_F,currSheet,'Sheet','mask29') 
% writetable(mask30_F,currSheet,'Sheet','mask30') 
% writetable(mask31_F,currSheet,'Sheet','mask31') 
% writetable(mask32_F,currSheet,'Sheet','mask32') 
% writetable(mask34_F,currSheet,'Sheet','mask34') 
% writetable(mask34_F,currSheet,'Sheet','mask34') 
% writetable(mask35_F,currSheet,'Sheet','mask35') 
% writetable(mask37_F,currSheet,'Sheet','mask37') 
% writetable(mask39_F,currSheet,'Sheet','mask39')
% writetable(mask40_F,currSheet,'Sheet','mask40') 
% writetable(mask41_F,currSheet,'Sheet','mask41')
% writetable(L_HC_F,currSheet,'Sheet','L_HC') 
% writetable(R_HC_F,currSheet,'Sheet','R_HC') 
% writetable(L_PhG_F,currSheet,'Sheet','L_PhG') 
% writetable(R_PhG_F,currSheet,'Sheet','R_PhG')
% writetable(L_Fus_F,currSheet,'Sheet','L_Fus') 
% writetable(R_Fus_F,currSheet,'Sheet','R_Fus') 

%%% old way was to display the variables in matlab and copy and paste one
%%% table at a time. Why did I ever let myself do this? I guess because I
%%% believed that I'd only need to do this once or twice AND I was
%%% motivated to get to the answer and decided I didn't mind putting in the
%%% time and work. But god DAMN does investing in automation pay off. You
%%% can't just comment/uncomment and scroll through a script to manually
%%% change things. Yes, maybe it takes the same amount of time or is even
%%% FASTER if you only use the script once, the trade-off switches the
%%% other way if you use it a few more times, and after a while you're just
%%% screwing your future self. Plus your memory of where to make changes in
%%% a long script does NOT last. 

% copy and paste into avgActivity_customROIs_PC_F.xlsx / ...F500.xlsx
% avgActivity_CleanCustomROIs_PC_F500
% avgActivity_customROIs_F_fac_encycl.xlsx %or tax, vis, fcn

% %%% Mem
% 
% % customMasks
% AvgROI = allActivityMat_memShort(:,1);
% mask28_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,2);
% mask29_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,3);
% mask30_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,4);
% mask31_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,5);
% mask32_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,6);
% mask34_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,7);
% mask35_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,8);
% mask37_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,9);
% mask39_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,10);
% mask40_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,11);
% mask41_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% % atlasMasks
% AvgROI = allActivityMat_memShort(:,12);
% L_HC_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,13);
% R_HC_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,14);
% L_PhG_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,15);
% R_PhG_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,16);
% L_Fus_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');
% 
% AvgROI = allActivityMat_memShort(:,17);
% R_Fus_mem = addvars(allButROI_memShort,AvgROI,'Before','Subj');


% if I want activity and mem only

% activity_mem_only = [AvgROI table2array(allButROI_memShort)];
% 
% save('activity_mem_only.mat','activity_mem_only')

%%%%%% OPTION 1 %%%%%%%
%% re-do where all activity cut to same length

% these indices are for the subject with the shortest activity array,
% meaning we had to cut the most out because they made the most covert
% naming errors. We have to cut the arrays down to the length of the
% shortest so we can average them
% 
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/STAMP_scripts;
% load indices_TT_S021
% % numFactors = 40;
% for subjects = 1:length(subjectNum)
% 
%     % Step 1: Grab the subject-specific rows 
%     subjectSpecificData = maskArray_ID_data(subjectCounter:subjectCounter+299,:);
% 
%     % Step 2: Cut PCs down to the 300 used in Encoding
% 
% 
%     % **** instead of doing all of this, could get ID and TT num from the clustermeans table
%     % currently activityPerTrialPerCustomCluster has ID but not TT. 
%  
%     all_betas_enc = dir(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Encoding_renamed/S',subjectNum{subjects},'/betas/*.nii'));
%     IDs_enc = cell(numel(all_betas_enc),1);
%     indices_TT = zeros(numel(all_betas_enc),1); % 1 for TT3 and 0 for anything else
%     
%         % get ID numbers used for the encoding trials
%     for row = 1:numel(all_betas_enc)
%     
%         % get the item number. Could also get from custommasks table
%         IDs_enc{row} = extractBetween(all_betas_enc(row).name,'Item', '_Enc');
%     
%         % is it TT3 (and should stay) or is it TT4, etc. 
%         % not TT3 means there was a mismatch in covert naming (false alarm)
%         % use indices_TT_S021, the shortest
%     end
% 
%     % now I have the ID numbers that are used for the encoding trials of this
%     % subject
%     % I have to find the PC val etc that is on the row where that ID number is in
%     % itemIDs_tbl. So, I want the row number
% 
%     indices_inOrder = zeros(numel(all_betas_enc),1);
%     for idNumber = 1:numel(all_betas_enc)
%         % IDs_enc is cell array of cells, so I have to peel them out
%         % then convert the char you get out to a num to match the table items
%         index = find(itemIDs_tbl{:,2}==str2num(cell2mat(IDs_enc{idNumber}))); %find the four-digit ID num in the second col of the table
%         indices_inOrder(idNumber) = index;
%     end
% 
%     score_subset = zeros(numel(all_betas_enc),994); 
%     factor_subset = zeros(numel(all_betas_enc),numFactors); 
% 
%     for row = 1:numel(all_betas_enc)
%         score_subset(row,:) = score(indices_inOrder(row),:); % the entire row which is all the PCs
%         factor_subset(row,:) = F(indices_inOrder(row),:); % the entire row which is all the factors
%     end
% 
%     % Step 3: Remove any trial that isn't labelled with TT3
%     % in the previous step I made indices_TT. If there's a 1 in the row, keep it
% 
%     subject_numbers = customMasksArray(:,1);
%     
%     for row = 1:numel(indices_TT_S021)
%         if indices_TT_S021(row) == 1
%             score_subset(row,:) = score_subset(row,:);
%             factor_subset(row,:) = factor_subset(row,:);
%             subjectSpecificData(row,:) = subjectSpecificData(row,:);
%             subject_numbers(row) = subject_numbers(row);
%         else 
%             score_subset(row,:) = NaN;
%             factor_subset(row,:) = NaN;
%             subjectSpecificData(row,:) = NaN;
%             subject_numbers(row) = NaN;
%         end 
%     end
% 
%     % remove the rows with NaN
%     score_subset(any(isnan(score_subset), 2), :) = [];
%     factor_subset(any(isnan(factor_subset), 2), :) = [];
%     subjectSpecificData(any(isnan(subjectSpecificData), 2), :) = [];
%     subject_numbers(any(isnan(subject_numbers), 2), :) = [];
% 
%     % store the PCs, factors, and activity
%     subjInfo(subjects).PCval = score_subset;
%     subjInfo(subjects).Fval = factor_subset;
%     subjInfo(subjects).IDs = subjectSpecificData(:,1);
%     subjInfo(subjects).subjNum = subject_numbers;
%     subjInfo(subjects).activityVal = subjectSpecificData(:,2:end);
% 
%     subjectCounter = subjectCounter + 300; % go to next subject's first row
% end
% 
% subjInfo_short = subjInfo;
% 
% save('subjInfo_short.mat','subjInfo_short')
% 
% %save('subjInfo_short_m40.mat','subjInfo_short')
% 
% % now I want to average them to make one 249x14 mat that's averaged
% % activity for the 14 custom ROIs
% 
% sumMat = subjInfo_short(1).activityVal+subjInfo_short(2).activityVal+subjInfo_short(3).activityVal+...
%     subjInfo_short(4).activityVal+subjInfo_short(5).activityVal+subjInfo_short(6).activityVal+...
%     subjInfo_short(7).activityVal+subjInfo_short(8).activityVal+subjInfo_short(9).activityVal+...
%     subjInfo_short(10).activityVal+subjInfo_short(11).activityVal+subjInfo_short(12).activityVal+...
%     subjInfo_short(13).activityVal+subjInfo_short(14).activityVal+subjInfo_short(15).activityVal+...
%     subjInfo_short(16).activityVal+subjInfo_short(17).activityVal+subjInfo_short(18).activityVal+...
%     subjInfo_short(19).activityVal;
% 
% activityMean_allSubj = sumMat ./ 19;
% 
% save('activityMean_allSubj.mat','activityMean_allSubj')
% 
% %%%%%%%%%%%%%%%%%%%%
% %%%%%% NOTE
% %%%% it's not a good idea to average activity. Just keep it
% %%%% subject-specific and do lmer() in R
% 
% 
% 
% 
% %% need averages to make plots for poster.
% %%% I want two scatter plots with regression line. One for mem and F and
% %%% one for mem and activity. Both need to be item-level.
% 
% % load these for all factor groups. They don't change
% load allButROI_memShort
% load allActivityMat_memShort.mat
% 
% %%% choose one at a time
% load allButROI_encycl.mat
% allButROI = allButROI_encycl;
% 
% load allButROI_noTax.mat
% allButROI = allButROI_noTax;
% 
% load allButROI_vis.mat
% allButROI = allButROI_vis;
% 
% load allButROI_fcn.mat
% allButROI = allButROI_fcn;
% 
% load allButROI_tax.mat
% allButROI = allButROI_tax;
% 
% %%%%%%%% grab from here
% % whichever I choose, it gets the same name, and then sort
% F_sorted = sortrows(allButROI,{'ItemID'},{'ascend'});
% 
% % now that the rows are sorted by item, average the rows that go with each
% % item. Unfortunately not all subjects saw all items, so I can't just do
% % every 19 rows 
% 
% 
% % find the positions of where the ItemID changes
% itemID_col = F_sorted{:,2};
% changePositions = zeros(length(itemID_col),1); % don't think you can append in matlab, so I'll just do 1s and 0s
% for row = 1:length(itemID_col)-1
% 
%     if itemID_col(row) ~= itemID_col(row+1) % if they're different
%         changePositions(row) = 1;
%     else
%         changePositions(row) = 0;
%     end
% end
% 
% % changePositions gives the LAST row of the group
% row_indices = find(changePositions == 1); % there are 299
% F_means = zeros(length(row_indices),10);
% F_itemIDs = strings(length(row_indices),1);
% 
% for group = 1:length(row_indices)
% 
%     if group == 1
%         firstRowOfGroup = 1;
%     else
%         firstRowOfGroup = 1 + (row_indices(group-1)); 
%     end
%     lastRowOfGroup = row_indices(group);
%     currMean = mean(F_sorted{firstRowOfGroup:lastRowOfGroup,3:12});
%     % 3 to 12 because first two cols are subj and ItemID
%     F_means(group,:) = currMean;
% 
%     % grab the ItemID number
%     F_itemIDs(group) = F_sorted{firstRowOfGroup,2}; % itemID col
% 
% end
% 
% %%% now I have a ref table of the ItemID and the ten F values that go with it
% 
% 
% 
%%%%% scatter of mem x F



load allActivityMat_memShort_jan23.mat
allActivityMat_all = allActivityMat_memShort;
load allActivityMat_memShort_remembered_jan23.mat
allActivityMat_remembered = allActivityMat_memShort;
load allActivityMat_memShort_forgotten_jan23.mat
allActivityMat_forgotten = allActivityMat_memShort;


load allButROI_memShort_jan23.mat
allButROI_all = allButROI_memShort;
load allButROI_memShort_remembered_jan23.mat
allButROI_remembered = allButROI_memShort;
load allButROI_memShort_forgotten_jan23.mat
allButROI_forgotten = allButROI_memShort;


% masks I need are AG_L, AG_R, Hipp_P_R, IFG_R, MVOC_L, PHC_R, pSTS_L
% col numbers are 1, 2, 27, 32, 52, 68, 79

activity_memROIs = horzcat(allActivityMat_all(:,1),allActivityMat_all(:,2),allActivityMat_all(:,27),...
    allActivityMat_all(:,32),allActivityMat_all(:,52),allActivityMat_all(:,68),allActivityMat_all(:,79));
activity_remembered_memROIs = horzcat(allActivityMat_remembered(:,1),allActivityMat_remembered(:,2),allActivityMat_remembered(:,27),...
    allActivityMat_remembered(:,32),allActivityMat_remembered(:,52),allActivityMat_remembered(:,68),allActivityMat_remembered(:,79));
activity_forgotten_memROIs = horzcat(allActivityMat_forgotten(:,1),allActivityMat_forgotten(:,2),allActivityMat_forgotten(:,27),...
    allActivityMat_forgotten(:,32),allActivityMat_forgotten(:,52),allActivityMat_forgotten(:,68),allActivityMat_forgotten(:,79));

activityTbl = array2table(activity_memROIs,'VariableNames',{'AG_L','AG_R',...
    'Hipp_P_R','IFG_R','MVOC_L','PHC_R','pSTS_L'});
activityTbl_remembered = array2table(activity_remembered_memROIs,'VariableNames',{'AG_L','AG_R',...
    'Hipp_P_R','IFG_R','MVOC_L','PHC_R','pSTS_L'});
activityTbl_forgotten = array2table(activity_forgotten_memROIs,'VariableNames',{'AG_L','AG_R',...
    'Hipp_P_R','IFG_R','MVOC_L','PHC_R','pSTS_L'});


%need to do the same sorting and averaging to allActivityMat_memShort
% remember, allActivityMat_memShort is just numbers, so I have to supply
% the col names
% activityTbl = array2table(allActivityMat_memShort,'VariableNames',...
%     {'AG','AG_L','AG_R','ATL','ATL_L','ATL_R','CG','CG_L','CG_R','FFA',...
%     'FFA_L','FFA_R','FuG','FuG_L','Fug_R','Hipp','Hipp_A','Hipp_A_L','Hipp_A_R',...
%     'Hipp_L','Hipp_P','Hipp_P_L','Hipp_P_R','Hipp_R','IFG','IFG_L','IFG_R',...
%     'INS','INS_L','INS_R','IPL','IPL_L','IPL_R','ITG','ITG_L','ITG_R','LOC',...
%     'LOC_L','LOC_R','MFG','MFG_L','MFG_R','MTG','MTG_L','MTG_R','MVOC',...
%     'MVOC_L','MVOC_R','OrG','OrG_L','OrG_R','PCL','PCL_L','PCL_R','PHC',...
%     'PHC_L','PHC_R','Pcun','Pcun_L','Pcun_R','Perirhinal','Perirhinal_L',...
%     'Perirhinal_R','PhG','PhG_L','PhG_R','PoG','PoG_L','PoG_R','PrG','PrG_L',...
%     'PrG_R','RSC','RSC_L','RSC_R','Rhinal','Rhinal_L','Rhinal_R','SFG',...
%     'SFG_L','SFG_R','SMG','SMG_L','SMG_R','SPL','SPL_L','SPL_R','STG',...
%     'STG_L','STG_R','pSTS','pSTS_L','pSTS_R'});


%ask which IDs there are mem values for and then get the F values
mem_sorted_all = sortrows(allButROI_all,{'ItemID'},{'ascend'});
mem_sorted_remembered = sortrows(allButROI_remembered,{'ItemID'},{'ascend'});
mem_sorted_forgotten = sortrows(allButROI_forgotten,{'ItemID'},{'ascend'});

%sort activity too
IDcol = allButROI_all{:,2};
memActivityTbl = addvars(activityTbl,IDcol,'After','pSTS_L'); 
memActivity_sorted = sortrows(memActivityTbl,{'IDcol'},{'ascend'});

IDcol = allButROI_remembered{:,2};
memActivityTbl = addvars(activityTbl_remembered,IDcol,'After','pSTS_L'); 
memActivity_sorted_remembered = sortrows(memActivityTbl,{'IDcol'},{'ascend'});

IDcol = allButROI_forgotten{:,2};
memActivityTbl = addvars(activityTbl_forgotten,IDcol,'After','pSTS_L'); 
memActivity_sorted_forgotten = sortrows(memActivityTbl,{'IDcol'},{'ascend'});



%thankfully the subj-wise mem values are just repeated, so I only need one
%still, I don't know which are missing, so I better find the ItemID
%changes, etc, just like I did for F


numMasks = size(activity_memROIs,2);

for trial_type = 1:3
    if trial_type == 1
        memActivity_sorted = memActivity_sorted; %all trials
        mem_sorted = mem_sorted_all;
    elseif trial_type == 2
        memActivity_sorted = memActivity_sorted_remembered;
        mem_sorted = mem_sorted_remembered;
    elseif trial_type == 3
        memActivity_sorted = memActivity_sorted_forgotten;
        mem_sorted = mem_sorted_forgotten;
    end

    %find the positions of where the ItemID changes
    itemID_col = mem_sorted{:,2}; %where is the ID col?
    changePositions = zeros(length(itemID_col),1); % don't think you can append in matlab, so I'll just do 1s and 0s
    for row = 1:length(itemID_col)-1
    
        if itemID_col(row) ~= itemID_col(row+1) % if they're different
            changePositions(row) = 1;
        else
            changePositions(row) = 0;
        end
    end
    %changePositions gives the LAST row of the group
    
    row_indices = find(changePositions == 1); 
    mem_means = zeros(length(row_indices),2);
    activity_means = zeros(length(row_indices),numMasks); %
    mem_itemIDs = strings(length(row_indices),1);
    
    for group = 1:length(row_indices)
        if group == 1
            firstRowOfGroup = 1;
        else
            firstRowOfGroup = 1 + (row_indices(group-1)); 
        end
        lastRowOfGroup = row_indices(group);
        currMean = mean(mem_sorted{firstRowOfGroup:lastRowOfGroup,3:4}); % 3 and 4 are MTurk lexMem and visMem                       
        currMean_activity = mean(memActivity_sorted{firstRowOfGroup:lastRowOfGroup,1:numMasks}); % 17 ROIs
        mem_means(group,:) = currMean;
        activity_means(group,:) = currMean_activity;
    
        %grab the ItemID number
        mem_itemIDs(group) = mem_sorted{firstRowOfGroup,2}; % itemID col
    end

    if trial_type == 1
        mem_means_all = mem_means;
        mem_itemIDs_all = mem_itemIDs;
        activity_means_all = activity_means;
    elseif trial_type == 2
        mem_means_remembered = mem_means;
        mem_itemIDs_remembered = mem_itemIDs;
        activity_means_remembered = activity_means;
    elseif trial_type == 3
        mem_means_forgotten = mem_means;
        mem_itemIDs_forgotten = mem_itemIDs;
        activity_means_forgotten = activity_means;
    end
end

%%% now I have a ref table of the ItemID and the two mem values that go with it
%%% Also, mem is 240 long and F is 299 long

%% mem x F [skip down for mem x BOLD]

%Grab the mem values and matching F value
F_short = zeros(length(mem_means),10);
for row = 1:length(mem_means)
    currID = mem_itemIDs(row);
    position = find(F_itemIDs == currID);
    F_short(row,:) = F_means(position,:);
end

%F_short cols 1 through 10 for the 10 factors
%mem_means cols 1 and 2 for lexMem and visMem (MTurk)


lexMemAll = mem_means(:,1);
visMemAll = mem_means(:,2);
f01_all = F_short(:,1);
f02_all = F_short(:,2);
f03_all = F_short(:,3);
f04_all = F_short(:,4);
f05_all = F_short(:,5);
f06_all = F_short(:,6);
f07_all = F_short(:,7);
f08_all = F_short(:,8);
f09_all = F_short(:,9);
f10_all = F_short(:,10);


tbl_mem_F = table(lexMemAll,visMemAll,f01_all,f02_all,f03_all,f04_all,...
    f05_all,f06_all,f07_all,f08_all,f09_all,f10_all);

%%%%%%%% grab down to here

%use this to get r values only. Skip down for plots
%% do one at a time
%lexMem
x = tbl_mem_F{:,1};

for f_val = 1:10
    y = tbl_mem_F{:,f_val+2};
    %pearson's coefficient
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

%visMem
x = tbl_mem_F{:,2};

for f_val = 1:10
    y = tbl_mem_F{:,f_val+2};
    %pearson's coefficient
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

%lexMem and F01 is .22, F02 is .18, F04 is .14
%visMem and F01 is .337, F02 is .212, F04 is .258

%now plot the ones that look best
set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white

mdl1 = fitlm(tbl_mem_F,'f09_all ~ lexMemAll');
plot(mdl1)
xlabel('Conceptual Memorability')
ylabel('Factor 9 Scores')
title('Conceptual Memorability and Factor 9 Scores')
txt = 'r = -.11';
text(-0.1, .6,txt,'FontSize',14)

figure

mdl2 = fitlm(tbl_mem_F,'f09_all ~ visMemAll');
plot(mdl2)
xlabel('Perceptual Memorability')
ylabel('Factor 9 Scores')
title('Perceptual Memorability and Factor 9 Scores')
txt = 'r = -.32';
text(-0.2, -1,txt,'FontSize',14)


%% do the same for mem x BOLD
lexMem_all = mem_means(:,1);
visMem_all = mem_means(:,2);
AG_L_all = activity_means(:,1);
AG_R_all = activity_means(:,2);
Hipp_P_R_all = activity_means(:,3);
IFG_R_all = activity_means(:,4);
MVOC_L_all = activity_means(:,5);
PHC_R_all = activity_means(:,6);
pSTS_L_all = activity_means(:,7);

tbl_mem_activity_all = table(lexMem_all,visMem_all,AG_L_all,AG_R_all,Hipp_P_R_all,...
    IFG_R_all,MVOC_L_all,PHC_R_all,pSTS_L_all);

lexMem_remembered = mem_means_remembered(:,1);
visMem_remembered = mem_means_remembered(:,2);
AG_L_remembered = activity_means_remembered(:,1);
AG_R_remembered = activity_means_remembered(:,2);
Hipp_P_R_remembered = activity_means_remembered(:,3);
IFG_R_remembered = activity_means_remembered(:,4);
MVOC_L_remembered = activity_means_remembered(:,5);
PHC_R_remembered = activity_means_remembered(:,6);
pSTS_L_remembered = activity_means_remembered(:,7);

tbl_mem_activity_remembered = table(lexMem_remembered,visMem_remembered,AG_L_remembered,AG_R_remembered,Hipp_P_R_remembered,...
    IFG_R_remembered,MVOC_L_remembered,PHC_R_remembered,pSTS_L_remembered);

lexMem_forgotten = mem_means_forgotten(:,1);
visMem_forgotten = mem_means_forgotten(:,2);
AG_L_forgotten = activity_means_forgotten(:,1);
AG_R_forgotten = activity_means_forgotten(:,2);
Hipp_P_R_forgotten = activity_means_forgotten(:,3);
IFG_R_forgotten = activity_means_forgotten(:,4);
MVOC_L_forgotten = activity_means_forgotten(:,5);
PHC_R_forgotten = activity_means_forgotten(:,6);
pSTS_L_forgotten = activity_means_forgotten(:,7);

tbl_mem_activity_forgotten = table(lexMem_forgotten,visMem_forgotten,AG_L_forgotten,AG_R_forgotten,Hipp_P_R_forgotten,...
    IFG_R_forgotten,MVOC_L_forgotten,PHC_R_forgotten,pSTS_L_forgotten);


% lexMemAll = mem_means(:,1);
% visMemAll = mem_means(:,2);
% mask28All = activity_means(:,1);
% mask29All = activity_means(:,2);
% mask30All = activity_means(:,3);
% mask31All = activity_means(:,4);
% mask32All = activity_means(:,5);
% mask34All = activity_means(:,6);
% mask35All = activity_means(:,7);
% mask37All = activity_means(:,8);
% mask39All = activity_means(:,9);
% mask40All = activity_means(:,10);
% mask41All = activity_means(:,11);
% LHCall = activity_means(:,12);
% RHCall = activity_means(:,13);
% LPhGall = activity_means(:,14);
% RPhGall = activity_means(:,15);
% LFusAll = activity_means(:,16);
% RFusAll = activity_means(:,17);
% 
% tbl_mem_activity = table(lexMemAll,visMemAll,mask28All,mask29All,mask30All,...
%     mask31All,mask32All,mask34All,mask35All,mask37All,mask39All,mask40All,...
%     mask41All,LHCall,RHCall,LPhGall,RPhGall,LFusAll,RFusAll);


%%% alternatively, just load my nine-ROI tval doc

tbl_mem_activity = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_mem_tVal_nineROIs.xlsx');





%% do one at a time <- these are from either seven or 102ish ROIs

%lexMem all
for col = 3:9
    x = tbl_mem_activity_all{:,1};
    y = tbl_mem_activity_all{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

% lexMem all: .0188, -.0041, .0672, -.0109, .0494, .07, .0561
% AG_L, AG_R, Hipp_P_R, IFG_R, MVOC_L, PHC_R, pSTS_L

%visMem all
for col = 3:9
    x = tbl_mem_activity_all{:,2};
    y = tbl_mem_activity_all{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end
% visMem all: -.011, -.0315, .0367, -.0493, -.0166, .0576, -.0039

%lexMem remembered
for col = 3:9
    x = tbl_mem_activity_remembered{:,1};
    y = tbl_mem_activity_remembered{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

% lexMem remembered: .1186, .1055, .0326, .0483, .0652, .0108, .0687
% AG_L, AG_R, Hipp_P_R, IFG_R, MVOC_L, PHC_R, pSTS_L

%visMem remembered
for col = 3:9
    x = tbl_mem_activity_remembered{:,2};
    y = tbl_mem_activity_remembered{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end
% visMem remembered: .0257, .0278, -.0281, -.0237, -.0376, -.0192, -.0309

%lexMem forgotten
for col = 3:9
    x = tbl_mem_activity_forgotten{:,1};
    y = tbl_mem_activity_forgotten{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

% lexMem forgotten: .0188, -.0041, .0672, -.0109, .0494, .07, .0561
% AG_L, AG_R, Hipp_P_R, IFG_R, MVOC_L, PHC_R, pSTS_L

%visMem forgotten
for col = 3:9
    x = tbl_mem_activity_forgotten{:,2};
    y = tbl_mem_activity_forgotten{:,col};
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end
% visMem forgotten: -.011, -.0315, .0367, -.0493, -.0166, .0576, -.0039



set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white

mdl1 = fitlm(tbl_mem_activity_remembered,'AG_L_remembered ~ lexMem_remembered');
plot(mdl1)
xlabel('Conceptual Memorability','FontSize',13)
ylabel('BOLD','FontSize',13)
title('Left AG','FontSize',14)
txt = 'r = .12';
text(0.2, -4,txt,'FontSize',14)

figure

mdl2 = fitlm(tbl_mem_activity_remembered,'AG_R_remembered ~ lexMem_remembered');
plot(mdl2)
xlabel('Conceptual Memorability','FontSize',13)
ylabel('BOLD','FontSize',13)
title('Right AG','FontSize',14)
txt = 'r = .11';
text(0.2, -3,txt,'FontSize',14)







%%%%%%%% old, fall 2022

%lexMem
x = tbl_mem_activity{:,1};
%visMem
x = tbl_mem_activity{:,2};

for f_val = 1:17
    y = tbl_mem_activity{:,f_val+2};
    %pearson's coefficient
    C = cov(x,y);
    p = C(2)/(std(x)*std(y));
    disp(p);
end

%lexMem mask30 .15, R PhG .13

%visMem L HC .12, R HC .13
set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white

mdl1 = fitlm(tbl_mem_activity,'RPhGall ~ lexMemAll');
plot(mdl1)
xlabel('Conceptual Memorability','FontSize',13)
ylabel('BOLD','FontSize',13)
title('R Parahippocampal Gyrus','FontSize',14)
txt = 'r = .13';
text(0.4, -1,txt,'FontSize',14)

figure

mdl2 = fitlm(tbl_mem_activity,'RPhGall ~ lexMemAll');
plot(mdl2)
xlabel('Conceptual Memorability','FontSize',13)
ylabel('BOLD','FontSize',13)
title('Supramarginal Gyrus','FontSize',14)
txt = 'r = .15';
text(0.4, -1,txt,'FontSize',14)

figure

mdl3 = fitlm(tbl_mem_activity,'RHCall ~ visMemAll');
plot(mdl3)
xlabel('Perceptual Memorability','FontSize',13)
ylabel('BOLD','FontSize',13)
title('Right Hippocampus','FontSize',14)
txt = 'r = .13';
text(0.4,-1.5,txt,'FontSize',14)







%%%%%%%%%
% plot

lexMemAll = allButROI_memShort{:,3};
visMemAll = allButROI_memShort{:,4};

activity_mask34 = allActivityMat_memShort(:,6);
activity_LPhG = allActivityMat_memShort(:,12);
activity_RPhG = allActivityMat_memShort(:,13);
activity_LFus = allActivityMat_memShort(:,16);
activity_RFus = allActivityMat_memShort(:,17);

tbl = table(MPG,Weight,Year);
mdl = fitlm(tbl,'MPG ~ Year + Weight^2');
plot(mdl)


tbl_mask34 = table(lexMemAll,visMemAll,activity_mask34);
tbl_LPhG = table(lexMemAll,visMemAll,activity_LPhG);
tbl_RPhG = table(lexMemAll,visMemAll,activity_RPhG);
tbl_LFus = table(lexMemAll,visMemAll,activity_LFus);
tbl_RFus = table(lexMemAll,visMemAll,activity_RFus);

mdl = fitlm(tbl_mask34,'activity_mask34 ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_LFus,'activity_LFus ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_RFus,'activity_RFus ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_LPhG,'activity_LPhG ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_RPhG,'activity_RPhG ~ lexMemAll');
plot(mdl)


mean_activityFus = mean([activity_LFus activity_RFus],2);
mean_activityPhG = mean([activity_LPhG activity_RPhG],2);

tbl_meanFus = table(mean_activityFus,lexMemAll,visMemAll);
tbl_meanPhG = table(mean_activityPhG,lexMemAll,visMemAll);

mdl = fitlm(tbl_meanFus,'mean_activityFus ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_meanFus,'mean_activityFus ~ visMemAll');
plot(mdl)

mdl = fitlm(tbl_meanPhG,'mean_activityPhG ~ lexMemAll');
plot(mdl)

mdl = fitlm(tbl_meanPhG,'mean_activityPhG ~ visMemAll');
plot(mdl)