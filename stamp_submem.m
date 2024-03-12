clear all

addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/

subjectNum = {'002' '005' '006' '008' '009' '010' '011' '013' '014' '015' '016' '018' '019' '021' '022' '023' '024' '025' '026'};

output_table = table([], [], [], [], [], [], [],[],[],[],[],[],[], [], [], [], [], [], [], [], 'VariableNames', ...
                            {'Subject', 'ItemID', 'Hit_CRET', 'Miss_CRET', 'FA_CRET', 'CR_CRET', ...
                            'hit_rate_CRET', 'false_alarm_rate_CRET', 'correctedRecog_CRET','d_prime_CRET',...
                            'crit_CRET','Hit_PRET', 'Miss_PRET', 'FA_PRET', 'CR_PRET', 'hit_rate_PRET',...
                            'false_alarm_rate_PRET','correctedRecog_PRET','d_prime_PRET', 'crit_PRET'});
   

for subjects = 1:length(subjectNum) 

    subject = sprintf('S%s',subjectNum{subjects});
    
    %cmem_ret_col = xlsread(strcat('/Users/matthewslayton/Documents/GitHub/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'Z:Z');
    %pmem_ret_col = xlsread(strcat('/Users/matthewslayton/Documents/GitHub/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'AA:AA');
    %IDs = xlsread(strcat('/Users/matthewslayton/Documents/GitHub/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'T:T');
    %old = xlsread(strcat('/Users/matthewslayton/Documents/GitHub/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'H:H');
    
    cmem_ret_col = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'Z:Z');
    pmem_ret_col = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'AA:AA');
    
    IDs = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'T:T');
    old = xlsread(strcat('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Behav/S',subjectNum{subjects},'/newretS', subjectNum{subjects},'_final3.xlsx'),'H:H');
    
    % for newret newCMEM is Z and newPMEM is AA

    oldresp_cmem = nan(length(cmem_ret_col), 1); % Initialize oldresp with NaNs
    newresp_cmem = nan(length(cmem_ret_col), 1); % Initialize newresp with NaNs
    
    for number = 1:length(cmem_ret_col)
        if old(number) == 1
            % Assign 0 or 1 to oldresp based on cmem_ret_col values (1,2 = miss, 3,4 = hit)
            oldresp_cmem(number) = cmem_ret_col(number) == 3 || cmem_ret_col(number) == 4;
        elseif old(number) == 0
            % Assign 0 or 1 to newresp based on cmem_ret_col values (1,2 = CR, 3,4 = FA)
            newresp_cmem(number) = cmem_ret_col(number) == 3 || cmem_ret_col(number) == 4;
        end
    end

    oldresp_pmem = nan(length(pmem_ret_col), 1); % Initialize oldresp for pmem with NaNs
    newresp_pmem = nan(length(pmem_ret_col), 1); % Initialize newresp for pmem with NaNs
    
    for number = 1:length(pmem_ret_col)
        if old(number) == 1
            % Assign 0 or 1 to oldresp based on pmem_ret_col values (1,2 = miss, 3,4 = hit)
            oldresp_pmem(number) = pmem_ret_col(number) == 3 || pmem_ret_col(number) == 4;
        elseif old(number) == 0
            % Assign 0 or 1 to newresp based on pmem_ret_col values (1,2 = CR, 3,4 = FA)
            newresp_pmem(number) = pmem_ret_col(number) == 3 || pmem_ret_col(number) == 4;
        end
    end

	% Count 1s and 0s in oldresp
	num_ones_oldresp = sum(oldresp_cmem == 1, 'omitnan');
	num_zeros_oldresp = sum(oldresp_cmem == 0, 'omitnan');

	% Count 1s and 0s in newresp
	num_ones_newresp = sum(newresp_cmem == 1, 'omitnan');
	num_zeros_newresp = sum(newresp_cmem == 0, 'omitnan');


    % Count 1s and 0s in oldresp
    hits_cmem = sum(oldresp_cmem == 1, 'omitnan');
    misses_cmem = sum(oldresp_cmem == 0, 'omitnan');
    
    % Count 1s and 0s in newresp
    false_alarms_cmem = sum(newresp_cmem == 1, 'omitnan');
    correct_rejections_cmem = sum(newresp_cmem == 0, 'omitnan');
    
    % Adjust for edge cases
    if false_alarms_cmem == 0
        false_alarms_adjusted_cmem = 1;
        hits_adjusted_cmem = hits + 1;
    else
        false_alarms_adjusted_cmem = false_alarms_cmem;
        hits_adjusted_cmem = hits_cmem;
    end

    % Calculate HR and FAR
    hit_rate_CRET = hits_adjusted_cmem / (hits_adjusted_cmem + misses_cmem);
    false_alarm_rate_CRET = false_alarms_adjusted_cmem / (false_alarms_adjusted_cmem + correct_rejections_cmem);
    correctedRecog_CRET = hit_rate_CRET - false_alarm_rate_CRET;
    
    % Calculate d' and c
    d_prime_CRET = norminv(hit_rate_CRET) - norminv(false_alarm_rate_CRET);
    crit_CRET = -0.5 * (norminv(hit_rate_CRET) + norminv(false_alarm_rate_CRET));

    % Count 1s and 0s in oldresp for pmem
    hits_pmem = sum(oldresp_pmem == 1, 'omitnan');
    misses_pmem = sum(oldresp_pmem == 0, 'omitnan');
    
    % Count 1s and 0s in newresp for pmem
    false_alarms_pmem = sum(newresp_pmem == 1, 'omitnan');
    correct_rejections_pmem = sum(newresp_pmem == 0, 'omitnan');
    
    % Adjust for edge cases in pmem
    if false_alarms_pmem == 0
        false_alarms_adjusted_pmem = 1;
        hits_adjusted_pmem = hits_pmem + 1;  % Ensure this is hits_pmem, not hits
    else
        false_alarms_adjusted_pmem = false_alarms_pmem;
        hits_adjusted_pmem = hits_pmem;
    end
    
    % Calculate HR and FAR for pmem
    hit_rate_PRET = hits_adjusted_pmem / (hits_adjusted_pmem + misses_pmem);  % Use adjusted hits for consistency
    false_alarm_rate_PRET = false_alarms_adjusted_pmem / (false_alarms_adjusted_pmem + correct_rejections_pmem);  % Use adjusted FA
    correctedRecog_PRET = hit_rate_PRET - false_alarm_rate_PRET;
    
    % Calculate d' and c for pmem
    d_prime_PRET = norminv(hit_rate_PRET) - norminv(false_alarm_rate_PRET);
    crit_PRET = -0.5 * (norminv(hit_rate_PRET) + norminv(false_alarm_rate_PRET));

    for item = 1:length(IDs)
        % Extract item-specific responses for CRET
        hit_CRET_item = oldresp_cmem(item) == 1;
        miss_CRET_item = oldresp_cmem(item) == 0;
        fa_CRET_item = newresp_cmem(item) == 1;
        cr_CRET_item = newresp_cmem(item) == 0;
        % Extract item-specific responses for PRET
        hit_PRET_item = oldresp_pmem(item) == 1;
        miss_PRET_item = oldresp_pmem(item) == 0;
        fa_PRET_item = newresp_pmem(item) == 1;
        cr_PRET_item = newresp_pmem(item) == 0;

        % SubjectID, d-prime, and crit will be the same for each row
        % as will subject-wise HR, FAR, and correctedRecog
        newRow = {subject, IDs(item), hit_CRET_item, miss_CRET_item, fa_CRET_item, cr_CRET_item,...
            hit_rate_CRET,false_alarm_rate_CRET,correctedRecog_CRET,d_prime_CRET, crit_CRET, ...
            hit_PRET_item, miss_PRET_item, fa_PRET_item, cr_PRET_item, hit_rate_PRET,...
            false_alarm_rate_PRET,correctedRecog_PRET,d_prime_PRET, crit_PRET};
        output_table = [output_table; newRow];
    end   
end 

% Initialize a flag to indicate the QC status
qc_passed = true;

% Loop through each row in the output_table
for i = 1:height(output_table)
    % Sum the hit, miss, fa, and cr for both CRET and PRET
    sum_CRET = output_table.Hit_CRET(i) + output_table.Miss_CRET(i) + output_table.FA_CRET(i) + output_table.CR_CRET(i);
    sum_PRET = output_table.Hit_PRET(i) + output_table.Miss_PRET(i) + output_table.FA_PRET(i) + output_table.CR_PRET(i);
    
    % Check if the sums are not equal to 1
    if sum_CRET ~= 1 || sum_PRET ~= 1
        qc_passed = false;
        fprintf('QC check failed for row %d: CRET sum = %d, PRET sum = %d\n', i, sum_CRET, sum_PRET);
    end
end

% Output the result of the QC check
if qc_passed
    disp('All QC checks passed.');
else
    disp('Some QC checks failed. Please review the output.');
end


stamp_submem_tbl = output_table;

save('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_submem_tbl.mat', 'stamp_submem_tbl');

writetable(stamp_submem_tbl,'/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_submem_tbl.csv')




    %%%%%% OPTION 1 %%%%%%%
    % Binarizes the responses. Shenyang has an
    % approach to adjust the mem values. See R_0_prepare_data.Rmd
%     oldresp = nan(length(cmem_ret_col),1); %if old 3,4 = hit; 1,2 = miss
%     newresp = nan(length(cmem_ret_col),1); %if new 3,4 = FA; 1,2 = CR
%     for number = 1:length(cmem_ret_col)
%         if old(number) == 1
%             if cmem_ret_col(number) == 1 %0 for miss
%                 oldresp(number) = 0;
%             elseif cmem_ret_col(number) == 2 %0 for miss
%                 oldresp(number) = 0;
%             elseif cmem_ret_col(number) == 3 %1 for hit
%                 oldresp(number) = 1;
%             elseif cmem_ret_col(number) == 4  %1 for hit
%                 oldresp(number) = 1;
%             end
%         elseif old(number) == 0
%             if cmem_ret_col(number) == 1 %0 for CR
%                 newresp(number) = 0;
%             elseif cmem_ret_col(number) == 2 % 0 for CR
%                 newresp(number) = 0;
%             elseif cmem_ret_col(number) == 3 %1 for FA
%                 newresp(number) = 1;
%             elseif cmem_ret_col(number) == 4  %3 for FA
%                 newresp(number) = 1;
%             end
%         end
%     end
