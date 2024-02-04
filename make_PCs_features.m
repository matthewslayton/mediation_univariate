% this comes from mds_practice.m It's the top and only useful part


set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white

%addpath('/Users/matthewslayton/Documents/GitHub/STAMP/STAMP_scripts/');
addpath('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/STAMP_scripts/');
addpath('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/');



% import items x features matrix
itemFeatures = readtable('featurematrix_4Cortney_withIDs.xlsx');
% note, I've udpated this spreadsheet. I added a col called
% "reduced_category" that has the following: home, outside, home_tool,
% outside_tool, animal, food, plant. The goal is to select items categories 
% but not do every single one because we have ~20

%'/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featurematrix_4Cortney_withIDs.xlsx'

% need to remove the columns and rows that don't matter
% featureMat = removevars(itemFeatures,{'Var2','Var3','Var4','Var7'}); %remove unneeded columns, leaving only 'item' and 'category'
% top row has "visual-form_and_surface, visual-color, encyclopedic, tactile, taxonomic, function, taste, sound"
% It'd be good to remove this row leaving the features (e.g. "has_legs")

% featureMat is 995x5522. First col is item, second col is category, the rest of the cols are feature hits
% need to make a separate item col before removing it and making justFeatures
% itemList = featureMat(:,1);
% items = table2array(itemList);
% categoryList = featureMat(:,2);
% justFeaturesTbl = removevars(featureMat,{'Var1','Var5','Var6'}); % remove item and category cols
% justFeaturesArray = table2array(justFeaturesTbl);


save('justFeaturesArray.mat','justFeaturesArray')
save('justFeaturesTbl.mat','justFeaturesTbl')
save('items.mat','items')

load justFeaturesArray.mat
load items.mat

feature_corrMat_995 = corrcoef(justFeaturesArray');
save('feature_corrMat_995.mat','feature_corrMat_995')

feature_corrMat_300 = corrcoef(justFeaturesArray(1:300,:)');
save('feature_corrMat_300.mat','feature_corrMat_300')

%%%% need corrMat to be sorted in stimID order, not item alphabetical order
%itemFeatures = readtable('featurematrix_4Cortney_withIDs.xlsx');

%remove the mccrae features row
itemFeatures(1,:) = [];

features_stimIDorder = itemFeatures(:,8:end);
save('features_stimIDorder.mat','features_stimIDorder')

features_stimID_corrmat = corrcoef(table2array(features_stimIDorder)');
save('features_stimID_corrmat.mat','features_stimID_corrmat')

featMat_names_ID = itemFeatures(:,1:2);
save('featMat_names_ID.mat','featMat_names_ID')

%%%% here's the problem, these here are sorted by STAMP ID, not stimID.
%%%% STAMP IDs are four digits and don't really have anything to do with
%%%% NetTMS. In NetTMS we use the dino lab object image database. That's
%%%% the same items as used in the STAMP study. The only difference is that
%%%% the stimIDs from low to high match the objects in alphabetical order,
%%%% so essentially the row number. That means we shouldn't sort at all!

%featMat_sortedByID = sortrows(itemFeatures,{'id'},"ascend");
%featMat_sortedByID_namesID = featMat_sortedByID(:,1:2);
%save('featMat_sortedByID_namesID.mat','featMat_sortedByID_namesID');
%featMat_sortedByID_onlyFeatures = featMat_sortedByID(:,8:end);
%save('featMat_sortedByID_onlyFeatures.mat','featMat_sortedByID_onlyFeatures')
%feature_sorted_corrMat = corrcoef(table2array(featMat_sortedByID_onlyFeatures)');
%save('feature_sorted_corrMat.mat','feature_sorted_corrMat')








%%% alternatively, I need to grab item subsets, so rows that match one of
%%% my reduced-category labels
itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','outside');
featureMat_outside = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_outside = table2array(featureMat_outside);
IDs = itemFeatures.id;
outside_IDs = IDs;
save('outside_IDs.mat','outside_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','home');
featureMat_home = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_home = table2array(featureMat_home);
IDs = itemFeatures.id;
home_IDs = IDs;
save('home_IDs.mat','home_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','home_tool');
featureMat_home_tool = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_home_tool = table2array(featureMat_home_tool);
IDs = itemFeatures.id;
home_tool_IDs = IDs;
save('home_tool_IDs.mat','home_tool_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','animal');
featureMat_animal = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_animal = table2array(featureMat_animal);
IDs = itemFeatures.id;
animal_IDs = IDs;
save('animal_IDs.mat','animal_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','food');
featureMat_food = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_food = table2array(featureMat_food);
IDs = itemFeatures.id;
food_IDs = IDs;
save('food_IDs.mat','food_IDs')

save('featMat_outside.mat','featMat_outside')
save('featMat_home.mat','featMat_home')
save('featMat_home_tool.mat','featMat_home_tool')
save('featMat_animal.mat','featMat_animal')
save('featMat_food.mat','featMat_food')

%%% doing the presentation items is easy. Just sort and remove the 1111
%%% items, which are lures. That also excludes plants completely, then
%%% re-sort by category to put back in alphabetical order, which is how it starts

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','outside_300');
featureMat_outside_300 = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_outside_300 = table2array(featureMat_outside_300);
IDs = itemFeatures.id;
outside_300_IDs = IDs;
save('outside_300_IDs.mat','outside_300_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','home_300');
featureMat_home_300 = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_home_300 = table2array(featureMat_home_300);
IDs = itemFeatures.id;
home_300_IDs = IDs;
save('home_300_IDs.mat','home_300_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','home_tool_300');
featureMat_home_tool_300 = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_home_tool_300 = table2array(featureMat_home_tool_300);
IDs = itemFeatures.id;
home_tool_300_IDs = IDs;
save('home_tool_300_IDs.mat','home_tool_300_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','animal_300');
featureMat_animal_300 = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_animal_300 = table2array(featureMat_animal_300);
IDs = itemFeatures.id;
animal_300_IDs = IDs;
save('animal_300_IDs.mat','animal_300_IDs')

itemFeatures = readtable('featurematrix_reducedCategories.xlsx','Sheet','food_300');
featureMat_food_300 = removevars(itemFeatures,{'concept','id','enc','ret','reduced_category','category','domain'});
featMat_food_300 = table2array(featureMat_food_300);
IDs = itemFeatures.id;
food_300_IDs = IDs;
save('food_300_IDs.mat','food_300_IDs')

save('featMat_outside_300.mat','featMat_outside_300')
save('featMat_home_300.mat','featMat_home_300')
save('featMat_home_tool_300.mat','featMat_home_tool_300')
save('featMat_animal_300.mat','featMat_animal_300')
save('featMat_food_300.mat','featMat_food_300')


% can't seem to find where I made these, but you'd just need the feature
% category labels, sort, separate, and place in new mats
load featMat_encycl.mat %these have 995 rows
load featMat_tax.mat
load featMat_vis.mat
load featMat_fcn.mat
load featMatNoTax.mat % though I think this one is done and subsetted already

% feature names (e.g. is made of metal) and feature category names (e.g. encyclopedic)
load featNames_encycl.mat
load featNames_tax.mat
load featNames_vis.mat
load featNames_fcn.mat


%%%%%%%%%%%%%%%
%%% Can I take the featMats for encycl, vis, etc that have 995 rows and cut
%%% to 300? To 360?

itemIDs_tbl = itemFeatures(:,2);
itemIDs = table2array(itemIDs_tbl);

%save('itemIDs.mat','itemIDs')
load itemIDs.mat

% these are the 300 items presented to S005, which is the same items as for
% everyone else. 
% Well, technically they saw 360 (330?), but when you take out the catch
% trials we only scanned for 300, so let's focus on those only when we're
% doing F score to BOLD activity


% 300
load IDs_enc.mat %cell

% itemIDs is a 995 double in the order of featMat_encycl etc

% select one
currFeatMat = featMat_encycl;
currFeatMat = featMat_vis;
currFeatMat = featMat_fcn;
% currFeatMat = featMat_tax;
% currFeatMat = featMatNoTax;
currFeatMat = justFeaturesArray; %all

newMat = zeros(300,size(currFeatMat,2)); 
for row = 1:numel(IDs_enc)
    currID = str2num(cell2mat(IDs_enc{row}));
    grabRow = find(itemIDs == currID);
    newMat(row,:) = currFeatMat(grabRow,:);
end

featMat_encycl_300 = newMat;
save('featMat_encycl_300.mat','featMat_encycl_300')

featMat_vis_300 = newMat;
save('featMat_vis_300.mat','featMat_vis_300')

featMat_fcn_300 = newMat;
save('featMat_fcn_300.mat','featMat_fcn_300')

featMat_all_300 = newMat;
save('featMat_all_300.mat','featMat_all_300')

% featMat_tax_300 = newMat;
% featMat_noTax_300 = newMat;
% save('featMat_tax_300.mat','featMat_tax_300')
% save('featMat_noTax_300.mat','featMat_noTax_300')


%%% can I make 360 versions?
% 360
load IDs_360.mat %double

% select one
currFeatMat = featMat_encycl;
currFeatMat = featMat_vis;
currFeatMat = featMat_fcn;
currFeatMat = justFeaturesArray;

newMat = zeros(360,size(currFeatMat,2)); 
for row = 1:numel(IDs_360)
    currID = IDs_360(row);
    grabRow = find(itemIDs == currID);
    if isempty(grabRow) == 1 %if ID doesn't match, just fill with zeros
        zeroArr = zeros(1,size(currFeatMat,2));
        newMat(row,:) = zeroArr;
    else
        newMat(row,:) = currFeatMat(grabRow,:);
    end
end

featMat_encycl_360 = newMat;
featMat_vis_360 = newMat;
featMat_fcn_360 = newMat;
featMat_all_360 = newMat;

save('featMat_encycl_360.mat','featMat_encycl_360')
save('featMat_vis_360.mat','featMat_vis_360')
save('featMat_fcn_360.mat','featMat_fcn_360')
save('featMat_all_360.mat','featMat_all_360')

% the first subject (S002) saw 330 items, not 360
load IDs_330.mat

% select one
currFeatMat = featMat_encycl;
currFeatMat = featMat_vis;
currFeatMat = featMat_fcn;
currFeatMat = featMat_all;

newMat = zeros(330,size(currFeatMat,2)); 
for row = 1:numel(IDs_330)
    currID = IDs_330(row);
    grabRow = find(itemIDs == currID);
    if isempty(grabRow) == 1 %if ID doesn't match, just fill with zeros
        zeroArr = zeros(1,size(currFeatMat,2));
        newMat(row,:) = zeroArr;
    else
        newMat(row,:) = currFeatMat(grabRow,:);
    end
end

featMat_encycl_330 = newMat;
save('featMat_encycl_330.mat','featMat_encycl_330')

featMat_vis_330 = newMat;
save('featMat_vis_330.mat','featMat_vis_330')

featMat_fcn_330 = newMat;
save('featMat_fcn_330.mat','featMat_fcn_330')

featMat_all_330 = newMat;
save('featMat_all_330.mat','featMat_all_330')


%% try factor analysis
cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/';
load justFeaturesArray.mat
load items.mat % object names

% https://www.mathworks.com/help/stats/factoran.html
% https://www.mathworks.com/matlabcentral/answers/196574-factor-analysis-a-covariance-matrix-is-not-positive-definite
% lambda: the factor loadings matrix lambda for the data matrix X with m common factors.
% psi: maximum likelihood estimates of the specific variances.
% T: the m-by-m factor loadings rotation matrix T.
% stats: the structure stats containing information relating to the null hypothesis H0 that the number of common factors is m.
% F: predictions of the common factors (factor scores). 
nfeatures = 823; % running all 5000+ features result in a covariance matrix that is not positive definite
m = 10; % number of factors 
% use oblique rotation to allow correlated factors
% https://www.youtube.com/watch?v=nIv8h4rQ7K4
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');
%%
clc
F_to_examine = 2;
[Fsorted, orderF] = sort(F(:, F_to_examine), 'descend');
T = table(Fsorted, items(orderF));

save('tenFactors_500features.mat','F')
save('tenFactors_500features_lambda.mat','lambda')
save('fortyFactors_200features.mat','F')
save('fortyFactors_200features_lambda.mat','lambda')

% take lambda and list of features
% take first col of lambda and the col of features. sort by first col
% largest to smallest
% get the rows that have a value > 0.3
% print/export the features that are on the rows where lambda is above
% threshold
% then take second col of lambda and repeat

load feat_fmri_merged_encycl.mat %has negatives
load feat_fmri_merged_encycl_noNeg.mat
% isolate the feature data
feat_merged_encycl_noNeg = feat_fmri_merged_encycl_noNeg{:,7:end};
save('feat_merged_encycl_noNeg.mat','feat_merged_encycl_noNeg')

%% feature names and categories
% get names
featNames_mergedEncycl_cell_full = feat_fmri_merged_encycl.Properties.VariableNames;
featNames_mergedEncycl = featNames_mergedEncycl_cell_full(7:end);

save('featNames_mergedEncycl.mat','featNames_mergedEncycl')

% feature mats
load featMat_encycl.mat
load featMat_tax.mat
load featMat_vis.mat
load featMat_fcn.mat
load featMatNoTax.mat % though I think this one is done and subsetted already

% feature names (e.g. is made of metal) and feature category names (e.g. encyclopedic)
load featNames_encycl.mat
load featNames_tax.mat
load featNames_vis.mat
load featNames_fcn.mat
featCat_all_tbl = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featureCategories_oneCol.xlsx');
featNames_all_tbl = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featureNames_oneCol.xlsx');
featCat_all = table2cell(featCat_all_tbl);
featNames_all = table2cell(featNames_all_tbl);
save('featCat_all.mat','featCat_all')
save('featNames_all.mat','featNames_all')

%load 'noTax_labels.mat'
featNames_noTax = cellstr(noTax_labels);
save('featNames_noTax.mat','featNames_noTax')

itemNamesCell = feat_fmri_merged_encycl{:,1};
% isolate the feature data
featCounts = feat_fmri_merged_encycl{:,7:end};

featMat_mergedEncycl = featCounts;
save('featMat_mergedEncycl.mat','featMat_mergedEncycl')

% for other feature matrices, just do:
featCounts = featMat_encycl;
featCounts = featMat_tax;
featCounts = featMat_vis;
featCounts = featMat_fcn;
featCounts = featMatNoTax;
featCounts = justFeaturesArray;

%% thresholding
% FA won't run if there are too many cols where the numbers are the same. 
%%%%%% make this a function so I don't have to do that renaming featCounts thing above
colSum = sum(featCounts,1);
indices = find(colSum==0); % where are the zeros
% remove zeros from itemNamesCell and featCounts
featCounts_nozero = featCounts;
featCounts_nozero(:, indices) = []; %----- can remove altogether
colSum_nozero = sum(abs(featCounts_nozero), 1);
[~, colSum_nozero_order] = sort(colSum_nozero, 'descend');
featCounts_nozero_ordered = featCounts_nozero(:, colSum_nozero_order); 
% no threshold, has 802 cols
% 1st level threshold of 3, has 357 cols

feat_all_nozero = featCounts_nozero_ordered;
save('feat_all_nozero.mat','feat_all_nozero')
 


nfeatures = 300; % or 100
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(featCounts_nozero_ordered(:, 1:nfeatures), m, 'rotate', 'promax');

% to get the feature names, we need the table col labels
featNames = feat_fmri_merged_encycl.Properties.VariableNames;
featNames_nozero = featNames(7:end);
featNames_nozero(:,indices) = [];
featNames_nozero_ordered = featNames_nozero(:,colSum_nozero_order);
% now grab top 200 or 100
featNames_top200 = featNames_nozero_ordered(1:200)';

save 200feat_encycl_F.mat F
save 200feat_encycl_lambda.mat lambda
save 250feat_encycl_F.mat F
save 250feat_encycl_lambda.mat lambda

save 150feat_fcn_F.mat F
save 150feat_fcn_lambda.mat lambda

save 140feat_tax_F.mat F
save 140feat_tax_lambda.mat lambda

save 200feat_vis_F.mat F
save 200feat_vis_lambda.mat lambda

save 200feat_noTax_F.mat F
save 200feat_noTax_lambda.mat lambda

%%%%%%%%%
%%%%% threshold approachces
% Cortney uses the following 2 thresholds:
% Two high-pass (inclusive) thresholds:
% 1st: number of people assigned feature X to each item (greater threshold -> more reliable feature)
% ---- cells with values below the threshold are changed to zeros
% 2nd: number of items that were assigend feature X (greater threshold -> more common the feature)
% ---- feature columns below the threshold are excluded

featCounts(abs(featCounts) < 2) = 0; % 1st level
thres_pass = find(sum(abs(featCounts) > 0) >= 3) + 8; % 2nd level

thres_pass = find(sum(abs(featCounts) > 0) >= 4); %gives the indices of the feats that meet the condition

featCounts_thres = featCounts(:,thres_pass);
save('featCounts_thres.mat','featCounts_thres')

% make a version of featNames_all and featCat_all that has the same cols
featNames_all_thres = featNames_all(thres_pass)';
featCat_all_thres = featCat_all(thres_pass)';
save('featNames_all_thres.mat','featNames_all_thres')
save('featCat_all_thres.mat','featCat_all_thres')


%%% featCounts = featMat_encycl 995x1930
thres_pass_indices = find(sum(abs(featCounts) > 0) >= 4);
% feature matrix
encycl_thres = featCounts(:,thres_pass_indices);
save('encycl_thres.mat','encycl_thres')
% feature names
encycl_thres_names = featNames_encycl(:,thres_pass_indices);
save('encycl_thres_names.mat','encycl_thres_names')

%%% featCounts = featMat_tax 995x353
thres_pass_indices = find(sum(abs(featCounts) > 0) >= 4);
% feature matrix
tax_thres = featCounts(:,thres_pass_indices);
save('tax_thres.mat','tax_thres')
% feature names
tax_thres_names = featNames_tax(:,thres_pass_indices);
save('tax_thres_names.mat','tax_thres_names')

%%% featCounts = featMat_vis 995x1886
thres_pass_indices = find(sum(abs(featCounts) > 0) >= 4);
% feature matrix
vis_thres = featCounts(:,thres_pass_indices);
save('vis_thres.mat','vis_thres')
% feature names
vis_thres_names = featNames_vis(:,thres_pass_indices);
save('vis_thres_names.mat','vis_thres_names')

%%% featCounts = featMat_fcn 995x1191
thres_pass_indices = find(sum(abs(featCounts) > 0) >= 4);
% feature matrix
fcn_thres = featCounts(:,thres_pass_indices);
save('fcn_thres.mat','fcn_thres')
% feature names
fcn_thres_names = featNames_fcn(:,thres_pass_indices)';
save('fcn_thres_names.mat','fcn_thres_names')


save tenFactors_200feat_shenyang_F.mat F
save tenFactors_200feat_shenyang_lambda.mat lambda

save tenFactors_100feat_shenyang_thresh2_F.mat F
save tenFactors_100feat_shenyang_thresh2_lambda.mat lambda


%% how many factors do I need to explain 60% of variance?
%%%%% this material comes from adj_r2_PCs_F_betas.m
% subjInfo structs come from PCs_betas_lmer_customROIs.m

% atlas ROI activity
load subjInfo.mat
% fugL = 3
% fugR = 4
% hippL = 5
% hippR = 6
% phgL = 17
% phgR = 18

% custom/mem ROI activity
load subjInfo_customROIs.mat
% R ATL = 5
% R Fus = 7
% L Precun = 10
% L/R Cun = 12
% L OFC = 13

num_Fs = 10;
figure

for subjNum = 1:19

    %%% custom ROIs
%     activity = subjInfo_c(subjNum).activityVal(:,7);
%     F_mat = subjInfo_c(subjNum).Fval;
    %%% atlas ROIs
    activity = cell2mat(subjInfo(subjNum).tmpVal(3))'; % change this index to match the ROI I want
    F_mat = subjInfo(subjNum).Fval;
   
    %%% part of loop that applies to both ROI types
    adj_r_sqr = zeros(num_Fs,1);
    r_sqr = zeros(num_Fs,1);
    
    for f_val = 1:num_Fs
    
        mdl_struct = fitlm(F_mat(:,1:f_val),activity);
        adj_r_sqr(f_val) = mdl_struct.Rsquared.Adjusted;
        r_sqr(f_val) = mdl_struct.Rsquared.Ordinary;
    
    end
    
    % flip array left and right
    y_val_adjRsqr = fliplr(adj_r_sqr);
    y_val_Rsqr = fliplr(r_sqr);
    
    % residual error (y_actual - y_predicted)^2
    resid_error_calc = (activity - mdl_struct.Fitted).^2;
    % remove the rows with NaN
    resid_error_calc(any(isnan(resid_error_calc), 2), :) = [];
    resid_error = sum(resid_error_calc);
    
    
    x = 1:num_Fs;
    set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white
    
    plot(x,y_val_adjRsqr,'LineWidth',2)
    hold on
    plot(x,y_val_Rsqr,'LineWidth',2)

end

xlabel('Factors as dimensions')
ylabel('Variance Explained')
title('Prediction of Activity in Right Fusiform Gyrus')
%legend('Adj-Rsqr','Ord-Rsqr')
%text(4,max(y_val_Rsqr)/2,horzcat('residual error: ',num2str(resid_error)))
hold off




%% start down here to run automated feature sorting
% all 995x5520
%load justFeaturesArray.mat

% no taxonomic features
load featMatNoTax.mat %load 'noTax_labels.mat'


load items.mat

% read in feature names
% featureNamesTbl = readtable('featureNames_oneCol.xlsx');
% featureNamesCell = table2array(featureNamesTbl);
% featureNames = strings(5520,1);
% for row = 1:numel(featureNamesCell)
%     featureNames(row) = cell2mat(featureNamesCell(row));
% end

% no tax labels
load 'noTax_labels.mat'

% re-label so it matches the code below

justFeaturesArray = featMatNoTax;
featureNames = noTax_labels';

%% start here (don't forget to computer featureNames above)
nfeatures = 500; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr(:,col) = thresholdMat; 
end

% make sure to copy and paste featureNamesSubset into excel to analyze with
% lambda
save tenFactors_500feat_noTax.mat F
save tenFactors_500feeat_noTax_lambda lambda

save tenFactors_440features.mat F % loads var F
save tenFactors_440features_lambda.mat lambda % loads var lambda

% start here
nfeatures = 470; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr2 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr2(:,col) = thresholdMat; 
end


% start here
nfeatures = 480; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr3 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr3(:,col) = thresholdMat; 
end

% start here
nfeatures = 490; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr4 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr4(:,col) = thresholdMat; 
end


% start here
nfeatures = 500; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr5 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr5(:,col) = thresholdMat; 
end


% start here
nfeatures = 510; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr6 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr6(:,col) = thresholdMat; 
end

% start here
nfeatures = 520; 
m = 10; % number of factors 
[lambda, psi, T, stats, F] = factoran(justFeaturesArray(:, 1:nfeatures), m, 'rotate', 'promax');

% need string array to output the factor features to
% lambda and m are calculated above in FA

factorFeatures_strArr7 = strings(length(lambda),m);

% cut featureNames to length of lambda

featureNamesSubset = featureNames(1:size(lambda,1));

for col = 1:size(lambda,2)

    % grab one lambda col at a time and horzcat with feature names subset
    currFeatureLambdaMat = [featureNamesSubset lambda(:,col)];
    % sort rows by lambda values in descending order
    sortedMat = sortrows(currFeatureLambdaMat,2,'descend');

    thresholdMat = strings(size(sortedMat,1), 1);
    for row = 1:size(sortedMat,1)
        if str2double(sortedMat(row,2)) > .3
            thresholdMat(row) = sortedMat(row,1);
        else
            thresholdMat(row) = NaN;
        end
    
    end
    % remove the missing rows
    %factorFeatures = rmmissing(thresholdMat);

    factorFeatures_strArr7(:,col) = thresholdMat; 
end



%% this stuff is all from the first version of this script. PCA and other stuff I didn't end up needing


%semDist = xcorr(sem_array(1,:),sem_array(2,:));
% justFeaturesTable = array2table(justFeaturesArray);
%
% change folder from Simon_Lab to sparsePCA-master
%p = sparsePCA(justFeaturesArray,3,3,0,1);
%sparsePCA(justFeatures,3,3,0,1);
%[F,adj_var,cum_var] = sparsePCA(justFeaturesArray,995,3,0,1);


%%%% this is the one I want
% makes 5520x994 coeff, 995x994 score
[coeff, score, latent, tsquared, explained, mu] = pca(justFeaturesArray);

% makes 995x995 coeff, 5520x995 score
%[coeff, score, latent, tsquared, explained, mu] = pca(justFeaturesArray');


[coeff, score, latent, tsquared, explained, mu] = pca(corrMatTransposed);

coeff_corr = coeff;
score_corr = score;
save('coeff_corr.mat','coeff_corr')
save('score_corr.mat','score_corr')

[coeff, score, latent, tsquared, explained, mu] = pca(covMatTransposed);

coeff_cov = coeff;
score_cov = score;
save('coeff_cov.mat','coeff_cov')
save('score_cov.mat','score_cov')

% corrcoef first, gives 5520x5520
corrMat = corrcoef(justFeaturesArray);
% 995x995
corrMatTransposed = corrcoef(justFeaturesArray');


save('corrMat_5520.mat','corrMat')
save('corrMat_995.mat','corrMatTransposed')

% 5520x5520
covMat = cov(justFeaturesArray);
% 995x995
covMatTransposed = cov(justFeaturesArray');


% explained tells you the % variance that each PC captures

% ~~~ % coefficients/loadings are coeffs on variables. It's the linear combination of the original variables
% from which the PCs are calculated.
% ~~~ % score are the coordinates of the variables in PC space (where the axes are the PCs)

% coeff has the coefficients. Coeff matrix gives coeffs of the linear
% combination for each PC
% score has the coordinates. Score is linear combo of original features



