clear
close all
clc

%rootDir = 'D:/local/STAMP_Connectivity/';
%cd([rootDir 'Scripts']);
%addpath([rootDir 'Scripts/utility']);
addpath /Users/matthewslayton/spm12;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/NIfTI_20140122/;
rootDir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP';
cd (rootDir)
addpath '/Users/matthewslayton/BCT'

% custom + atlas
%maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/visLex_CR_masksClean/';
%addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/visLex_CR_masksClean/';

% BNA brainnetome for jan23
% maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23';
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23;
% maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23_9memMasks';
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23_9memMasks;
% maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23_7memMasksOnly';
% addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23_7memMasksOnly;
%maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_feb23';
%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_feb23;

%%% STAMP 36 ROIs
maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/masks_nov23';
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/masks_nov23;

% include the custom cleaned masks as well as the six atlas mem masks
cd (maskdir)
ROIfiles = dir('*.nii');
ROIlist = {ROIfiles.name}.';

%% make color brain maps
% find the mask voxels and add t-values
% need 10 brain maps for the 10 factors

% run lmer() in R and then do summary(model) to see t-values
% copy and paste t-values into spreadsheet 

% in R, you might run something like avgActivity_customROIs_mem.xlsx or
% avgActivity_customROIs_PC_F.xlsx
% These are output from PCs_betas_lmer_customROIs.m
% This uses activityPerTrialPerCustomCluster_cortney.m to get the activity
% in the specific clusters that I got from making a cluster table using 
% fsl in the command line

%% factors
%tVal_tenIndiv = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/f500_tValues_tenIndiv.xlsx');
%tVal_oneBig = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/f500_tValues_oneBig.xlsx');
%tVal_tenIndiv = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/f500_tValues_tenIndiv.xlsx');
%tVal_oneBig = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/f500_tValues_oneBig.xlsx');

%%% what about nnmf? they come from stamp_lmer_nnmf.R
%%% also, no Hipp_A and Hipp_P, but that's ok because they're not significant for lexMem or visMem anyway
%tVal_fac = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_nnmf_fac_tval.xlsx');
%tVal_fac = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_nnmf_fac_tval_eightAndFiveFac_feb23.xlsx');
%tVal_fac = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_nnmf_fac_tval_reducedFromAll_feb23.xlsx');
%%% these are t-values from FA stamp_lmer_v2.R or v3
%tVal_fac = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_fac_tval.xlsx');


est_lex = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_mediation_multMem_36ROI_unilateral.xlsx','Sheet','lexMem');
est_vis = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_mediation_multMem_36ROI_unilateral.xlsx','Sheet','visMem');

vis_fourFac = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_mediation_multMem_36ROI_unilateral.xlsx','Sheet','multFac');

estSing = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_mediation_singleMem_36ROI_unilateral_encycl.xlsx');


prop = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_multilevel_propMediated.xlsx');

%% mem
%tVal_bna = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_mem_tval_nineROIs.xlsx');
%%% you get mem values and then paste into a separate doc. This doc has 21
%%% rows of t-values. Not sure why
%tVal_bna = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bna_mem_tval.xlsx');

%tVal_twoIndiv = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/custom_atlas_mem_tval.xlsx');
%tVal_oneBig = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/custom_atlas_mem_tVal_oneBig.xlsx');
%tVal_bothMem = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothMem_masks.xlsx');
%tVal_oneBig = readtable('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/tVal_cmem_pmem_lexMem_visMem.xlsx');

% remove the ROI col
est_lex.ROI = [];
est_vis.ROI = [];
vis_fourFac.ROI = [];
estSing.ROI = [];

prop.ROI = [];
prop.total = [];
prop.indirect = [];

output_dir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/outputMaps_nov23/';



currData = est_lex;

%%%% below here is a ton of content from previous iterations. I'm not super
%%%% comfortable with all of it, so I'd almost like to re-do
for currCol = 1:12
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * currData{currROI,currCol}; % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end
    if currCol == 1
        save_nii(fac,strcat(output_dir,'lexMem-encycl-F01_indirect_est.nii'))
    elseif currCol == 2
        save_nii(fac,strcat(output_dir,'lexMem-encycl-F02_indirect_est.nii'))
    elseif currCol == 3 
        save_nii(fac,strcat(output_dir,'lexMem-encycl-F03_indirect_est.nii'))
    elseif currCol == 4 
        save_nii(fac,strcat(output_dir,'lexMem-encycl-F04_indirect_est.nii'))
    elseif currCol == 5 
        save_nii(fac,strcat(output_dir,'lexMem-vis-F01_indirect_est.nii'))
    elseif currCol == 6
        save_nii(fac,strcat(output_dir,'lexMem-vis-F02_indirect_est.nii'))
    elseif currCol == 7
        save_nii(fac,strcat(output_dir,'lexMem-vis-F03_indirect_est.nii'))
    elseif currCol == 8
        save_nii(fac,strcat(output_dir,'lexMem-vis-F04_indirect_est.nii'))
    elseif currCol == 9
        save_nii(fac,strcat(output_dir,'lexMem-fcn-F01_indirect_est.nii'))
    elseif currCol == 10
        save_nii(fac,strcat(output_dir,'lexMem-fcn-F02_indirect_est.nii'))
    elseif currCol == 11
        save_nii(fac,strcat(output_dir,'lexMem-fcn-F03_indirect_est.nii'))
    elseif currCol == 12
        save_nii(fac,strcat(output_dir,'lexMem-fcn-F04_indirect_est.nii'))
   
    end %end if statement
end %end currCol loop 


currData = est_vis;
for currCol = 1:12
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * currData{currROI,currCol}; % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end

    if currCol == 1
        save_nii(fac,strcat(output_dir,'visMem-encycl-F01_indirect_est.nii'))
    elseif currCol == 2
        save_nii(fac,strcat(output_dir,'visMem-encycl-F02_indirect_est.nii'))
    elseif currCol == 3
        save_nii(fac,strcat(output_dir,'visMem-encycl-F03_indirect_est.nii'))
    elseif currCol == 4
        save_nii(fac,strcat(output_dir,'visMem-encycl-F04_indirect_est.nii'))
    elseif currCol == 5
        save_nii(fac,strcat(output_dir,'visMem-vis-F01_indirect_est.nii'))
    elseif currCol == 6
        save_nii(fac,strcat(output_dir,'visMem-vis-F02_indirect_est.nii'))
    elseif currCol == 7
        save_nii(fac,strcat(output_dir,'visMem-vis-F03_indirect_est.nii'))
    elseif currCol == 8
        save_nii(fac,strcat(output_dir,'visMem-vis-F04_indirect_est.nii'))
    elseif currCol == 9
        save_nii(fac,strcat(output_dir,'visMem-fcn-F01_indirect_est.nii'))
    elseif currCol == 10
        save_nii(fac,strcat(output_dir,'visMem-fcn-F02_indirect_est.nii'))
    elseif currCol == 11
        save_nii(fac,strcat(output_dir,'visMem-fcn-F03_indirect_est.nii'))
    elseif currCol == 12
        save_nii(fac,strcat(output_dir,'visMem-fcn-F04_indirect_est.nii'))
    end
 %end if statement
end %end currCol loop 

currData = vis_fourFac;
for currCol = 1:4
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * currData{currROI,currCol}; % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end
    if currCol == 1
        save_nii(fac,strcat(output_dir,'visMem-vis-F01_highest.nii'))
    elseif currCol == 2
        save_nii(fac,strcat(output_dir,'visMem-vis-F02_highest.nii'))
    elseif currCol == 3 
        save_nii(fac,strcat(output_dir,'visMem-vis-F03_highest.nii'))
    elseif currCol == 4 
        save_nii(fac,strcat(output_dir,'visMem-vis-F04_highest.nii'))
   
    end %end if statement
end %end currCol loop 


currData = estSing;

output_dir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/outputMaps_nov23/';

%%%% below here is a ton of content from previous iterations. I'm not super
%%%% comfortable with all of it, so I'd almost like to re-do
for currCol = 1:4
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * currData{currROI,currCol}; % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end
    if currCol == 1
        save_nii(fac,strcat(output_dir,'singMed_lexMem-encycl-F01_indirect_est.nii'))
    elseif currCol == 2
        save_nii(fac,strcat(output_dir,'singMed_lexMem-encycl-F02_indirect_est.nii'))
    elseif currCol == 3 
        save_nii(fac,strcat(output_dir,'singMed_lexMem-encycl-F03_indirect_est.nii'))
    elseif currCol == 4 
        save_nii(fac,strcat(output_dir,'singMed_lexMem-encycl-F04_indirect_est.nii'))
   
    end %end if statement
end %end currCol loop 
    

currData = prop;
currCol = 1;
mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
fac = struct;
fac.('hdr') = mask_AG_L.hdr;
fac.('filetype') = 2;
fac.('fileprefix') = 'whole';
fac.('machine') = 'ieee-le';
fac.('ext') = [];
fac.('img') = zeros(43,51,35);

for currROI = 1:length(ROIlist) 
    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * currData{currROI,currCol}; % change this second number for the col i want
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                fac.img(voxel) = multMask(voxel);
            end
        end
end

save_nii(fac,strcat(output_dir,'propMed_lexMem-encycl.nii'))


%% let's merge some bilateral ROIs and make unilateral combined ROIs

%%% STAMP 36 ROIs
maskdir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/masks_nov23';
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/masks_nov23;

% include the custom cleaned masks as well as the six atlas mem masks
cd (maskdir)
ROIfiles = dir('*.nii');
ROIlist = {ROIfiles.name}.';

% Assuming ROIlist contains the names of your ROIs without prefixes like '_L' or '_R'
% maskdir is the directory where your ROIs are stored
% output_dir is the directory where you want to save the combined ROIs

output_dir = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/merged_masks_nov23';

for currROI = 1:length(ROIlist)
    ROI = cell2mat(ROIlist(currROI));

    % Extract ROI name between "ROI_" and "_L" or "_R"
    [~, name, ~] = fileparts(ROI);
    [startIdx, endIdx] = regexp(name, 'ROI_(.*?)_([LR])$', 'once', 'start', 'end');
    
    if ~isempty(startIdx) && ~isempty(endIdx)
        % Get the name between "ROI_" and "_L" or "_R"
        roi_name = name(startIdx:endIdx - 2);
        
        % Load left and right ROIs
        mask_L = load_untouch_nii(fullfile(maskdir, ['BNAmerged_' roi_name '_L.nii']));
        mask_R = load_untouch_nii(fullfile(maskdir, ['BNAmerged_' roi_name '_R.nii']));
        
        % Combine left and right ROIs to create a bilateral ROI
        bilateral_ROI = mask_L;
        bilateral_ROI.img = mask_L.img | mask_R.img;

        % Set intensity range to 0 and 1
        bilateral_ROI.hdr.dime.glmax = 1;
        bilateral_ROI.hdr.dime.glmin = 0;

        % Save the bilateral ROI
        save_untouch_nii(bilateral_ROI, fullfile(output_dir, ['BNAmerged_' roi_name '_bilateral.nii']));
    else
        fprintf('Error: Unable to extract ROI name from %s\n', ROI);
    end
end










%% pick one of these
%tVal_mat = table2array(tVal_bna);
%tVal_mat = table2array(tVal_fac);

%tVal_mat = table2array(tVal_tenIndiv);
%tVal_mat = table2array(tVal_twoIndiv);
%tVal_mat = table2array(tVal_bothMem);

% alternatively, combine the 10 separate 3D files to be 4D


%% fac
% load random mask for header info
% currCol: encycl, encycl 300, vis, vis 300, fcn, fcn 300 for all,
% remembered, and forgotten. That's six facs for three trial types, so 18 total

% Seven masks related to memory
% rows of tVal_fac table have seven F01's, seven F02's, up to F08 for
% encycl and vis. fcn has five and fcn 300 has four
% so it's AG_L_F01, AG_R_F01, etc

% maybe I should split tVal_mat so I'm only running one factor at a time.
% Otherwise they're all on top of each other in an ROI?

%%%%% need to split the individual factors into their own cols. The end
%%%%% result should have seven rows and 123 cols (8+8+8+8+5+4) * 3 for all,
%%%%% remembered, and forgotten. 8+8+8+8+5+4 = 41
%%%%% So, the cols are encycl-all-F01, encycl-300-all-F01, etc


%%%% below here is for all only for NNMF since the all trials are sig for mem

tVal_indivFac = zeros(7,41);
selectCol = 1;
tVal_col = 0; %which col of tVal_indivFac to fill
for col = 1:6
    currCol = tVal_mat(:,selectCol);
    if col == 5 %fcn
        row = 1;
        for fac = tVal_col+1:tVal_col+5
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 5; %need to fill the next col in tVal_indivFac
    elseif col == 6 %fcn 300
        row = 1;
        for fac = tVal_col+1:tVal_col+4
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 4;
    else
        row = 1;
        for fac = tVal_col+1:tVal_col+8
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 8;
    end
    selectCol = selectCol + 1; %grab next col of tVal_mat
end

%%% now I have tVal_indivFac which is 7x41. In each col we have one mask
%%% and one factor
%save('tVal_indivFac_nnmf_allTrials.xlsx','tVal_indivFac')

mask_pSTS_L = load_untouch_nii('BNAmerged_ROI_pSTS_L.nii');

for currCol = 1:41
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * tVal_indivFac(currROI,currCol); % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end
    %cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23'

    %%% choose the cols that have cells where t>2 or t<-2, just to simplify
    if currCol == 1
        save_nii(fac,'encycl-all-F01_tmap.nii')
    elseif currCol == 3 
        save_nii(fac,'encycl-all-F03_tmap.nii')
    elseif currCol == 4 
        save_nii(fac,'encycl-all-F04_tmap.nii')
    elseif currCol == 6 
        save_nii(fac,'encycl-all-F06_tmap.nii')
    elseif currCol == 7 
        save_nii(fac,'encycl-all-F07_tmap.nii')
    elseif currCol == 20
        save_nii(fac,'vis-all-F04_tmap.nii')
    elseif currCol == 21
        save_nii(fac,'vis-all-F05_tmap.nii')
    elseif currCol == 24
        save_nii(fac,'vis-all-F08_tmap.nii')
    elseif currCol == 27
        save_nii(fac,'vis-300-all-F03_tmap.nii')
    elseif currCol == 32
        save_nii(fac,'vis-300-all-F08_tmap.nii')
    elseif currCol == 36
        save_nii(fac,'fcn-all-F04_tmap.nii')
    elseif currCol == 37
        save_nii(fac,'fcn-all-F05_tmap.nii')
    elseif currCol == 40
        save_nii(fac,'fcn-300-all-F03.nii')
    end %end if statement
end %end currCol loop 

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% below here is for all, remembered, and forgotten for FA

tVal_indivFac = zeros(7,123);
selectCol = 1;
tVal_col = 0; %which col of tVal_indivFac to fill
for col = 1:18 %18 total cols in tVal_fac
    currCol = tVal_mat(:,selectCol);
    if col == 5 || col == 11 || col == 17 %fcn
        row = 1;
        for fac = tVal_col+1:tVal_col+5
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 5; %need to fill the next col in tVal_indivFac
    elseif col == 6 || col == 12 || col == 18 %fcn 300
        row = 1;
        for fac = tVal_col+1:tVal_col+4
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 4;
    else
        row = 1;
        for fac = tVal_col+1:tVal_col+8
            tVal_indivFac(:,fac) = currCol(row:row+6);
            row = row+7;
        end
        tVal_col = tVal_col + 8;
    end
    selectCol = selectCol + 1; %grab next col of tVal_mat
end

%%% now I have tVal_indivFac which is 7x123. In each col we have one mask
%%% and one factor

for currCol = 1:123
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    fac = struct;
    fac.('hdr') = mask_AG_L.hdr;
    fac.('filetype') = 2;
    fac.('fileprefix') = 'whole';
    fac.('machine') = 'ieee-le';
    fac.('ext') = [];
    fac.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * tVal_indivFac(currROI,currCol); % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    fac.img(voxel) = multMask(voxel);
                end
            end
    end
    %cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23'

    %%% choose the cols that have cells where t>2 or t<-2, just to simplify
    if currCol == 1
        save_nii(fac,'encycl-all-F01_tmap.nii')
    elseif currCol == 2 
        save_nii(fac,'encycl-all-F02_tmap.nii')
    elseif currCol == 13 
        save_nii(fac,'encycl-300-all-F05_tmap.nii')
    elseif currCol == 19 
        save_nii(fac,'vis-all-F03_tmap.nii')
    elseif currCol == 20
        save_nii(fac,'vis-all-F04_tmap.nii')
    elseif currCol == 26 
        save_nii(fac,'vis-300-all-F02_tmap.nii')
    elseif currCol == 27
        save_nii(fac,'vis-300-all-F03_tmap.nii')
    elseif currCol == 29
        save_nii(fac,'vis-300-all-F05_tmap.nii')
    elseif currCol == 32
        save_nii(fac,'vis-300-all-F08_tmap.nii')
    elseif currCol == 36
        save_nii(fac,'fcn-all-F04_tmap.nii')
    elseif currCol == 42
        save_nii(fac,'encycl-remembered-F01_tmap.nii')
    elseif currCol == 43
        save_nii(fac,'encycl-remembered-F02_tmap.nii')
    elseif currCol == 47
        save_nii(fac,'encycl-remembered-F06_tmap.nii')
    elseif currCol == 50
        save_nii(fac,'encycl-300-remembered-F01_tmap.nii')
    elseif currCol == 52
        save_nii(fac,'encycl-300-remembered-F03_tmap.nii')
    elseif currCol == 54
        save_nii(fac,'encycl-300-remembered-F05_tmap.nii')
    elseif currCol == 61
        save_nii(fac,'vis-remembered-F04_tmap.nii')
    elseif currCol == 67
        save_nii(fac,'vis-300-remembered-F02_tmap.nii')
    elseif currCol == 73
        save_nii(fac,'vis-300-remembered-F08_tmap.nii')
    elseif currCol == 76
        save_nii(fac,'fcn-remembered-F03_tmap.nii')
    elseif currCol == 81
        save_nii(fac,'fcn-300-remembered-F03_tmap.nii')
    elseif currCol == 83
        save_nii(fac,'encycl-forgotten-F01_tmap.nii')
    elseif currCol == 93
        save_nii(fac,'encycl-300-forgotten-F03_tmap.nii')
    elseif currCol == 101
        save_nii(fac,'vis-forgotten-F03_tmap.nii')
    elseif currCol == 104
        save_nii(fac,'vis-forgotten-F06_tmap.nii')    
    elseif currCol == 106 
        save_nii(fac,'vis-forgotten-F08_tmap.nii')
    elseif currCol == 109
        save_nii(fac,'vis-300-forgotten-F01_tmap.nii')
    elseif currCol == 111
        save_nii(fac,'vis-300-forgotten-F03_tmap.nii')
    elseif currCol == 115 
        save_nii(fac,'fcn-forgotten-F01_tmap.nii')
    elseif currCol == 116
        save_nii(fac,'fcn-forgotten-F02_tmap.nii')
    elseif currCol == 118
        save_nii(fac,'fcn-forgotten-F04_tmap.nii')
    elseif currCol == 123
        save_nii(fac,'fcn-300-forgotten-F0f_tmap.nii')  
    end

end %end currCol loop 

%% mem
% load random mask for header info
% currCol: lexMem-all, visMem-all, lexMem-remembered, visMem-remembered, lexMem-forgotten, visMem-forgotten

for currCol = 1:6
    mask_AG_L = load_untouch_nii('BNAmerged_ROI_AG_L.nii');
    mem = struct;
    mem.('hdr') = mask_AG_L.hdr;
    mem.('filetype') = 2;
    mem.('fileprefix') = 'whole';
    mem.('machine') = 'ieee-le';
    mem.('ext') = [];
    mem.('img') = zeros(43,51,35);

    for currROI = 1:length(ROIlist) 
        ROI = cell2mat(ROIlist(currROI));
        currMask = load_untouch_nii(fullfile(maskdir,ROI));
        multMask = currMask.img * tVal_mat(currROI,currCol); % change this second number for the col i want
            for voxel = 1:numel(multMask)
                % use linear indexing
                if multMask(voxel) ~= 0 %not included in the atlas
                    mem.img(voxel) = multMask(voxel);
                end
            end
    end
    %cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/BNA_masks_jan23'
    if currCol == 1 %lexMem-all
        save_nii(mem,'lexMem-all_tmap.nii')
    elseif currCol == 2 %visMem-all
        save_nii(mem,'visMem-all_tmap.nii')
    elseif currCol == 3 %lexMem-remembered
        save_nii(mem,'lexMem-remembered_tmap.nii')
    elseif currCol == 4 %visMem-remembered
        save_nii(mem,'visMem-remembered_tmap.nii')
    elseif currCol == 5 %lexMem-forgotten
        save_nii(mem,'lexMem-forgotten_tmap.nii')
    elseif currCol == 6 %visMem-forgotten
        save_nii(mem,'visMem-forgotten_tmap.nii')
    end
end %end currCol loop 


%%%%% old loops from 2022 below
mask28_clean = load_untouch_nii('mask28_clean.nii');

% create rest of the struct
% conceptual 
mem = struct;
mem.('hdr') = mask28_clean.hdr;
mem.('filetype') = 2;
mem.('fileprefix') = 'whole';
mem.('machine') = 'ieee-le';
mem.('ext') = [];
mem.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,4); % change this second number for the col i want
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                mem.img(voxel) = multMask(voxel);
            end
        end

end
%cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visLexMem_re-do/allSubj_lex/mapFiles';
%save_nii(conMem,'conMem.nii')

cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas'
save_nii(bothMem,'bothMem_tmap.nii')


cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/'
save_nii(mem,'lexMem_tmap.nii')
save_nii(mem,'cmem_tmap.nii')
save_nii(mem,'visMem_tmap.nii')
save_nii(mem,'pmem_tmap.nii')



mask28_clean = load_untouch_nii('mask28_clean.nii');

% create rest of the struct
factor_01 = struct;
factor_01.('hdr') = mask28_clean.hdr;
factor_01.('filetype') = 2;
factor_01.('fileprefix') = 'whole';
factor_01.('machine') = 'ieee-le';
factor_01.('ext') = [];
factor_01.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 1;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
   % factor_01.img = factor_01.img + multMask;
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_01.img(voxel) = multMask(voxel);
            end
        end

end

% make sure to save the nifti files in this folder
% pick one of these
%cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/factor_brainMaps/;
%cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/factor_maps_clean_oneBigModel/;
%cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/factor_maps_clean_tenIndiv/;
cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/factorMaps_clustersOnly/;
cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/bothVisLex_CR_betas/factorMaps_memAtlasROIsOnly/;


save_nii(factor_01,'factor_01.nii')


% mem
mask28_clean = load_untouch_nii('mask28_clean.nii');

% create rest of the struct
% conceptual 
conMem = struct;
conMem.('hdr') = mask28_clean.hdr;
conMem.('filetype') = 2;
conMem.('fileprefix') = 'whole';
conMem.('machine') = 'ieee-le';
conMem.('ext') = [];
conMem.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,1); % col 1 for conceptual
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                conMem.img(voxel) = multMask(voxel);
            end
        end

end
cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visLexMem_re-do/allSubj_lex/mapFiles';
%save_nii(conMem,'conMem.nii')
save_nii(conMem,'conMem_oneBig.nii')

% perceptual
cd (maskdir)

mask28_clean = load_untouch_nii('mask28_clean.nii');
% create rest of the struct
perMem = struct;
perMem.('hdr') = mask28_clean.hdr;
perMem.('filetype') = 2;
perMem.('fileprefix') = 'whole';
perMem.('machine') = 'ieee-le';
perMem.('ext') = [];
perMem.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,2); % col 2 for perceptual
         for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                perMem.img(voxel) = multMask(voxel);
            end
        end

end

cd '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visLexMem_re-do/allSubj_vis/mapFiles';
%save_nii(perMem,'perMem.nii')
save_nii(perMem,'perMem_oneBig.nii')

% create rest of the struct
factor_02 = struct;
factor_02.('hdr') = mask28_clean.hdr;
factor_02.('filetype') = 2;
factor_02.('fileprefix') = 'whole';
factor_02.('machine') = 'ieee-le';
factor_02.('ext') = [];
factor_02.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 2;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_02.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_02,'factor_02.nii')

% create rest of the struct
factor_03 = struct;
factor_03.('hdr') = mask28_clean.hdr;
factor_03.('filetype') = 2;
factor_03.('fileprefix') = 'whole';
factor_03.('machine') = 'ieee-le';
factor_03.('ext') = [];
factor_03.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 3;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_03.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_03,'factor_03.nii')

% create rest of the struct
factor_04 = struct;
factor_04.('hdr') = mask28_clean.hdr;
factor_04.('filetype') = 2;
factor_04.('fileprefix') = 'whole';
factor_04.('machine') = 'ieee-le';
factor_04.('ext') = [];
factor_04.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 4;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_04.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_04,'factor_04.nii')

% create rest of the struct
factor_05 = struct;
factor_05.('hdr') = mask28_clean.hdr;
factor_05.('filetype') = 2;
factor_05.('fileprefix') = 'whole';
factor_05.('machine') = 'ieee-le';
factor_05.('ext') = [];
factor_05.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 5;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_05.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_05,'factor_05.nii')

% create rest of the struct
factor_06 = struct;
factor_06.('hdr') = mask28_clean.hdr;
factor_06.('filetype') = 2;
factor_06.('fileprefix') = 'whole';
factor_06.('machine') = 'ieee-le';
factor_06.('ext') = [];
factor_06.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 6;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_06.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_06,'factor_06.nii')


% create rest of the struct
factor_07 = struct;
factor_07.('hdr') = mask28_clean.hdr;
factor_07.('filetype') = 2;
factor_07.('fileprefix') = 'whole';
factor_07.('machine') = 'ieee-le';
factor_07.('ext') = [];
factor_07.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 7;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_07.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_07,'factor_07.nii')

% create rest of the struct
factor_08 = struct;
factor_08.('hdr') = mask28_clean.hdr;
factor_08.('filetype') = 2;
factor_08.('fileprefix') = 'whole';
factor_08.('machine') = 'ieee-le';
factor_08.('ext') = [];
factor_08.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 8;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_08.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_08,'factor_08.nii')

% create rest of the struct
factor_09 = struct;
factor_09.('hdr') = mask28_clean.hdr;
factor_09.('filetype') = 2;
factor_09.('fileprefix') = 'whole';
factor_09.('machine') = 'ieee-le';
factor_09.('ext') = [];
factor_09.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 9;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_09.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_09,'factor_09.nii')

% create rest of the struct
factor_10 = struct;
factor_10.('hdr') = mask28_clean.hdr;
factor_10.('filetype') = 2;
factor_10.('fileprefix') = 'whole';
factor_10.('machine') = 'ieee-le';
factor_10.('ext') = [];
factor_10.('img') = zeros(43,51,35);

% this combines all of the masks that go into one factor
factor = 10;
for currROI = 1:length(ROIlist) % ROIlist goes from mask28 to mask41

    ROI = cell2mat(ROIlist(currROI));
    currMask = load_untouch_nii(fullfile(maskdir,ROI));
    multMask = currMask.img * tVal_mat(currROI,factor);
        for voxel = 1:numel(multMask)
            % use linear indexing
            if multMask(voxel) ~= 0 %not included in the atlas
                factor_10.img(voxel) = multMask(voxel);
            end
        end

end

save_nii(factor_10,'factor_10.nii')

%% failed stuff below

%% make brain map of IRAF values
% load atlas
%V = spm_vol([rootDir, '/BNA_thr25_resliced_43_51_35.nii']);
%nvoxel = prod(V.dim);


% load my visLex interaction masks (28 through 41, or skip 41 and do 28-40)
% these masks have 1s and 0s for in or out of ROI
% need to multiply these by integer so you have unique ints for each mask
% need table of masks and PCs/Fs where number of cols = unique numbers in
% atlas. 
% The atlas is the mergedMemROIs that I then feed into the script that
% adds color

% this is a test. Can I combine my int-mult masks into one nifti file
% after that I do something with the t-values


%addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/indiv_masks_resliced/

%% make integer maps and combine masks
% load random mask for header info
%PcunL_1 = load_untouch_nii('Pcun_L_4_1.nii');

% mask28_clean = load_untouch_nii('mask28_clean.nii');
% % create rest of the struct
% fullbrain = struct;
% %fullbrain.('hdr') = PcunL_1.hdr;
% fullbrain.('hdr') = mask28_clean;
% fullbrain.('filetype') = 2;
% fullbrain.('fileprefix') = 'whole';
% fullbrain.('machine') = 'ieee-le';
% fullbrain.('ext') = [];
% fullbrain.('img') = zeros(43,51,35);
% 
% % find the mask voxels and multiply by different integers
% for currROI = 1:length(ROIlist)
% 
%     ROI = cell2mat(ROIlist(currROI));
%     currMask = load_untouch_nii(fullfile(maskdir,ROI));
%     multMask = currMask.img * currROI;
%     fullbrain.img = fullbrain.img + multMask;

%  end


%% Shenyang code follows below

% fullbrain now has ints where the masks have values
save_nii(fullbrain,'fullbrain.nii')

% write nifti
for RDM = RDMs; RDM = RDM{1};
    %fullbrain.(RDM) = nan(nvoxel, 1);
    % get t values from R result table
    load([rootDir '/brainnetome/BNA246_LOC.mat']);
    for iROI = 1:length(LOC)
        roi = LOC(iROI).roi;
        t = Step4_df_rsa_result.(['t_' RDM])(contains(Step4_df_rsa_result.ROI, LOC(iROI).name));
        fullbrain.(RDM)(roi) = repmat(t, length(roi), 1);
    end

    % reshape brain to 3D
    fullbrain.(RDM) = reshape(fullbrain.(RDM), V.dim(1), V.dim(2), V.dim(3));

    % write nifti
    V.fname = sprintf('%s/%s/Step4_RSA_vggonly/Step4_df_rsa_lmer_regtime_all_%s_%s.nii', rootDir, fig_path, 'Encoding', RDM);
    V.private.dat.fname = V.fname;
    V.descrip = '';
    spm_write_vol(V, fullbrain.(RDM));
    fprintf('----Step 4 created brain map for RSA results with RDM %s\n', RDM);
end




% phases = {'Encoding' 'Retrieval'};
phases = {'Encoding'};

Step4_df_rsa_result = readtable('data/Step4/Networks/Step4_df_rsa_lmer_regtime_all_vggonly.csv');
fig_path = 'Figures_regtime';
if ~exist([rootDir '/' fig_path '/Step4_RSA_vggonly'], 'dir')
    mkdir([rootDir '/' fig_path '/Step4_RSA_vggonly']);
end

% get RDMs from results table
varnames = Step4_df_rsa_result.Properties.VariableNames;
RDMs = extractAfter(varnames(startsWith(varnames, 't_')), 't_');
