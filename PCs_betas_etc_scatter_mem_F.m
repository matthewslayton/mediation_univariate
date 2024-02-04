% remember PCs_betas_lmer_customROIs.m has a bunch of different stuff
% it might be better to copy and paste the different components, both to make it more organized
% and MAYBE, god willing, that I can freaking just hit Run instead of scrolling to find the 
% end of the loop to run only the part that I want

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