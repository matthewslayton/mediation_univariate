% PCs_betas_lmer_customROIs.m is the script that has all of the factors and
% betas and outputs to spreadsheets. Let's try nonnegative matrix
% factorization as an alternative to Factor Analysis. Both FA and PCA give
% positive and negative loadings and aren't really meant for sparse data.
% NMF gives only positive, identifies latent factor, and is meant to reduce
% dimensionality on sparse datasets. 

% https://blog.acolyer.org/2019/02/18/the-why-and-how-of-nonnegative-matrix-factorization/
% https://danhdtruong.com/Non-negative-Matrix-Factorization/
% https://www.mathworks.com/help/stats/nnmf.html

% [W,H] = nnmf(A,k)
% W and H are non-negative factors, which are a lower-rank approximation of
% matrix A. k determines the size of these factors, meaning it's the 'rank'
% of the factors
% W is the features matrix (i.e. basis)
% H coefficient matrix

% in FA, loadings are like coeff in PCA and factor score are like PCA score
% That would suggest that H is the loadings (so it tells us how much each
% feature loads on that factor) and W is like the score, so I use that in
% lmer()


set(0,'defaultfigurecolor',[1 1 1]) %set background of plot to white
%cd /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts;
addpath '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/'
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/Scripts/spm12');
addpath('/Users/matthewslayton/Documents/Duke/Simon_Lab/function_files')
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/;
addpath /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v3/;

% what about the raw matrices?
% these are made in make_PCs_features.m
load featMat_encycl.mat %995x1930
load featMat_vis.mat %995x1886
load featMat_fcn.mat %995x1101
load featMat_encycl_300 %300x1930
load featMat_vis_300.mat %300x1886
load featMat_fcn_300.mat %300x1101
load featMat_encycl_330.mat
load featMat_vis_330.mat
load featMat_fcn_330.mat
load featMat_encycl_360 %300x1930
load featMat_vis_360.mat %300x1886
load featMat_fcn_360.mat %300x1101
load justFeaturesArray.mat %995x5520
featMat_all = justFeaturesArray;
load featMat_all_300.mat
load featMat_all_360.mat
load featMat_all_330.mat
load featMat_outside.mat
load featMat_home.mat
load featMat_home_tool.mat
load featMat_animal.mat
load featMat_food.mat
load featMat_outside_300.mat
load featMat_home_300.mat
load featMat_home_tool_300.mat
load featMat_animal_300.mat
load featMat_food_300.mat



load featNames_encycl.mat
load featNames_vis.mat
load featNames_fcn.mat
load featNames_all.mat

featNames_encycl = featNames_encycl';
featNames_vis = featNames_vis';
featNames_fcn = featNames_fcn';

%%% how many zeros in the feature norm matrix?
zero_count = nnz(justFeaturesArray == 0);
total_elements = numel(justFeaturesArray);
sparsity_ratio = zero_count/total_elements;



% choose one, we're doing this manually
% currMat = featMat_encycl;
% currMat = featMat_encycl_300;
% currMat = featMat_encycl_360;
% currMat = featMat_vis;
% currMat = featMat_vis_300;
% currMat = featMat_vis_360;
% currMat = featMat_fcn;
% currMat = featMat_fcn_300;
% currMat = featMat_fcn_360;

[W,H] = nnmf(currMat,4); %5 for fcn, 4 for fcn_300, 8 for the rest? it's what I got from vss for FA
H = H'; %I want features in row direction so I can sort the eight cols

% manually paste into nnmf_results.xlsx. Sort and paste into
% nnmf_factorLoadings.xlsx

%%%% can normalize entire matrix, or better yet, normalize each row.
%%%% That means you're normalizing on a per-object basis. 
%%%% miles: that way you can interpret each bin for each object basically 
%%%% as "frequency of object description as this", which may also make the
%%%% nmf more interpretable"
%%%% if the only other solution would be to run NMF for more iterations
%%%% because of the number of features, maybe I should just threshold and
%%%% remove some of them

% normalize by row
norm_animal = normr(featMat_animal);

% threshold
featMat_animal_thres = featMat_animal(:,sum(featMat_animal,1)>=20);


%%%% **** rather than cutoff, what if more than 5% of Ps responded "has
%%%% legs" then you can include it. Total the responses per row and then
%%%% just pick a cutoff based on a percentage
%%%% then just say you need 80 factors. 
% So, I need a row sum and then ask what percentage a given cell is of the
% total. Though, it sounds like it'd be item-specific, so maybe I take an
% average? I want to cut columns ultimately.

rowSum_test = sum(featMat_encycl,2);
colSum_test = sum(featMat_encycl,1);

% colSum_test tells me how many 'votes' each feature got. Can also just
% count the non-zeros, but for now let's look at votes. For example, the
% first col has 1764. It's "is useful." Now, the question is, what do I do?
% Ah! How many Ps gave votes? 553 Ps gave five votes each. That means we
% had 2765 total votes. So, "is useful" got 63.8% of the possible votes it
% could have gotten, so it's WAY over threshold. The question is, how many
% votes is too few? 5% of 2765 is 138.25. So, If you got <139 votes, you're
% too few. 

% before I was doing >=20, so how many cols are left?
featMat_encycl_thres = featMat_encycl(:,sum(featMat_encycl,1)>=20);
% this leaves 228 columns. But, if I wanted to cut everything that got
% fewer than 5%, I think my cutoff is 139!!
featMat_encycl_thres = featMat_encycl(:,sum(featMat_encycl,1)>=139);
% leave 26 columns. Seem like too few
% this would be a 2% cutoff. Gives 67 cols
featMat_encycl_thres = featMat_encycl(:,sum(featMat_encycl,1)>=56);
% this would be a 1% cutoff. Gives 155 cols
featMat_encycl_thres = featMat_encycl(:,sum(featMat_encycl,1)>=28);


%%% hm, how many total votes would the 300 items get?
featMat_encycl_300_thres = featMat_encycl_300(:,sum(featMat_encycl_300,1)> 28);


% featMat_vis_thres = featMat_vis(:,sum(featMat_vis,1)>=20);
% featMat_fcn_thres = featMat_fcn(:,sum(featMat_fcn,1)>=20);

%%%% how many total vote were there? Well, 553 Ps, five votes each, times
%%%% the number of images.
%%%% Or, you can just do sum(featureMat,'all') which is the same as suming
%%%% the row or col sums

% sum(featMat_all,'all') = 118,930
% sum(featMat_all_300,'all') = 36840

%%% in Mariam's paper she did do preprocessing, which included
%%% collapsing/editing responses. Doesn't say how, but it may be best to
%%% just derive the total votes per matrix from the matrix itself. 

% assuming no preprocessing, which there was, we'd have 553*5*40

featMat_all_thres = featMat_all(:,sum(featMat_all,1)>=5946);

test = sum(featMat_all,1);

%%% what shoud the cutoff be? Well, Shenyang's point is that 995 objects
%%% had 29 categories and 300 objects had 12 categories, so threshold
%%% should not be greater than 12/300 or 29/995, which is .0291 and .04
%%% The point being that each category should have an equal number of
%%% votes. Any features that are lower can be removed. It's arbitrary, but
%%% it does make sense given the structure of our dataset.
% but, there might be meaningful features that have low votes, like
% separating birds that fly from birds that don't, so you have to play
% around with thresholds. 

%%% we have featMat all, encycl, vis, fcn 995 and 300, 330, 360
%%% then animal, food, home, home tool, outside 995 and 300
% The 995's should be 4%. What about 1%?
% The 300s should be 2.91% so let's do 0.7%
% (all one fourth which just feels arbitrarily more reasonable based on the column sums)

% these are TOO HIGH. What about half? Just divide these by half
% sum(featMat_all,'all') %118,930 = 1189.3 = 594.65
% sum(featMat_all_300,'all') %36,840 = 257.88 = 128.94
% sum(featMat_all_330,'all') %8603 = 60.221 = 30.1105
% sum(featMat_all_360,'all') %9233 = 64.63 = 32.315
% sum(featMat_encycl,'all') %27623 = 276.23
% sum(featMat_encycl_300,'all') %7986 = 55.902
% sum(featMat_encycl_330,'all') %8803 = 61.621
% sum(featMat_encycl_360,'all') %9571 = 66.997
% sum(featMat_vis,'all') %60898 = 608.98
% sum(featMat_vis_300,'all') %18573 = 130.011
% sum(featMat_vis_330,'all') %20346 = 142.422
% sum(featMat_vis_360,'all') %22195 = 155.365
% sum(featMat_fcn,'all') %10975 = 109.75
% sum(featMat_fcn_300,'all') %2555 = 17.885
% sum(featMat_fcn_330,'all') %2779 = 19.453
% sum(featMat_fcn_360,'all') %3011 = 21.077
% sum(featMat_animal,'all') %20619 = 206.19
% sum(featMat_animal_300,'all') %11697 = 81.879
% sum(featMat_food,'all') %12935 = 129.35
% sum(featMat_food_300,'all') %11789 = 82.523
% sum(featMat_home,'all') %33902 = 339.02
% sum(featMat_home_300,'all') %10136 = 70.952
% sum(featMat_home_tool,'all') %28513 = 285.13
% sum(featMat_home_tool_300,'all') %9705 = 67.935
% sum(featMat_outside,'all') %21051 = 210.51
% sum(featMat_outside_300,'all') %12825 = 89.775

% honestly though, why would I use the 995? Who cares what the data set is?
% The Ps only saw 330 or 360, so.... just use that! 

cutoff_995 = .04/20;
cutoff_300 = .0291/20;
all_cutoff = sum(featMat_all,'all') * cutoff_995;
all_300_cutoff = sum(featMat_all_300,'all') * cutoff_300; 
all_330_cutoff = sum(featMat_all_330,'all') * cutoff_300; 
all_360_cutoff = sum(featMat_all_360,'all') * cutoff_300; 

encycl_cutoff = sum(featMat_encycl,'all') * cutoff_995;
encycl_300_cutoff = sum(featMat_encycl_300,'all') * cutoff_300; 
encycl_330_cutoff = sum(featMat_encycl_330,'all') * cutoff_300; 
encycl_360_cutoff = sum(featMat_encycl_360,'all') * cutoff_300; 
vis_cutoff = sum(featMat_vis,'all') * cutoff_995;
vis_300_cutoff = sum(featMat_vis_300,'all') * cutoff_300; 
vis_330_cutoff = sum(featMat_vis_330,'all') * cutoff_300; 
vis_360_cutoff = sum(featMat_vis_360,'all') * cutoff_300; 
fcn_cutoff = sum(featMat_fcn,'all') * cutoff_995; 
fcn_300_cutoff = sum(featMat_fcn_300,'all') * cutoff_300; 
fcn_330_cutoff = sum(featMat_fcn_330,'all') * cutoff_300; 
fcn_360_cutoff = sum(featMat_fcn_360,'all') * cutoff_300; 
animal_cutoff = sum(featMat_animal,'all') * cutoff_995; 
animal_300_cutoff = sum(featMat_animal_300,'all') * cutoff_300; 
food_cutoff = sum(featMat_food,'all') * cutoff_995;
food_300_cutoff = sum(featMat_food_300,'all') * cutoff_300; 
home_cutoff = sum(featMat_home,'all') * cutoff_995; 
home_300_cutoff = sum(featMat_home_300,'all') * cutoff_300; 
home_tool_cutoff = sum(featMat_home_tool,'all') * cutoff_995; 
home_tool_300_cutoff = sum(featMat_home_tool_300,'all') * cutoff_300; 
outside_cutoff = sum(featMat_outside,'all') * cutoff_995; 
outside_300_cutoff = sum(featMat_outside_300,'all') * cutoff_300;


%%% what about just colSum vs all sum?

sum(featMat_encycl,'all')
cols = sum(featMat_encycl,1);

% thing is, colSum goes in reverse order because the matrix is already in
% reverse order! So, you end up removing the ones on the end, but you don't
% necessarily get a cutoff value. Could be a percentage of the largest
% term? For example, first col of featMat_encycl sums to 1764. 4% of that
% is 70.56

test_thres = featMat_encycl(:,sum(featMat_encycl,1)>=71);
% has 52 cols

% cutoff_995 = .04/20;
% cutoff_300 = .0291/20;
% all_cutoff = sum(featMat_all,'all') * cutoff_995;
% all_300_cutoff = sum(featMat_all_300,'all') * cutoff_300; 
% all_330_cutoff = sum(featMat_all_330,'all') * cutoff_300; 
% all_360_cutoff = sum(featMat_all_360,'all') * cutoff_300; 


featMat_all_thres = featMat_all(:,sum(featMat_all,1)>=all_cutoff);
featMat_all_300_thres = featMat_all_300(:,sum(featMat_all_300,1)>=all_300_cutoff);
featMat_all_330_thres = featMat_all_330(:,sum(featMat_all_330,1)>=all_330_cutoff);
featMat_all_360_thres = featMat_all_360(:,sum(featMat_all_360,1)>=all_360_cutoff);
featMat_encycl_thres = featMat_encycl(:,sum(featMat_encycl,1)>=encycl_cutoff);
featMat_encycl_300_thres = featMat_encycl_300(:,sum(featMat_encycl_300,1)>=encycl_300_cutoff);
featMat_encycl_330_thres = featMat_encycl_330(:,sum(featMat_encycl_330,1)>=encycl_330_cutoff);
featMat_encycl_360_thres = featMat_encycl_360(:,sum(featMat_encycl_360,1)>=encycl_360_cutoff);
featMat_vis_thres = featMat_vis(:,sum(featMat_vis,1)>=vis_cutoff);
featMat_vis_300_thres = featMat_vis_300(:,sum(featMat_vis_300,1)>=vis_300_cutoff);
featMat_vis_330_thres = featMat_vis_330(:,sum(featMat_vis_330,1)>=vis_330_cutoff);
featMat_vis_360_thres = featMat_vis_360(:,sum(featMat_vis_360,1)>=vis_360_cutoff);
featMat_fcn_thres = featMat_fcn(:,sum(featMat_fcn,1)>=fcn_cutoff);
featMat_fcn_300_thres = featMat_fcn_300(:,sum(featMat_fcn_300,1)>=fcn_300_cutoff);
featMat_fcn_330_thres = featMat_fcn_330(:,sum(featMat_fcn_330,1)>=fcn_330_cutoff);
featMat_fcn_360_thres = featMat_fcn_360(:,sum(featMat_fcn_360,1)>=fcn_360_cutoff);
featMat_animal_thres = featMat_animal(:,sum(featMat_animal,1)>=animal_cutoff);
featMat_animal_300_thres = featMat_animal_300(:,sum(featMat_animal_300,1)>=animal_300_cutoff);
featMat_food_thres = featMat_food(:,sum(featMat_food,1)>=food_cutoff);
featMat_food_300_thres = featMat_food_300(:,sum(featMat_food_300,1)>=food_300_cutoff);
featMat_home_thres = featMat_home(:,sum(featMat_home,1)>=home_cutoff);
featMat_home_300_thres = featMat_home_300(:,sum(featMat_home_300,1)>=home_300_cutoff);
featMat_home_tool_thres = featMat_home_tool(:,sum(featMat_home_tool,1)>=home_tool_cutoff);
featMat_home_tool_300_thres = featMat_home_tool_300(:,sum(featMat_home_tool_300,1)>=home_tool_300_cutoff);
featMat_outside_thres = featMat_outside(:,sum(featMat_outside,1)>=outside_cutoff);
featMat_outside_300_thres = featMat_outside_300(:,sum(featMat_outside_300,1)>=outside_300_cutoff);



%%% check to see if/where error levels off (you want to reduce error in NMF)
%currMat = featMat_animal_thres;
currMat = featMat_all_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
title('How Well Does Reduced Matrices Match Original')
xlabel('How Many Factors Included','FontSize',14)
ylabel('Residuals (error)','FontSize',14)

%%% we want to see how important an individual factor is. Miles says to do
%%% MSE without the factor divided by MSE with the full matrix
%%% quantified as what the percent increase of MSE is
% before i want to know what the error D is. I suppose we want the MSE
% between W and H

%%% what percentage does error increase by eliminating information about this factor
%%% so if W_i is W with column i set to zeros,
%%% for each i, do MSE(W_i * H,A)/MSE(W*H,A)

currMat = featMat_all_thres;
% A is the original matrix
A = currMat;
%fac = size(currMat,2);
fac = 20;
allFeat_err = zeros(5,1);
for col = 1:5
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    mse_orig = norm((W*H)-A,'fro')^2/numel(W*H);
    %mse_orig = immse(W*H,A);
    W(:,col) = zeros(size(W,1),1);
    mse_zero = norm((W*H)-A,'fro')^2/numel(W*H);
    %mse_zero = immse(W*H,A);
    percent_increase = mse_zero/mse_orig;
    disp(percent_increase)
    allFeat_err(col) = percent_increase;
end

currColor = [.3 .7 .4];
bar(allFeat_err,0.3,'FaceColor',currColor,'EdgeColor',currColor)
title('How Important is Each Factor?','FontSize',20)
ylabel('Percent Increase in MSE','FontSize',14)
xlabel('Individual Factor Removed','FontSize',14)




figure
currMat = featMat_all_300_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_encycl_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_encycl_300_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_vis_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_vis_300_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_fcn_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure
currMat = featMat_fcn_300_thres;
D_arr = zeros(size(currMat,2),1);
for fac = 1:size(currMat,2)
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)
figure






currMat = norm_animal;
D_arr = zeros(100,1);
for fac = 1:100 
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)

%%%% ^trouble is, D is not really leveling off. Can normalize the matrix
%%%% first, so what's the max value? Divide everything by it so all the
%%%% numbers are between 0 and 1

maxValPerRow = zeros(size(featMat_encycl,1),1);
for row = 1:size(featMat_encycl,1)
   maxValPerRow(row) = max(featMat_encycl(row,:));
end

totalMax = max(maxValPerRow); % this is 30

featMat_encycl_norm = featMat_encycl./totalMax;

currMat = featMat_encycl_norm;
D_arr = zeros(100,1);
for fac = 1:100 
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)

figure
imagesc(currMat-W*H)




currMat = featMat_all_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_all = W;
save('W_all.mat','W_all')

currMat = featMat_all_300_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_all_300 = W;
save('W_all_300.mat','W_all_300')

currMat = featMat_all_330_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_all_330 = W;
save('W_all_330.mat','W_all_330')

currMat = featMat_all_300_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_all_360 = W;
save('W_all_360.mat','W_all_360')

currMat = featMat_encycl_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); %limits to however many cols there are after thresholding
W_encycl = W;
H_encycl = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_encycl.mat'),'W_encycl')
save(strcat(path,'H_encycl.mat'),'H_encycl')

% try limiting the number of columns
currMat = featMat_encycl;
[W,H] = nnmf(currMat,10,'algorithm','als');
W_encycl_10 = W;
H_encycl_10 = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_encycl_10.mat'),'W_encycl_10')
save(strcat(path,'H_encycl_10.mat'),'H_encycl_10')


%[W_als,H_als] = nnmf(currMat,8,'algorithm','als');

currMat = featMat_encycl_300_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_encycl_300 = W;
save('W_encycl_300.mat','W_encycl_300')

currMat = featMat_encycl_360_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_encycl_360 = W;
save('W_encycl_360.mat','W_encycl_360')

currMat = featMat_encycl_330_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als');
W_encycl_330 = W;
save('W_encycl_330.mat','W_encycl_330')

currMat = featMat_vis_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_vis = W;
H_vis = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_vis.mat'),'W_vis')
save(strcat(path,'H_vis.mat'),'H_vis')

currMat = featMat_vis;
[W,H] = nnmf(currMat,10,'algorithm','als'); 
W_vis_10 = W;
H_vis_10 = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_vis_10.mat'),'W_vis_10')
save(strcat(path,'H_vis_10.mat'),'H_vis_10')

currMat = featMat_vis_300_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_vis_300 = W;
save('W_vis_300.mat','W_vis_300')

currMat = featMat_vis_360_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_vis_360 = W;
save('W_vis_360.mat','W_vis_360')

currMat = featMat_vis_330_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_vis_330 = W;
save('W_vis_330.mat','W_vis_330')

currMat = featMat_fcn_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_fcn = W;
H_fcn = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_fcn.mat'),'W_fcn')
save(strcat(path,'H_fcn.mat'),'H_fcn')

currMat = featMat_fcn;
[W,H] = nnmf(currMat,10,'algorithm','als'); 
W_fcn_10 = W;
H_fcn_10 = H;
path = '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_nmf_26ROI/';
save(strcat(path,'W_fcn_10.mat'),'W_fcn_10')
save(strcat(path,'H_fcn_10.mat'),'H_fcn_10')

currMat = featMat_fcn_300_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_fcn_300 = W;
save('W_fcn_300.mat','W_fcn_300')

currMat = featMat_fcn_360_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_fcn_360 = W;
save('W_fcn_360.mat','W_fcn_360')

currMat = featMat_fcn_330_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_fcn_330 = W;
save('W_fcn_330.mat','W_fcn_330')

currMat = featMat_outside_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_outside = W;
save('W_outside.mat','W_outside')

currMat = featMat_outside_300_thres;
[W,H] = nnmf(currMat,size(currMat,1),'algorithm','als'); %have to switch to rows because there are fewer rows than cols
W_outside_300 = W;
save('W_outside_300.mat','W_outside_300')

currMat = featMat_home_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_home = W;
save('W_home.mat','W_home')

currMat = featMat_home_300_thres;
[W,H] = nnmf(currMat,size(currMat,1),'algorithm','als'); 
W_home_300 = W;
save('W_home_300.mat','W_home_300')

currMat = featMat_home_tool_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_home_tool = W;
save('W_home_tool.mat','W_home_tool')

currMat = featMat_home_tool_300_thres;
[W,H] = nnmf(currMat,size(currMat,1),'algorithm','als'); 
W_home_tool_300 = W;
save('W_home_tool_300.mat','W_home_tool_300')

currMat = featMat_animal_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_animal = W;
save('W_animal.mat','W_animal')

currMat = featMat_animal_300_thres;
[W,H] = nnmf(currMat,size(currMat,1),'algorithm','als'); 
W_animal_300 = W;
save('W_animal_300.mat','W_animal_300')

currMat = featMat_food_thres;
[W,H] = nnmf(currMat,size(currMat,2),'algorithm','als'); 
W_food = W;
save('W_food.mat','W_food')

currMat = featMat_food_300_thres;
[W,H] = nnmf(currMat,size(currMat,1),'algorithm','als'); 
W_food_300 = W;
save('W_food_300.mat','W_food_300')



%%%%%%%%% can also do other algorithms

% default is iterative algorithm starting with random initial values for W and H

% multiplicative algorithm
[W_mult,H_mult] = nnmf(currMat,8,'Algorithm','mult');
H_mult = H_mult'; 
W_encycl_mult = W_mult;
save('W_encycl_mult.mat','W_encycl_mult')

% alternating least squares
[W_als,H_als] = nnmf(currMat,8,'algorithm','als');
H_als = H_als'; 
W_encycl_als = W_als;
save('W_encycl_als.mat','W_encycl_als')

%%%%%% HEAT MAP FOR ERROR
% convincing reviewers that the level of error is fine (something like a heatmap 
% colored by size of A -W*H, or plotting the magnitude of each bin divided 
% by the size of the error in that bin)

heatmap(tbl,xvar,yvar)

% let's try featMat_encycl for example
% when I plot the loss D, that's telling you the average sqrt difference per cell of your matrix
% So,  you can keep plotting D for an increasing number of factors to see
% how much loss there is per bin.
%%% also remember that NMF takes a matrix A and makes an approximate of it
% with two mats W and H. So, you can look at the difference between A and
% the approximation like this: (A - W*H).
% So, you can get that value for each number of factors and generate a
% heatmap to depict how much error you're saving per factor

%%% You want to compare the size of the error to the size of the thing you're 
% trying to match. If the error is large where things are small, you're only 
% really missing elements of your original matrix that are small, which would 
% be more okay for your analysis. If it is large where things are large, that's 
% more problematic and you%d likely want to include more factors




currMat = featMat_encycl;
diff_arr = zeros(100,1);
for fac = 1:100 
    [W,H] = nnmf(currMat,fac,'algorithm','als');
    diff_arr(fac) = currMat - W*H;
end

test2 = currMat./(currMat-W*H);
test3 = currMat./(currMat-W*H);

% *** where are the errors?
% 1. maybe you don't have enough factors and getting big errors
% 2. maybe your reconstruction is actually good. Maybe your errors are
% proportional to the feature counts in the original matrix
% 3. things are bad in general and you see errors everywhere with no rhyme
% or reason

heatmap(array2table(featMat_encycl),feat,item)

[W,H] = nnmf(currMat,fac,'algorithm','als');
figure
imagesc(currMat-W*H)
%%% the more factors we add, the largest errors are smaller, but there are
%%% still errors. The perfect NMF results would look like just blue. No
%%% errors (so, all around 0)

%A./(A - W*H) 


[W,H] = nnmf(currMat,8,'algorithm','als');
W_encycl = W;
save('W_encycl.mat','W_encycl')

currMat = featMat_animal_thres;
D_arr = zeros(100,1);
for fac = 1:100 
    [W,H,D] = nnmf(currMat,fac,'algorithm','als');
    D_arr(fac) = D;
end
plot(D_arr)


%%%%%%%%%%%
%%%% miles feedback
% % threshold
featMat_animal_thres = featMat_animal(:,sum(featMat_animal,1)>=20);


%%%% **** rather than cutoff, what if more than 5% of Ps responded "has
%%%% legs" then you can include it. Total the responses per row and then
%%%% just pick a cutoff based on a percentage. That'd data-driven
%%%% then just say you need 80 factors and show that error plot where it
%%%% levels off. Say that because the factors appear to indicate semantics
%%%% and so we wanted to include them in the model. "dropped rare features
%%%% based on 5% threshold. Fewer than 5% of Ps used X semantic label,
%%%% dropped from feature analysis"
% [W,H] = nnmf(currMat,fac,'algorithm','als');
% figure
% imagesc(currMat-W*H)
%%%% can add supp figure with 80 factors


