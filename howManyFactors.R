library(R.matlab) #read in mat files
library(tidyverse)
library(dplyr)
library(psycho) #psycho package to use n_factors()
library(RcmdrMisc)
library(openxlsx)
library(data.table)
library(psych)
# if you don't have something, like psycho, do install.packages("psycho")
# or install.packages( ) and select from the list
# https://github.com/neuropsychology/psycho.R
# https://rpubs.com/pjmurphy/758265

# /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FA_results

# /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/

# loads as list
#### note: these have not been thresholded. Just raw
featMat_tax <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_tax.mat")
featMat_encycl <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_encycl.mat")
featMat_fcn <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_fcn.mat")
featMat_vis <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_vis.mat")
featMatNoTax <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMatNoTax.mat")
#featMat_mergedEncycl <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/feat_merged_encycl_noNeg.mat")
featMat_mergedEncycl <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/merged_encycl.mat")
featMat_all <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/justFeaturesArray.mat")
#featMat_all_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featCounts_thres.mat")

# justFeaturesArray.mat is the raw feature matrix 995x5520
# featCounts_thres.mat 995x2460
# Threshold based on number of items that were assigned a given feature.
# helps remove features that only apply to a handful of items

# feat_fmri_merged_encycl.mat has six cols of concept names and ID. Need cols 7 to end

# convert list to data frame
data <- lapply(featMat_tax,unlist,use.names=FALSE)
df_tax <- as.data.frame(data)
data <- lapply(featMat_encycl,unlist,use.names=FALSE)
df_encycl <- as.data.frame(data)
data <- lapply(featMat_fcn,unlist,use.names=FALSE)
df_fcn <- as.data.frame(data)
data <- lapply(featMat_vis,unlist,use.names=FALSE)
df_vis <- as.data.frame(data)
data <- lapply(featMatNoTax,unlist,use.names=FALSE)
df_noTax <- as.data.frame(data)
data <- lapply(featMat_mergedEncycl,unlist,use.names=FALSE)
df_mergedEncycl <- as.data.frame(data)
data <- lapply(featMat_all,unlist,use.names=FALSE)
df_all <- as.data.frame(data)

#data <- lapply(featMat_all_thres,unlist,use.names=FALSE)
#df_all_thres <- as.data.frame(data)

## df variable/cols should have names
encycl_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_encycl.mat")
fcn_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_fcn.mat")
tax_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_tax.mat")
vis_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_vis.mat")
#mergedEncycl_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_mergedEncycl.mat")
#feat_names_all_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_all_thres.mat")
#feat_cat_all_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featCat_all_thres.mat")
feat_names_all <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_all.mat")
#feat_cat_all <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featCat_all.mat")
noTax_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featNames_noTax.mat")

# add the names
encyclNamesArr = array(unlist(encycl_names),dim=c(1,1930))
names(df_encycl) <- encyclNamesArr
visNamesArr = array(unlist(vis_names),dim=c(1,1886))
names(df_vis) <- visNamesArr
fcnNamesArr = array(unlist(fcn_names),dim=c(1,1101))
names(df_fcn) <-fcnNamesArr



#### I don't know the short ones yet, so I'll just make whole ones.
# feature names
%all_namesArr = array(unlist(feat_names_all),dim=c(1,5520))
# relabel col var names for df_all
%names(df_all) <- all_namesArr

#all_thres_namesArr = array(unlist(feat_names_all_thres),dim=c(1,2460))
# relabel col var names for df_all_thres
#names(df_all_thres) <- all_thres_namesArr

# feature categories
#all_catArr = array(unlist(feat_cat_all),dim=c(1,5520))
# relabel col var names as the feat CATEGORIES for df_all
#names(df_all) <- all_catArr

#all_thres_catArr = array(unlist(feat_cat_all_thres),dim=c(1,2460))
# relabel col var names as the feat CATEGORIES for df_all_thres
#names(df_all_thres) <- all_thres_catArr

######## Jan 2023
### I wanted to start a new section because there's a TON of stuff below. Mostly experimenting with scree and factanal, and then outputting to a spreadsheet
### I have a solution to one of the issues, so I'd like to see the results cleanly through some copying and pasting up here and some re-writing

# here are the same feature mats but with only the 300 rows that match the trials we have fMRI data for

featMat_tax_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_tax_300.mat")
featMat_encycl_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_encycl_300.mat")
featMat_fcn_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_fcn_300.mat")
featMat_vis_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_vis_300.mat")
featMat_noTax_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_noTax_300.mat")
#featMat_mergedEncycl <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/merged_encycl.mat")
featMat_all_300 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_all_300.mat")


# convert list to data frame
data <- lapply(featMat_tax_300,unlist,use.names=FALSE)
df_tax_300 <- as.data.frame(data)
data <- lapply(featMat_encycl_300,unlist,use.names=FALSE)
df_encycl_300 <- as.data.frame(data)
data <- lapply(featMat_fcn_300,unlist,use.names=FALSE)
df_fcn_300 <- as.data.frame(data)
data <- lapply(featMat_vis_300,unlist,use.names=FALSE)
df_vis_300 <- as.data.frame(data)
data <- lapply(featMat_noTax_300,unlist,use.names=FALSE)
df_noTax_300 <- as.data.frame(data)
data <- lapply(featMat_all_300,unlist,use.names=FALSE)
df_all_300 <- as.data.frame(data)

# add the names
encyclNamesArr = array(unlist(encycl_names),dim=c(1,827))
names(df_encycl_300) <- encyclNamesArr
taxNamesArr = array(unlist(tax_names),dim=c(1,184))
names(df_tax_300) <- taxNamesArr
visNamesArr = array(unlist(vis_names),dim=c(1,937))
names(df_vis_300) <- visNamesArr
fcnNamesArr = array(unlist(fcn_names),dim=c(1,355))
names(df_fcn_300) <-fcnNamesArr
noTaxNamesArr = array(unlist(noTax_names),dim=c(1,5167))
names(df_noTax_300) <-noTaxNamesArr
allNamesArr = array(unlist(feat_names_all),dim=c(1,5520))
names(df_all_300) <-allNamesArr

####### 360
# load feat mats
featMat_encycl_360 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_encycl_360.mat")
featMat_fcn_360 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_fcn_360.mat")
featMat_vis_360 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_vis_360.mat")
featMat_encycl_330 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_encycl_330.mat")
featMat_fcn_330 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_fcn_330.mat")
featMat_vis_330 <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/featMat_vis_330.mat")

# convert to df
data <- lapply(featMat_encycl_360,unlist,use.names=FALSE)
df_encycl_360 <- as.data.frame(data)
data <- lapply(featMat_fcn_360,unlist,use.names=FALSE)
df_fcn_360 <- as.data.frame(data)
data <- lapply(featMat_vis_360,unlist,use.names=FALSE)
df_vis_360 <- as.data.frame(data)
data <- lapply(featMat_encycl_330,unlist,use.names=FALSE)
df_encycl_330 <- as.data.frame(data)
data <- lapply(featMat_fcn_330,unlist,use.names=FALSE)
df_fcn_330 <- as.data.frame(data)
data <- lapply(featMat_vis_330,unlist,use.names=FALSE)
df_vis_330 <- as.data.frame(data)

# add names
encyclNamesArr = array(unlist(encycl_names),dim=c(1,1930))
names(df_encycl_360) <- encyclNamesArr
visNamesArr = array(unlist(vis_names),dim=c(1,1886))
names(df_vis_360) <- visNamesArr
fcnNamesArr = array(unlist(fcn_names),dim=c(1,1101))
names(df_fcn_360) <-fcnNamesArr
encyclNamesArr = array(unlist(encycl_names),dim=c(1,1930))
names(df_encycl_330) <- encyclNamesArr
visNamesArr = array(unlist(vis_names),dim=c(1,1886))
names(df_vis_330) <- visNamesArr
fcnNamesArr = array(unlist(fcn_names),dim=c(1,1101))
names(df_fcn_330) <-fcnNamesArr


#### Jan 14 2023 --- we're all good. Just skip down 50ish lines and find which 
#### threshold is required for scree and factanal to run


### Jan 11 2023 --- FA. Can I get it to work? What's wrong with my matrix?
#### NOTE: the feature mats seem to have zero cols? Didn't the thresholding work? I'm going to do it here instead

# for example, df_encycl is 995x1930
# can we remove all cols which sum to less than 4?

# >= 4 is 995x676
# >= 3 is 995x1055

# 1930 to 1055
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 3]

# 823 cols to 356 cols
df_mergedEncycl_thres <- df_mergedEncycl[, colSums(df_mergedEncycl) >= 3]

### FA needs more observations (items) than variables (features)
### We can do SVD to reduce the matrix and then do SVD on that or just SVD and do something with it
### or, just use a more conservative threshold and reduce the cols that way

# 1930 to 1118
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 5] #scree won't run without error
# 1930 to 916
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 6] 
# 1930 to 768
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 7] 
# 1930 to 663
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 8] 
# 1930 to 593
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 9] 
# 1930 to 529
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 10] 
# 1930 to 463
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 11] 
# to 413
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 12] 
# 379
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 13] 
# 350
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 14] 
# 322
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 15] 
# 300
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 16] 
# 289
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 17] 
# 271
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 18] 
# 249
df_encycl_thres <- df_encycl[, colSums(df_encycl) >= 19] #this runs without errors. Looks like Nfacs = 25

# 1930 to 241
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 7] 
# 1930 to 206
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 8] #this runs without errors. Nfacs = 30
# 1930 to 181
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 9] #this runs. Nfacs = 25
# 161
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 10] #this runs. Nfacs = 25 again
# 130
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 12] #this runs. Nfacs = 18
# 104
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 14] 
# 93
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 15] 
# 87
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 16] 

# 1886 to 895
df_vis_thres <- df_vis[, colSums(df_vis) >= 7] 
# 813
df_vis_thres <- df_vis[, colSums(df_vis) >= 8] 
# 751
df_vis_thres <- df_vis[, colSums(df_vis) >= 9] 
# 701
df_vis_thres <- df_vis[, colSums(df_vis) >= 10]
# 659
df_vis_thres <- df_vis[, colSums(df_vis) >= 11]
# 614
df_vis_thres <- df_vis[, colSums(df_vis) >= 12] #this runs. Nfacs = 80
# 477 
df_vis_thres <- df_vis[, colSums(df_vis) >= 16] #this runs. Nfacs = 60
# 398
df_vis_thres <- df_vis[, colSums(df_vis) >= 20] #this runs. Nfacs = 40
# 331
df_vis_thres <- df_vis[, colSums(df_vis) >= 25] #30-35
# 288
df_vis_thres <- df_vis[, colSums(df_vis) >= 30] #25

# 1886 to 269
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 10]
# 247
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 11]
# 220
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 12] #this runs. Nfacs = 40
# 191
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 14] #this runs. Nfacs = 30
# 143
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 20] #this runs. NFacs = 18
# 125
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 24] 
# 118
df_vis_300_thres <- df_vis_300[, colSums(df_vis_300) >= 26] 

# 1101 to 261
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 10]
# 169
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 14]
# 152
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 15]
# 137
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 16] #this runs. Nfacs = 10
# 126
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 17] 
# 111
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 19] 
# 92
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 22] 
# 79
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 24] 
# 71
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 25] 
# 67
df_fcn_thres <- df_fcn[, colSums(df_fcn) >= 26] 

# 1101 to 545
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 1]
# 382
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 2] # doesn't run
# 299
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 3] 
# 206
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 4] 
# 100
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 7] 
#81
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 8] 
#69
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 9] 
# 58
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 10] #this runs. Nfacs = 4
# 35
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 14]
# 30
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 16]
# 28
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 18]
# 19
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 20]
# 16
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 22]
# 14
df_fcn_300_thres <- df_fcn_300[, colSums(df_fcn_300) >= 26]

# 5520 to 1736
df_all_thres <- df_all[, colSums(df_all) >= 10]
# 1428
df_all_thres <- df_all[, colSums(df_all) >= 12]
#988
df_all_thres <- df_all[, colSums(df_all) >= 18] #doesn't run
#866
df_all_thres <- df_all[, colSums(df_all) >= 20] #doesn't run
#827
df_all_thres <- df_all[, colSums(df_all) >= 21] #doesn't run
#792
df_all_thres <- df_all[, colSums(df_all) >= 22] #runs. Nfacs = 45 <-- where did I get this? Looks like >100
#725
df_all_thres <- df_all[, colSums(df_all) >= 24] #runs
#666
df_all_thres <- df_all[, colSums(df_all) >= 26] #runs. Nfacs = 90 (?)
#595
df_all_thres <- df_all[, colSums(df_all) >= 30] #runs. Nfacs = 80
#501
df_all_thres <- df_all[, colSums(df_all) >= 36] #runs. Nfacs = 60
#447
df_all_thres <- df_all[, colSums(df_all) >= 40] #runs. Nfacs = 50

#596
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 10]
#484
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 12]
#361
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 16]
#317
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 18]
#290
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 20]
#267
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 22]
#252
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 24]
#242
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 25]
#229
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 26] #this runs. Nfacs = 30
#217
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 27] #Nfacs = 25
#210
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 28] #Nfacs = 25
#203
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 29] #Nfacs = 20
#196
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 30]
#184
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 32]
#179
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 34]
#157
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 38]
#152
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 40] #Nfacs = 15
#146
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 42] #Nfacs = 18
#140
df_all_300_thres <- df_all_300[, colSums(df_all_300) >= 44] #Nfacs = 18 

# 5167 to 1601
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 10]
#988
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 16]
#900
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 18]
#790
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 20] #runs. Nfacs = 110
#540
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 30] #runs. Nfacs = 75
#468
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 35] #runs. Nfacs = 60
#404
df_noTax_thres <- df_noTax[, colSums(df_noTax) >= 40] #runs. Nfacs = 50

# 5167 to 539
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 10]
#321
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 16]
#301
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 17]
#283
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 18] #runs. Nfacs = 45
#261
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 20] #runs. Nfacs = 40
#225
df_noTax_300_thres <- df_noTax_300[, colSums(df_noTax_300) >= 24] #Nfacs = 30



df_encycl_360_thres <- df_encycl_360[, colSums(df_encycl_360) >= 18]

df_vis_360_thres <- df_vis_360[, colSums(df_vis_360) >= 20]

df_fcn_360_thres <- df_fcn_360[, colSums(df_fcn_360) >= 18]

df_encycl_330_thres <- df_encycl_330[, colSums(df_encycl_330) >= 18]

df_vis_330_thres <- df_vis_330[, colSums(df_vis_330) >= 20]

df_fcn_330_thres <- df_fcn_330[, colSums(df_fcn_330) >= 18]


vss(df_vis_360_thres,rotate="promax")

#scree(df_fcn_thres,pc=FALSE) #<- these just don't run without errors
#scree(df_noTax_300_thres,pc=FALSE)



# df_encycl_thres
#### can get an exact number with something like this, though it does use a different standard
fa.parallel(df_encycl_thres, fm = 'minres', fa= 'fa')


#Nfacs <- 25 
#fit <- factanal(df_encycl_thres,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1) #scores can equal nothing, "regression" or "Bartlett"


#### ok, let's fun factanal() and save these guys

curr_df <- df_encycl_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings,Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_encycl_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_encycl_jan23.csv")

curr_df <- df_encycl_300_thres
Nfacs <- 8 
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_encycl_300_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_encycl_300_jan23.csv")

curr_df <- df_vis_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_vis_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_vis_jan23.csv")

curr_df <- df_vis_300_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_vis_300_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_vis_300_jan23.csv")

curr_df <- df_fcn_thres
Nfacs <- 5 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_fcn_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_fcn_jan23.csv")

curr_df <- df_fcn_300_thres
Nfacs <- 4 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_fcn_300_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_fcn_300_jan23.csv")

curr_df <- df_all_thres
Nfacs <- 50 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacLoads_all_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacScores_all_jan23.csv")

curr_df <- df_all_300_thres
Nfacs <- 18 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacLoads_all_300_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacScores_all_300_jan23.csv")

curr_df <- df_noTax_thres
Nfacs <- 50 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacLoads_noTax_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacScores_noTax_jan23.csv")

curr_df <- df_noTax_300_thres
Nfacs <- 30 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacLoads_noTax_300_jan23.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_jan23/FacScores_noTax_300_jan23.csv")


curr_df <- df_encycl_360_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_encycl_360.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_encycl_360.csv")

curr_df <- df_vis_360_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_vis_360.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_vis_360.csv")

curr_df <- df_fcn_360_thres
Nfacs <- 5 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_fcn_360.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_fcn_360.csv")

curr_df <- df_encycl_330_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_encycl_330.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_encycl_330.csv")

curr_df <- df_vis_330_thres
Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_vis_330.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_vis_330.csv")

curr_df <- df_fcn_330_thres
Nfacs <- 5 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings, Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacLoads_fcn_330.csv")
FactorScores <- round(fit$scores, Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/factanal_results_v2/FacScores_fcn_330.csv")


### make .mat files in predict_mturk_mem_betas_withPCs.m


scree(df_vis_thres,pc=FALSE) #<- these just don't run without errors

scree(df_encycl[,1:269],pc=FALSE) #<- this one does run without errors

# Cortney uses the following 2 thresholds:
# Two high-pass (inclusive) thresholds:
# 1st: number of people assigned feature X to each item (greater threshold -> more reliable feature)
# ---- cells with values below the threshold are changed to zeros
# 2nd: number of items that were assigned feature X (greater threshold -> more common the feature)
# ---- feature columns below the threshold are excluded

### maybe just take out the 0 cols? 
#test <- df_mergedEncycl[, colSums(df_mergedEncycl != 0) >0]

### re-do but with the 300-row matx
# 1930 to 383
df_encycl_300_thres <- df_encycl_300[, colSums(df_encycl_300) >= 3]


## encycl_thres has 1055 features
scree(df_encycl_thres,pc=FALSE) #wants 150 factors

## encycl_300_thres has 383 features
scree(df_encycl_300_thres,pc=FALSE)

# very simple structure. VSS from psych package
# https://rdrr.io/cran/psych/man/VSS.html
vss(df_encycl_300_thres)
vss(df_encycl_300_thres,rotate="promax")

# try screen_min?

## alternative forms of FA to try
#https://stackoverflow.com/questions/15759226/factor-analysis-using-r
# if you're getting warnings and errors that your matrix is singular, then no solution exists to the optimization problem
# that means that you have to use a different FA method appropriate to a singular matrix
# two options are pa (Principal axis factor analysis) and minres (Minimum residual factor analysis)

# https://www.upgrad.com/blog/factor-analysis-in-r/
# first step, how  many factors?
fa.parallel(df_encycl_thres, fm = 'minres', fa= 'fa')

EFA.Comp.Data(Data=df_encycl_thres, F.max=1055, Graph=T)

# very simple structure. VSS from psych package
vss(df_encycl_thres)

# parallel analysis
fa.parallel(df_encycl_thres)

library(psych)
library(GPArotation)
fa(r=cor(df_encycl_300_thres), nfactors=8, rotate="varimax", SMC=FALSE, fm="minres")
fa(r=cor(df_encycl_300_thres), nfactors=8, rotate="promax", SMC=FALSE, fm="minres")

fa.parallel(df_encycl_300_thres, fm = 'minres', fa= 'fa')



# can also check the factorability of the data by looking at the correlation matrix and doing KMO test
library(psych)
library(corrplot)
library(ggplot2)
library(car)
datamatrix <- cor(df_encycl_thres)
corrplot(datamatrix,method="number")

KMO(r=cor(df_encycl_thres)) #data must have KMO >= 60 to be factorable

# can do bartlett's test of sphericity
cortest.bartlett(df_encycl_thres)
# chisq is inf
# p.value is 0
# df is 555985
det(cor(df_encycl_thres))
# positive determinant means FA will probably run. out determinant = 0

# how many factors to extract?
library(ggplot2)
X <- df_encycl_thres

fafitfree <- fa(dat,nfactors = ncol(X), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")


### SVD
# https://towardsdatascience.com/singular-value-decomposition-with-example-in-r-948c3111aa43
# take a matrix and 'break' it into three other matrices, u, v, and d
# you can use these to recover most of the original matrix.
# you also get t which is the transpose of v (?)

comp <- svd(df_encycl)
str(comp)

# comp$d is a list of weights in descending order

# d is a list, but you need to put it in a diagonal
d<- diag(comp$d)

# u stays the same but v has to be transposed

# you can pick some amount of the cols. In the tutorial they do 25

compressed_encycl <- (comp$u[,1:10] %*% diag(comp$d[1:10]) %*% t(comp$v[,1:10]))

### looks like it gives me back the original col number. I suppose it's simplified/compressed, but that doesn't help

# I guess I can do FA stuff to it now? Will it work better?

scree(compressed_encycl_300_thres,pc=FALSE) 



factor.scores(df_encycl_300_thres)

#### FACTANAL()

Nfacs <- 8 #have to play with this number too to get it to run
fit <- factanal(t(df_encycl_300),Nfacs,scores = "Bartlett",rotation="promax",lower = 0.4) #scores can equal nothing, "regression" or "Bartlett"
fit_encycl_thres <- fit 

FactorLoadings <- round(fit$loadings, Nfacs)

Nfacs <- 25
fit <- factanal(df_encycl[,1:200],Nfacs, scores="Bartlett",rotation="promax")

scree(df_encycl[,1:200])


# https://stats.stackexchange.com/questions/70899/what-correlation-makes-a-matrix-singular-and-what-are-implications-of-singularit
### a correlation matrix is singular when there is dependency between the variables. So, you can use values in some cols to calculate other cols?
### so the problem can be multicollinearity


FactorLoadings <- round(fit$loadings[ 1:827,], 150)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_encycl_thres.csv")
FactorScores <- round(fit$scores[ 1:827,], 150)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_encycl_thres.csv")


## tax_thres has 214 cols, so less than the 995 rows
scree(df_tax_thres,pc=FALSE)


### from Jan 9 and 10 2023

## cat-specific but thresholded
## these have all had the correction where the feature has to apply to >= 4 objects
# counts/mats
encycl_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_thres.mat")
tax_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/tax_thres.mat")
vis_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/vis_thres.mat")
fcn_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/fcn_thres.mat")
# names
encycl_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_thres_names.mat")
tax_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/tax_thres_names.mat")
vis_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/vis_thres_names.mat")
fcn_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/fcn_thres_names.mat")

### convert to df
data <- lapply(encycl_thres,unlist,use.names=FALSE)
df_encycl_thres <- as.data.frame(data)
data <- lapply(tax_thres,unlist,use.names=FALSE)
df_tax_thres <- as.data.frame(data)
data <- lapply(vis_thres,unlist,use.names=FALSE)
df_vis_thres <- as.data.frame(data)
data <- lapply(fcn_thres,unlist,use.names=FALSE)
df_fcn_thres <- as.data.frame(data)

### add the names
encyclNamesArr = array(unlist(encycl_thres_names),dim=c(1,827))
names(df_encycl_thres) <- encyclNamesArr
taxNamesArr = array(unlist(tax_thres_names),dim=c(1,184))
names(df_tax_thres) <- taxNamesArr
visNamesArr = array(unlist(vis_thres_names),dim=c(1,937))
names(df_vis_thres) <- visNamesArr
fcnNamesArr = array(unlist(fcn_thres_names),dim=c(1,355))
names(df_fcn_thres) <-fcnNamesArr

### how many feats?
curr_df <- df_encycl_thres 
curr_df <- df_tax_thres 
curr_df <- df_vis_thres 
curr_df <- df_fcn_thres 
curr_df <- df_mergedEncycl

# how many factors in principle
scree(curr_df,pc=FALSE) 

#### scree wants X factors
# encycl: all 1930 wants 400ish. 250 subset needs 25
# tax: all 353 needs 50. 140 subset needs 15
# vis: all 1886 needs 400ish. 200 subset needs 20ish
# fcn: all 1101 needs 200. 150 subset needs 10ish
# merged encycl: all 823 needs

# ******* but I can run bigger subsets!
# let's start with the 25, 15, 20, and 10, and see how many features will run.
# then I can re-do scree and see if the needed factors will still run

#### thresholding removes features that occur in fewer than 4 items. So, >=4 stays in

## encycl_thres has 827 features
scree(df_encycl_thres,pc=FALSE) #wants 150 factors
Nfacs <- 150 #have to play with this number too to get it to run
fit <- factanal(df_encycl_thres,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.2)
fit_encycl_thres <- fit 

FactorLoadings <- round(fit$loadings[ 1:827,], 150)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_encycl_thres.csv")
FactorScores <- round(fit$scores[ 1:827,], 150)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_encycl_thres.csv")

## fcn_thres has 355 features
scree(df_fcn_thres,pc=FALSE) #wants 45 factors
Nfacs <- 45 #have to play with this number too to get it to run
fit <- factanal(df_fcn_thres,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1)

## vis_thres has 937 features #wants 150 factors
scree(df_vis_thres,pc=FALSE)
Nfacs <- 150 #have to play with this number too to get it to run
fit <- factanal(df_vis_thres,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1)


## tax_thres has 184 features #wants 10 factors
scree(df_tax_thres,pc=FALSE)
Nfacs <- 10 #have to play with this number too to get it to run
fit <- factanal(df_tax_thres,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1)

## all thres has 2460 features #wants 400 factors
#Factor scores don't work well with "smoothed" correlation matrices. 
#The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.
scree(df_all_thres,pc=FALSE)

## merged encycl has 859 (not thresholded)
scree(df_mergedEncycl,pc=FALSE) #has missing values?
# it looks like the problem is zero cols

test <- df_mergedEncycl[, colSums(df_mergedEncycl != 0) >0]

colSums(df_mergedEncycl)

test <- df_mergedEncycl[ , colSums(df_mergedEncycl!= 0, na.rm = TRUE) > 0]

# remove 0 cols and you get 831 features
scree(test,pc=FALSE) #wants 160 factors

#lots of errors again. Matrix not positive definite, so smoothing was done.
#then it says that estimated weights for factor scores are probably incorrect, just like above.




KMO(df_vis_short)

# It's advised to remove all variables (for us, features) whos individual MSA is < 0.5
# Get rid of all variables with MSA < 0.50
# function of the squared elements of the `image' matrix compared to the squares 
# of the original correlations.
df_encycl_KMOclean <- df_encycl[, KMO(df_encycl)$MSAi>0.5]
# then you can run again
KMO(df_encycl_KMOclean)


plot(colSums(df_mergedEncycl$feat.merged.encycl.noNeg.5>0))

### short versions
##### don't run if I want the versions with feature names above
# df_encycl_short <- df_encycl[,1:250]
# df_fcn_short <- df_fcn[,1:150]
# df_mergedEncycl_short <- df_mergedEncycl[,1:200]
# df_noTax_short <- df_noTax[,1:200]
# df_tax_short <- df_tax[,1:140]
# df_vis_short <- df_vis[,1:200]


#### are there nans?
missings <- colSums(is.na(df_mergedEncycl)) # Count # missing in each column
summary(missings) # Evaluate the breakdown of missings


# remove cols from df
curr_df_short = curr_df[,1:990] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short
Nfacs <- 400 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1)
# this lower parameter helps factanal run
# FA has a criterion for uniqueness when optimizing. If the solution doesn't converge at a certain point, it will throw an error.
# the lower parameter defaults to .005. So, just increase it and you can run a bigger feature matrix


##### ~dec 2022
# mat files load as lists
# convert list to array
# I got the var lengths through guess and check using factoran() in matlab
#### could also just add ALL names to all feat counts instead of worrying about the short versions
# first, I only need first 250
encycl_names_short <- encycl_names[1:250]
encyclNameArr=array(unlist(encycl_names_short),dim=c(1,250))
df_encycl_short = df_encycl[,1:250]
# relabel col var names for df_encycl_short
names(df_encycl_short) <- encyclNameArr

fcn_names_short <- fcn_names[1:150]
fcnNameArr=array(unlist(fcn_names_short),dim=c(1,150))
df_fcn_short = df_fcn[,1:150]
# relabel col var names for df_encycl_short
names(df_fcn_short) <- fcnNameArr

tax_names_short <- tax_names[1:140]
taxNameArr=array(unlist(tax_names_short),dim=c(1,140))
df_tax_short = df_tax[,1:140]
# relabel col var names for df_encycl_short
names(df_tax_short) <- taxNameArr

vis_names_short <- vis_names[1:200]
visNameArr=array(unlist(vis_names_short),dim=c(1,200))
df_vis_short = df_vis[,1:200]
# relabel col var names for df_encycl_short
names(df_vis_short) <- visNameArr

mergedEncycl_names_short <- mergedEncycl_names[1:200]
mergedEncyclNameArr=array(unlist(mergedEncycl_names_short),dim=c(1,200))
df_mergedEncycl_short = df_mergedEncycl[,1:200]
# relabel col var names for df_encycl_short
names(df_mergedEncycl_short) <-mergedEncyclNameArr

mergedEncyclNamesArr=array(unlist(mergedEncycl_names),dim=c(1,859))
names(df_mergedEncycl) <- mergedEncyclNamesArr

#### I don't know the short ones yet, so I'll just make whole ones.
# feature names
all_namesArr = array(unlist(feat_names_all),dim=c(1,5520))
# relabel col var names for df_all
names(df_all) <- all_namesArr

#### deal with the names of feat_all and feat_all_thres
### (1) do these one at a time
### (2) run factanal() a few times to see how many features it'll allow and still run.
### NFacs can be anything. We'll get an exact number in (3)
### (3) run scree again to get number of factors
### (4) run factanal()
### (5) then scroll down to export loadings

all_thres_namesArr = array(unlist(feat_names_all_thres),dim=c(1,2460))
# relabel col var names for df_all_thres
#names(df_all_thres) <- all_thres_namesArr



# feature categories
all_catArr = array(unlist(feat_cat_all),dim=c(1,5520))
# relabel col var names as the feat CATEGORIES for df_all
names(df_all) <- all_catArr

all_thres_catArr = array(unlist(feat_cat_all_thres),dim=c(1,2460))
# relabel col var names as the feat CATEGORIES for df_all_thres
names(df_all_thres) <- all_thres_catArr

curr_df <- df_all
# curr_df <- df_mergedEncycl


curr_df <- df_all_thres 
#990 runs with NFacs = 20
#850 runs with NFacs = 30

scree(curr_df,pc=FALSE) #will 990 run?

# remove cols from df
curr_df_short = curr_df[,1:990] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short

Nfacs <- 20 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")

# 990 with 180 factors won't run
# 900 with 180 factors won't run
# 990 with 50 factors won't run
# 30 won't
# 25 won't
# 20 works


##### let's check some other feature categories
#### try some scree sizes
curr_df <- df_encycl
curr_df_short = curr_df[,1:270] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short

test <- curr_df$featMat.encycl.270
sum(test>0)

plot(colSums(curr_df))
plot(colSums(curr_df>0))
# doesn't like having too many 1s in a row?
# the threshold procedure should remove those

#scree(df_encycl,pc=FALSE)
Nfacs <- 25 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")

# try 10 features for exploratory purposes
# 1000 encycl
# 900
# 800
#.... only 200 runs. Maybe I'll try thresholding


curr_df <- df_mergedEncycl

scree(curr_df,pc=FALSE) #will 990 run?

curr_df <- df_encycl
curr_df_short = curr_df[,1:400] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short

scree(curr_df,pc=FALSE) #will 990 run?

Nfacs <- 10 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")

#### cat-specific but thresholded
##### these have all had the correction where the feature has to apply to >= 4 objects
# counts/mats
encycl_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_thres.mat")
tax_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/tax_thres.mat")
vis_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/vis_thres.mat")
fcn_thres <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/fcn_thres.mat")
# names
encycl_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_thres_names.mat")
tax_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/tax_thres_names.mat")
vis_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/vis_thres_names.mat")
fcn_thres_names <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/fcn_thres_names.mat")

### convert to df
data <- lapply(encycl_thres,unlist,use.names=FALSE)
df_encycl_thres <- as.data.frame(data)
data <- lapply(tax_thres,unlist,use.names=FALSE)
df_tax_thres <- as.data.frame(data)
data <- lapply(vis_thres,unlist,use.names=FALSE)
df_vis_thres <- as.data.frame(data)
data <- lapply(fcn_thres,unlist,use.names=FALSE)
df_fcn_thres <- as.data.frame(data)

### add the names
encyclNamesArr = array(unlist(encycl_thres_names),dim=c(1,827))
names(df_encycl_thres) <- encyclNamesArr
taxNamesArr = array(unlist(tax_thres_names),dim=c(1,184))
names(df_tax_thres) <- taxNamesArr
visNamesArr = array(unlist(vis_thres_names),dim=c(1,937))
names(df_vis_thres) <- visNamesArr
fcnNamesArr = array(unlist(fcn_thres_names),dim=c(1,355))
names(df_fcn_thres) <-fcnNamesArr

### how many feats?
curr_df <- df_encycl_thres 
curr_df <- df_tax_thres 
curr_df <- df_vis_thres 
curr_df <- df_fcn_thres 

# how many factors in principle
scree(curr_df,pc=FALSE) 

# remove cols from df
curr_df_short = curr_df[,1:990] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short

# remember, fewer factors the more features you can have
Nfacs <- 50 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")


# do thresholded encycl
curr_df <- df_encycl_thres 
curr_df_short = curr_df[,1:800] #change this based on what subset a given category needs. 985 worked. 999 did not
curr_df <- curr_df_short
Nfacs <- 25 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax",lower = 0.1)


plot(colSums(curr_df>0))

### short versions
##### don't run if I want the versions with feature names above
# df_encycl_short <- df_encycl[,1:250]
# df_fcn_short <- df_fcn[,1:150]
# df_mergedEncycl_short <- df_mergedEncycl[,1:200]
# df_noTax_short <- df_noTax[,1:200]
# df_tax_short <- df_tax[,1:140]
# df_vis_short <- df_vis[,1:200]


#### are there nans?
missings <- colSums(is.na(df_mergedEncycl)) # Count # missing in each column
summary(missings) # Evaluate the breakdown of missings

# https://rpubs.com/pjmurphy/758265
### remember, you could have as many factors as variables, but that would defeat the purpose
### instead, we want latent structure within the data. Factors that explain a large
#### proportion of the variation in the data

## (1) Scree plots. Approximately many factors?

#curr_df <- df_tax
curr_df <- df_mergedEncycl
curr_df <- df_mergedEncycl_short

ev <- eigen(cor(curr_df))
ev$values

# remove cols from df
curr_df_short = curr_df[,1:200] #change this based on what subset a given category needs
curr_df <- curr_df_short

scree(curr_df, pc=FALSE) #set pc to FALSE for FA
# Note, in this scree plot it had to do smoothing because matrix was not positive definite
# You want to look at where the plot crosses the horizontal line

fa.parallel(curr_df, fa="fa") #parallel analysis
# Corr matrix is singular, so it used an approximation

##### need to investigate df_all and df_all_thres. How many factors?
scree(df_all,pc=FALSE)

scree(df_all_thres,pc=FALSE)

## (2) Decide which rotation to use
# Oblique rotations are safe because they assume the factors are correlated (promax, oblimin)
# You also do hypothesis test to see if X factors are sufficient

### factanal won't run if solve() (internal to factanal()) won't run
### one reason is that the matrix is singular with no inverse. Or, like in matlab
### if covariance matrix is not positive definite

# remove cols from df
#curr_df_short = curr_df[,1:250]
#curr_df <- curr_df_short
curr_df <- df_encycl_short #this one has features names
curr_df <- df_tax_short #this one has features names
curr_df <- df_fcn_short #this one has features names
curr_df <- df_vis_short #this one has features names

Nfacs <- 15 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,rotation="promax")

print(fit,digits=2,cutoff=0.3,sort=TRUE)

# scree(curr_df_short, pc=FALSE)

#### KMO Test
# Kaiser-Meyer-Olkin factor adequacy

KMO(df_encycl)
KMO(df_fcn)
KMO(df_mergedEncycl)
KMO(df_noTax)
KMO(df_tax)
KMO(df_vis)


KMO(df_encycl_short)
KMO(df_fcn_short)
KMO(df_mergedEncycl_short)
KMO(df_noTax_short)
KMO(df_tax_short)
KMO(df_vis_short)

# It's advised to remove all variables (for us, features) whos individual MSA is < 0.5
# Get rid of all variables with MSA < 0.50
# function of the squared elements of the `image' matrix compared to the squares 
# of the original correlations.
df_encycl_KMOclean <- df_encycl[, KMO(df_encycl)$MSAi>0.5]
# then you can run again
KMO(df_encycl_KMOclean)

df_fcn_KMOclean <- df_fcn[, KMO(df_fcn)$MSAi>0.5]
# then you can run again
KMO(df_fcn_KMOclean)

df_noTax_KMOclean <- df_noTax[, KMO(df_noTax)$MSAi>0.5]
# then you can run again
KMO(df_noTax_KMOclean)

df_tax_KMOclean <- df_tax[, KMO(df_tax)$MSAi>0.5]
# then you can run again
KMO(df_tax_KMOclean)

df_vis_KMOclean <- df_vis[, KMO(df_vis)$MSAi>0.5]
# then you can run again
KMO(df_vis_KMOclean)


mydat <- mydata[, KMO(mydata)$MSAi>0.50] 
mydata <- mydat

#Let’s take a look at the KMO value now that those problematic variables have been removed.

round( KMO(mydata)$MSA, 2 )

## (3) Export the loadings. How many variables and how many factors?

# for encycl there are 150 variables and 25 factors
curr_df <- df_encycl_short 
Nfacs <- 25 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings[ 1:150,], 25)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_encycl.csv")
FactorScores <- round(fit$scores[ 1:995,], 25)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_encycl.csv")

# for fcn there are 150 variables and 10 factors
curr_df <- df_fcn_short #this one has features names
Nfacs <- 10
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
FactorLoadings <- round(fit$loadings[ 1:150,], 10)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_fcn.csv")
FactorScores <- round(fit$scores[ 1:995,], 10)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_fcn.csv")

# for vis there are 200 variables and 20 factors
curr_df <- df_vis_short #this one has features names
Nfacs <- 20
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
FactorLoadings <- round(fit$loadings[ 1:200,], 20)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_vis.csv")
FactorScores <- round(fit$scores[ 1:995,], 20)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_vis.csv")

# for tax there are 140 variables and 15 factors
curr_df <- df_tax_short #this one has features names
Nfacs <- 15
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
FactorLoadings <- round(fit$loadings[ 1:140,], 15)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_tax.csv")
FactorScores <- round(fit$scores[ 1:995,], 15)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_tax.csv")

# all features
curr_df <- df_all
Nfacs <- 25 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings[ 1:150,], 25)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_allFeat.csv")
FactorScores <- round(fit$scores[ 1:995,], 25)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_allFeat.csv")


# all features with threshold
curr_df <- df_all_threshold
Nfacs <- 20 #have to play with this number too to get it to run
fit <- factanal(curr_df,Nfacs,scores = "Bartlett",rotation="promax")
#The two most common predictors for computing factor scores are regression and Bartlett
FactorLoadings <- round(fit$loadings[ 1:990,], Nfacs)
write.csv(FactorLoadings, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacLoads_allFeat_thres_cat_20F.csv")
FactorScores <- round(fit$scores[ 1:990,], Nfacs)
write.csv(FactorScores, file="/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_allFeat_thres_cat_20F.csv")




# Consistency: Cronbach's Alpha
#### how consistent are the factors? Meaning, are the variances internally consistent between variables in a factor?
#### you have to name the variables in each factor so you see which variables occur in more than one factor
#### then use alpha()


#### scatter for mem x F
#### I want corr coeffs


### Step 1: my Fs are 995 long, so I have to cut down to 300

# this is 995x2 with the names and item IDs
# the order (alphabetical) matches the 995 F values
itemIDs <- read.xlsx('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx')

### in matlab, I get the item IDs from the subject-specific Enc runs, so I know which items that subject saw
### indices_inOrder is 300x1 and tells me which of the items (from the 995 list) was presented
### I then go further to remove trials that aren't labelled with TT3, which means they were catch trials
### I end up labeling those with NaNs and removing them so I have a subject-specific 'cutting factor' for my PC score values, F, mem, etc
### For my purposes here, indices_inOrder should be sufficient because I really just need to cut the mem and F values down to 300.
### Remember, there are stimulus presentation groups, so the order of the indices is different, but it's always the same 300
#####  OR WAIT!!! When I load visMem_CR (for corrected recognition) or lexMem_CR, they're 995 long too. Do I even need to cut down to 300?
### I say let's do both

### Step 2: load mem values
visMem <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visMem_CR.mat")
lexMem <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/lexMem_CR.mat")

# convert list to data frame
data <- lapply(visMem,unlist,use.names=FALSE)
df_visMem <- as.data.frame(data)
data <- lapply(lexMem,unlist,use.names=FALSE)
df_lexMem <- as.data.frame(data)
#these are 995 long


### Step 3: run corr coeff
# we want to compare the mem scores with one col of F at a time

#### clean the mem variables
#nanRows <- which(is.na(df_lexMem))
lexMem_clean <- df_lexMem[!is.na(df_lexMem)]
visMem_clean <- df_visMem[!is.na(df_visMem)]


# encycl
encycl_F <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_encycl.csv")

x <- lexMem_clean #has had NaNs removed

for (f_val in 1:(length(encycl_F)-1)){
  currCol <- encycl_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_lexMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_clean
for (f_val in 1:(length(encycl_F)-1)){
  currCol <- encycl_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_visMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}

# tax
tax_F <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_tax.csv")


x <- lexMem_clean #has had NaNs removed

for (f_val in 1:(length(tax_F)-1)){
  currCol <- tax_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_lexMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_clean
for (f_val in 1:(length(tax_F)-1)){
  currCol <- tax_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_visMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}


# vis
vis_F <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_vis.csv")

x <- lexMem_clean #has had NaNs removed

for (f_val in 1:(length(vis_F)-1)){
  currCol <- vis_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_lexMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_clean
for (f_val in 1:(length(vis_F)-1)){
  currCol <- vis_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_visMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}


# fcn
fcn_F <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_fcn.csv")

x <- lexMem_clean #has had NaNs removed

for (f_val in 1:(length(fcn_F)-1)){
  currCol <- fcn_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_lexMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_clean
for (f_val in 1:(length(fcn_F)-1)){
  currCol <- fcn_F[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(df_visMem)]
  result = cor(x, y, method = "pearson")
  print(result)
}


# all thres
allThres_F <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/FacScores_allFeat_thres_cat_20F.csv")
### hm, it's 990 x 21. Why not 995?

for (f_val in 1:length(allThres_F)){
  x <- df_lexMem
  y <- allThres_F[,f_val]
  result = cor(x, y, method = "pearson")
}

#### re-do with the 300 items presented to the subjects

#itemIDs

indices_inOrder <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/indices_inOrder.mat")

# convert list to vector
indices_vec <- as.numeric(unlist(indices_inOrder))


encycl_F
df_visMem
df_lexMem


encycl_300 <- encycl_F[indices_vec,]
vis_300 <- vis_F[indices_vec,]
tax_300 <- tax_F[indices_vec,]
fcn_300 <- fcn_F[indices_vec,]
lexMem_300 <- df_lexMem[indices_vec,]
visMem_300 <- df_visMem[indices_vec,]

# clean the mem variables
lexMem_300_clean <- lexMem_300[!is.na(lexMem_300)]
visMem_300_clean <- visMem_300[!is.na(visMem_300)]

# encycl
x <- lexMem_300_clean #has had NaNs removed

for (f_val in 1:(length(encycl_300)-1)){
  currCol <- encycl_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(lexMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_300_clean
for (f_val in 1:(length(encycl_300)-1)){
  currCol <- encycl_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(visMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}


# vis
x <- lexMem_300_clean #has had NaNs removed

for (f_val in 1:(length(vis_300)-1)){
  currCol <- vis_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(lexMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_300_clean
for (f_val in 1:(length(vis_300)-1)){
  currCol <- vis_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(visMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

# tax
x <- lexMem_300_clean #has had NaNs removed

for (f_val in 1:(length(tax_300)-1)){
  currCol <- tax_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(lexMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_300_clean
for (f_val in 1:(length(tax_300)-1)){
  currCol <- tax_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(visMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

# fcn
x <- lexMem_300_clean #has had NaNs removed

for (f_val in 1:(length(fcn_300)-1)){
  currCol <- fcn_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(lexMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

x <- visMem_300_clean
for (f_val in 1:(length(fcn_300)-1)){
  currCol <- fcn_300[,f_val+1] #first col is just consecutive numbers so start with col 2
  y <- currCol[!is.na(visMem_300)]
  result = cor(x, y, method = "pearson")
  print(result)
}

