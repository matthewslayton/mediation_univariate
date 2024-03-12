# this is going to make some of the modifications to the varExplained script that I made to the whereIsTheKnee script




#### the goal here is to look for R2 change.Remember, I want to know which are the top factors
### looking at "individual R2" with each predictor on its own isn't the best way to do this
### because you're not accounting for covariation between predictors.
### What if F32 and F8 covary/are collinear? When they're not in a model together where
### you make them compete, you can't account for the relationship between the two

### also, it's not really legitimate to find the top factors for the a-path and b-path
### find the overlap, and maximize the indirect effect. Technically you can but it's
### hard to justify because you're pre-loading the results before you even do the analysis!

### cortney recommends looking at R2 change. Remember when you're doing mediation, the effect of 
### X on Y that goes through M, you should look at the interaction between X and M
### X*M is an approximation of the relationship that you'll probe more specifically in mediation

### to do this you find the top factors y1 ~ X*M1 + X*M2 + X*M3 + … and y2 ~ X*M2 + X*M3 +…
### Then you do R2_change = R2_y1 – R2_y2 which will give you the R2 contribution by M1. 
### That’s how “important” factor 1 is. 

## note, interaction is when the effect of one explanatory variable on the DV is 
## modified by the level of another explanatory variable.
## So when you have Y ~ X*M, you're looking at the interaction between X and M.
## So, the effect of X on Y isn't constant but depends on M and vice versa.
## You have a beta for that term. e.g.
# Y - b0 + b1*X + b2*M + b3(X*M) + epsilon

# Remove all objects from the global environment
rm(list = ls())

library(dplyr)
library(ggplot2)
library(effects)
library(readxl)
library(openxlsx)
#library(xlsx)
library(writexl)
#library(lmerTest) # BAH, don't use this one. it screws things up when there's two packages with lmer. people seem to like the other one
library(lme4)
library(R.matlab)
library(sjPlot)
library(rio) # install.packages("rio")
library(tibble)
library(mediation)
library(stringr)
library(lavaan)
library(effects)
library(MASS)
library(pscl)
library(tictoc)
library(glmnet)
library(psych)


#factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

ROI_name_variable <- c('mask_AG_L','mask_AG_R','mask_ATL_L','mask_ATL_R',
                       'mask_FuG_L','mask_FuG_R','mask_Hipp_A','mask_Hipp_L',
                       'mask_Hipp_P','mask_Hipp_R','mask_IFG_L','mask_IFG_R',
                       'mask_ITG_L','mask_ITG_R','mask_LOC_L','mask_LOC_R',
                       'mask_MVOC_L','mask_MVOC_R','mask_Pcun_L','mask_Pcun_R',
                       'mask_Perirhinal_L','mask_Perirhinal_R',
                       'mask_PHC_L','mask_PHC_R','mask_PhG_L','mask_PhG_R',
                       'mask_PoG_L','mask_PoG_R','mask_PrG_L','mask_PrG_R',
                       'mask_pSTS_L','mask_pSTS_R','mask_Rhinal_L','mask_Rhinal_R',
                       'mask_RSC_L','mask_RSC_R','mask_SMG_L','mask_SMG_R')


# ROI_noHemisphere <- c('mask_AG','mask_ATL','mask_FuG','mask_Hipp','mask_IFG',
#                       'mask_ITG','mask_LOC','mask_MVOC','mask_Pcun','mask_Perirhinal',
#                       'mask_PHC','mask_PhG','mask_PoG','mask_PrG',
#                       'mask_pSTS','mask_Rhinal','mask_RSC','mask_SMG')


# # using a data frame and rbind is SO SLOW! It has to make a new df each time, so as it gets bigger the runtime gets longer
# results_list <- list()
# pvals <- list()
# iteration <- 1 #don't forget to increment in the inntermost loop (which is the top_factors loop)

# factorNumber <- 150

nmf_types <- c(200,150,100,50) #need to do 150 too
for (factorNumber in nmf_types) {

########## encycl, unilateral, lexMem
## NMF with 228 factors and select the first 100
#encycl <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
#vis <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
#fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

## what about NMF with 100 factors and select all of them
# encycl <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_100fac/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
# vis <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_100fac/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
# fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_100fac/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")

encyclPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx", factorNumber)
visPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx", factorNumber)
fcnPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx", factorNumber)
allPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_all_additionalROIs_unilateral.xlsx", factorNumber)


encycl <- import_list(encyclPath)
vis <- import_list(visPath)
fcn <- import_list(fcnPath)
all <- import_list(allPath)

# # read in our data
# lexMem_HR <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/lexMem_HR.mat")
# lexMem_FAR <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/lexMem_FAR.mat")
# visMem_HR <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visMem_HR.mat")
# visMem_FAR <- readMat("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/visMem_FAR.mat")
# 
# # Combine lists into a data frame
# mem_data <- data.frame(lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR)
# # Write the data frame to an Excel file
# write.xlsx(mem_data, file = "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/mem.xlsx", sheetName = "Memory Data")


tbl_names <- c("encycl","vis","fcn","all")
#tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR","lexical_HR","visual_HR","lexical_FAR","visual_FAR")


## need to add category cols
## nmf from 228
#ref_tbl_encycl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="encycl")
#names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
#ref_tbl_vis <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="vis")
#names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
#ref_tbl_fcn <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="fcn")
#names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"
## nmf from 100
# ref_tbl_encycl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs_100.xlsx",sheet="encycl")
# names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
# ref_tbl_vis <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs_100.xlsx",sheet="vis")
# names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
# ref_tbl_fcn <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs_100.xlsx",sheet="fcn")
# names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"

refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
ref_tbl_encycl <- read_excel(refTblPath, sheet="encycl")
names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
ref_tbl_vis <- read_excel(refTblPath, sheet="vis")
names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
ref_tbl_fcn <- read_excel(refTblPath, sheet="fcn")
names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
ref_tbl_all <- read_excel(refTblPath, sheet="all")
names(ref_tbl_all)[names(ref_tbl_all) == "id"] <- "ItemID"


y_m_top_10 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
y_xm_top_10 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
x_m_top_10 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
y_x_m_top_10 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

# simon wants to plot where the knee is, so where when we keep adding predictors for X ~ M and the adj R2 starts to taper off
x_m_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())


# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'
# mem <- "lexical_HR"


for (tbl in tbl_names) {
  for (mem in memType) {
    for (ROI_name in ROI_name_variable) {
      tic()
      # (0) get your variables
      #curr_fac_tbl <- fac_tbl[[ROI_name]] # or curr_fac_tbl <- fac_tbl
      curr_tbl <- get(tbl) #get encycl, vis, or fcn
      curr_fac_tbl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
      hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
      ROI <- gsub('mask_','',ROI_name)
      
      # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
      curr_fac_tbl$visual_CR <- memColWithNaN$Sheet1$visual_CR
      curr_fac_tbl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
      
      
      ### need to add the rest of the facs that match the rows correctly
      # there are more than 67 factors, but 67 is the number of top factors after applying thresholding
      how_many_fac <- 150 #100 #228 #67, 8
      
      if (tbl == "encycl" || tbl == "vis" || tbl == "all") {
        
        # Generate column names for the additional factors beyond F08
        factor_cols <- paste0("F", sprintf("%02d", 9:how_many_fac))
        
      } else if (tbl == "fcn") {
        
        # Generate column names for the additional factors beyond F05
        factor_cols <- paste0("F", sprintf("%02d", 6:how_many_fac))
      }
      
      
      
      # ref_tbl_subset <- dplyr::select(ref_tbl, ItemID, all_of(factor_cols))
      # # Perform the left join
      # updated_curr_fac_tbl <- left_join(curr_fac_tbl, ref_tbl_subset, by = "ItemID")
      # # Update curr_fac_tbl
      # curr_fac_tbl <- updated_curr_fac_tbl
      
      # i hate that I have to do this, but it's simpler than re-doing everything.
      # i'm going to loop through all six mem types and just redundantly load the others
      
      # ref_tbl_subset <- dplyr::select(ref_tbl, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols))
      # # Perform the left join to add these columns to curr_fac_tbl
      # updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl, ref_tbl_subset, by = "ItemID")
      # # Update curr_fac_tbl with the newly added columns
      # curr_fac_tbl <- updated_curr_fac_tbl
      # # rename because I'm swimming in freaking arbitrary naming conventions
      # curr_fac_tbl <- curr_fac_tbl %>%
      #   rename(
      #     lexical_HR = lexMem_HR,
      #     lexical_FAR = lexMem_FAR,
      #     visual_HR = visMem_HR,
      #     visual_FAR = visMem_FAR
      #   )
      
      if (tbl == "encycl") {
        
        ref_tbl_encycl_subset <- dplyr::select(ref_tbl_encycl, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols)) 
        # Perform the left join to add these columns to curr_fac_tbl
        updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl, ref_tbl_encycl_subset, by = "ItemID")
        # Update curr_fac_tbl with the newly added columns
        curr_fac_tbl <- updated_curr_fac_tbl
        # rename because I'm swimming in freaking arbitrary naming conventions
        curr_fac_tbl <- curr_fac_tbl %>%
          rename(
            lexical_HR = lexMem_HR,
            lexical_FAR = lexMem_FAR,
            visual_HR = visMem_HR,
            visual_FAR = visMem_FAR
          )
        
      } else if (tbl == "vis") {
        ref_tbl_vis_subset <- dplyr::select(ref_tbl_vis, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols))
        # Perform the left join to add these columns to curr_fac_tbl
        updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl, ref_tbl_vis_subset, by = "ItemID")
        # Update curr_fac_tbl with the newly added columns
        curr_fac_tbl <- updated_curr_fac_tbl
        # rename because I'm swimming in freaking arbitrary naming conventions
        curr_fac_tbl <- curr_fac_tbl %>%
          rename(
            lexical_HR = lexMem_HR,
            lexical_FAR = lexMem_FAR,
            visual_HR = visMem_HR,
            visual_FAR = visMem_FAR
          )
        
      } else if (tbl == "fcn") {
        ref_tbl_fcn_subset <- dplyr::select(ref_tbl_fcn, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols))
        # Perform the left join to add these columns to curr_fac_tbl
        updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl, ref_tbl_fcn_subset, by = "ItemID")
        # Update curr_fac_tbl with the newly added columns
        curr_fac_tbl <- updated_curr_fac_tbl
        # rename because I'm swimming in freaking arbitrary naming conventions
        curr_fac_tbl <- curr_fac_tbl %>%
          rename(
            lexical_HR = lexMem_HR,
            lexical_FAR = lexMem_FAR,
            visual_HR = visMem_HR,
            visual_FAR = visMem_FAR
          )
      } else if (tbl == "all") {
        ref_tbl_all_subset <- dplyr::select(ref_tbl_all, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols))
        # Perform the left join to add these columns to curr_fac_tbl
        updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl, ref_tbl_all_subset, by = "ItemID")
        # Update curr_fac_tbl with the newly added columns
        curr_fac_tbl <- updated_curr_fac_tbl
        # rename because I'm swimming in freaking arbitrary naming conventions
        curr_fac_tbl <- curr_fac_tbl %>%
          rename(
            lexical_HR = lexMem_HR,
            lexical_FAR = lexMem_FAR,
            visual_HR = visMem_HR,
            visual_FAR = visMem_FAR
          )
      }
      
      # make my mediator variables
      M_list <- lapply(1:how_many_fac, function(i) curr_fac_tbl[[sprintf("F%02d", i)]])
      names(M_list) <- paste0("M", 1:how_many_fac)
      
      X = curr_fac_tbl[[mem]]
      Y = curr_fac_tbl$AvgROI
      # note, lexical_CR is a character object. need to convert to double
      # given how I'm converting earlier in the loop it shouldn't matter by this point
      if (class(X) == "character")
      {
        X = as.numeric(X)
      }
      
      Subj <- curr_fac_tbl$Subj
      ItemID <- curr_fac_tbl$ItemID
      
      
      #Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID)
      #Data_mixed_clean <- na.omit(Data_mixed)
      
      # (2) fill the data frame with what I need
      Data_mixed <- data.frame(X=X, Y=Y, Subj=Subj, ItemID=ItemID)
      
      # Add M1 to M8 (or M1 to M67 if needed, up to how_many_fac) from M_list to Data_mixed
      for (i in 1:how_many_fac) {  
        Data_mixed[paste0("M", i)] <- M_list[[i]]  # Use the index directly since M_list is now a list of columns
      }
      
      # Clean the data frame by removing rows with any NA values
      Data_mixed_clean <- na.omit(Data_mixed)
      
      # (3) average by item since we're just doing lm() to get variance explained
      # when I tried getting it from lmer() I got major overfitting
      average_values_by_Item <- Data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          Average_X = mean(X, na.rm = TRUE),
          Average_Y = mean(Y, na.rm = TRUE),
          # Use across() to apply mean() to all M columns
          across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
        )
      
      
      
      ### let's look at R2_change
      # y1 ~ X*M1 + X*M2 + X*M3 + …
      # y2 ~ X*M2 + X*M3 +…
      # do R2_change = R2_y1 – R2_y2 
      
      
      # for now let's do M1 through M50, and it can be modified later
      # Assuming 'df' is your data frame with all variables
      results <- data.frame(mediator = character(), 
                            r2_included = numeric(), 
                            r2_excluded = numeric(), 
                            r2_change = numeric(),
                            adj_r2_included = numeric(), 
                            adj_r2_excluded = numeric(), 
                            adj_r2_change = numeric())
      
      for (i in 1:how_many_fac) {
        # Create the formula for the model excluding the current mediator
        excluded_mediators <- paste0("Average_M", setdiff(1:how_many_fac, i), collapse = " + ")
        formula_excluded <- as.formula(paste("Average_X ~ ", excluded_mediators))
        
        # all mediators
        included_mediators <- paste0("Average_M", 1:how_many_fac, collapse = " + ")
        formula_included <- as.formula(paste("Average_X ~ ", included_mediators))
        
        # Fit the models
        model_included <- lm(formula_included, data = average_values_by_Item)
        model_excluded <- lm(formula_excluded, data = average_values_by_Item)
        
        # Calculate R2 values
        r2_included <- summary(model_included)$r.squared
        r2_excluded <- summary(model_excluded)$r.squared
        adj_r2_included <- summary(model_included)$adj.r.squared
        adj_r2_excluded <- summary(model_excluded)$adj.r.squared
        
        # Calculate R2 change
        r2_change <- r2_included - r2_excluded
        adj_r2_change <- adj_r2_included - adj_r2_excluded
        
        # Store the results
        results <- rbind(results, data.frame(mediator = paste0("M", i), 
                                             r2_included = r2_included, 
                                             r2_excluded = r2_excluded, 
                                             r2_change = r2_change, 
                                             adj_r2_included = adj_r2_included, 
                                             adj_r2_excluded = adj_r2_excluded, 
                                             adj_r2_change = adj_r2_change))
      # }
      # 
        
        #### I THINK THIS DOESN"T WORK. WHy would we run M2 through M150 and get the R2
        #### and then compare to the R2 from M1 on its own. Shouldn't we always be comparing 
        #### the one without to a standard that has all of them?
      # for (i in 1:how_many_fac) {
      #   # Create the formula for the model including all mediators
      #   included_mediators <- paste0("Average_X*Average_M", setdiff(1:how_many_fac, i), collapse = " + ")
      #   formula_included <- as.formula(paste("Average_Y ~ ", included_mediators))
      #   
      #   # Create the formula for the model excluding the current mediator
      #   excluded_mediators <- paste0("Average_X*Average_M", i, collapse = " + ")
      #   formula_excluded <- as.formula(paste("Average_Y ~ ", excluded_mediators))
      #   
      #   # Fit the models
      #   model_included <- lm(formula_included, data = average_values_by_Item)
      #   model_excluded <- lm(formula_excluded, data = average_values_by_Item)
      #   
      #   # Calculate R2 values
      #   r2_included <- summary(model_included)$r.squared
      #   r2_excluded <- summary(model_excluded)$r.squared
      #   adj_r2_included <- summary(model_included)$adj.r.squared
      #   adj_r2_excluded <- summary(model_excluded)$adj.r.squared
      #   
      #   # Calculate R2 change
      #   r2_change <- r2_included - r2_excluded
      #   adj_r2_change <- adj_r2_included - adj_r2_excluded
      #   
      #   # Store the results
      #   results <- rbind(results, data.frame(mediator = paste0("M", i), 
      #                                        r2_included = r2_included, 
      #                                        r2_excluded = r2_excluded, 
      #                                        r2_change = r2_change, 
      #                                        adj_r2_included = adj_r2_included, 
      #                                        adj_r2_excluded = adj_r2_excluded, 
      #                                        adj_r2_change = adj_r2_change))
      # }
      
      # all of the adjusted R2 are negative, but I think that's ok. I'm really just looking for the factors
      # whose exclusion makes the biggest difference.
      # now, if adj_r2_change is negative, that means excluding that factor decreases the adj R2. 
      # if it's positive, that means excluding the factor helps.
      # now, they're all negative, so I'm really looking for HOW negative. So, sorting from least to greatest gives me the top factors
      
      # Sort the results data frame by adj_r2_change column from least to greatest
      #results_sorted <- results[order(results$adj_r2_change), ]
      # you want descending order
      results_sorted <- results[order(-results$adj_r2_change), ]
      
      top_factors <- results_sorted$mediator[1:10]
      
      ### X ~ M all top factors
      # cumulative variance explained of factors on memorability
      cumulative_rsquared <- numeric(length(top_factors))
      cumulative_adj_rsquared <- numeric(length(top_factors))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_factors)) {
        selected_factors <- top_factors[1:i]
        # Prepend "Average_" to each factor name
        formula_terms <- lapply(selected_factors, function(factor) paste0("Average_", factor))
        # Collapse the formula terms into a single string separated by " + "
        formula_terms_combined <- paste(formula_terms, collapse = " + ")
        cumulative_formula <- as.formula(paste0("Average_X ~ ", formula_terms_combined))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        x_m_top_10 <- rbind(x_m_top_10, data.frame(tbl = tbl,
                                                   mem = mem,
                                                   ROI = ROI,
                                                   num_factors = selected_factors,
                                                   cumulative_rsq = cumulative_rsquared,
                                                   cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      ### Y ~ M all top factors
      # cumulative variance explained of factors on brain activity
      cumulative_rsquared <- numeric(length(top_factors))
      cumulative_adj_rsquared <- numeric(length(top_factors))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_factors)) {
        selected_factors <- top_factors[1:i]
        # Prepend "Average_" to each factor name
        formula_terms <- lapply(selected_factors, function(factor) paste0("Average_", factor))
        # Collapse the formula terms into a single string separated by " + "
        formula_terms_combined <- paste(formula_terms, collapse = " + ")
        cumulative_formula <- as.formula(paste0("Average_Y ~ ", formula_terms_combined))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        y_m_top_10 <- rbind(y_m_top_10, data.frame(tbl = tbl,
                                                   mem = mem,
                                                   ROI = ROI,
                                                   num_factors = selected_factors,
                                                   cumulative_rsq = cumulative_rsquared,
                                                   cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      ### Y ~ X * M
      # the purpose of including interaction terms is to look for how the relationship between
      # each factor and Y varies as a function of X. Let's you look at moderation
      cumulative_rsquared <- numeric(length(top_factors))
      cumulative_adj_rsquared <- numeric(length(top_factors))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_factors)) {
        selected_factors <- top_factors[1:i]
        # Prepend "Average_" to each factor name
        formula_terms <- lapply(selected_factors, function(factor) paste0("Average_X*Average_", factor))
        # Collapse the formula terms into a single string separated by " + "
        formula_terms_combined <- paste(formula_terms, collapse = " + ")
        cumulative_formula <- as.formula(paste0("Average_Y ~ ", formula_terms_combined))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        y_xm_top_10 <- rbind(y_xm_top_10, data.frame(tbl = tbl,
                                                     mem = mem,
                                                     ROI = ROI,
                                                     num_factors = selected_factors,
                                                     cumulative_rsq = cumulative_rsquared,
                                                     cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      ### Y ~ X + M
      # treats these are independent predictors
      cumulative_rsquared <- numeric(length(top_factors))
      cumulative_adj_rsquared <- numeric(length(top_factors))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_factors)) {
        selected_factors <- top_factors[1:i]
        # Prepend "Average_" to each factor name
        formula_terms <- lapply(selected_factors, function(factor) paste0("Average_", factor))
        # Collapse the formula terms into a single string separated by " + "
        formula_terms_combined <- paste(formula_terms, collapse = " + ")
        cumulative_formula <- as.formula(paste0("Average_Y ~ Average_X+", formula_terms_combined))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        y_x_m_top_10 <- rbind(y_x_m_top_10, data.frame(tbl = tbl,
                                                       mem = mem,
                                                       ROI = ROI,
                                                       num_factors = selected_factors,
                                                       cumulative_rsq = cumulative_rsquared,
                                                       cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      toc()
    } #end ROI_name_variable
    print(paste("finished mem:",mem))
  } #end memType
  print(paste("finished tbl:",tbl))
} #end tbl_names


# Create a list of data frames with names
dfs_list <- list(
  "y_m" = y_m_top_10,
  "y_xm" = y_xm_top_10,
  "x_m" = x_m_top_10,
  "y_x_m" = y_x_m_top_10
)



# Create a new workbook
wb <- createWorkbook()
# Loop through each data frame in the list and write to a new sheet in the workbook
for (sheet_name in names(dfs_list)) {
  addWorksheet(wb, sheet_name)  # Add a new worksheet with the sheet name
  writeData(wb, sheet_name, dfs_list[[sheet_name]])  # Write the data frame to the worksheet
}
# Save the workbook to a file
#file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_topFac_top10_from50.xlsx"
#file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_topFac_100_nmf100.xlsx"
file_path <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_encyclVisFcnAll_topFac_nmf%d.xlsx",factorNumber)


saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook

} #nmf_type 



# file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/y_m.csv"
# write.csv(y_m_top_10, file = file_path, row.names = FALSE)
# 
# file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/y_xm.csv"
# write.csv(y_xm_top_10, file = file_path, row.names = FALSE)
# 
# file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/x_m.csv"
# write.csv(x_m_top_10, file = file_path, row.names = FALSE)
# 
# file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/y_x_m.csv"
# write.csv(y_x_m_top_10, file = file_path, row.names = FALSE)


