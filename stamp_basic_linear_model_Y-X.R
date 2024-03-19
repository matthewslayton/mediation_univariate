#### the goal here is to look for R2 change.Remember, I want to know which are the top factors
### looking at "individual R2" with each predictor on its own isn't the best way to do this
### because you're not accounting for covariation between predictors.
### What if F32 and F8 covary/are collinear? When they're not in a model together where
### you make them compete, you can't account for the relatinoship between the two

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
#library(lavaan)
library(effects)
library(MASS)
library(pscl)
library(tictoc)
library(glmnet)
library(psych)
library(tidyr)


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



# ## simon wants to plot where the knee is, so where when we keep adding predictors for X ~ M and the adj R2 starts to taper off
# x_m_top_100 <- data.frame(nmf_type=character(),
#                           tbl=character(),
#                           mem=character(),
#                           num_factors=numeric(),
#                           cumulative_rsq=numeric(),
#                           cumulative_adj_rsq=numeric(),
#                           degrees_of_freedom=numeric())
# 
# pval <- data.frame(nmf_type=character(),
#                    tbl = character(),
#                    mem = character(),
#                    factor = character(),
#                    p_value = numeric(),
#                    significant = logical())
# 
# 
# M <- 200  # Replace 200 with the actual number of rows you expect
# 
# N <- 100  # Replace 100 with the actual number of rows you expect
# 
# x_m_top_100 <- data.frame(
#   nmf_type=character(N),
#   tbl=character(N),
#   mem=character(N),
#   num_factors=numeric(N),
#   cumulative_rsq=numeric(N),
#   cumulative_adj_rsq=numeric(N),
#   degrees_of_freedom=numeric(N),
#   stringsAsFactors = FALSE  # To avoid converting character vectors to factors
# )
# 
# 
# pval <- data.frame(
#   nmf_type=character(M),
#   tbl=character(M),
#   mem=character(M),
#   factor=character(M),
#   p_value=numeric(M),
#   significant=logical(M),
#   stringsAsFactors = FALSE  # To avoid converting character vectors to factors
# )

# using a data frame and rbind is SO SLOW! It has to make a new df each time, so as it gets bigger the runtime gets longer
results_original <- list()
results_adjusted <- list()
iteration <- 1 #don't forget to increment in the inntermost loop (which is the top_factors loop)


#nmf_types <- c(200,150,100,50) #need to do 150 too
#nmf_types <- c("mostCol", "50", "100")
#nmf_types <- c("50", "100")
nmf_types <- c("100")
#nmf_types <- c("200","300")
#for (factorNumber in nmf_types) {

factorNumber <- "100"

########## encycl, unilateral, lexMem
## NMF with 228 factors and select the first 100
# encycl <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
# vis <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
# fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

# encyclPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx", factorNumber)
# visPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx", factorNumber)
# fcnPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx", factorNumber)
# allPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_all_additionalROIs_unilateral.xlsx", factorNumber)


encyclPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx", factorNumber)
visPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx", factorNumber)
fcnPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx", factorNumber)
allPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_all_additionalROIs_unilateral.xlsx", factorNumber)


encycl <- import_list(encyclPath)
vis <- import_list(visPath)
fcn <- import_list(fcnPath)
all <- import_list(allPath)

subMem <- read.csv("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_submem_tbl.csv")

# re-label col name to match the rest of the data
subMem$Subj <- subMem$Subject

#### Y (activity) is mean-centered, so you need to mean-center correctedRecog_CRET and _PRET
# Calculate the mean of the variables
mean_correctedRecog_CRET <- mean(subMem$correctedRecog_CRET, na.rm = TRUE)
mean_correctedRecog_PRET <- mean(subMem$correctedRecog_PRET, na.rm = TRUE)

# Subtract the mean from each observation to mean-center the variable
subMem$correctedRecog_CRET <- subMem$correctedRecog_CRET - mean_correctedRecog_CRET
subMem$correctedRecog_PRET <- subMem$correctedRecog_PRET - mean_correctedRecog_PRET


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


## need to add category cols
## nmf from 228
# ref_tbl_encycl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="encycl")
# names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
# ref_tbl_vis <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="vis")
# names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
# ref_tbl_fcn <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="fcn")
# names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"


# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_encycl <- read_excel(refTblPath, sheet="encycl")
# names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_vis <- read_excel(refTblPath, sheet="vis")
# names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_fcn <- read_excel(refTblPath, sheet="fcn")
# names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_all <- read_excel(refTblPath, sheet="all")
# names(ref_tbl_all)[names(ref_tbl_all) == "id"] <- "ItemID"



refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_encycl <- read_excel(refTblPath, sheet="encycl")
names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_vis <- read_excel(refTblPath, sheet="vis")
names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_fcn <- read_excel(refTblPath, sheet="fcn")
names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_all <- read_excel(refTblPath, sheet="all")
names(ref_tbl_all)[names(ref_tbl_all) == "id"] <- "ItemID"

tbl_names <- c("encycl","vis","fcn","all")
#tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR","lexical_HR","visual_HR","lexical_FAR","visual_FAR")


# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'
# mem <- "lexical_HR"

# ROI_name <- 'mask_Hipp_R'
# ROI_name <- 'mask_Pcun_L'
# ROI_name <- 'mask_PoG_R'

# for (tbl in tbl_names) {
#  tic()
tbl <- "encycl"
  for (mem in memType) {
    tic()
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
     # how_many_fac <- 228 #50 #100 #228 #67, 8
       how_many_fac <- factorNumber

       how_many_fac <- as.integer(how_many_fac)
      
      # # Function to extract the numeric part from the right-most column name
      # extract_numeric_part <- function(data_frame) {
      #   # Get the name of the right-most column
      #   right_most_column <- names(data_frame)[ncol(data_frame)]
        
      #   # Extract the numeric part from the column name
      #   # This assumes the column name format is one letter followed by numbers
      #   numeric_part <- as.integer(gsub("F", "", right_most_column))
        
      #   return(numeric_part)
      # }
      
      # if (tbl == "encycl") {
      #   how_many_fac <- extract_numeric_part(ref_tbl_encycl)
      # } else if (tbl == "vis") {
      #   how_many_fac <- extract_numeric_part(ref_tbl_vis)
      # } else if (tbl == "fcn") {
      #   how_many_fac <- extract_numeric_part(ref_tbl_fcn)
      # } else if (tbl == "all") {
      #   how_many_fac <- extract_numeric_part(ref_tbl_all)
      # }
      
      # break
      
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


      # Calculate the mean of the variables
      mean_X <- mean(Data_mixed$X, na.rm = TRUE)
      # Subtract the mean from each observation to mean-center the variable
      Data_mixed$X <- Data_mixed$X - mean_X
      
      # add subject-wise subMem performance
      subMem_subset <- dplyr::select(subMem, ItemID, Subj, correctedRecog_CRET, correctedRecog_PRET)
      updated_data_mixed_clean <- dplyr::left_join(Data_mixed_clean, subMem_subset, by = c("Subj", "ItemID"))
      
      # use ItemID and Subj as factor
      updated_data_mixed_clean$ItemID <- as.factor(updated_data_mixed_clean$ItemID)
      updated_data_mixed_clean$Subj<- as.factor(updated_data_mixed_clean$Subj)

      model_both <- lmer(Y ~ correctedRecog_CRET + correctedRecog_PRET + ItemID + (1|Subj),data=updated_data_mixed_clean)
       #get predicted values for each item. fit.model and use these (like you use the averages) in the mediation model
      # we want to know the effect of different items on Y. That's why it's a fixed effect
      model_sum <- summary(model_both)
      coefficients_df <- model_sum$coefficients
      row_names <- rownames(coefficients_df)
      # Find the row names that contain "ItemID" and get the corresponding estimates
      itemID_rows <- coefficients_df[grep("ItemID", row_names), "Estimate"]
      # Create a new data frame from the item IDs and their estimates
      itemID_data <- data.frame(
        ItemID = gsub("ItemID", "", row_names[grep("ItemID", row_names)]), # This extracts just the numeric part
        Estimate = itemID_rows
      )
      # Make sure that the ItemID columns are of the same type before the join
      itemID_data$ItemID <- as.factor(itemID_data$ItemID)
      updated_data_mixed_clean$ItemID <- as.factor(updated_data_mixed_clean$ItemID)
      # Merge the new data frame with the updated_data_mixed_clean data frame
      # by matching the ItemID column, and update the Y values
      updated_data_mixed_clean <- dplyr::left_join(updated_data_mixed_clean, itemID_data, by = "ItemID")
     
      updated_data_mixed_clean$Y_adjusted <- predict(model_both, re.form = NA)
      
      ### need y_adjusted, itemID, and X (it's not subject-wise, so just need 242 total)
      unique_item_y_adjusted <- updated_data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(Y_adjusted = first(Y_adjusted),
                  X = first(X),
                  .groups = 'drop')
      
      ### for original Y which is subject-wise, have to average
      average_values_by_Item <- updated_data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          X = mean(X, na.rm = TRUE),
          Y = mean(Y, na.rm = TRUE),
          # Apply mean() to all M columns without changing their names
          across(starts_with("M"), mean, na.rm = TRUE)
        )
      
      ### if you're working manually, skip down to the bottom to plot
      
      #### it's the same Y for each item for all subjects
      # so just make a new data frame
      
      #### include fixed effects of corrected recognition for CRET and PRET AND itemID.
      #### don't include random effects of Subj.
      ### That means that we get predicted values for Y, adjusting for these fixed effects, without
      ### the confounding influence of subject-level variability. 
      
      ### at this point we have updated_data_mixed_clean
      
      ###### don't just show subjects images and get betas. 
      ## we're arguing that we need to adjust the brain activity, and then we can look
      ## at item memorability. We have to account for the subjects' memory (conceptual and perceptual)
      ## for the items. We adjust that out of the activation so it's more intrinsic to the item,
      ## accounting for subject-level variance.
      ## Can critique Contier et al. 2023. Three subjects
      ## Can be a figure
      
      
      
      
      # need beta coeffs (which are slopes), p-values, and R2 for var explained
      
      
      ### now, we've controlled for the effect of subject and item, but X wasn't included
      ### in the adjustment. X is repeated for each of the 19 subjects, so we have to control for that
      
      # Fit the linear model for Y~X
      model_adjusted <- lm(Y_adjusted ~ X, data = unique_item_y_adjusted)

      model_original <- lm(Y ~ X, data = average_values_by_Item)


      ## scatter plot Y_adjusted and avg_Y (by item)
      
      ## plot mturk memorability on y-axis and brain activity x-axis and make scatter 
      ## plot and line of best fit to show how adjusted Y vs original Y compares
      #### (see which Y predicts mem better)
      
      # Summary for model_adjusted
      summary_adjusted <- summary(model_adjusted)
      # R-squared and Adjusted R-squared values
      r_squared_adjusted <- summary_adjusted$r.squared
      adj_r_squared_adjusted <- summary_adjusted$adj.r.squared
      # Degrees of freedom
      df_adjusted <- summary_adjusted$df[2]
      # Beta coefficient for X
      beta_adjusted <- coef(summary_adjusted)["X", "Estimate"]
      # P-value for X
      p_value_adjusted <- coef(summary_adjusted)["X", "Pr(>|t|)"]
      # Consolidate results for model_adjusted
      results_adjusted[[iteration]] <- list(
        mem = mem,
        ROI = ROI_name,
        beta = beta_adjusted, 
        p_value = p_value_adjusted, 
        r_squared = r_squared_adjusted,
        df = df_adjusted,
        adj_r_squared = adj_r_squared_adjusted
      )
      
      # Summary for model_original
      summary_original <- summary(model_original)
      # R-squared and Adjusted R-squared values
      r_squared_original <- summary_original$r.squared
      adj_r_squared_original <- summary_original$adj.r.squared
      # Degrees of freedom
      df_original <- summary_original$df[2]
      # Beta coefficient for X
      beta_original <- coef(summary_original)["X", "Estimate"]
      # P-value for X
      p_value_original <- coef(summary_original)["X", "Pr(>|t|)"]
      
      # Consolidate results for model_original
      results_original[[iteration]] <- list(
        mem = mem,
        ROI = ROI_name,
        beta = beta_original, 
        p_value = p_value_original, 
        r_squared = r_squared_original,
        df = df_original,
        adj_r_squared = adj_r_squared_original
      )
      
      iteration = iteration + 1
    
  
      ROI_time <- toc(log = TRUE)  # Store the time without printing
      cat(sprintf("Finished ROI: %s. Total time %f sec elapsed for this ROI\n", ROI_name, ROI_time$toc - ROI_time$tic))
    } #end ROI_name_variable
    mem_time <- toc(log = TRUE)  # Store the time without printing
    cat(sprintf("Finished mem: %s. Total time %f sec elapsed for this mem.\n", mem, mem_time$toc - mem_time$tic))
    } #end memType
 # tbl_time <- toc(log = TRUE)  # Store the time without printing
#  cat(sprintf("Finished tbl: %s. Total time %f sec elapsed for this tbl.\n", tbl, tbl_time$toc - tbl_time$tic))
#  } #tbl
#} #end nmf_types (when we do NMF done with 200, 100, or 50 factors)


# convert lists back to df
results_original_df <- do.call(rbind, lapply(results_original, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
results_adjusted_df <- do.call(rbind, lapply(results_adjusted, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))

results_original_df_renamed <- results_original_df %>%
  rename(
    beta_original = beta,
    p_value_original = p_value,
    r_squared_original = r_squared,
    adj_r_squared_original = adj_r_squared,
    df_original = df
  )

# Rename columns for the adjusted data frame
results_adjusted_df_renamed <- results_adjusted_df %>%
  rename(
    beta_adjusted = beta,
    p_value_adjusted = p_value,
    r_squared_adjusted = r_squared,
    adj_r_squared_adjusted = adj_r_squared,
    df_adjusted = df
  )

# convert all this shit to numeric
results_adjusted_df_renamed <- results_adjusted_df_renamed %>%
  mutate(across(-c(mem, ROI), as.numeric))
results_original_df_renamed <- results_original_df_renamed %>%
  mutate(across(-c(mem, ROI), as.numeric))

# Bonferroni Correction
#results_original_df_renamed$p_value_bonferroni_original <- p.adjust(results_original_df_renamed$p_value_original, method = "bonferroni")
#results_adjusted_df_renamed$p_value_bonferroni_adjusted <- p.adjust(results_adjusted_df_renamed$p_value_adjusted, method = "bonferroni")



merged_df <- inner_join(results_original_df_renamed, results_adjusted_df_renamed, by = c("mem", "ROI"))

write_xlsx(merged_df,"/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/Y-X_sigROIs.xlsx")





#### when you pick ROIs, not just top, but hypothesis-based too
average_values_selected <- dplyr::select(average_values_by_Item, ItemID, Y, X)
unique_item_y_adjusted_selected <- dplyr::select(unique_item_y_adjusted, ItemID, Y_adjusted, X)

# Joining the dataframes on 'ItemID'
combined_data <- left_join(average_values_selected, unique_item_y_adjusted_selected, by = "ItemID")

colnames(combined_data)[3] <- 'X'

# Calculate correlation coefficients
cor_Y <- cor(combined_data$Y, combined_data$X, use = "complete.obs")
cor_Y_adjusted <- cor(combined_data$Y_adjusted, combined_data$X, use = "complete.obs")

# Reshape the data for plotting with ggplot2
long_data <- combined_data %>%
  pivot_longer(cols = c(Y, Y_adjusted), names_to = "Variable", values_to = "Brain_Activity")

# Plot
ggplot(long_data, aes(x = Brain_Activity, y = X, color = Variable)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add linear regression line without confidence interval
  theme_minimal() +
  labs(x = "Brain Activity", y = "Memorability", title = "Original vs Adjusted Brain Activity PoG R") +
  scale_color_manual(values = c("Y" = "blue", "Y_adjusted" = "red"), labels = c("Y" = "Original Y", "Y_adjusted" = "Adjusted Y")) +
  annotate("text", x = Inf, y = Inf, label = sprintf("Cor(Y, X): %.2f\nCor(Y_adjusted, X): %.2f", cor_Y, cor_Y_adjusted),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black") +
  theme(legend.position = "bottom")

