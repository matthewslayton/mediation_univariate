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
results_list <- list()
pvals <- list()
iteration <- 1 #don't forget to increment in the inntermost loop (which is the top_factors loop)


#nmf_types <- c(200,150,100,50) #need to do 150 too
#nmf_types <- c("mostCol", "50", "100")
#nmf_types <- c("50", "100")
nmf_types <- c("100")
#nmf_types <- c("200","300")
for (factorNumber in nmf_types) {

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
memType <- c("lexical_CR","visual_CR")#,"lexical_HR","visual_HR","lexical_FAR","visual_FAR")


# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'
# mem <- "lexical_HR"


for (tbl in tbl_names) {
  tic()
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
     
      updated_data_mixed_clean$Y <- predict(model_both, re.form = NA)

            # Mean-center M1 to M100?
      for (i in 1:how_many_fac) {
        column_name <- paste0("M", i) # Construct the column name dynamically
        updated_data_mixed_clean[[column_name]] <- updated_data_mixed_clean[[column_name]] - mean(updated_data_mixed_clean[[column_name]], na.rm = TRUE)
      }


      # (3) average by item since we're just doing lm() to get variance explained
      # when I tried getting it from lmer() I got major overfitting
      # average_values_by_Item <- updated_data_mixed_clean %>%
      #   group_by(ItemID) %>%
      #   summarize(
      #     Average_X = mean(X, na.rm = TRUE),
      #     Average_Y = mean(Y, na.rm = TRUE),
      #     # Use across() to apply mean() to all M columns
      #     across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      #   )
      average_values_by_Item <- updated_data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          X = mean(X, na.rm = TRUE),
          Y = mean(Y, na.rm = TRUE),
          # Apply mean() to all M columns without changing their names
          across(starts_with("M"), mean, na.rm = TRUE)
        )
      
      
      
      ### let's look at R2_change
      # y1 ~ X*M1 + X*M2 + X*M3 + …
      # y2 ~ X*M2 + X*M3 +…
      # do R2_change = R2_y1 – R2_y2 
      
      
      # for now let's do M1 through M50, and it can be modified later
      # Assuming 'df' is your data frame with all variables
      # results <- data.frame(mediator = character(), 
      #                       r2_included = numeric(), 
      #                       r2_excluded = numeric(), 
      #                       r2_change = numeric(),
      #                       adj_r2_included = numeric(), 
      #                       adj_r2_excluded = numeric(), 
      #                       adj_r2_change = numeric())
      # 
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
      results <- data.frame(mediator = character(), 
                            r2_included = numeric(), 
                            r2_excluded = numeric(), 
                            r2_change = numeric(),
                            adj_r2_included = numeric(), 
                            adj_r2_excluded = numeric(), 
                            adj_r2_change = numeric())
      
      for (i in 1:how_many_fac) {
        # Create the formula for the model excluding the current mediator
        excluded_mediators <- paste0("M", setdiff(1:how_many_fac, i), collapse = " + ")
        formula_excluded <- as.formula(paste("Y ~ ", excluded_mediators))
        
        # all mediators
        included_mediators <- paste0("M", 1:how_many_fac, collapse = " + ")
        formula_included <- as.formula(paste("Y ~ ", included_mediators))
        
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
      }
      # all of the adjusted R2 are negative, but I think that's ok. I'm really just looking for the factors
      # whose exclusion makes the biggest difference.
      # now, if adj_r2_change is negative, that means excluding that factor decreases the adj R2. 
      # if it's positive, that means excluding the factor helps.
      # now, they're all negative, so I'm really looking for HOW negative. So, sorting from least to greatest gives me the top factors
      
      # Sort the results data frame by adj_r2_change column from greatest to least
      # because the biggest difference would mean that factor is important
      results_sorted <- results[order(-results$adj_r2_change), ]
      #results_sorted <- results[order(results$adj_r2_change), ] #i had it like this before
      
      top_factors <- results_sorted$mediator
      
      # for testing
      #top_factors <- c("M1","M2","M3","M4","M5")
      
      ### X ~ M all top factors
      # cumulative variance explained of factors on memorability
      cumulative_rsquared <- numeric(length(top_factors))
      cumulative_adj_rsquared <- numeric(length(top_factors))
      cumulative_df <- numeric(length(top_factors))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_factors)) {
        selected_factors <- top_factors[1:i]
        # Use factor names directly without prepending "Average_"
        formula_terms <- lapply(selected_factors, function(factor) factor)  # No need to prepend "Average_", just use the factor directly
        # Collapse the formula terms into a single string separated by " + "
        formula_terms_combined <- paste(formula_terms, collapse = " + ")
        cumulative_formula <- as.formula(paste0("Y ~ ", formula_terms_combined))  # Assuming the outcome variable is just "Y"
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)  # Ensure 'average_values_by_Item' has the correctly named variables
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        cumulative_df <- summary(cumulative_model)$df[1] + summary(cumulative_model)$df[2]  # Sum of model and residual degrees of freedom
        
        # Combine selected_factors into a single comma-separated string
        factors_string <- paste(selected_factors, collapse = ", ")
      

        
        # x_m_top_100 <- rbind(x_m_top_100, data.frame(nmf_type=as.character(factorNumber),
        #                                            tbl = tbl,
        #                                            mem = mem,
        #                                            num_factors = factors_string,
        #                                            cumulative_rsq = cumulative_rsquared,
        #                                            cumulative_adj_rsq = cumulative_adj_rsquared,
        #                                            degrees_of_freedom = cumulative_df))
        
        results_list[[iteration]] <- list(
          nmf_type = as.character(factorNumber),
          tbl = tbl,
          mem = mem,
          ROI = ROI_name,
          num_factors = factors_string,
          cumulative_rsq = cumulative_rsquared,
          cumulative_adj_rsq = cumulative_adj_rsquared,
          degrees_of_freedom = cumulative_df
        )
        
        # Extract predictor names and their p-values
        coefficients_summary <- summary(cumulative_model)$coefficients
        if (!is.null(coefficients_summary)) {
          predictor_names <- rownames(coefficients_summary)[-1]  # Exclude intercept
          predictor_p_values <- coefficients_summary[-1, 4]  # Assuming the fourth column contains p-values
          # Ensure predictor_p_values matches the length of predictor_names
          if (length(predictor_p_values) < length(predictor_names)) {
            predictor_p_values <- c(predictor_p_values, rep(NA, length(predictor_names) - length(predictor_p_values)))
          }
          # Create a dataframe row for each predictor and append to some results container (assuming it's 'pval')
          for (j in 1:length(predictor_names)) {
            factor_name <- predictor_names[j]  # Use predictor names directly without removing any prefix
            p_value <- predictor_p_values[j]
            significant <- !is.na(p_value) && p_value < 0.05
            # pval <- rbind(pval, data.frame(nmf_type=as.character(factorNumber),
            #                                tbl=tbl,
            #                                mem=mem,
            #                                factor=factor_name,
            #                                p_value=p_value,
            #                                significant=significant,
            #                                stringsAsFactors = FALSE))
            pvals[[iteration]] <- list(
              nmf_type=as.character(factorNumber),
               tbl=tbl,
               mem=mem,
               ROI=ROI_name,
               factor=factor_name,
               p_value=p_value,
               significant=significant)
          }
        }
        iteration <- iteration + 1  # Increment iteration after each assignment
      } #top_factors
     
  
            ROI_time <- toc(log = TRUE)  # Store the time without printing
      cat(sprintf("Finished ROI: %s. Total time %f sec elapsed for this ROI\n", ROI_name, ROI_time$toc - ROI_time$tic))
    } #end ROI_name_variable
    mem_time <- toc(log = TRUE)  # Store the time without printing
    cat(sprintf("Finished mem: %s. Total time %f sec elapsed for this mem.\n", mem, mem_time$toc - mem_time$tic))
    } #end memType
  tbl_time <- toc(log = TRUE)  # Store the time without printing
  cat(sprintf("Finished tbl: %s. Total time %f sec elapsed for this tbl.\n", tbl, tbl_time$toc - tbl_time$tic))
  } #tbl
} #end nmf_types (when we do NMF done with 200, 100, or 50 factors)

# convert lists back to df
x_m_top_100 <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
#x_m_top_100 <- as.data.frame(results_list)
pval <- do.call(rbind, lapply(pvals, data.frame, stringsAsFactors = FALSE))
#pval <- as.data.frame(pvals)


# Create a list of data frames with names
dfs_list <- list(
  "x_m" = x_m_top_100,
  "pval" = pval
)



# Create a new workbook
wb <- createWorkbook()
# Loop through each data frame in the list and write to a new sheet in the workbook
for (sheet_name in names(dfs_list)) {
  addWorksheet(wb, sheet_name)  # Add a new worksheet with the sheet name
  writeData(wb, sheet_name, dfs_list[[sheet_name]])  # Write the data frame to the worksheet
}
# Save the workbook to a file
#file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_200_150_100_50_nmf_whereIsTheKnee_encyclVisFcnAll.xlsx"
#file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_percentile_100_50_nmf_whereIsTheKnee_encyclVisFcnAll.xlsx"
file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_Y-M.xlsx"
saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook


subset_var <- x_m_top_100[c("ROI", "cumulative_adj_rsq")]
# now get the fifth row for each ROI
subset_var_fifth_rows <- subset_var[seq(1, nrow(subset_var), by = 5), ]
subset_var_fifth_rows$ROI <- gsub("mask_", "", subset_var_fifth_rows$ROI)

#from mediation script
subset_data_sig <- filtered_data_sig[c("ROI","estimate")]

# plot cumulative_adj_rsq and estimate matched for ROI. Also there will be more in cumulative_adj_rsq, so skip those
# Ensure we're only working with ROIs present in both subsets
matched_data <- inner_join(subset_var_fifth_rows, subset_data_sig, by = "ROI")
cor_value <- cor(matched_data$cumulative_adj_rsq, matched_data$estimate)

ggplot(matched_data, aes(x = cumulative_adj_rsq, y = estimate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste("R =", round(cor_value, 2)), hjust = 1.1, vjust = 2) +
  theme_minimal() +
  labs(x = "Cumulative Adjusted R-Squared", y = "Estimate", title = "Y~M Cum Adj R2 x Y~M Estimate for Encycl LexCR")

# 
# 
# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "encycl" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# # Extract the right-most/new factor from each string in subset_df$num_factors
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# # Convert latest_factor to a factor with levels in the order they appear
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Encyclopedic Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")
# 
# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "vis" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# # Extract the right-most/new factor from each string in subset_df$num_factors
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# # Convert latest_factor to a factor with levels in the order they appear
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq)) +
#   geom_bar(stat = "identity", fill = "brown2") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Visual Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")
# 
# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "fcn" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# # Extract the right-most/new factor from each string in subset_df$num_factors
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# # Convert latest_factor to a factor with levels in the order they appear
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq)) +
#   geom_bar(stat = "identity", fill = "darkgoldenrod1") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Functional Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")
# # 
# # # I did darkorange3, steelblue, and darkorchid4. maybe do darkgoldenrod1 to match fcn
# 
# subset_df <- subset(x_m_top_100, nmf_type == 150 & tbl == "all" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# # Extract the right-most/new factor from each string in subset_df$num_factors
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# # Convert latest_factor to a factor with levels in the order they appear
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq)) +
#   geom_bar(stat = "identity", fill = "green4") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR All Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")

### heat map using factor index as the continuous value for the color gradient

# # get the highest adj R2 so I can set the ylim to the same height for all plots
# max_rsq <- max(x_m_top_100$cumulative_adj_rsq, na.rm = TRUE)

# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "encycl" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq

# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))

# subset_df$factor_index <- as.numeric(as.factor(subset_df$latest_factor))
# # Now plot using this index for the fill color
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq, fill = factor_index)) +
#   geom_bar(stat = "identity") +
#   scale_fill_gradient(low = "#1f77b4", high = "#d62728", guide = FALSE) +  # Adjust colors as needed
#   ylim(0, max_rsq) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Encycl Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")

# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "vis" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))

# subset_df$factor_index <- as.numeric(as.factor(subset_df$latest_factor))
# # Now plot using this index for the fill color
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq, fill = factor_index)) +
#   geom_bar(stat = "identity") +
#   scale_fill_gradient(low = "#377eb8", high = "#ff7f00", guide = FALSE) +  # Adjust colors as needed
#   ylim(0, max_rsq) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Vis Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")

# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "fcn" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))

# subset_df$factor_index <- as.numeric(as.factor(subset_df$latest_factor))
# # Now plot using this index for the fill color
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq, fill = factor_index)) +
#   geom_bar(stat = "identity") +
#   scale_fill_gradient(low = "#4daf4a", high = "#984ea3", guide = FALSE) +  # Adjust colors as needed
#   ylim(0, max_rsq) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR Fcn Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")

# subset_df <- subset(x_m_top_100, nmf_type == 100 & tbl == "all" & mem == "lexical_CR")
# cumulative_adj_rsq <- subset_df$cumulative_adj_rsq
# subset_df$latest_factor <- sapply(strsplit(subset_df$num_factors, ", "), function(x) tail(x, 1))
# subset_df$latest_factor <- factor(subset_df$latest_factor, levels = unique(subset_df$latest_factor))

# subset_df$factor_index <- as.numeric(as.factor(subset_df$latest_factor))
# # Now plot using this index for the fill color
# ggplot(subset_df, aes(x = latest_factor, y = cumulative_adj_rsq, fill = factor_index)) +
#   geom_bar(stat = "identity") +
#   scale_fill_gradient(low = "#a6cee3" , high = "#fdbf6f", guide = FALSE) +  # Adjust colors as needed
#   ylim(0, max_rsq) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   labs(title = "Cumulative Adj R-Squared lexMem CR All Fac",
#        x = "Factor",
#        y = "Cumulative Adj R-Squared")

# Adjusted Blue and Red:
#   Low: "#1f77b4" (adjusted blue)
# High: "#d62728" (adjusted red)
# 
# Blue and Orange:
#   Low: "#377eb8" (vivid blue)
# High: "#ff7f00" (bright orange)
# 
# Green and Purple:
#   Low: "#4daf4a" (muted green)
# High: "#984ea3" (muted purple)
# 
# Sky Blue and Yellow-Orange:
#   Low: "#a6cee3" (light, sky blue)
# High: "#fdbf6f" (yellow-orange)

