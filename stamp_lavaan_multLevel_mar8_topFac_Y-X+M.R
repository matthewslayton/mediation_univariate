# the goal here is simple. I want to put the mariam stats and NMF factors into one big model

### This script gets top factors from X~M
### go to stamp_lavaan_multLevel_mar8_altTopFacModels.R for the others
### Y~M, Y~X*M, Y~X+M

### note that here diff from _feb24, we control for individual subject behavior


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
library(tictoc)
library(performance)

factorNumber <- 100
# 
# factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

# add mask_Hipp_A and mask_Hipp_P
ROI_name_variable <- c('mask_AG_L','mask_AG_R','mask_ATL_L','mask_ATL_R',
                       'mask_FuG_L','mask_FuG_R','mask_Hipp_L',
                       'mask_Hipp_R','mask_IFG_L','mask_IFG_R',
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

########## encycl, unilateral, lexMem
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
# let's check the 300 the Ps actually saw. How could the full 995 be better?
# encycl_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
# vis_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")

#mariamVals_df <- read.xlsx("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/mariamMemVals.xlsx")


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

#### do the same for X?

## need to add category cols
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
#memType <- c("lexical_CR","visual_CR")
memType <- c("lexical_CR","visual_CR")#,"lexical_HR","visual_HR","lexical_FAR","visual_FAR")



#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),resultType=character(),factorNumber=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric(),ci_lower=numeric(),ci_upper=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

#mediation_output_list <- list()
#iteration <- 1 #don't forget to increment in the inntermost loop (which is the top_factors loop)

# tbl <- "encycl"
# mem <- "lexical_CR"
# mem <- "lexical_HR"
# ROI_name <- 'mask_AG_L'
# ROI_name <- 'mask_Pcun_L'


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
      ### now that curr_fac_tbl is complete, let's add the additional cols with the mariam mem data
      # first make a col with the row order so we can go back after the merge
      # Add a new column to the first data frame with the row order
      # curr_fac_tbl$order <- 1:nrow(curr_fac_tbl)
      # # merge based on ItemID
      # merged_df <- merge(curr_fac_tbl, mariamVals_df, by="ItemID")
      # # put the order back
      # merged_df <- merged_df[order(merged_df$order),]
      
      how_many_fac <- factorNumber #100 #228 #67, 8
      
      #### it looks like in varExplained_whereIsTheKnee
      #### how_many_fac is an integer
      #how_many_fac <- as.numeric(how_many_fac)
      how_many_fac <- as.integer(how_many_fac)
      
      if (tbl == "encycl" || tbl == "vis" || tbl == "all") {
        
        # Generate column names for the additional factors beyond F08
        factor_cols <- paste0("F", sprintf("%02d", 9:how_many_fac))
        
      } else if (tbl == "fcn") {
        
        # Generate column names for the additional factors beyond F05
        factor_cols <- paste0("F", sprintf("%02d", 6:how_many_fac))
      }
      
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
      # M1 <- merged_df[["F01"]]
      # M2 <- merged_df[["F02"]]
      # M3 <- merged_df[["F03"]]
      # M4 <- merged_df[["F04"]]
      # M5 <- merged_df[["F05"]]
      # M6 <- merged_df[["F06"]]
      # M7 <- merged_df[["F07"]]
      # M8 <- merged_df[["F08"]]
      # M9 <- merged_df[["CS_adj"]]
      # M10 <- merged_df[["MD"]]
      # M11 <- merged_df[["slope"]]
      # M12 <- merged_df[["frequency_adj"]]
      # M13 <- merged_df[["NumFeat_adj"]]
      # M14 <- merged_df[["name_agreement_adj"]]
      
      
      # Subj <- c(curr_fac_tbl$Subj, curr_fac_tbl_df$Subj)
      # ItemID <- c(curr_fac_tbl$ItemID, merged_df$ItemID)
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
      # Subj <- curr_fac_tbl$Subj
      # ItemID <- curr_fac_tbl$ItemID
      
      # (2) clean those NaN rows
      #Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,M9=M9,M10=M10,M11=M11,M12=M12,M13=M13,M14=M14,Subj=Subj,ItemID=ItemID)
      #Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,M9=M9,M10=M10,Subj=Subj,ItemID=ItemID)
      
      #Data_mixed_clean <- na.omit(Data_mixed)
      
      
      # (2) fill the data frame with what I need
      Data_mixed <- data.frame(X=X, Y=Y, Subj=Subj, ItemID=ItemID)
      
      # Add M1 to M8 (or M1 to M67 if needed, up to how_many_fac) from M_list to Data_mixed
      for (i in 1:how_many_fac) {  
        Data_mixed[paste0("M", i)] <- M_list[[i]]  # Use the index directly since M_list is now a list of columns
      }
      
      # #### mean ctr X
      # # Calculate the mean of the variables
      # mean_X <- mean(Data_mixed$X, na.rm = TRUE)
      # # Subtract the mean from each observation to mean-center the variable
      # Data_mixed$X <- Data_mixed$X - mean_X
      
      # Clean the data frame by removing rows with any NA values
      Data_mixed_clean <- na.omit(Data_mixed)
      
      # try mean-centering AFTER removing the rows with NaN.
     
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
      
      # explore the effect of subject-wise performance on the relationship between item and Y
      # model_1 <- lmer(Y ~ ItemID + (1|Subj),data=updated_data_mixed_clean)
      # 
      # plot(allEffects(model_1)) #gives estimated Y for each item
      # 
      # plot_model(model_1,type="re") #we do have subject-wise variability, so we want to add subj performance
      # 
      # model_2 <- lmer(Y ~ correctedRecog_CRET + ItemID + (1|Subj),data=updated_data_mixed_clean)
      # plot(allEffects(model_2)) #CR_CRET is positively associated with Y in AG_L
      # plot_model(model_2,type="re") #slightly different
      # 
      # summary(model_1) #std .2303
      # summary(model_2) #std .1961 --- and a little tighter
      # 
      # model_3 <- lmer(Y ~ correctedRecog_CRET*ItemID + (1|Subj),data=updated_data_mixed_clean)
      # plot_model(model_3,type="re")
      # summary(model_3) #std .1888, but not really that much lower. Doesn't justify such a complex model
      # # the subject-level variance isn't interacting with the item. So how well the subject does
      # # doesn't really impact the activity based on the item.
      # 
      model_both <- lmer(Y ~ correctedRecog_CRET + correctedRecog_PRET + ItemID + (1|Subj),data=updated_data_mixed_clean)
      # summary(model_both) #std .2039
      # plot_model(model_both,type="re")
      # cor(updated_data_mixed_clean$correctedRecog_CRET,updated_data_mixed_clean$correctedRecog_PRET)
      # # 0.3187622, so can keep them both in. Not sure why including both increases the variance slightly,
      # # but they don't seem colinear
      # 
      
      #get predicted values for each item. fit.model and use these (like you use the averages) in the mediation model
      model_sum <- summary(model_both)
      # estimates from this model get used instead of Y.
      # the predicted activity associated with that item, controling for each subject's performance
      # start from ItemID2022 0.372423
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
      # Replace Y with the estimates
      # This assumes that the Estimate column from itemID_data now exists in updated_data_mixed_clean after the join
     
      #updated_data_mixed_clean$Y <- updated_data_mixed_clean$Estimate
      #### My new Y isn't varying by item and I cluster by item in the mediation model
      # Could be due to including only fixed effects in the estimates. We have a random effect of subject
      # which is important becaused correctedRecog_CRET and _PRET are subject-specific
      
      ###### IF I DON"T DO THIS, I SHOULD BE ABLE TO RUN THE OLD Y
      #updated_data_mixed_clean$Y <- predict(model_both, re.form = NULL)
      updated_data_mixed_clean$Y <- predict(model_both, re.form = NA)
      
      
      
      # Mean-center M1 to M100?
      for (i in 1:how_many_fac) {
        column_name <- paste0("M", i) # Construct the column name dynamically
        updated_data_mixed_clean[[column_name]] <- updated_data_mixed_clean[[column_name]] - mean(updated_data_mixed_clean[[column_name]], na.rm = TRUE)
      }
      
      
      # Optionally, you can remove the Estimate column if it's no longer needed
      #updated_data_mixed_clean$Estimate <- NULL

      # # get average values just for extracting top factors
      # average_values_by_Item <- Data_mixed_clean %>%
      #   group_by(ItemID) %>%
      #   summarize(
      #     Average_X = mean(X, na.rm = TRUE),
      #     Average_Y = mean(Y, na.rm = TRUE),
      #     # Use across() to apply mean() to all M columns
      #     across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      #   )
      
      # comes from stamp_varExplained_R2_change.R
      
      #### this uses lm(), but in the original script (also whereIsTheKnee) I had
      ### averaged by item. Maybe I need to do the same here
      ### could average them too
      # average_values_by_Item <- updated_data_mixed_clean %>%
      #   group_by(ItemID) %>%
      #   summarize(
      #     Average_X = mean(X, na.rm = TRUE),
      #     Average_Y = mean(Y, na.rm = TRUE),
      #     # Use across() to apply mean() to all M columns
      #     across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      #   )
      
      #### AVERAGE AND DO LM()
      average_values_by_Item <- updated_data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          X = mean(X, na.rm = TRUE),
          Y = mean(Y, na.rm = TRUE),
          # Apply mean() to all M columns without changing their names
          across(starts_with("M"), mean, na.rm = TRUE)
        )


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
        formula_excluded <- as.formula(paste0("Y ~ X + ", excluded_mediators))

        included_mediators <- paste0("M", 1:how_many_fac, collapse = " + ")
        formula_included <- as.formula(paste0("Y ~ X + ", included_mediators))

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
      results_sorted <- results[order(-results$adj_r2_change), ]
     #results_sorted <- results[order(-results$r2_change), ]

      ##### what if we grab the top factors differently
#       results <- data.frame(mediator = character(), 
#                             r2_included = numeric(), 
#                             r2_excluded = numeric(), 
#                             r2_change = numeric(),
#                             adj_r2_included = numeric(), 
#                             adj_r2_excluded = numeric(), 
#                             adj_r2_change = numeric())
#       
#       for (i in 1:how_many_fac) {
#         
#         excluded_mediators <- paste0("M", setdiff(1:how_many_fac, i), collapse = " + ")
#         formula_excluded <- as.formula(paste0("Y ~ X + ", excluded_mediators))
#         
#         included_mediators <- paste0("M", 1:how_many_fac, collapse = " + ")
#         formula_included <- as.formula(paste0("Y ~ X + ", included_mediators))
#         
#         # Fit the models
#         model_included <- lm(formula_included, data = average_values_by_Item)
#         model_excluded <- lm(formula_excluded, data = average_values_by_Item)
#         
#         # Calculate R2 values
#         r2_included <- summary(model_included)$r.squared
#         r2_excluded <- summary(model_excluded)$r.squared
#         adj_r2_included <- summary(model_included)$adj.r.squared
#         adj_r2_excluded <- summary(model_excluded)$adj.r.squared
#         
#         # Calculate R2 change
#         r2_change <- r2_included - r2_excluded
#         adj_r2_change <- adj_r2_included - adj_r2_excluded
#         
#         # Store the results
#         results <- rbind(results, data.frame(mediator = paste0("M", i), 
#                                              r2_included = r2_included, 
#                                              r2_excluded = r2_excluded, 
#                                              r2_change = r2_change, 
#                                              adj_r2_included = adj_r2_included, 
#                                              adj_r2_excluded = adj_r2_excluded, 
#                                              adj_r2_change = adj_r2_change))
#       }
#       results_sorted <- results[order(-results$adj_r2_change), ]
#       # ##### GET PSEUDO-R2 AND USE LMER
#       # results <- data.frame(mediator = character(),
#       #                       r2_included = numeric(),
#       #                       r2_excluded = numeric(),
#       #                       r2_change = numeric())
#       #                       #adj_r2_included = numeric(),
#       #                       #adj_r2_excluded = numeric(),
#       #                       #adj_r2_change = numeric())
#       # 
#       # for (i in 1:how_many_fac) {
#       # 
#       #   excluded_mediators <- paste0("M", setdiff(1:how_many_fac, i), collapse = " + ")
#       #   formula_excluded <- as.formula(paste("X ~ ", excluded_mediators, " + (1|ItemID)"))
#       # 
#       #   included_mediators <- paste0("M", 1:how_many_fac, collapse = " + ")
#       #   formula_included <- as.formula(paste("X ~ ", included_mediators, " + (1|ItemID)"))
#       # 
#       #   model_included <- lmer(formula_included, data = updated_data_mixed_clean)
#       #   model_excluded <- lmer(formula_excluded, data = updated_data_mixed_clean)
#       # 
#       #     # Calculate R2 values
#       #   r2_included <- performance::r2(model_included)
#       #   r2_excluded <- performance::r2(model_excluded)
#       # 
#       #   # Calculate R2 change
#       #   r2_change <- r2_included$R2_conditional - r2_excluded$R2_conditional
#       # 
#       #     # Store the results
#       #     results <- rbind(results, data.frame(mediator = paste0("M", i),
#       #                                          r2_included = r2_included$R2_conditional,
#       #                                          r2_excluded = r2_excluded$R2_conditional,
#       #                                          r2_change = r2_change#,
#       #                                          #adj_r2_included = NA,  # Adjusted R^2 isn't directly available
#       #                                          #adj_r2_excluded = NA,
#       #                                          #adj_r2_change = NA
#       #                                          ))
#       # }
# # 
# #        results_sorted <- results[order(-results$r2_change), ]
#        
#       # all of the adjusted R2 are negative, but I think that's ok. I'm really just looking for the factors
#       # whose exclusion makes the biggest difference.
#       # now, if adj_r2_change is negative, that means excluding that factor decreases the adj R2. 
#       # if it's positive, that means excluding the factor helps.
#       # now, they're all negative, so I'm really looking for HOW negative. So, sorting from least to greatest gives me the top factors
#       
#       # Sort the results data frame by adj_r2_change column from least to greatest
#       #results_sorted <- results[order(results$adj_r2_change), ]
#       # you want descending order
# 
#       
        top_factors <- results_sorted$mediator#[1:10]
        numeric_factors <- as.numeric(sub("M", "", top_factors))
# 

     model_string <- sprintf("\n      level:1\n      Y~1\n      level:2\n      Y~b1*M%s + b2*M%s + b3*M%s + b4*M%s + b5*M%s + c*X\n      M%s~a1*X\n      M%s~a2*X\n      M%s~a3*X\n      M%s~a4*X\n      M%s~a5*X\n\n      #indirect and total effects\n      ab1:=a1*b1\n      ab2:=a2*b2\n      ab3:=a3*b3\n      ab4:=a4*b4\n      ab5:=a5*b5\n      # Total indirect effect\n      totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5\n\n      # Total effect\n      totalEffect := totalIndirect + c\n      ",
              numeric_factors[1], numeric_factors[2], numeric_factors[3], numeric_factors[4],numeric_factors[5],
              numeric_factors[1], numeric_factors[2], numeric_factors[3], numeric_factors[4],numeric_factors[5])

     #### this looks good. Can't see how the strings are different. Maybe I'm just grabbing the wrong factors
     #model_string <- ("\n      level:1\n      Y~1\n      level:2\n      Y~b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + c*X\n      M1~a1*X\n      M2~a2*X\n      M3~a3*X\n      M4~a4*X\n      M5~a5*X\n\n      #indirect and total effects\n      ab1:=a1*b1\n      ab2:=a2*b2\n      ab3:=a3*b3\n      ab4:=a4*b4\n      ab5:=a5*b5\n      # Total indirect effect\n      totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5\n\n      # Total effect\n      totalEffect := totalIndirect + c\n      ")

    #model_string <- ("\n      level:1\n      Y~1\n      level:2\n      Y~b1*M29 + b2*M25 + b3*M53 + b4*M99 + b5*M65 + c*X\n      M29~a1*X\n      M25~a2*X\n      M53~a3*X\n      M99~a4*X\n      M65~a5*X\n\n      #indirect and total effects\n      ab1:=a1*b1\n      ab2:=a2*b2\n      ab3:=a3*b3\n      ab4:=a4*b4\n      ab5:=a5*b5\n      # Total indirect effect\n      totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5\n\n      # Total effect\n      totalEffect := totalIndirect + c\n      ")
     
     # M29, M25, M53, M99, M65
     
     # fit_test <- sem(model_string,
     #                 data = Data_mixed_clean,
     #                 cluster = "ItemID",
     #                 se = "robust.huber.white")
      
      # model221 <- '
      # # level:1
      # Y~1
      # # level:2
      # Y~b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + c*X
      # M1~a1*X
      # M2~a2*X
      # M3~a3*X
      # M4~a4*X
      # M5~a5*X
      # 
      # # #indirect and total effects
      # ab1:=a1*b1
      # ab2:=a2*b2
      # ab3:=a3*b3
      # ab4:=a4*b4
      # ab5:=a5*b5
      # 
      # # Total indirect effect
      # totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5
      # 
      # # Total effect
      # totalEffect := totalIndirect + c
      # '


      # fit221<-sem(model221,data=Data_mixed_clean ,cluster="ItemID")
      # summary(fit221,fit.measures=TRUE)
      
      # fit221 <- sem(model221,
      #               data = Data_mixed_clean,
      #               cluster = "ItemID",
      #               #se = "robust.huber.white")
      #               se = "robust.cluster")
      # results_summary <- summary(fit221,fit.measures=TRUE)
      # # Extract parameter estimates with robust standard errors
      # pe_results <- parameterEstimates(fit221, standardized = TRUE)
      
      fit_new <- try(sem(model_string, 
                    data = updated_data_mixed_clean,
                    cluster = "ItemID",
                    se = "robust.huber.white"),silent=TRUE)
                    #se = "robust.cluster"),silent=TRUE)
      
      ### a back-up copy outside the try statement
      # fit_new <- sem(model_string,
      #                    data = updated_data_mixed_clean,
      #                    cluster = "ItemID",
      #                    se = "robust.huber.white")
      #                    #se = "robust.cluster")
      
      
      # Check if an error occurred
      if(class(fit_new) == "try-error") {
        # Prepare a row with tbl, mem, ROI, and NA for other columns
        error_row <- c(tbl, mem, ROI, rep(NA, ncol(mediation_output_df) - 4))
        
        # Add the error_row to mediation_output_df
        mediation_output_df <- rbind(mediation_output_df, error_row)
      } else {
        
        results_summary <- summary(fit_new,fit.measures=TRUE)
        # Extract parameter estimates with robust standard errors
        pe_results <- parameterEstimates(fit_new, standardized = TRUE)
        #pe_results <- parameterEstimates(fit_new, robust = TRUE, standardized = TRUE)
        
        
        #### need to extract results in a different way
        # a1_row <- pe_results[pe_results$rhs == "X" & grepl("M1", pe_results$lhs), ]
        # a2_row <- pe_results[pe_results$rhs == "X" & grepl("M2", pe_results$lhs), ]
        # a3_row <- pe_results[pe_results$rhs == "X" & grepl("M3", pe_results$lhs), ]
        # a4_row <- pe_results[pe_results$rhs == "X" & grepl("M4", pe_results$lhs), ]
        # a5_row <- pe_results[pe_results$rhs == "X" & grepl("M5", pe_results$lhs), ]
        # 
        # Assuming pe_results is your parameterEstimates dataframe
        
        # For 'a' paths
        a_start_index <- which(pe_results$rhs == "X" & grepl("^M", pe_results$lhs))[1]
        
        a_indices <- a_start_index:(a_start_index + 4)  # Assuming 5 'a' paths
        a_rows <- pe_results[a_indices, ]
        
        # For 'b' paths
        b_start_index <- which(pe_results$lhs == "Y" & grepl("^M", pe_results$rhs))[1]  # Index of the first 'b' path
        b_indices <- b_start_index:(b_start_index + 4)  # Assuming 5 'b' paths
        b_rows <- pe_results[b_indices, ]
        
        
        a1 <- c(tbl, mem, ROI, "a1",
                a_rows[1,]$lhs,      # factor number
                a_rows[1,]$est,      # Estimate for a1
                a_rows[1,]$se,       # Standard Error for a1
                a_rows[1,]$z,        # Z-value for a1
                a_rows[1,]$pvalue,   # P-value for a1
                a_rows[1,]$ci.lower, # Lower bound of the confidence interval for a1
                a_rows[1,]$ci.upper) # Upper bound of the confidence interval for a1
        a2 <- c(tbl, mem, ROI, "a2",
                a_rows[2,]$lhs,
                a_rows[2,]$est,
                a_rows[2,]$se,
                a_rows[2,]$z,
                a_rows[2,]$pvalue,
                a_rows[2,]$ci.lower,
                a_rows[2,]$ci.upper)
        a3 <- c(tbl, mem, ROI, "a3",
                a_rows[3,]$lhs,
                a_rows[3,]$est,
                a_rows[3,]$se,
                a_rows[3,]$z,
                a_rows[3,]$pvalue,
                a_rows[3,]$ci.lower,
                a_rows[3,]$ci.upper)
        a4 <- c(tbl, mem, ROI, "a4",
                a_rows[4,]$lhs,
                a_rows[4,]$est,
                a_rows[4,]$se,
                a_rows[4,]$z,
                a_rows[4,]$pvalue,
                a_rows[4,]$ci.lower,
                a_rows[4,]$ci.upper)
        a5 <- c(tbl, mem, ROI, "a5",
                a_rows[5,]$lhs,
                a_rows[5,]$est,
                a_rows[5,]$se,
                a_rows[5,]$z,
                a_rows[5,]$pvalue,
                a_rows[5,]$ci.lower,
                a_rows[5,]$ci.upper)
        
        # b1_row <- pe_results[pe_results$lhs == "Y" & grepl("M1", pe_results$rhs), ]
        # b2_row <- pe_results[pe_results$lhs == "Y" & grepl("M2", pe_results$rhs), ]
        # b3_row <- pe_results[pe_results$lhs == "Y" & grepl("M3", pe_results$rhs), ]
        # b4_row <- pe_results[pe_results$lhs == "Y" & grepl("M4", pe_results$rhs), ]
        # b5_row <- pe_results[pe_results$lhs == "Y" & grepl("M5", pe_results$rhs), ]
        # 
        b1 <- c(tbl, mem, ROI, "b1",
                b_rows[1,]$rhs,      # factor number
                b_rows[1,]$est,      # Estimate for b1
                b_rows[1,]$se,       # Standard Error for b1
                b_rows[1,]$z,        # Z-value for b1
                b_rows[1,]$pvalue,   # P-value for b1
                b_rows[1,]$ci.lower, # Lower bound of the confidence interval for b1
                b_rows[1,]$ci.upper) # Upper bound of the confidence interval for b1
        b2 <- c(tbl, mem, ROI, "b2",
                b_rows[2,]$rhs,
                b_rows[2,]$est,
                b_rows[2,]$se,
                b_rows[2,]$z,
                b_rows[2,]$pvalue,
                b_rows[2,]$ci.lower,
                b_rows[2,]$ci.upper)
        b3 <- c(tbl, mem, ROI, "b3",
                b_rows[3,]$rhs,
                b_rows[3,]$est,
                b_rows[3,]$se,
                b_rows[3,]$z,
                b_rows[3,]$pvalue,
                b_rows[3,]$ci.lower,
                b_rows[3,]$ci.upper)
        b4 <- c(tbl, mem, ROI, "b4",
                b_rows[4,]$rhs,
                b_rows[4,]$est,
                b_rows[4,]$se,
                b_rows[4,]$z,
                b_rows[4,]$pvalue,
                b_rows[4,]$ci.lower,
                b_rows[4,]$ci.upper)
        b5 <- c(tbl, mem, ROI, "b5",
                b_rows[5,]$rhs,
                b_rows[5,]$est,
                b_rows[5,]$se,
                b_rows[5,]$z,
                b_rows[5,]$pvalue,
                b_rows[5,]$ci.lower,
                b_rows[5,]$ci.upper)
        
        # Identify the row for the c path based on lhs and rhs
        c_row <- pe_results[pe_results$lhs == "Y" & pe_results$rhs == "X", ]
        
        # Extract the relevant information for the c path
        c_path <- c(tbl, mem, ROI, "c",
                    NA,            # something to put in the factor number column
                    c_row$est,      # Estimate for the c path
                    c_row$se,       # Standard Error for the c path
                    c_row$z,        # Z-value for the c path
                    c_row$pvalue,   # P-value for the c path
                    c_row$ci.lower, # Lower bound of the confidence interval for the c path
                    c_row$ci.upper) # Upper bound of the confidence interval for the c path
        
        indirect_estimates <- list()
        for (i in 1:5) {
          # Identify the row for each indirect effect based on the label
          indirect_row <- pe_results[pe_results$label == paste("ab", i, sep=""), ]
          
          if(a_rows[i,]$lhs == b_rows[i,]$rhs) {
            mediator_value <- a_rows[i,]$lhs  # or b_rows[1,]$rhs, since they match
            # Extract the relevant information for each indirect effect
            indirect_effect <- c(tbl, mem, ROI, paste("ab", i, sep=""),
                               mediator_value,                         # placeholder for factor number column
                               indirect_row$est,      # Estimate for the indirect effect
                               indirect_row$se,       # Standard Error for the indirect effect
                               indirect_row$z,        # Z-value for the indirect effect
                               indirect_row$pvalue,   # P-value for the indirect effect
                               indirect_row$ci.lower, # Lower bound of the CI for the indirect effect
                               indirect_row$ci.upper) # Upper bound of the CI for the indirect effect
          } else {
            indirect_effect <- c(tbl, mem, ROI, paste("ab", i, sep=""),
                               NA,                         # placeholder for factor number column
                               indirect_row$est,      # Estimate for the indirect effect
                               indirect_row$se,       # Standard Error for the indirect effect
                               indirect_row$z,        # Z-value for the indirect effect
                               indirect_row$pvalue,   # P-value for the indirect effect
                               indirect_row$ci.lower, # Lower bound of the CI for the indirect effect
                               indirect_row$ci.upper) # Upper bound of the CI for the indirect effect
            
          }
          # Store the extracted indirect effect in the list
          indirect_estimates[[i]] <- indirect_effect
          # Extract the relevant information for each indirect effect
        }
        
        # Combine all indirect effects into a matrix for easier binding with a, b, and c paths later
        indirect_estimates_matrix <- do.call(rbind, indirect_estimates)
        
        # Extract the row for totalIndirect effect
        totalIndirect_row <- pe_results[pe_results$label == "totalIndirect", ]
        
        # Add the mediator values that were used for the individual a-paths and b-paths
        unique_mediators <- c(a_rows[1,]$lhs,a_rows[2,]$lhs,a_rows[3,]$lhs,a_rows[4,]$lhs,a_rows[5,]$lhs)
        totalIndirect <- c(tbl, mem, ROI, "totalIndirect",
                             paste(unique(unique_mediators), collapse=", "),
                             totalIndirect_row$est,      # Estimate for the indirect effect
                             totalIndirect_row$se,       # Standard Error for the indirect effect
                             totalIndirect_row$z,        # Z-value for the indirect effect
                             totalIndirect_row$pvalue,   # P-value for the indirect effect
                             totalIndirect_row$ci.lower, # Lower bound of the CI for the indirect effect
                             totalIndirect_row$ci.upper) # Upper bound of the CI for the indirect effect
         
        # Extract the row for totalEffect
        totalEffect_row <- pe_results[pe_results$label == "totalEffect", ]
        
        # Extract the relevant information for totalEffect
        totalEffect <- c(tbl, mem, ROI, "totalEffect",
                         paste(unique(unique_mediators), collapse=", "),
                         totalEffect_row$est,      # Estimate for totalEffect
                         totalEffect_row$se,       # Standard Error for totalEffect
                         totalEffect_row$z,        # Z-value for totalEffect
                         totalEffect_row$pvalue,   # P-value for totalEffect
                         totalEffect_row$ci.lower, # Lower bound of the CI for totalEffect
                         totalEffect_row$ci.upper) # Upper bound of the CI for totalEffect
        
        
        mediation_output_df <- rbind(mediation_output_df,b1,b2,b3,b4,b5,
                                     a1,a2,a3,a4,a5,
                                     indirect_estimates_matrix,
                                     totalIndirect,totalEffect)
        
        # mediation_output_list[[iteration]] <- list(
        #   b1 = b1,
        #   b2 = b2,
        #   b3 = b3,
        #   b4 = b4,
        #   b5 = b5,
        #   a1 = a1,
        #   a2 = a2,
        #   a3 = a3,
        #   a4 = a4,
        #   a5 = a5,
        #   indirect_estimates_matrix = indirect_estimates_matrix,
        #   totalIndirect = totalIndirect,
        #   totalEffect = totalEffect
        # )
        
        
      } #end else statement as part of tryCatch statement
      
      #iteration <- iteration + 1
      ROI_time <- toc(log = TRUE)  # Store the time without printing
      cat(sprintf("Finished ROI: %s. Total time %f sec elapsed for this ROI\n", ROI_name, ROI_time$toc - ROI_time$tic))
    } #end ROI_name_variable
    mem_time <- toc(log = TRUE)  # Store the time without printing
    cat(sprintf("Finished mem: %s. Total time %f sec elapsed for this mem.\n", mem, mem_time$toc - mem_time$tic))
    } #end memType
  tbl_time <- toc(log = TRUE)  # Store the time without printing
  cat(sprintf("Finished tbl: %s. Total time %f sec elapsed for this tbl.\n", tbl, tbl_time$toc - tbl_time$tic))
} #end tbl_names

# convert lists back to df
#mediation_output_df <- do.call(rbind, lapply(mediation_output_list, data.frame, stringsAsFactors = FALSE))
### this is a list of lists, so it's tricky to convert to df


# Loop through each row and add the 'significant' column based on CI values
mediation_output_df$significant <- NA  #Initialize the column with NA values
colnames(mediation_output_df) <- c('tbl','mem','ROI','resultType','factorNumber','estimate','std_err','zval','pval','ci_lower','ci_upper','significant')  

for (row in 1:nrow(mediation_output_df)) {
  # Check if the confidence interval excludes zero
  mediation_output_df$significant[row] <- mediation_output_df$ci_lower[row] > 0 & mediation_output_df$ci_upper[row] > 0 | mediation_output_df$ci_lower[row] < 0 & mediation_output_df$ci_upper[row] < 0
}

# fix the weird data type issues
if (class(mediation_output_df$estimate) == "character")
{
  mediation_output_df$estimate = as.numeric(mediation_output_df$estimate)
}
if (class(mediation_output_df$std_err) == "character")
{
  mediation_output_df$std_err = as.numeric(mediation_output_df$std_err)
}
if (class(mediation_output_df$zval) == "character")
{
  mediation_output_df$zval = as.numeric(mediation_output_df$zval)
}
if (class(mediation_output_df$pval) == "character")
{
  mediation_output_df$pval = as.numeric(mediation_output_df$pval)
}
if (class(mediation_output_df$ci_lower) == "character")
{
  mediation_output_df$ci_lower = as.numeric(mediation_output_df$ci_lower)
}
if (class(mediation_output_df$ci_upper) == "character")
{
  mediation_output_df$ci_upper = as.numeric(mediation_output_df$ci_upper)
}

# write to spreadsheet
#filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_mariam_and_nmf.xlsx'
#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_mar5_nmf%d.xlsx',factorNumber)
#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_topFacFromX-M_mar12_nmf%d.xlsx',factorNumber)

#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_topFacFromX-M_mar12_nmf%d_original_Y.xlsx',factorNumber)
#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_topFacFromX-M_mar12_nmf%d_newY_meanCtr_X.xlsx',factorNumber)
#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_topFacFromX-M_mar12_nmf%d_newY_meanCtr_X_meanCtr_M_conPerTogether_topFacWithAveragingFirst_runAvgData_Y_X_plus_M.xlsx',factorNumber)
#filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_mar12_nmf%d_newY_meanCtr_X_meanCtr_M_conPerTogether_topFac_Y-M.xlsx',factorNumber)
filename <- sprintf('/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_mar12_nmf%d_newY_meanCtr_X_meanCtr_M_conPerTogether_topFac_Y-X+M.xlsx',factorNumber)

write_xlsx(mediation_output_df, filename)


