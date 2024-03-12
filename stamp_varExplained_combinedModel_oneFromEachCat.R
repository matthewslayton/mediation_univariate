# same as /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts/stamp_varExplained_whereIsTheKnee.R
# but with the top factors from the knee
# will be 43 encycl, 36 vis, 36 fac

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
#library(tictoc)
library(glmnet)
library(psych)


factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

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


ROI_noHemisphere <- c('mask_AG','mask_ATL','mask_FuG','mask_Hipp','mask_IFG',
                      'mask_ITG','mask_LOC','mask_MVOC','mask_Pcun','mask_Perirhinal',
                      'mask_PHC','mask_PhG','mask_PoG','mask_PrG',
                      'mask_pSTS','mask_Rhinal','mask_RSC','mask_SMG')


factorNumber <- 100 #set which nmf type we're running. Can't loop because I need to identify specific factors

########## encycl, unilateral, lexMem
## NMF with 228 factors and select the first 100
#encycl <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
#vis <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
#fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

# encyclPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx", factorNumber)
# visPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx", factorNumber)
# fcnPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx", factorNumber)

encyclPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx", factorNumber)
visPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx", factorNumber)
fcnPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx", factorNumber)

encycl <- import_list(encyclPath)
vis <- import_list(visPath)
fcn <- import_list(fcnPath)

tbl_names <- c("encycl","vis","fcn")
#tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR") #,"lexical_HR","visual_HR","lexical_FAR","visual_FAR")


## need to add category cols
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_encycl <- read_excel(refTblPath, sheet="encycl")
# names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_vis <- read_excel(refTblPath, sheet="vis")
# names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
# refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_%dfac/itemIDs_%d.xlsx", factorNumber, factorNumber)
# ref_tbl_fcn <- read_excel(refTblPath, sheet="fcn")
# names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"

refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_encycl <- read_excel(refTblPath, sheet="encycl")
names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_vis <- read_excel(refTblPath, sheet="vis")
names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
refTblPath <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_percentile_%s/itemIDs_percentile_%s.xlsx", factorNumber, factorNumber)
ref_tbl_fcn <- read_excel(refTblPath, sheet="fcn")
names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"

# results_list <- list()
# pvals <- list()
# iteration <- 1 #don't forget to increment in the inntermost loop (which is the top_factors loop)
# 
# 
# # simon wants to plot where the knee is, so where when we keep adding predictors for X ~ M and the adj R2 starts to taper off
x_m_top_100 <- data.frame(num_factors=numeric(),cumulative_rsq=numeric(),
                          cumulative_adj_rsq=numeric(),degrees_of_freedom=numeric())

pval <- data.frame(tbl = character(),
                   mem = character(),
                   factor = character(),
                   p_value = numeric(),
                   significant = logical())

# ## start with NMF with 228 factors and select the first 100
# # encycl, lexMem CR
# encycl_fac <- c("M100", "M29", "M86", "M45", "M26", "M11", "M2", "M44", "M1", "M75", "M22", "M13", "M90", "M97", "M31", "M65", 
#                 "M8", "M95", "M18", "M21", "M67", "M94", "M78", "M24", "M36", "M35", "M60", "M23", "M16", "M52", "M72", "M19", 
#                 "M91", "M27", "M96", "M12", "M66", "M32", "M92", "M54", "M34", "M64", "M3")
# # vis, lexMem CR
# vis_fac <- c("M26", "M52", "M6", "M72", "M29", "M33", "M31", "M94", "M41", "M9", "M10", "M27", "M22", "M2", "M1", 
#              "M19", "M3", "M96", "M67", "M20", "M70", "M77", "M76", "M69", "M62", "M36", "M8", "M15", "M65", "M68", 
#              "M80", "M30", "M100", "M99", "M82", "M71")
# # fcn, lexMem CR
# fcn_fac <- c("M100", "M38", "M55", "M26", "M86", "M67", "M72", "M29", "M64", "M84", "M1", "M51", "M50", "M92", 
#              "M61", "M62", "M94", "M2", "M68", "M15", "M11", "M33", "M98", "M78", "M44", "M99", "M81", "M31", 
#              "M87", "M12", "M9", "M73", "M93", "M25", "M24", "M7")
# 
# ## start with NMF with 100 factors and select all of them
# encycl_fac <- c("M10", "M27", "M41", "M1", "M2", "M20", "M30", "M25", "M32", "M21", 
#                 "M23", "M18", "M13", "M43", "M8", "M28", "M19", "M15", "M39", "M16", 
#                 "M31", "M38", "M22", "M42", "M45", "M3", "M33")
# vis_fac <- c("M24", "M31", "M28", "M13", "M26", "M32", "M40", "M20", "M3", "M17", 
#              "M18", "M49", "M1", "M8", "M21", "M10", "M9", "M45", "M2", "M48", 
#              "M29", "M33", "M39", "M38", "M5", "M42")
# fcn_fac <- c("M3", "M25", "M7", "M35", "M1", "M14", "M37", "M21")

## NMF with 150
# encycl_fac <- c("M91", "M10", "M141", "M126", "M24", "M144", "M2", "M125", "M1", 
#                 "M52", "M20", "M19", "M62", "M26", "M13", "M148", "M111", "M112", 
#                 "M139", "M41", "M137", "M149", "M37", "M31", "M122", "M77")
# 
# vis_fac <- c("M32", "M122", "M123", "M129", "M33", "M144", "M21", "M6", "M92", 
#              "M43", "M113", "M95", "M48", "M88", "M24", "M116", "M44", "M13", 
#              "M79", "M19", "M29", "M47", "M120", "M30", "M141", "M147", "M1", 
#              "M99", "M100", "M26", "M38", "M36", "M10")
# 
# fcn_fac <- c("M91", "M115", "M133", "M130", "M68", "M71", "M51", "M116", "M41", 
#              "M122", "M58", "M22", "M100", "M80")

# ## NMF with 50
# encycl_fac <- c("M8", "M41", "M2", "M27", "M1", "M24", "M26", "M23", "M31", "M19", 
#                 "M15", "M28", "M22", "M44", "M18", "M9", "M29", "M3", "M37")
# 
# vis_fac <- c("M25", "M43", "M30", "M31", "M13", "M26", "M34", "M1", "M19", "M3", 
#              "M27", "M7", "M16", "M41", "M15", "M2", "M10", "M9", "M22", "M48", "M32")
# 
# fcn_fac <- c("M19", "M3", "M37", "M1", "M39", "M11", "M44", "M48", "M28", "M50", 
#              "M15", "M10", "M24", "M4")

##### NMF 100 re-done with R2 change calculated correctly <- though it actually wasn't correct
# encycl_fac <- c("M10", "M27", "M20", "M98", "M100", "M1", "M77", "M31", "M19",
#                 "M21", "M97", "M99", "M64", "M15", "M75", "M18", "M96", "M74", 
#                 "M76", "M8", "M12", "M13")
# vis_fac <- c("M1", "M10", "M13", "M8", "M9", "M5", "M2", "M7", "M3")
# fcn_fac <- c("M84","M79")

#### NMF percentile 100 <- this calculates R2 change correctly AND gets the top factors correctly in descending order
encycl_fac <- c("M29", "M25", "M53", "M99", "M65", "M16", "M84", "M1", "M20", 
                "M2", "M31", "M26", "M60", "M18", "M72", "M9", "M49", "M40", 
                "M38", "M27", "M78", "M45", "M37", "M51", "M82", "M12")
vis_fac <- c("M51", "M32", "M1", "M2", "M24", "M10", "M74", "M28", "M90", "M36", 
             "M11", "M81", "M87", "M40", "M100", "M7", "M75", "M91", "M21", "M92", 
             "M29", "M31", "M83", "M63", "M99", "M66", "M18", "M26", "M72", "M65", 
             "M98", "M25", "M84", "M19", "M17", "M42", "M78", "M13")
fcn_fac <- c("M92", "M80", "M91")




# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'
# mem <- "lexical_HR"


#for (tbl in tbl_names) {
  #for (mem in memType) {
    #for (ROI_name in ROI_name_variable) {
    #tic()
  
    # have to hard-code the specific factors for each mem type
    mem <- "lexical_CR"
  
    # X and M are the same for every ROI, so we don't need that loop
    ROI_name <- 'mask_AG_L'
    
    tbl <- "encycl"
    # (0) get your variables
    #curr_fac_tbl <- fac_tbl[[ROI_name]] # or curr_fac_tbl <- fac_tbl
    curr_tbl <- get(tbl) #get encycl, vis, or fcn
    curr_fac_tbl_encycl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
    hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
    ROI <- gsub('mask_','',ROI_name)
    
    # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
    curr_fac_tbl_encycl$visual_CR <- memColWithNaN$Sheet1$visual_CR
    curr_fac_tbl_encycl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
    
    tbl <- "vis"
    curr_tbl <- get(tbl) #get encycl, vis, or fcn
    curr_fac_tbl_vis<- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
    hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
    ROI <- gsub('mask_','',ROI_name)
    
    # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
    curr_fac_tbl_vis$visual_CR <- memColWithNaN$Sheet1$visual_CR
    curr_fac_tbl_vis$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
    
    
    tbl <- "fcn"
    curr_tbl <- get(tbl) #get encycl, vis, or fcn
    curr_fac_tbl_fcn <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
    hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
    ROI <- gsub('mask_','',ROI_name)
    
    # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
    curr_fac_tbl_fcn$visual_CR <- memColWithNaN$Sheet1$visual_CR
    curr_fac_tbl_fcn$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
    
    
    ### need to add the rest of the facs that match the rows correctly
    # there are more than 67 factors, but 67 is the number of top factors after applying thresholding
   # factorNumber <- 100 #100 #228 #67, 8
    
   # if (tbl == "encycl" || tbl == "vis") {
      
      # Generate column names for the additional factors beyond F08
      factor_cols_encycl <- paste0("F", sprintf("%02d", 9:factorNumber))
      factor_cols_vis <- paste0("F", sprintf("%02d", 9:factorNumber))
      
    #} else if (tbl == "fcn") {
      
      # Generate column names for the additional factors beyond F05
      factor_cols_fcn <- paste0("F", sprintf("%02d", 6:factorNumber))
    #}
    
    
    
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
    
    #if (tbl == "encycl") {
      
      ref_tbl_encycl_subset <- dplyr::select(ref_tbl_encycl, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols_encycl)) 
      # Perform the left join to add these columns to curr_fac_tbl
      updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl_encycl, ref_tbl_encycl_subset, by = "ItemID")
      # Update curr_fac_tbl with the newly added columns
      curr_fac_tbl_encycl <- updated_curr_fac_tbl
      # rename because I'm swimming in freaking arbitrary naming conventions
      curr_fac_tbl_encycl <- curr_fac_tbl_encycl %>%
        rename(
          lexical_HR = lexMem_HR,
          lexical_FAR = lexMem_FAR,
          visual_HR = visMem_HR,
          visual_FAR = visMem_FAR
        )
      
   # } else if (tbl == "vis") {
      ref_tbl_vis_subset <- dplyr::select(ref_tbl_vis, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols_vis))
      # Perform the left join to add these columns to curr_fac_tbl
      updated_curr_fac_tbl_vis <- dplyr::left_join(curr_fac_tbl_vis, ref_tbl_vis_subset, by = "ItemID")
      # Update curr_fac_tbl with the newly added columns
      curr_fac_tbl_vis <- updated_curr_fac_tbl
      # rename because I'm swimming in freaking arbitrary naming conventions
      curr_fac_tbl_vis <- curr_fac_tbl_vis %>%
        rename(
          lexical_HR = lexMem_HR,
          lexical_FAR = lexMem_FAR,
          visual_HR = visMem_HR,
          visual_FAR = visMem_FAR
        )
      
  #  } else if (tbl == "fcn") {
      ref_tbl_fcn_subset <- dplyr::select(ref_tbl_fcn, ItemID, lexMem_HR, lexMem_FAR, visMem_HR, visMem_FAR, all_of(factor_cols_fcn))
      # Perform the left join to add these columns to curr_fac_tbl
      updated_curr_fac_tbl <- dplyr::left_join(curr_fac_tbl_fcn, ref_tbl_fcn_subset, by = "ItemID")
      # Update curr_fac_tbl with the newly added columns
      curr_fac_tbl_fcn <- updated_curr_fac_tbl
      # rename because I'm swimming in freaking arbitrary naming conventions
      curr_fac_tbl_fcn <- curr_fac_tbl_fcn %>%
        rename(
          lexical_HR = lexMem_HR,
          lexical_FAR = lexMem_FAR,
          visual_HR = visMem_HR,
          visual_FAR = visMem_FAR
        )
   # }
    
    ## encycl
    # make my mediator variables
    M_list <- lapply(1:factorNumber, function(i) curr_fac_tbl_encycl[[sprintf("F%02d", i)]])
    names(M_list) <- paste0("M", 1:factorNumber)
    X = curr_fac_tbl_encycl[[mem]]
    Y = curr_fac_tbl_encycl$AvgROI
    if (class(X) == "character")
    {
      X = as.numeric(X)
    }
    Subj <- curr_fac_tbl_encycl$Subj
    ItemID <- curr_fac_tbl_encycl$ItemID
    # (2) fill the data frame with what I need
    Data_mixed <- data.frame(X=X, Y=Y, Subj=Subj, ItemID=ItemID)
    # Add M1 to M8 (or M1 to M67 if needed, up to factorNumber) from M_list to Data_mixed
    for (i in 1:factorNumber) {  
      Data_mixed[paste0("M", i)] <- M_list[[i]]  # Use the index directly since M_list is now a list of columns
    }
    # Clean the data frame by removing rows with any NA values
    Data_mixed_clean <- na.omit(Data_mixed)
    # (3) average by item since we're just doing lm() to get variance explained
    # when I tried getting it from lmer() I got major overfitting
    average_values_by_Item_encycl <- Data_mixed_clean %>%
      group_by(ItemID) %>%
      summarize(
        Average_X = mean(X, na.rm = TRUE),
        Average_Y = mean(Y, na.rm = TRUE),
        # Use across() to apply mean() to all M columns
        across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      )
    ## vis
    # make my mediator variables
    M_list <- lapply(1:factorNumber, function(i) curr_fac_tbl_vis[[sprintf("F%02d", i)]])
    names(M_list) <- paste0("M", 1:factorNumber)
    X = curr_fac_tbl_vis[[mem]]
    Y = curr_fac_tbl_vis$AvgROI
    if (class(X) == "character")
    {
      X = as.numeric(X)
    }
    Subj <- curr_fac_tbl_vis$Subj
    ItemID <- curr_fac_tbl_vis$ItemID
    # (2) fill the data frame with what I need
    Data_mixed <- data.frame(X=X, Y=Y, Subj=Subj, ItemID=ItemID)
    # Add M1 to M8 (or M1 to M67 if needed, up to factorNumber) from M_list to Data_mixed
    for (i in 1:factorNumber) {  
      Data_mixed[paste0("M", i)] <- M_list[[i]]  # Use the index directly since M_list is now a list of columns
    }
    # Clean the data frame by removing rows with any NA values
    Data_mixed_clean <- na.omit(Data_mixed)
    # (3) average by item since we're just doing lm() to get variance explained
    # when I tried getting it from lmer() I got major overfitting
    average_values_by_Item_vis <- Data_mixed_clean %>%
      group_by(ItemID) %>%
      summarize(
        Average_X = mean(X, na.rm = TRUE),
        Average_Y = mean(Y, na.rm = TRUE),
        # Use across() to apply mean() to all M columns
        across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      )
    ## fcn
    # make my mediator variables
    M_list <- lapply(1:factorNumber, function(i) curr_fac_tbl_fcn[[sprintf("F%02d", i)]])
    names(M_list) <- paste0("M", 1:factorNumber)
    X = curr_fac_tbl_fcn[[mem]]
    Y = curr_fac_tbl_fcn$AvgROI
    if (class(X) == "character")
    {
      X = as.numeric(X)
    }
    Subj <- curr_fac_tbl_fcn$Subj
    ItemID <- curr_fac_tbl_fcn$ItemID
    # (2) fill the data frame with what I need
    Data_mixed <- data.frame(X=X, Y=Y, Subj=Subj, ItemID=ItemID)
    # Add M1 to M8 (or M1 to M67 if needed, up to factorNumber) from M_list to Data_mixed
    for (i in 1:factorNumber) {  
      Data_mixed[paste0("M", i)] <- M_list[[i]]  # Use the index directly since M_list is now a list of columns
    }
    # Clean the data frame by removing rows with any NA values
    Data_mixed_clean <- na.omit(Data_mixed)
    # (3) average by item since we're just doing lm() to get variance explained
    # when I tried getting it from lmer() I got major overfitting
    average_values_by_Item_fcn <- Data_mixed_clean %>%
      group_by(ItemID) %>%
      summarize(
        Average_X = mean(X, na.rm = TRUE),
        Average_Y = mean(Y, na.rm = TRUE),
        # Use across() to apply mean() to all M columns
        across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
      )
    
    # Rename columns to prefix with category names
    names(average_values_by_Item_encycl) <- paste("encycl", names(average_values_by_Item_encycl), sep = "_")
    names(average_values_by_Item_vis) <- paste("vis", names(average_values_by_Item_vis), sep = "_")
    names(average_values_by_Item_fcn) <- paste("fcn", names(average_values_by_Item_fcn), sep = "_")
    merged_df <- cbind(average_values_by_Item_encycl, average_values_by_Item_vis, average_values_by_Item_fcn)

    
    # Combine the factor arrays into one
    all_factors <- c(encycl_fac, vis_fac, fcn_fac)
    all_factors <- c(paste("encycl", encycl_fac, sep = "_"), paste("vis", vis_fac, sep = "_"), paste("fcn", fcn_fac, sep = "_"))
    
    # but remember I have the word "Average" in all of these. ugh
    # rename_columns <- function(df, prefix) {
    #   # Create new column names by adding 'Average_' between the prefix and the original column names
    #   new_col_names <- sapply(names(df), function(name) paste(prefix, "Average", name, sep = "_"))
    #   # Assign the new column names to the dataframe
    #   colnames(df) <- new_col_names
    #   return(df)
    # }
    # average_values_by_Item_encycl <- rename_columns(average_values_by_Item_encycl, "encycl")
    # average_values_by_Item_vis <- rename_columns(average_values_by_Item_vis, "vis")
    # average_values_by_Item_fcn <- rename_columns(average_values_by_Item_fcn, "fcn")
    # 
    # # After renaming, you can merge the data frames if needed
    # # Ensure that if there are any common columns (other than the ones to be merged), they are handled appropriately
    # merged_df <- cbind(average_values_by_Item_encycl, average_values_by_Item_vis, average_values_by_Item_fcn)
    encycl_factors <- all_factors[grep("^encycl_", all_factors)]
    vis_factors <- all_factors[grep("^vis_", all_factors)]
    fcn_factors <- all_factors[grep("^fcn_", all_factors)]
    
    # Determine the total length of the longest list
    total_length <- max(length(encycl_factors), length(vis_factors), length(fcn_factors))
    selected_factors <- c()
    # Calculate total length for the loop
    #total_length <- length(all_factors)
    
    cumulative_rsquared <- numeric(total_length)
    cumulative_adj_rsquared <- numeric(total_length)
    cumulative_df <- numeric(total_length)
    # Initialize counters for each category
    encycl_index <- 1
    vis_index <- 1
    fcn_index <- 1

    # Initialize a variable to store the selected factor for this iteration
    #selected_factor <- NULL
    for (i in 1:(3 * total_length)) {
      # Use modulo to cycle through each category
      # category <- i %% 3
      # index <- ceiling(i / 3)
      category_index <- (i - 1) %% 3 + 1  # Will cycle through 1, 2, 3 for encycl, vis, fcn
      
      
      selected_factor <- NULL

      # # Select the factor based on the category and index, using modulo to cycle within each category
      # if (category == 1 && index <= length(encycl_factors)) {  # 'encycl'
      #   selected_factor <- encycl_factors[index]
      # } else if (category == 2 && index <= length(vis_factors)) {  # 'vis'
      #   selected_factor <- vis_factors[index]
      # } else if (category == 0 && index <= length(fcn_factors)) {  # 'fcn' (Note: 0 because 3 %% 3 is 0)
      #   selected_factor <- fcn_factors[index]
      # }
      # Select one factor from each category in turn, if available
      # if (encycl_index <= length(encycl_factors)) {
      #   selected_factor <- encycl_factors[encycl_index]
      #   encycl_index <- encycl_index + 1
      # } else if (vis_index <= length(vis_factors)) {
      #   selected_factor <- vis_factors[vis_index]
      #   vis_index <- vis_index + 1
      # } else if (fcn_index <= length(fcn_factors)) {
      #   selected_factor <- fcn_factors[fcn_index]
      #   fcn_index <- fcn_index + 1
      # }
      if (category_index == 1 && encycl_index <= length(encycl_factors)) {
        selected_factor <- encycl_factors[encycl_index]
        encycl_index <- encycl_index + 1
      } else if (category_index == 2 && vis_index <= length(vis_factors)) {
        selected_factor <- vis_factors[vis_index]
        vis_index <- vis_index + 1
      } else if (category_index == 3 && fcn_index <= length(fcn_factors)) {
        selected_factor <- fcn_factors[fcn_index]
        fcn_index <- fcn_index + 1
      }
      if (!is.null(selected_factor)) {
        selected_factors <- c(selected_factors, selected_factor)
      }
      #selected_factors <- c(selected_factors, selected_factor)
      
      # Check if we've reached the end of all factors
      # Skip the iteration if no factor was selected
      # if (length(selected_factors) < i) {
      #   next
      # }
      if (is.null(selected_factor)) {
        next
      }
        # for (i in 1:total_length) {
    #   
    #   selected_factors <- all_factors[1:i]
      formula_terms_combined <- sapply(selected_factors, function(factor) {
        # Split the factor into type and variable name
        parts <- strsplit(factor, "_")[[1]]
        type <- parts[1]  # e.g., "encycl"
        variable_name <- parts[2]  # e.g., "M100"
        # Reconstruct the factor name with "Average_" inserted
        paste(type, "Average", variable_name, sep="_")
      })
      
      formula_terms_combined <- paste(formula_terms_combined, collapse = " + ")
      cumulative_formula <- as.formula(paste0("encycl_Average_X ~ ", formula_terms_combined)) #the X's are all the same. It's memorability which doesn't vary by the type of factor
      # Fit the linear model with the cumulative set of factors
      cumulative_model <- lm(cumulative_formula, data = merged_df)
      cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
      cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
      cumulative_df[i] <- summary(cumulative_model)$df[1] + summary(cumulative_model)$df[2]  # Sum of model and residual degrees of freedom
      # Combine selected_factors into a single comma-separated string
      factors_string <- paste(selected_factors, collapse = ", ")
      
      x_m_top_100 <- rbind(x_m_top_100, data.frame(num_factors = factors_string,
                                                   cumulative_rsq = cumulative_rsquared[i],
                                                   cumulative_adj_rsq = cumulative_adj_rsquared[i],
                                                   degrees_of_freedom = cumulative_df[i]))
      
      
    
      # results_list[[iteration]] <- list(
      #   nmf_type = as.character(factorNumber),
      #   num_factors = factors_string,
      #   cumulative_rsq = cumulative_rsquared,
      #   cumulative_adj_rsq = cumulative_adj_rsquared,
      #   degrees_of_freedom = cumulative_df)
      
      # Extract predictor names and their p-values
      coefficients_summary <- summary(cumulative_model)$coefficients
      if (!is.null(coefficients_summary)) {
        predictor_names <- rownames(coefficients_summary)[-1]  # Exclude intercept
        predictor_p_values <- coefficients_summary[-1, 4]  # Assuming the last column contains p-values
        # Ensure predictor_p_values matches the length of predictor_names
        if (length(predictor_p_values) < length(predictor_names)) {
          predictor_p_values <- c(predictor_p_values, rep(NA, length(predictor_names) - length(predictor_p_values)))
        }
        # Create a dataframe row for each predictor and append to pval
        for (j in 1:length(predictor_names)) {
          factor_name <- gsub("Average_", "", predictor_names[j])  # Remove "Average_" prefix
          p_value <- predictor_p_values[j]
          significant <- !is.na(p_value) && p_value < 0.05
          pval <- rbind(pval, data.frame(factor=factor_name,
                                         p_value=p_value,
                                         significant=significant,
                                         stringsAsFactors = FALSE))
          # pvals[[iteration]] <- list(
          #   nmf_type=as.character(factorNumber),
          #   factor=factor_name,
          #   p_value=p_value,
          #   significant=significant)
        }
      }
      # If all categories are exhausted, break the loop
      if (encycl_index > length(encycl_factors) && vis_index > length(vis_factors) && fcn_index > length(fcn_factors)) {
        break
      }
      
     } #total_length
    
    
    #toc()
    #} #end ROI_name_variable
   # print(paste("finished mem:",mem))
  #} #end memType
#  print(paste("finished tbl:",tbl))
#} #end tbl_names

  # convert lists back to df
  # x_m_top_100 <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
  # pval <- do.call(rbind, lapply(pvals, data.frame, stringsAsFactors = FALSE))

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
#file_path <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf%d_addOneFromEachCat.xlsx",factorNumber)
file_path <- sprintf("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf_percentile_%s_addOneFromEachCat.xlsx",factorNumber)
saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook

