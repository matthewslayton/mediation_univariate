
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

########## encycl, unilateral, lexMem
encycl <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
vis <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

mariamVals_df <- read.xlsx("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/mariamMemVals.xlsx")


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


tbl_names <- c("encycl","vis","fcn")
#tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR","lexical_HR","visual_HR","lexical_FAR","visual_FAR")


## need to add category cols
ref_tbl_encycl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="encycl")
names(ref_tbl_encycl)[names(ref_tbl_encycl) == "id"] <- "ItemID"
ref_tbl_vis <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="vis")
names(ref_tbl_vis)[names(ref_tbl_vis) == "id"] <- "ItemID"
ref_tbl_fcn <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="fcn")
names(ref_tbl_fcn)[names(ref_tbl_fcn) == "id"] <- "ItemID"

#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
#mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {
# output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# 
# output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# 
# output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# 
# output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

output_df_top_csa <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),resultType=character(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
interaction_output_df_top_csa <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),resultType=character(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
mem_output_df_top_csa <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),resultType=character(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

output_df_top_csa_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),resultType=character(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
interaction_output_df_top_csa_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),resultType=character(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
mem_output_df_top_csa_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),resultType=character(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())


# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'

# mem <- "lexical_HR"


for (tbl in tbl_names) {
  for (mem in memType) {
    for (ROI_name in ROI_name_variable) {
      tic()
      # (0) get your variables
      
      curr_tbl <- encycl #get encycl, vis, or fcn
      curr_fac_tbl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
      hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
      ROI <- gsub('mask_','',ROI_name)
      
      # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
      curr_fac_tbl$visual_CR <- memColWithNaN$Sheet1$visual_CR
      curr_fac_tbl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
      
      
      ### need to add the rest of the facs that match the rows correctly
      # there are more than 67 factors, but 67 is the number of top factors after applying thresholding
      how_many_fac <- 100 #228 #67, 8
      
      # Generate column names for the additional factors beyond F08
      factor_cols <- paste0("F", sprintf("%02d", 9:how_many_fac))
      
      # i hate that I have to do this, but it's simpler than re-doing everything.
      # i'm going to loop through all six mem types and just redundantly load the others
      
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
      }
      
      # make my mediator variables
      # M_list_encycl <- lapply(1:how_many_fac, function(i) curr_fac_tbl_encycl[[sprintf("F%02d", i)]])
      # names(M_list_encycl) <- paste0("M_lex_", 1:how_many_fac)
      #names(M_list_encycl) <- paste0("M", 1:how_many_fac)
      
      X = curr_fac_tbl[[mem]] #these are the same for vis
      Y = curr_fac_tbl$AvgROI
      # note, lexical_CR is a character object. need to convert to double
      # given how I'm converting earlier in the loop it shouldn't matter by this point
      if (class(X) == "character")
      {
        X = as.numeric(X)
      }
      
      # M_list_vis <- lapply(1:how_many_fac, function(i) curr_fac_tbl_vis[[sprintf("F%02d", i)]])
      # names(M_list_vis) <- paste0("M_vis_", 1:how_many_fac)
      #names(M_list_vis) <- paste0("M", 229:(228 + how_many_fac))
      
      ### now that curr_fac_tbl is complete, let's add the additional cols with the mariam mem data
      # first make a col with the row order so we can go back after the merge
      # Add a new column to the first data frame with the row order
      curr_fac_tbl$order <- 1:nrow(curr_fac_tbl)
      # merge based on ItemID
      merged_df <- merge(curr_fac_tbl, mariamVals_df, by="ItemID")
      # put the order back
      merged_df <- merged_df[order(merged_df$order),]
      
      Subj <- curr_fac_tbl$Subj
      ItemID <- curr_fac_tbl$ItemID
      CS_adj <- merged_df[["CS_adj"]]
      MD <- merged_df[["MD"]]
      slope <- merged_df[["slope"]]
      frequency_adj <- merged_df[["frequency_adj"]]
      NumFeat_adj <- merged_df[["NumFeat_adj"]]
      name_agreement_adj <- merged_df[["name_agreement_adj"]]
      
      #Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID)
      #Data_mixed_clean <- na.omit(Data_mixed)
      
      # (2) fill the data frame with what I need
      Data_mixed <- data.frame(X = X,
                               Y = Y,
                               Subj = Subj,
                               ItemID = ItemID,
                               CS_adj = CS_adj,
                               MD = MD,
                               slope = slope,
                               frequency_adj = frequency_adj,
                               NumFeat_adj = NumFeat_adj,
                               name_agreement_adj = name_agreement_adj)
      
      # Add M1 to M8 (or M1 to M67 if needed, up to how_many_fac) from M_list to Data_mixed
      # for (i in 1:how_many_fac) {
      #   # Add M_lex variables to Data_mixed
      #   Data_mixed[paste0("M_lex_", i)] <- M_list_encycl[[i]]
      #   # Add M_vis variables to Data_mixed, adjusting the index for vis variables
      #   Data_mixed[paste0("M_vis_", i)] <- M_list_vis[[i]]
      # }
      
      
      # Clean the data frame by removing rows with any NA values
      Data_mixed_clean <- na.omit(Data_mixed)
      
      # (3) average by item since we're just doing lm() to get variance explained
      # when I tried getting it from lmer() I got major overfitting
      average_values_by_Item <- Data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          Average_X = mean(X, na.rm = TRUE),
          Average_Y = mean(Y, na.rm = TRUE),
          Average_CS_adj = mean(CS_adj, na.rm = TRUE),
          Average_MD = mean(MD, na.rm = TRUE),
          Average_slope = mean(slope, na.rm = TRUE),
          Average_frequency_adj = mean(frequency_adj, na.rm = TRUE),
          Average_NumFeat_adj = mean(NumFeat_adj, na.rm = TRUE),
          Average_name_agreement_adj = mean(name_agreement_adj, na.rm = TRUE),
          # Use across() to apply mean() to all M columns
          across(starts_with("M"), mean, na.rm = TRUE, .names = "Average_{.col}")
        )
      
      # Initialize a vector to store accumulated new variables
      accumulated_new_variables <- character(0)
      # Initialize a vector to store result types
      result_types <- c("CS_adj", "MD", "slope", "frequency_adj", "NumFeat_adj", "name_agreement_adj")
      new_variables <- c("Average_CS_adj", "Average_MD", "Average_slope", "Average_frequency_adj", "Average_NumFeat_adj", "Average_name_agreement_adj")
      cumulative_rsquared <- numeric(length(new_variables))
      cumulative_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        accumulated_new_variables <- new_variables[1:i]
        formula_terms <- paste(accumulated_new_variables, collapse=" + ")
        # Create the cumulative formula for the current set of factors
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        # Store the results in mem_output_df_top_16
        mem_output_df_top_csa <- rbind(mem_output_df_top_csa, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          num_factors = i,
          resultType = result_types[i],
          cumulative_rsq = cumulative_rsquared[i],
          cumulative_adj_rsq = cumulative_adj_rsquared[i]
        ))
      }
      
      accumulated_new_variables <- character(0)
      # Initialize a vector to store result types
      result_types <- c("CS_adj", "MD", "slope", "frequency_adj", "NumFeat_adj", "name_agreement_adj")
      new_variables <- c("Average_CS_adj", "Average_MD", "Average_slope", "Average_frequency_adj", "Average_NumFeat_adj", "Average_name_agreement_adj")
      cumulative_rsquared <- numeric(length(new_variables))
      cumulative_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        accumulated_new_variables <- new_variables[1:i]
        formula_terms <- paste(accumulated_new_variables, collapse=" + ")
        
        # Create the cumulative formula for the current set of factors
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        # Store the results in mem_output_df_top_16
        output_df_top_csa <- rbind(output_df_top_csa, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          num_factors = i,
          resultType = result_types[i],
          cumulative_rsq = cumulative_rsquared[i],
          cumulative_adj_rsq = cumulative_adj_rsquared[i]
        ))
      }
      
      accumulated_new_variables <- character(0)
      # Initialize a vector to store result types
      result_types <- c("CS_adj", "MD", "slope", "frequency_adj", "NumFeat_adj", "name_agreement_adj")
      new_variables <- c("Average_CS_adj", "Average_MD", "Average_slope", "Average_frequency_adj", "Average_NumFeat_adj", "Average_name_agreement_adj")
      cumulative_rsquared <- numeric(length(new_variables))
      cumulative_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        
        accumulated_new_variables <- new_variables[1:i]
        formula_terms <- paste(paste0("Average_X*",accumulated_new_variables), collapse=" + ")
        # Create the cumulative formula for the current set of factors
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        # Store the results in mem_output_df_top_16
        interaction_output_df_top_csa <- rbind(interaction_output_df_top_csa, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          num_factors = i,
          resultType = result_types[i],
          cumulative_rsq = cumulative_rsquared[i],
          cumulative_adj_rsq = cumulative_adj_rsquared[i]
        ))
      }
      
      # Initialize vectors for individual R-squared and adjusted R-squared values
      indiv_rsquared <- numeric(length(new_variables))
      indiv_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        # Define the model with only the current variable
        indiv_formula <- as.formula(paste("Average_X ~ ", new_variables[i]))
        # Fit the linear model with just this variable
        indiv_model <- lm(indiv_formula, data = average_values_by_Item)
        # Extract and store the individual R-squared and adjusted R-squared values
        indiv_rsquared[i] <- summary(indiv_model)$r.squared
        indiv_adj_rsquared[i] <- summary(indiv_model)$adj.r.squared
        
        # Optional: Store individual results in a data frame (you can create a new one or modify the existing one)
        # For demonstration, we'll add to the existing one but with an indication that these are individual values
        mem_output_df_top_csa_indiv <- rbind(mem_output_df_top_csa_indiv, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          resultType = result_types[i],
          indiv_rsq = indiv_rsquared[i],
          indiv_adj_rsq = indiv_adj_rsquared[i]
        ))
      }
      
      # Initialize vectors for individual R-squared and adjusted R-squared values
      indiv_rsquared <- numeric(length(new_variables))
      indiv_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        # Define the model with only the current variable
        indiv_formula <- as.formula(paste("Average_Y ~ ", new_variables[i]))
        # Fit the linear model with just this variable
        indiv_model <- lm(indiv_formula, data = average_values_by_Item)
        # Extract and store the individual R-squared and adjusted R-squared values
        indiv_rsquared[i] <- summary(indiv_model)$r.squared
        indiv_adj_rsquared[i] <- summary(indiv_model)$adj.r.squared
        
        # Optional: Store individual results in a data frame (you can create a new one or modify the existing one)
        # For demonstration, we'll add to the existing one but with an indication that these are individual values
        output_df_top_csa_indiv <- rbind(output_df_top_csa_indiv, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          resultType = result_types[i],
          indiv_rsq = indiv_rsquared[i],
          indiv_adj_rsq = indiv_adj_rsquared[i]
        ))
      }
      
      # Initialize vectors for individual R-squared and adjusted R-squared values
      indiv_rsquared <- numeric(length(new_variables))
      indiv_adj_rsquared <- numeric(length(new_variables))
      
      for (i in 1:length(new_variables)) {
        formula_terms <- paste(paste0("Average_X*",new_variables[i]))
        # Create the cumulative formula for the current set of factors
        indiv_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with just this variable
        indiv_model <- lm(indiv_formula, data = average_values_by_Item)
        # Extract and store the individual R-squared and adjusted R-squared values
        indiv_rsquared[i] <- summary(indiv_model)$r.squared
        indiv_adj_rsquared[i] <- summary(indiv_model)$adj.r.squared
        
        # Optional: Store individual results in a data frame (you can create a new one or modify the existing one)
        # For demonstration, we'll add to the existing one but with an indication that these are individual values
        interaction_output_df_top_csa_indiv <- rbind(interaction_output_df_top_csa_indiv, data.frame(
          tbl = tbl,
          mem = mem,
          ROI = ROI,
          resultType = result_types[i],
          indiv_rsq = indiv_rsquared[i],
          indiv_adj_rsq = indiv_adj_rsquared[i]
        ))
      }
      
      
      toc()
    } #end ROI_name_variable
  } #end memType
} #end tbl_names

# Create a list of data frames with names
dfs_list <- list(
  "output" = output_df_top_csa,
  "interaction" = interaction_output_df_top_csa,
  "mem_output" = mem_output_df_top_csa,
  "output_indiv" = output_df_top_csa_indiv,
  "interaction_indiv" = interaction_output_df_top_csa_indiv,
  "mem_output_indiv" = mem_output_df_top_csa_indiv
)

# Create a new workbook
wb <- createWorkbook()
# Loop through each data frame in the list and write to a new sheet in the workbook
for (sheet_name in names(dfs_list)) {
  addWorksheet(wb, sheet_name)  # Add a new worksheet with the sheet name
  writeData(wb, sheet_name, dfs_list[[sheet_name]])  # Write the data frame to the worksheet
}
# Save the workbook to a file
file_path <-'/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_csa_only.xlsx'
saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook





