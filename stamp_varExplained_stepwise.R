

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

output_df_top_forward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
interaction_output_df_top_forward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
mem_output_df_top_forward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

output_df_top_backward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
interaction_output_df_top_backward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
mem_output_df_top_backward <- data.frame(tbl=character(),mem=character(),ROI=character(),numStepFactors=numeric(),factor_list=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())


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
      how_many_fac <- 100 #50 #228 #67, 8
      
      if (tbl == "encycl" || tbl == "vis") {
        
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
      
      
      
      ### what about stepwise regression to extract top factors?
      forwardModel <- list()
      top_predictors_forward <- list()
      selected_factors <- 1:100
      # Generate the predictor terms with "Average_M" prefix and concatenate them with " + "
      formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
      # Combine the predictor terms with the dependent variable to form the complete formula
      full_formula <- paste("Average_X ~", formula_terms)
      # Full model
      full_model <- lm(full_formula, data=average_values_by_Item)
      # Intercept-only model for the starting point
      intercept_model <- lm(Average_X ~ 1, data=average_values_by_Item)
      # Perform stepwise regression, starting from the intercept-only model and considering all predictors
      forward_model <- step(intercept_model, direction='forward', scope=list(lower=intercept_model, upper=full_model), trace=0)
      # Store the stepwise model in the forwardModel list under ROI_name
      forwardModel[[ROI_name]] <- forward_model
      # forward_model$coefficients has the significant contributors to the model
      top_predictors_forward[[ROI_name]] <- names(forward_model$coefficients)[-1]  # Exclude the intercept term, if present
      
      cumulative_rsquared_x <- numeric(length(top_predictors_forward))
      cumulative_adj_rsquared_x <- numeric(length(top_predictors_forward))
      
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_predictors_forward[[ROI_name]])) {
        # Select the top factors up to the i-th
        selected_factors <- top_predictors_forward[[ROI_name]][1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(selected_factors, collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        
        numFactors <- length(top_predictors_forward[[ROI_name]])
        # Extracting factor numbers from the selected predictors for this iteration
        factor_numbers <- sapply(strsplit(selected_factors, "_"), function(x) x[2])
        factor_numbers_list <- paste(factor_numbers, collapse=", ")
        
        # Assuming mem_output_df_top_8 is initialized elsewhere and tbl, mem, ROI, hemisphere variables are defined
        mem_output_df_top_forward <- rbind(mem_output_df_top_forward, data.frame(tbl = tbl,
                                                                                 mem = mem,
                                                                                 ROI = ROI,
                                                                                 numStepFactors = numFactors,
                                                                                 factor_list = factor_numbers_list,
                                                                                 num_factors = i,
                                                                                 cumulative_rsq = cumulative_rsquared_x[i],
                                                                                 cumulative_adj_rsq = cumulative_adj_rsquared_x[i]))
      }
      
      forwardModel <- list()
      top_predictors_forward <- list()
      selected_factors <- 1:100
      # Generate the predictor terms with "Average_M" prefix and concatenate them with " + "
      formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
      # Combine the predictor terms with the dependent variable to form the complete formula
      full_formula <- paste("Average_Y ~", formula_terms)
      # Full model
      full_model <- lm(full_formula, data=average_values_by_Item)
      # Intercept-only model for the starting point
      intercept_model <- lm(Average_X ~ 1, data=average_values_by_Item)
      # Perform stepwise regression, starting from the intercept-only model and considering all predictors
      forward_model <- step(intercept_model, direction='forward', scope=list(lower=intercept_model, upper=full_model), trace=0)
      # Store the stepwise model in the forwardModel list under ROI_name
      forwardModel[[ROI_name]] <- forward_model
      # forward_model$coefficients has the significant contributors to the model
      top_predictors_forward[[ROI_name]] <- names(forward_model$coefficients)[-1]  # Exclude the intercept term, if present
      
      cumulative_rsquared_x <- numeric(length(top_predictors_forward))
      cumulative_adj_rsquared_x <- numeric(length(top_predictors_forward))
      
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_predictors_forward[[ROI_name]])) {
        # Select the top factors up to the i-th
        selected_factors <- top_predictors_forward[[ROI_name]][1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(selected_factors, collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        
        numFactors <- length(top_predictors_forward[[ROI_name]])
        # Extracting factor numbers from the selected predictors for this iteration
        factor_numbers <- sapply(strsplit(selected_factors, "_"), function(x) x[2])
        factor_numbers_list <- paste(factor_numbers, collapse=", ")
        
        # Assuming mem_output_df_top_8 is initialized elsewhere and tbl, mem, ROI, hemisphere variables are defined
        output_df_top_forward <- rbind(output_df_top_forward, data.frame(tbl = tbl,
                                                                                 mem = mem,
                                                                                 ROI = ROI,
                                                                                 numStepFactors = numFactors,
                                                                                 factor_list = factor_numbers_list,
                                                                                 num_factors = i,
                                                                                 cumulative_rsq = cumulative_rsquared_x[i],
                                                                                 cumulative_adj_rsq = cumulative_adj_rsquared_x[i]))
      }
      
      
      
    
      forwardModel <- list()
      top_predictors_forward <- list()
      selected_factors <- 1:100
      # Generate the predictor terms with "Average_M" prefix and concatenate them with " + "
      formula_terms <- paste(paste0("Average_X*Average_M", selected_factors), collapse=" + ")
      # Combine the predictor terms with the dependent variable to form the complete formula
      full_formula <- paste("Average_Y ~", formula_terms)
      # Full model
      full_model <- lm(full_formula, data=average_values_by_Item)
      # Intercept-only model for the starting point
      intercept_model <- lm(Average_X ~ 1, data=average_values_by_Item)
      # Perform stepwise regression, starting from the intercept-only model and considering all predictors
      forward_model <- step(intercept_model, direction='forward', scope=list(lower=intercept_model, upper=full_model), trace=0)
      # Store the stepwise model in the forwardModel list under ROI_name
      forwardModel[[ROI_name]] <- forward_model
      # forward_model$coefficients has the significant contributors to the model
      top_predictors_forward[[ROI_name]] <- names(forward_model$coefficients)[-1]  # Exclude the intercept term, if present
      
      cumulative_rsquared_x <- numeric(length(top_predictors_forward))
      cumulative_adj_rsquared_x <- numeric(length(top_predictors_forward))
      
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_predictors_forward[[ROI_name]])) {
        # Select the top factors up to the i-th
        selected_factors <- top_predictors_forward[[ROI_name]][1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_X*", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        
        numFactors <- length(top_predictors_forward[[ROI_name]])
        # Extracting factor numbers from the selected predictors for this iteration
        factor_numbers <- sapply(strsplit(selected_factors, "_"), function(x) x[2])
        factor_numbers_list <- paste(factor_numbers, collapse=", ")
        
        # Assuming mem_output_df_top_8 is initialized elsewhere and tbl, mem, ROI, hemisphere variables are defined
        interaction_output_df_top_forward <- rbind(interaction_output_df_top_forward, data.frame(tbl = tbl,
                                                                                 mem = mem,
                                                                                 ROI = ROI,
                                                                                 numStepFactors = numFactors,
                                                                                 factor_list = factor_numbers_list,
                                                                                 num_factors = i,
                                                                                 cumulative_rsq = cumulative_rsquared_x[i],
                                                                                 cumulative_adj_rsq = cumulative_adj_rsquared_x[i]))
      }
      
      
      
      
      toc()
    } #end ROI_name_variable
  } #end memType
} #end tbl_names


# Create a list of data frames with names
dfs_list <- list(
  "output" = output_df_top_forward,
  "interaction" = interaction_output_df_top_forward,
  "mem_output" = mem_output_df_top_forward#,
  # "output_df_top_50" = output_df_top_50,
  # "interaction_output_df_top_50" = interaction_output_df_top_50,
  # "mem_output_df_top_50" = mem_output_df_top_50
)

# Create a new workbook
wb <- createWorkbook()
# Loop through each data frame in the list and write to a new sheet in the workbook
for (sheet_name in names(dfs_list)) {
  addWorksheet(wb, sheet_name)  # Add a new worksheet with the sheet name
  writeData(wb, sheet_name, dfs_list[[sheet_name]])  # Write the data frame to the worksheet
}
# Save the workbook to a file
file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_topFacFrom_stepwise.xlsx"
saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook





