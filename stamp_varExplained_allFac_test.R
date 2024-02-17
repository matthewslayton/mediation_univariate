# I'm not supposed to report indirect effects. Instead, I need to report variance explained

# I need to calculate R^2 for a few relationships.
# First for the effect of X on Y, then an R^2 that includes the M
# Then I subtract one from the other:
# R^2 mediation = R^2 total - R^2 IV only
# Then you do this per brain region

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

#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
#mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),resultType=character(),estimate=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {
# output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_all <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
interaction_output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
mem_output_df_top_8 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
interaction_output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
mem_output_df_top_50 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

# output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# interaction_output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())
# mem_output_df_top_100 <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),cumulative_rsq=numeric(),cumulative_adj_rsq=numeric())

# output_df_all_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
# interaction_output_df_all_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
# mem_output_df_all_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())

output_df_top_8_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
interaction_output_df_top_8_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
mem_output_df_top_8_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())

output_df_top_50_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
interaction_output_df_top_50_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())
mem_output_df_top_50_indiv <- data.frame(tbl=character(),mem=character(),ROI=character(),num_factors=numeric(),indiv_rsq=numeric(),indiv_adj_rsq=numeric())



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
      
      
      #### Model the interactions between X and the M's
      # That means what is the effect of M on Y depending on the value of X
      ### Or you can look at the M's on their own.
      # Base part of the formula
      # formula_str <- "Average_Y ~ "
      # # Loop to add each interaction term
      # for (i in 1:how_many_fac) {
      #   term <- paste0("Average_M", i, "*Average_X")
      #   # Append the term to the formula string
      #   formula_str <- paste0(formula_str, term, " + ")
      # }
      # # Remove the last " + " from the formula string
      # formula_str <- substr(formula_str, 1, nchar(formula_str) - 3)
      # # Convert the string to a formula
      # formula_obj <- as.formula(formula_str)
      # test_Average_Model <- lm(formula_obj, data = average_values_by_Item)
      # 
      # 
      # test_model <- lm(average_Y_by_Subj~M1*X + M2*X + M3*X + M4*X + M5*X + M6*X + M7*X + M8*X, data=Data_mixed_clean)
      # test_summary <- summary(test_model,rsq=TRUE)
      # 
      # test_Average_Model <- lm(Average_Y~Average_M1*Average_X + Average_M2*Average_X + Average_M3*Average_X +
      #                            Average_M4*Average_X + Average_M5*Average_X + Average_M6*Average_X + Average_M7*Average_X +
      #                            Average_M8*Average_X, data=average_values_by_Item)
      # 
      # test_avg_summary <- summary(test_Average_Model)
      # rsq_y <- test_avg_summary$r.squared
      # adj_rsq_y <- test_avg_summary$adj.r.squared
      # rsq_vector <- c(tbl, mem, ROI, hemisphere, "rsq_y", rsq_y)
      # adj_rsq_vector <- c(tbl, mem, ROI, hemisphere, "adj_rsq_y", adj_rsq_y)
      # 
     # (4a) Let's look at the M's on their own
      
      ##### Y ~ M
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      
      # Get indices of the top 10 individual R^2 values
      top_indices_8 <- order(adj_rsquared_individual, decreasing = TRUE)[1:8]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_8 <- adj_rsquared_individual[top_indices_8]
      
      # Initialize vectors to store R-squared and adjusted R-squared values for the top factors
      rsquared_top <- numeric(length(top_indices_8))
      adj_rsquared_top <- numeric(length(top_indices_8))
      # Loop over the top_indices instead of 1:how_many_fac
      for (i in seq_along(top_indices_8)) {
        # Get the factor index from top_indices
        factor_index <- top_indices_8[i]
        # Define the formula for the linear model using the top factor
        formula <- as.formula(paste("Average_Y ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model <- lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared and adjusted R-squared values
        rsquared_top[i] <- summary(model)$r.squared
        adj_rsquared_top[i] <- summary(model)$adj.r.squared
        output_df_top_8_indiv <- rbind(output_df_top_8_indiv, data.frame(
                                      tbl = tbl,
                                      mem = mem,
                                      ROI = ROI,
                                      num_factors = i,
                                      indiv_rsq = indiv_rsquared[i],
                                      indiv_adj_rsq = adj_rsquared_top[i]))
      }
      
      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared <- numeric(length(top_indices_8))
      cumulative_adj_rsquared <- numeric(length(top_indices_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        output_df_top_8 <- rbind(output_df_top_8, data.frame(tbl = tbl,
                                                             mem = mem,
                                                             ROI = ROI,
                                                             num_factors = i,
                                                             cumulative_rsq = cumulative_rsquared ,
                                                             cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      # Initialize vectors to store R-squared values
      rsquared_individual_x = numeric(how_many_fac)
      adj_rsquared_individual_x = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_x[i] = summary(model)$r.squared
        adj_rsquared_individual_x[i] = summary(model)$adj.r.squared
        mem_output_df_top_8_indiv <- rbind(mem_output_df_top_8_indiv, data.frame(
                                          tbl = tbl,
                                          mem = mem,
                                          ROI = ROI,
                                          num_factors = i,
                                          indiv_rsq = rsquared_individual_x[i],
                                          indiv_adj_rsq = adj_rsquared_individual_x[i]))
      }
      
      # Get indices of the top 10 R^2 values
      top_indices_x_8 <- order(adj_rsquared_individual_x, decreasing = TRUE)[1:8]
      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared_x <- numeric(length(top_indices_x_8))
      cumulative_adj_rsquared_x <- numeric(length(top_indices_x_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_x_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_x_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_x <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared
        
        mem_output_df_top_8 <- rbind(mem_output_df_top_8, data.frame(tbl = tbl,
                                                                     mem = mem,
                                                                     ROI = ROI,
                                                                     hemisphere = hemisphere,
                                                                     num_factors = i,
                                                                     cumulative_rsq = cumulative_rsquared_x,
                                                                     cumulative_adj_rsq = cumulative_adj_rsquared_x))
      }
      
      
      ##### now let's look at Y ~ X*M
      # Initialize vectors to store R-squared values
      rsquared_individual_xm = numeric(how_many_fac)
      adj_rsquared_individual_xm = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_xm[i] = summary(model)$r.squared
        adj_rsquared_individual_xm[i] = summary(model)$adj.r.squared
        interaction_output_df_top_8_indiv <- rbind(interaction_output_df_top_8_indiv, data.frame(
                                                  tbl = tbl,
                                                  mem = mem,
                                                  ROI = ROI,
                                                  num_factors = i,
                                                  indiv_rsq = rsquared_individual_xm[i],
                                                  indiv_adj_rsq = adj_rsquared_individual_xm[i]))
      }
      
      # Get indices of the top 10 R^2 values
      top_indices_xm_8 <- order(adj_rsquared_individual_xm, decreasing = TRUE)[1:8]
      
      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared_xm <- numeric(length(top_indices_xm_8))
      cumulative_adj_rsquared_xm <- numeric(length(top_indices_xm_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_xm_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_xm_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        #formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        #cumulative_formula <- as.formula(paste("Average_Y ~ Average_X + ", formula_terms))
        formula_terms <- paste(paste0("Average_X*Average_M", selected_factors), collapse=" + ")
        # Combine the predictor terms with the dependent variable to form the complete formula
        cumulative_formula <- paste("Average_Y ~", formula_terms)
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_xm[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_xm[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_xm <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_xm <- summary(cumulative_model)$adj.r.squared
        
        interaction_output_df_top_8 <- rbind(interaction_output_df_top_8, data.frame(tbl = tbl,
                                                                                     mem = mem,
                                                                                     ROI = ROI,
                                                                                     hemisphere = hemisphere,
                                                                                     num_factors = i,
                                                                                     cumulative_rsq = cumulative_rsquared_xm,
                                                                                     cumulative_adj_rsq = cumulative_adj_rsquared_xm))
        
      }
      ##### Y ~ M
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # 
      # # Get indices of the top 10 R^2 values
      top_indices_50 <- order(adj_rsquared_individual, decreasing = TRUE)[1:50]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_50 <- adj_rsquared_individual[top_indices_50]

      # Initialize vectors to store R-squared and adjusted R-squared values for the top factors
      rsquared_top <- numeric(length(top_indices_50))
      adj_rsquared_top <- numeric(length(top_indices_50))
      # Loop over the top_indices instead of 1:how_many_fac
      for (i in seq_along(top_indices_50)) {
        # Get the factor index from top_indices
        factor_index <- top_indices_50[i]
        # Define the formula for the linear model using the top factor
        formula <- as.formula(paste("Average_Y ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model <- lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared and adjusted R-squared values
        rsquared_top[i] <- summary(model)$r.squared
        adj_rsquared_top[i] <- summary(model)$adj.r.squared
        output_df_top_50_indiv <- rbind(output_df_top_50_indiv, data.frame(
                                    tbl = tbl,
                                    mem = mem,
                                    ROI = ROI,
                                    num_factors = i,
                                    indiv_rsq = rsquared_top[i],
                                    indiv_adj_rsq =  adj_rsquared_top[i]))
      }

      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared <- numeric(length(top_indices_50))
      cumulative_adj_rsquared <- numeric(length(top_indices_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared

        output_df_top_50 <- rbind(output_df_top_50, data.frame(tbl = tbl,
                                                               mem = mem,
                                                               ROI = ROI,
                                                               hemisphere = hemisphere,
                                                               num_factors = i,
                                                               cumulative_rsq = cumulative_rsquared ,
                                                               cumulative_adj_rsq = cumulative_adj_rsquared))
      }

      # # Get indices of the top 10 R^2 values
      # top_indices_100 <- order(adj_rsquared_individual, decreasing = TRUE)[1:100]
      # # Extract the top 10 R^2 values using the indices
      # top_rsq_values_100 <- adj_rsquared_individual[top_indices_100]
      # 
      # # Initialize vectors to store R-squared and adjusted R-squared values for the top factors
      # rsquared_top <- numeric(length(top_indices_100))
      # adj_rsquared_top <- numeric(length(top_indices_100))
      # # Loop over the top_indices instead of 1:how_many_fac
      # for (i in seq_along(top_indices_100)) {
      #   # Get the factor index from top_indices
      #   factor_index <- top_indices_100[i]
      #   # Define the formula for the linear model using the top factor
      #   formula <- as.formula(paste("Average_Y ~ ", paste0("Average_M", factor_index)))
      #   # Fit the linear model
      #   model <- lm(formula, data = average_values_by_Item)
      #   # Extract and store the R-squared and adjusted R-squared values
      #   rsquared_top[i] <- summary(model)$r.squared
      #   adj_rsquared_top[i] <- summary(model)$adj.r.squared
      # }
      
      # # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      # cumulative_rsquared <- numeric(length(top_indices_100))
      # cumulative_adj_rsquared <- numeric(length(top_indices_100))
      # # Loop to calculate cumulative R-squared values using top factors
      # for (i in 1:length(top_indices_100)) {
      #   # Select the top factors up to the i-th
      #   selected_factors <- top_indices_100[1:i]
      #   # Create the model formula by including the selected top factors cumulatively
      #   formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
      #   cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
      #   # Fit the linear model with the cumulative set of factors
      #   cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
      #   # Extract and store the cumulative R-squared and adjusted R-squared values
      #   # cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
      #   # cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
      #   cumulative_rsquared <- summary(cumulative_model)$r.squared
      #   cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
      #   
      #   output_df_top_100 <- rbind(output_df_top_100, data.frame(tbl = tbl,
      #                                                          mem = mem,
      #                                                          ROI = ROI,
      #                                                          hemisphere = hemisphere,
      #                                                          num_factors = i,
      #                                                          cumulative_rsq = cumulative_rsquared ,
      #                                                          cumulative_adj_rsq = cumulative_adj_rsquared))
      # }
      
      ##### now let's look at X ~ M
      # Initialize vectors to store R-squared values
      rsquared_individual_x = numeric(how_many_fac)
      adj_rsquared_individual_x = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_x[i] = summary(model)$r.squared
        adj_rsquared_individual_x[i] = summary(model)$adj.r.squared
        
        mem_output_df_top_50_indiv <- rbind(mem_output_df_top_50_indiv, data.frame(
                                      tbl = tbl,
                                      mem = mem,
                                      ROI = ROI,
                                      num_factors = i,
                                      indiv_rsq =rsquared_individual_x[i],
                                      indiv_adj_rsq =  adj_rsquared_individual_x[i]))
      }
      # 
      # Get indices of the top 10 R^2 values
      top_indices_x_50 <- order(adj_rsquared_individual_x, decreasing = TRUE)[1:50]

      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared_x <- numeric(length(top_indices_x_50))
      cumulative_adj_rsquared_x <- numeric(length(top_indices_x_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_x_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_x_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_x <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared

        mem_output_df_top_50 <- rbind(mem_output_df_top_50, data.frame(tbl = tbl,
                                                                       mem = mem,
                                                                       ROI = ROI,
                                                                       hemisphere = hemisphere,
                                                                       num_factors = i,
                                                                       cumulative_rsq = cumulative_rsquared_x,
                                                                       cumulative_adj_rsq = cumulative_adj_rsquared_x))
      }

      # # Get indices of the top 10 R^2 values
      # top_indices_x_100 <- order(adj_rsquared_individual_x, decreasing = TRUE)[1:100]
      # 
      # # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      # cumulative_rsquared_x <- numeric(length(top_indices_x_100))
      # cumulative_adj_rsquared_x <- numeric(length(top_indices_x_100))
      # # Loop to calculate cumulative R-squared values using top factors
      # for (i in 1:length(top_indices_x_100)) {
      #   # Select the top factors up to the i-th
      #   selected_factors <- top_indices_x_100[1:i]
      #   # Create the model formula by including the selected top factors cumulatively
      #   formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
      #   cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
      #   # Fit the linear model with the cumulative set of factors
      #   cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
      #   # Extract and store the cumulative R-squared and adjusted R-squared values
      #   # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
      #   # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
      #   cumulative_rsquared_x <- summary(cumulative_model)$r.squared
      #   cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared
      #   
      #   mem_output_df_top_100 <- rbind(mem_output_df_top_100, data.frame(tbl = tbl,
      #                                                                  mem = mem,
      #                                                                  ROI = ROI,
      #                                                                  hemisphere = hemisphere,
      #                                                                  num_factors = i,
      #                                                                  cumulative_rsq = cumulative_rsquared_x,
      #                                                                  cumulative_adj_rsq = cumulative_adj_rsquared_x))
      # }
      
      ##### now let's look at Y ~ X*M
      # Initialize vectors to store R-squared values
      rsquared_individual_xm = numeric(how_many_fac)
      adj_rsquared_individual_xm = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        #formula = as.formula(paste("Average_Y ~ Average_X + ", paste0("Average_M", i)))
        formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_xm[i] = summary(model)$r.squared
        adj_rsquared_individual_xm[i] = summary(model)$adj.r.squared
        interaction_output_df_top_50_indiv <- rbind(interaction_output_df_top_50_indiv, data.frame(
                                          tbl = tbl,
                                          mem = mem,
                                          ROI = ROI,
                                          num_factors = i,
                                          indiv_rsq = rsquared_individual_xm[i],
                                          indiv_adj_rsq =  adj_rsquared_individual_xm[i]))
        
      }
      # 
      # Get indices of the top 10 R^2 values
      top_indices_xm_50 <- order(adj_rsquared_individual_xm, decreasing = TRUE)[1:50]

      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared_xm <- numeric(length(top_indices_xm_50))
      cumulative_adj_rsquared_xm <- numeric(length(top_indices_xm_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_xm_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_xm_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        #formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        #cumulative_formula <- as.formula(paste("Average_Y ~ Average_X + ", formula_terms))
        formula_terms <- paste(paste0("Average_X*Average_M", selected_factors), collapse=" + ")
        # Combine the predictor terms with the dependent variable to form the complete formula
        cumulative_formula <- paste("Average_Y ~", formula_terms)
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_xm[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_xm[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_xm <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_xm <- summary(cumulative_model)$adj.r.squared

        interaction_output_df_top_50 <- rbind(interaction_output_df_top_50, data.frame(tbl = tbl,
                                                                                       mem = mem,
                                                                                       ROI = ROI,
                                                                                       hemisphere = hemisphere,
                                                                                       num_factors = i,
                                                                                       cumulative_rsq = cumulative_rsquared_xm,
                                                                                       cumulative_adj_rsq = cumulative_adj_rsquared_xm))

      }

      # # Get indices of the top 10 R^2 values
      # top_indices_xm_100 <- order(adj_rsquared_individual_xm, decreasing = TRUE)[1:100]
      # 
      # # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      # cumulative_rsquared_xm <- numeric(length(top_indices_xm_100))
      # cumulative_adj_rsquared_xm <- numeric(length(top_indices_xm_100))
      # # Loop to calculate cumulative R-squared values using top factors
      # for (i in 1:length(top_indices_xm_100)) {
      #   # Select the top factors up to the i-th
      #   selected_factors <- top_indices_xm_100[1:i]
      #   # Create the model formula by including the selected top factors cumulatively
      #   formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
      #   cumulative_formula <- as.formula(paste("Average_Y ~ Average_X + ", formula_terms))
      #   # Fit the linear model with the cumulative set of factors
      #   cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
      #   # Extract and store the cumulative R-squared and adjusted R-squared values
      #   # cumulative_rsquared_xm[i] <- summary(cumulative_model)$r.squared
      #   # cumulative_adj_rsquared_xm[i] <- summary(cumulative_model)$adj.r.squared
      #   cumulative_rsquared_xm <- summary(cumulative_model)$r.squared
      #   cumulative_adj_rsquared_xm <- summary(cumulative_model)$adj.r.squared
      #   
      #   interaction_output_df_top_100 <- rbind(interaction_output_df_top_100, data.frame(tbl = tbl,
      #                                                                                  mem = mem,
      #                                                                                  ROI = ROI,
      #                                                                                  hemisphere = hemisphere,
      #                                                                                  num_factors = i,
      #                                                                                  cumulative_rsq = cumulative_rsquared_xm,
      #                                                                                  cumulative_adj_rsq = cumulative_adj_rsquared_xm))
      #   
      # }
      
      # # Initialize vectors to store R-squared values for cumulative models
      # rsquared_cumulative = numeric(how_many_fac)
      # 
      # # Loop through each set of M variables
      # for (i in 1:how_many_fac) {
      #   # Define the formula including all M variables up to the current iteration
      #   m_vars = paste0("Average_M", 1:i, collapse = " + ")
      #   formula = as.formula(paste("Average_Y ~ ", m_vars))
      #   # Fit the linear model
      #   model = lm(formula, data = average_values_by_Item)
      #   # Extract and store the R-squared value
      #   rsquared_cumulative[i] = summary(model)$r.squared
      #   adj_rsquared_individual[i] = summary(model)$adj.r.squared
      # }
      # 
      # # (4b) #### This is for Y ~ M1 + M2 etc
      # for (i in 1:how_many_fac) {
      #   # Construct the model formula dynamically to include up to the i-th factor
      #   formula_str <- paste("Average_Y ~ ", paste(paste0("Average_M", 1:i), collapse = " + "))
      #   # Fit the linear model with the current set of factors
      #   model <- lm(as.formula(formula_str), data = average_values_by_Item )
      #   # Extract the R-squared value
      #   rsq_value <- summary(model)$r.squared
      #   adj_rsq_value <- summary(model)$adj.r.squared
      #   # Append the data to output_df
      #   output_df_all <- rbind(output_df_all, data.frame(tbl = tbl,
      #                                                    mem = mem,
      #                                                    ROI = ROI,
      #                                                    hemisphere = hemisphere,
      #                                                    num_factors = i,
      #                                                    cumulative_rsq = rsq_value,
      #                                                    cumulative_adj_rsq = adj_rsq_value))
      # }
      # 
      # ## (4c) #### This is for Y ~ X*M1 + X*M2 etc
      # for (i in 1:how_many_fac) {
      #   # Construct interaction terms with Average_X and up to the i-th Average_M factor
      #  # interaction_terms <- paste(paste0("Average_M", 1:i, ":Average_X"), collapse = " + ")
      #   # Construct the model formula including Average_X, Average_M1 to Average_Mi, and their interactions with Average_X
      #   #formula_str <- paste("Average_Y ~ Average_X + ", paste0("Average_M", 1:i, " + "), interaction_terms)
      #   
      #   formula_str = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
      #   
      #   
      #   # Fit the linear model with the current set of factors and their interactions with Average_X
      #   model <- lm(as.formula(formula_str), data = average_values_by_Item)
      #   # Extract the R-squared value
      #   rsq_value <- summary(model)$r.squared
      #   adj_rsq_value <- summary(model)$adj.r.squared
      #   # Append the data to interaction_output_df
      #   interaction_output_df_all <- rbind(interaction_output_df_all, data.frame(tbl = tbl,
      #                                                                            mem = mem,
      #                                                                            ROI = ROI,
      #                                                                            hemisphere = hemisphere,
      #                                                                            num_factors = i,
      #                                                                            cumulative_rsq = rsq_value,
      #                                                                            cumulative_adj_rsq = adj_rsq_value))
      # }
      # 
      # # (4d) #### This is for X ~ M1 + M2 etc
      # 
      # for (i in 1:how_many_fac) {
      #     # Construct the model formula dynamically to include up to the i-th factor
      #     #formula_str <- paste("Average_X ~ ", paste(paste0("Average_M", 1:i), collapse = " + "))
      #     # Fit the linear model with the current set of factors
      #     #model <- lm(as.formula(formula_str), data = average_values_by_Item )
      #     
      #     formula_terms <- paste(paste0("Average_M", 1:i), collapse=" + ")
      #     cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
      #     # Fit the linear model with the cumulative set of factors
      #     model <- lm(cumulative_formula, data = average_values_by_Item)
      #     
      #     # Extract the R-squared value
      #     rsq_value <- summary(model)$r.squared
      #     adj_rsq_value <- summary(model)$adj.r.squared
      #     # Append the data to output_df
      #     mem_output_df_all <- rbind(mem_output_df_all, data.frame(tbl = tbl,
      #                                              mem = mem,
      #                                              ROI = ROI,
      #                                              hemisphere = hemisphere,
      #                                              num_factors = i,
      #                                              cumulative_rsq = rsq_value,
      #                                              cumulative_adj_rsq = adj_rsq_value))
      # }
      
      #### this looks like a repeat
            # Initialize vectors to store cumulative R-squared and adjusted R-squared values
            # cumulative_rsquared_x <- numeric(length(top_indices_x))
            # cumulative_adj_rsquared_x <- numeric(length(top_indices_x))
            # # Loop to calculate cumulative R-squared values using top factors
            # for (i in 1:length(top_indices_x)) {
            #   # Select the top factors up to the i-th
            #   selected_factors <- top_indices_x[1:i]
            #   # Create the model formula by including the selected top factors cumulatively
            #   formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
            #   cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
            #   # Fit the linear model with the cumulative set of factors
            #   cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
            #   # Extract and store the cumulative R-squared and adjusted R-squared values
            #   # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
            #   # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
            #   cumulative_rsquared_x <- summary(cumulative_model)$r.squared
            #   cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared
            # 
            #   mem_output_df_top_8 <- rbind(mem_output_df_top_8, data.frame(tbl = tbl,
            #                                                    mem = mem,
            #                                                    ROI = ROI,
            #                                                    hemisphere = hemisphere,
            #                                                    num_factors = i,
            #                                                    cumulative_rsq = cumulative_rsquared_x,
            #                                                    cumulative_adj_rsq = cumulative_adj_rsquared_x))
            # }
      
    toc()
    } #end ROI_name_variable
  } #end memType
} #end tbl_names


# Create a list of data frames with names
dfs_list <- list(
  #"output_df_all" = output_df_all,
  #"interaction_output_df_all" = interaction_output_df_all,
  #"mem_output_df_all" = mem_output_df_all,
  "output_top_8" = output_df_top_8,
  "interaction_top_8" = interaction_output_df_top_8,
  "mem_top_8" = mem_output_df_top_8,
  "output_50" = output_df_top_50,
  "interaction_top_50" = interaction_output_df_top_50,
  "mem_top_50" = mem_output_df_top_50,
  "output_top_8_indiv" = output_df_top_8_indiv,
  "interaction_top_8_indiv" = interaction_output_df_top_8_indiv,
  "mem_top_8_indiv" = mem_output_df_top_8_indiv,
  "output_50_indiv" = output_df_top_50_indiv,
  "interaction_top_50_indiv" = interaction_output_df_top_50_indiv,
  "mem_top_50_indiv_indiv" = mem_output_df_top_50_indiv
)



# Create a new workbook
wb <- createWorkbook()
# Loop through each data frame in the list and write to a new sheet in the workbook
for (sheet_name in names(dfs_list)) {
  addWorksheet(wb, sheet_name)  # Add a new worksheet with the sheet name
  writeData(wb, sheet_name, dfs_list[[sheet_name]])  # Write the data frame to the worksheet
}
# Save the workbook to a file
file_path <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_topFacFrom100_allFac_withFcn.xlsx"
saveWorkbook(wb, file = file_path, overwrite = TRUE)  # Save the workbook





# Load the existing workbook
wb <- loadWorkbook(file_path)

removeWorksheet(wb, "mem_output_df_all")
addWorksheet(wb, "mem_output_df_all")
# Write the updated data frame to the new worksheet
writeData(wb, "mem_output_df_all", mem_output_df_all)
# Save the workbook, overwriting the existing file
saveWorkbook(wb, file = file_path, overwrite = TRUE)


#colnames(output_df) <- c('tbl','mem','ROI','hemisphere','num_factors','cumulative_rsq')  

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_cumulative_varExplained.xlsx'
write_xlsx(output_df, filename)

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_cumulative_varExplained_interaction.xlsx'
write_xlsx(interaction_output_df, filename)

# Subset output_df to include only rows where num_factors equals 228
max_factors_df <- output_df %>% 
  filter(num_factors == 228)

#filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/encycl_cumulative_varExplained_maxFactors.xlsx'
#write_xlsx(max_factors_df, filename)

max_factors_df <- interaction_output_df %>% 
  filter(num_factors == 228)

ggplot(max_factors_df, aes(x = ROI, y = cumulative_rsq, fill = ROI)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(title = "Cumulative Rsq by ROI ",
       x = "ROI",
       y = "Cumulative Rsq")

# Initialize vectors to store R-squared values
rsquared_individual = numeric(how_many_fac)
# Loop through each M variable
for (i in 1:how_many_fac) {
  # Define the formula for the linear model
  formula = as.formula(paste("Average_Y ~ ", paste0("Average_M", i)))
  # Fit the linear model
  model = lm(formula, data = average_values_by_Item)
  # Extract and store the R-squared value
  rsquared_individual[i] = summary(model)$r.squared
}

# Initialize vectors to store R-squared values for cumulative models
rsquared_cumulative = numeric(how_many_fac)

# Loop through each set of M variables
for (i in 1:how_many_fac) {
  # Define the formula including all M variables up to the current iteration
  m_vars = paste0("Average_M", 1:i, collapse = " + ")
  formula = as.formula(paste("Average_Y ~ ", m_vars))
  # Fit the linear model
  model = lm(formula, data = average_values_by_Item)
  # Extract and store the R-squared value
  rsquared_cumulative[i] = summary(model)$r.squared
}

# Assuming 'df' is your data frame
df <- data.frame(M = paste0("F", 1:how_many_fac), R_squared = rsquared_individual)
# Convert 'M' to a factor and reorder based on the numeric part of the M values
df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
ggplot(df, aes(x = M, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Variance Explained by Each Factor", x = "Factors", y = "R-squared")

# Assuming 'df' is your data frame
df <- data.frame(M = paste0("F", 1:how_many_fac), R_squared = rsquared_cumulative)
# Convert 'M' to a factor and reorder based on the numeric part of the M values
df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
ggplot(df, aes(x = M, y = R_squared, group = 1)) +  # Add 'group = 1' to treat all points as part of one group
  geom_bar(stat = "identity", fill = "steelblue") +  # Plot the bars
  #geom_point(color = "grey", size = 3) +  # Add points on top of the bars
  #geom_line(color = "black", linewidth = 1) +  # Connect the points with a line, using 'linewidth' for line thickness
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Cumulative Variance Explained by Factors", x = "Factors", y = "R-squared")


##### let's use the data frames as output so I don't have to go back and change all the variables
## Y ~ M
# output_df_all / top_50 / top_8 

## Y ~ X*M
# interaction_output_df_all / top_xm_50 / top_xm_8 

## X ~ M 
# mem_output_df_all / top_x_50 / top_x_8 
df <- data.frame(M = paste0("F", top_indices_x_8), R_squared = mem_output_df_top_8$cumulative_adj_rsq[1:8])
# Convert 'M' to a factor and reorder based on the numeric part of the M values
#df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
df$M <- factor(df$M, levels = paste0("F", top_indices_x_8))
ggplot(df, aes(x = M, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Variance Explained by Each Factor", x = "Factors", y = "R-squared")

df <- data.frame(M = paste0("F", top_indices_x_50), R_squared = mem_output_df_top_50$cumulative_adj_rsq)
# Convert 'M' to a factor and reorder based on the numeric part of the M values
#df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
df$M <- factor(df$M, levels = paste0("F", top_indices_x_50))
ggplot(df, aes(x = M, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Variance Explained by Each Factor", x = "Factors", y = "R-squared")

df <- data.frame(M = paste0("F", 1:how_many_fac), R_squared = mem_output_df_all$cumulative_adj_rsq)
# Convert 'M' to a factor and reorder based on the numeric part of the M values
df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
ggplot(df, aes(x = M, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Variance Explained by Each Factor", x = "Factors", y = "R-squared")

df <- data.frame(M = paste0("F", top_indices_x_100), R_squared = mem_output_df_top_100$cumulative_adj_rsq)
# Convert 'M' to a factor and reorder based on the numeric part of the M values
#df$M <- factor(df$M, levels = df$M[order(as.numeric(gsub("F", "", df$M)))])
df$M <- factor(df$M, levels = paste0("F", top_indices_x_100))
ggplot(df, aes(x = M, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(colour = "black")) +  # Add axis lines
  labs(title = "Variance Explained by Each Factor", x = "Factors", y = "R-squared")


#### investigate overfitting
set.seed(123)  # For reproducibility
split_index <- sample(1:nrow(average_values_by_Item), 0.7 * nrow(average_values_by_Item))  # 70% for training
train_data <- average_values_by_Item[split_index, ]
validation_data <- average_values_by_Item[-split_index, ]

# Select the top 50 factors
selected_factors <- top_indices_x_8
# Create the model formula with all XX top factors
formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))

# Fit the linear model with the full set of factors on the training data
cumulative_model <- lm(cumulative_formula, data = train_data)

# Predict on validation data and evaluate the model
predictions <- predict(cumulative_model, newdata = validation_data)
mse <- mean((validation_data$Average_X - predictions)^2)
rmse <- sqrt(mse)

# Print or store the evaluation metrics
cat("MSE on validation data:", mse, "\n")
# MSE on validation data: 4.031414e+58 
cat("RMSE on validation data:", rmse, "\n")
# RMSE on validation data: 2.007838e+29 

#library(glmnet)

# Prepare the matrix of predictors and the response vector
x_train <- as.matrix(train_data[, top_indices_x_8])
y_train <- train_data$Average_X

x_validation <- as.matrix(validation_data[, top_indices_x_8])
y_validation <- validation_data$Average_X

# Fit LASSO model
lasso_model <- glmnet(x_train, y_train, alpha = 1)  # alpha = 1 for LASSO

# Determine the lambda that minimizes the cross-validation error
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
optimal_lambda <- cv_lasso$lambda.min

# Predict on validation data using the optimal lambda
predictions <- predict(lasso_model, newx = x_validation, s = optimal_lambda)

# Calculate MSE and RMSE
mse <- mean((y_validation - predictions)^2)
rmse <- sqrt(mse)

cat("MSE on validation data:", mse, "\n")
# MSE on validation data: 1.787239e-05 
cat("RMSE on validation data:", rmse, "\n")
# RMSE on validation data: 0.004227575 

# now let's get adjusted R2 from this corrected model
# Compute R^2
ss_res <- sum((y_validation - predictions)^2)
ss_tot <- sum((y_validation - mean(y_validation))^2)
r_squared <- 1 - ss_res / ss_tot

# Determine the effective number of predictors (non-zero coefficients in LASSO model)
effective_predictors <- sum(coef(lasso_model, s = optimal_lambda) != 0) - 1  # Subtract 1 to exclude intercept

# Compute Adjusted R^2
n <- length(y_validation)  # Number of observations in the validation set
adjusted_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - effective_predictors - 1))

# Print Adjusted R^2
cat("Adjusted R^2 on validation data:", adjusted_r_squared, "\n")

# Assuming x_train, y_train, x_validation, and y_validation are already defined

# Fit Ridge regression model using glmnet with alpha = 0
ridge_model <- glmnet(x_train, y_train, alpha = 0)

# Perform cross-validation to find the optimal lambda value
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

# Extract the optimal lambda value
optimal_lambda_ridge <- cv_ridge$lambda.min

# Predict on the validation set using the optimal lambda
predictions_ridge <- predict(ridge_model, newx = x_validation, s = optimal_lambda_ridge)

# Compute MSE and RMSE
mse_ridge <- mean((y_validation - predictions_ridge)^2)
rmse_ridge <- sqrt(mse_ridge)

# Compute R^2
ss_res_ridge <- sum((y_validation - predictions_ridge)^2)
ss_tot_ridge <- sum((y_validation - mean(y_validation))^2)
r_squared_ridge <- 1 - ss_res_ridge / ss_tot_ridge

# Compute Adjusted R^2
n <- length(y_validation)  # Number of observations in the validation set
p <- sum(coef(ridge_model, s = optimal_lambda_ridge) != 0) - 1  # Effective number of predictors, excluding intercept
adjusted_r_squared_ridge <- 1 - (1 - r_squared_ridge) * ((n - 1) / (n - p - 1))


