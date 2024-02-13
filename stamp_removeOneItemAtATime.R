# after stamp_explore_items_influence_on_modMed.R I want to try item-wise.
# a single item probably won't make much of a difference, but it's worth a try

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
library(reshape2)
library(broom)
library(tidyr)
#library(parallel)
#detectCores()
#library(foreach)
#library(doParallel)
#registerDoParallel(cores = 4)



factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

# add mask_Hipp_A and mask_Hipp_P
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
#fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

#tbl_names <- c("encycl","vis","fcn")
tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR")


## need to add category cols
ref_tbl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="Sheet1")
names(ref_tbl)[names(ref_tbl) == "id"] <- "ItemID"

# Load matched_names from the file
matched_names <- readRDS("/Users/matthewslayton/matched_names.rds")
# Fixed column names
fixed_col_names <- c("path","tbl", "mem", "ROI", "hemisphere")
# Assuming 'item_names' is a vector with the names of your 242 items
# Concatenate fixed column names with item names
all_col_names <- c(matched_names,fixed_col_names)
# Create an empty data frame with specified column names
# Initialize each column as character type; adjust as needed for your data
all_data_by_ROI <- data.frame(matrix(ncol = length(all_col_names), nrow = 0))
colnames(all_data_by_ROI) <- all_col_names

# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'


for (tbl in tbl_names) {
  for (mem in memType) {

    #if (tbl == "encycl" && mem == "lexical_CR"){
    #  next
  
      #foreach(ROI_name = ROI_name_variable) %dopar% {
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
        
        # add the category column
        curr_fac_tbl <- left_join(curr_fac_tbl, ref_tbl[, c("ItemID", "category","reduced_category")], by = "ItemID")
        
        X = curr_fac_tbl[[mem]]
        Y = curr_fac_tbl$AvgROI
        # note, lexical_CR is a character object. need to convert to double
        # given how I'm converting earlier in the loop it shouldn't matter by this point
        if (class(X) == "character")
        {
          X = as.numeric(X)
        }
        
        M1 <- curr_fac_tbl[["F01"]]
        M2 <- curr_fac_tbl[["F02"]]
        M3 <- curr_fac_tbl[["F03"]]
        M4 <- curr_fac_tbl[["F04"]]
        M5 <- curr_fac_tbl[["F05"]]
        M6 <- curr_fac_tbl[["F06"]]
        M7 <- curr_fac_tbl[["F07"]]
        M8 <- curr_fac_tbl[["F08"]]
        
        Subj <- curr_fac_tbl$Subj
        ItemID <- curr_fac_tbl$ItemID
        category <- curr_fac_tbl$category
        reduced_category <- curr_fac_tbl$reduced_category
        
        # (2) clean those NaN rows
        Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID,category=category,reduced_category=reduced_category)
        Data_mixed_clean <- na.omit(Data_mixed)
        
        # make a composite M column that averages all eight
        Data_mixed_clean$CompMod = rowMeans(Data_mixed_clean[, c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")])
        # for moderation
        Data_mixed_clean$XxCompMod = Data_mixed_clean$X * Data_mixed_clean$CompMod
        
        
        model221 <- '
            level:1
            Y~1
            level:2
            Y~b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 + b7*M7 + b8*M8 +c*X
            M1~a1*X
            M2~a2*X
            M3~a3*X
            M4~a4*X
            M5~a5*X
            M6~a6*X
            M7~a7*X
            M8~a8*X
            
            #indirect and total effects
            ab1:=a1*b1
            ab2:=a2*b2
            ab3:=a3*b3
            ab4:=a4*b4
            ab5:=a5*b5
            ab6:=a6*b6
            ab7:=a7*b7
            ab8:=a8*b8
            
            # Total indirect effect
            totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5 + ab6 + ab7 + ab8
            
            # Total effect
            totalEffect := totalIndirect + c
            '
        # 
        # # same just with
        # model_compMod <- '
        #     level:1
        #     Y~1
        #     level:2
        #     Y~b*CompMod + c*X
        #     CompMod~a*X
        #     
        #     # indirect effects
        #     ab:=a*b
        #     
        #     # Total indirect effect
        #     totalIndirect := ab
        #     
        #     # Total effect
        #     totalEffect := totalIndirect + c
        #     '
        # 
        # ### would I want a moderated mediation model too?
        # model_modMed <- '
        #       level:1
        #       Y~1
        #       level:2
        #       Y ~ b1*CompMod + b2*XxCompMod + c*X
        #       CompMod ~ a*X
        #       
        #       # indirect effect
        #       ab1 := a*b1
        #       
        #       # moderated indirect effect
        #       modIndirect := ab1 + a*b2*X
        #       
        #       # Total indirect effect
        #       totalIndirect := modIndirect
        # 
        #       # Total effect
        #       totalEffect := totalIndirect + c
        #     '
        
        
        unique_items <- unique(Data_mixed_clean$ItemID)
        impact_assessment_items <- vector("list", length(unique_items))
        errors_items <- vector("list", length(unique_items))
        
        for (i in seq_along(unique_items)) {
          item_to_remove <- unique_items[i]
          
          # Create a subset of the data without the current item's category
          modified_data <- subset(Data_mixed_clean, ItemID != item_to_remove)
          
          # Re-fit the model
          modified_fit <- try(sem(model221,
                                  data = modified_data,
                                  cluster = "ItemID",
                                  se = "robust.huber.white"),
                              silent=TRUE)
          
          if (class(modified_fit) == "try-error") {
            impact_assessment_items[[i]] <- NULL
            errors_items[[i]] <- paste("Error with category:", item_to_remove)
          } else {
            impact_assessment_items[[i]] <- summary(modified_fit)
            errors_items[[i]] <- NA  # No error
          }
          # Store the summary of the model
          impact_assessment_items[[i]] <- summary(modified_fit)
        }
        
        ###### INDIVIDUAL ITEMS
        impact_assessment <- impact_assessment_items
        # Extracting estimates for the a-paths
        a_paths <- paste("a", 1:8, sep="")
        a_estimates <- matrix(NA, nrow = length(a_paths), ncol = length(impact_assessment))
        b_paths <- paste("a", 1:8, sep="")
        b_estimates <- matrix(NA, nrow = length(b_paths), ncol = length(impact_assessment))
        indirect_paths <- paste("ab",1:8, sep="")
        indirect_estimates <- matrix(NA, nrow = length(indirect_paths), ncol = length(impact_assessment))
        # get c, totalIndirect, and totalEffect too
        c_totalInd_totalEff <- matrix(NA, nrow = 3, ncol = length(impact_assessment))
        
        for (i in seq_along(impact_assessment)) {
          if ("lavaan.summary" %in% class(impact_assessment[[i]])) {
            for (j in seq_along(a_paths)) {
              a_path <- a_paths[j]
              curr_est <- impact_assessment[[i]]
              ests <- curr_est$pe
              a_estimate <- ests[ests$op == "~" & ests$label == a_path, "est"]
              if (length(a_estimate) > 0) {
                a_estimates[j, i] <- a_estimate
              } else {
                a_estimates[j, i] <- NA
              }
              b_path <- b_paths[j]
              b_estimate <- ests[ests$op == "~" & ests$label == b_path, "est"]
              if (length(b_estimate) > 0) {
                b_estimates[j, i] <- b_estimate
              } else {
                b_estimates[j, i] <- NA
              }
              
              indirect <- indirect_paths[j]
              indirect_estimate <- ests[ests$label == indirect, "est"]
              if (length(indirect_estimate) > 0) {
                indirect_estimates[j, i] <- indirect_estimate
              } else {
                indirect_estimates[j, i] <- NA
              }
            } #end of j loop
            c_est <- ests[ests$op == "~" & ests$label == "c", "est"]
            totalInd_est <- ests[ests$label == "totalIndirect", "est"]
            totalEff_est <- ests[ests$label == "totalEffect", "est"]
            
            if (length(c_est) > 0) {
              c_totalInd_totalEff[1, i] <- c_est
              c_totalInd_totalEff[2, i] <- totalInd_est
              c_totalInd_totalEff[3, i] <- totalEff_est
            } else {
              c_totalInd_totalEff[1, i] <- NA
              c_totalInd_totalEff[2, i] <- NA
              c_totalInd_totalEff[3, i] <- NA
            }
            
          }
        }
        
        # Converting to a data frame for easier handling
        # Creating labels
        a_labels <- paste("a", 1:8, sep="")
        b_labels <- paste("b", 1:8, sep="")
        ab_labels <- paste("ab", 1:8, sep="")
        # Assuming only one each for these paths
        other_labels <- c("c", "totalIndirect", "totalEffect")
        # Combine all labels
        all_labels <- c(a_labels, b_labels, ab_labels, other_labels)
        all_df_items <- rbind(a_estimates,b_estimates,indirect_estimates,c_totalInd_totalEff)
        all_df_items <- as.data.frame(all_df_items)
        all_df_items$path <- all_labels
        
        # Match names and set column names for all_df_items
        matched_positions <- match(unique_items, ref_tbl$ItemID)
        matched_names <- ref_tbl[matched_positions, 1][[1]]
        colnames(all_df_items)[1:(ncol(all_df_items) - 1)] <- matched_names
        
        # Add loop variables as columns to all_df_items before appending to all_data_by_ROI
        all_df_items$tbl <- tbl
        all_df_items$mem <- mem
        all_df_items$ROI <- ROI
        all_df_items$hemisphere <- hemisphere
        
        #### some iterations have unique items that others don't, so we have to 
        ### check and add them
        # Identify any new columns in all_df_items that aren't in all_data_by_ROI
        new_cols <- setdiff(colnames(all_df_items), colnames(all_data_by_ROI))
        # For each new column, add it to all_data_by_ROI with NA values for existing rows
        for (col in new_cols) {
          all_data_by_ROI[[col]] <- NA
        }
        
        # Identify any columns in all_data_by_ROI that aren't in all_df_items
        missing_cols <- setdiff(colnames(all_data_by_ROI), colnames(all_df_items))
        # For each missing column, add it to all_df_items with NA values
        for (col in missing_cols) {
          all_df_items[[col]] <- NA
        }
        
        # Ensure all_df_items has the same column order as all_data_by_ROI
        all_df_items <- all_df_items[colnames(all_data_by_ROI)]
        
        # Append all_df_items to all_data_by_ROI
        all_data_by_ROI <- rbind(all_data_by_ROI, all_df_items)
        
        
      
        toc()
      } #end ROI_name_variable
    
   #} #conditional for skipping encycl and lex

  } #end memType
} #end tbl_names

# ### clean up the columns
# # Identify the columns you want to move
# cols_to_move_front <- colnames(all_data_by_ROI)[244:247]
# col_to_move_next <- colnames(all_data_by_ROI)[243]
# # Combine these columns with the remaining columns, excluding the ones being moved to the front
# remaining_cols <- setdiff(colnames(all_data_by_ROI), c(cols_to_move_front, col_to_move_next))
# # Reorder the columns
# all_data_by_ROI <- all_data_by_ROI[c(cols_to_move_front, col_to_move_next, remaining_cols)]

# Specify the columns you want to move to the front
cols_to_move_front <- c("tbl", "mem", "ROI", "hemisphere", "path")

# Combine these columns with the remaining columns, excluding the ones being moved to the front
remaining_cols <- setdiff(colnames(all_data_by_ROI), cols_to_move_front)

# Reorder the columns
all_data_by_ROI <- all_data_by_ROI[c(cols_to_move_front, remaining_cols)]


# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_item_atATime.xlsx'
write_xlsx(all_data_by_ROI, filename)

# # Install and load the sendmailR package
# install.packages("sendmailR")
# library(sendmailR)
# 
# # Define the sender and recipient
# from <- "<your-email>@domain.com"
# to <- "<recipient-email>@domain.com"
# subject <- "R Script Finished"
# 
# # Define the body of the email
# body <- "Your long-running R script has finished executing."
# 
# # Define the SMTP server settings
# smtp <- list(host.name = "smtp.gmail.com", port = 465, user.name = "<your-email>@gmail.com", passwd = "<your-password>", ssl = TRUE)
# 
# # Send the email
# sendmail(from, to, subject, body, control = smtp)

# library(parallel)
# detectCores()
# 
# library(foreach)
# library(doParallel)
# 
# # Register the number of cores you want to use
# registerDoParallel(cores = 4)
# 
# # Use foreach instead of a standard for loop
# foreach(i = 1:10) %dopar% {
#   # Your code here
# }
# 
# # Use foreach instead of a standard for loop
# foreach(ROI_name = ROI_name_variable) %dopar% {
#   # Your code here
# }
