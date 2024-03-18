
# Remove all objects from the global environment
rm(list = ls())

library(rio)
library(zoo)
library(readxl)
library(openxlsx)
library(writexl)
library(ggplot2)


##### Y~M is per ROI
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

nmf_types <- c(100)
tbl_types <- c("encycl","vis","fcn","all")
#memType <- c("lexical_CR","visual_CR","lexical_HR","visual_HR","lexical_FAR","visual_FAR")
memType <- c("lexical_CR","visual_CR") #,"lexical_HR","visual_HR","lexical_FAR","visual_FAR")

#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_Y-M.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_Y-X*M.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_Y-X+M.xlsx")
results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2_Y-X.xlsx")
data <- results$x_m

final_results <- list()

# nmf <- "100"
# tbl_type <- "encycl"
# mem_type <- "lexical_CR"
# ROI_name <- "mask_AG_L"

for (nmf in nmf_types) {
  for (tbl_type in tbl_types) {
    for (mem_type in memType) {
      for (ROI_name in ROI_name_variable) {
      
      filtered_data <- subset(data,nmf_type==nmf & tbl==tbl_type & mem == mem_type & ROI == ROI_name)
      if (nrow(filtered_data) == 0) {
        ROI_name <- sub("mask_","",ROI_name)
        filtered_data <- subset(data,nmf_type==nmf & tbl==tbl_type & mem == mem_type & ROI == ROI_name)
      }
      all_cum_adj_r2 <- filtered_data$cumulative_adj_rsq
      
      # Calculate the differences between successive elements
      differences <- diff(all_cum_adj_r2)
      
      if (length(differences) > 0) {
        # Define a threshold - this is subjective and might need adjustment
        threshold <- 0.0005
        # Find where the differences fall below the threshold
        #below_threshold <- which(differences < threshold)
        # Find where the differences are consistently below the threshold (so, where do the incremental gains fall)
        
        consistent_small_change <- which(rollapply(differences, width = 3, FUN = function(x) all(x < threshold), align = 'left', fill = NA))
        
        # The knee point is the first position where the difference falls below the threshold
        #knee_point <- ifelse(length(below_threshold) > 0, below_threshold[1], NA)
        knee_point <- if (length(consistent_small_change) > 0) consistent_small_change[1] else NA
        print(paste(nmf,tbl_type, mem_type,ROI_name,knee_point))
        
        # Extract the row corresponding to the knee point
        knee_row <- filtered_data[ifelse(!is.na(knee_point), knee_point, nrow(filtered_data)), ]
        # Append to final_results
        final_results <- rbind(final_results, cbind(
          nmf_type = nmf,
          tbl = tbl_type,
          mem = mem_type,
          ROI = ROI_name,
          knee_point = knee_point,
          num_factors = knee_row$num_factors,
          cumulative_rsq = knee_row$cumulative_rsq,
          cumulative_adj_rsq = knee_row$cumulative_adj_rsq,
          degrees_of_freedom = knee_row$degrees_of_freedom))
      } else {
        knee_point <- 1
        print(paste(nmf, tbl_type, mem_type, ROI_name, "Single row - knee point at row", knee_point))
        knee_row <- filtered_data 
        # Append to final_results
        final_results <- rbind(final_results, cbind(
          nmf_type = nmf,
          tbl = tbl_type,
          mem = mem_type,
          ROI = ROI_name,
          knee_point = knee_point,
          num_factors = knee_row$num_factors,
          cumulative_rsq = knee_row$rsq,
          cumulative_adj_rsq = knee_row$adj_rsq,
          degrees_of_freedom = knee_row$degrees_of_freedom))
      }

      } #ROI_name
    } #memType
  } #tbl_type
} #nmf_type

final_results_df <- as.data.frame(final_results)

final_results_df$knee_point <- as.numeric(final_results_df$knee_point)
final_results_df$cumulative_rsq <- as.numeric(final_results_df$cumulative_rsq)
final_results_df$cumulative_adj_rsq <- as.numeric(final_results_df$cumulative_adj_rsq)
final_results_df$degrees_of_freedom <- as.numeric(final_results_df$degrees_of_freedom)
final_results_df$ROI <- unlist(final_results_df$ROI)

final_results_df$tbl <- unlist(final_results_df$tbl)
final_results_df$mem <- unlist(final_results_df$mem)

#filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/whereIsTheKnee_Y-M.xlsx'
#filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/whereIsTheKnee_Y-X+M.xlsx'
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/whereIsTheKnee_Y-X.xlsx'
#filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/whereIsTheKnee_Y-X*M.xlsx'
write.xlsx(final_results_df, filename)


  # Assuming final_results_df is your data frame and it contains 'tbl', 'mem', 'ROI', 'cumulative_adj_rsq'
  ggplot(final_results_df, aes(x = ROI, y = cumulative_adj_rsq)) +
    geom_col() +
    theme_minimal() +
    labs(x = "ROI", y = "Cumulative Adjusted R-Squared", 
         #title = "Y~X*M Cumulative Adj R-Squared by ROI") +
         title = "Y~X Cumulative Adj R-Squared by ROI") +
    facet_grid(tbl ~ mem) # Creates a grid of plots by 'tbl' and 'mem'
    #facet_wrap(~ tbl + mem, scales = "free")

