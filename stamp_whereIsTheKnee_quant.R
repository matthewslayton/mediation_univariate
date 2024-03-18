##### There's a "knee point detection" algorithm. It quantifies the point 
## where the curve bends the most significantly. You draw a line from the first 
## point to the last point of the curve and find the point that is farthest from 
## this line. That's the knee
#### You can also use a derivative-based method where you calculate the derivative 
## (or difference in the case of discrete points) in the adjusted R2 values. 
## When the derivative starts to be consistently below a certain threshold, 
## that can be the knee point. You define that threshold to see which one 
## aligns the best with visual inspection.

# Remove all objects from the global environment
rm(list = ls())

library(rio)
library(zoo)

# let's use this as a test 
# test_results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_top50_from_nmf100_whereIsTheKnee.xlsx")
# data <- test_results$x_m
# filtered_results <- subset(data, tbl=="encycl" & mem == "lexical_CR")
# all_cum_adj_r2 <- filtered_results$cumulative_adj_rsq

##### (1) SEPARATE FAC TYPES

# this came from the other whereIsTheKnee script and was done with 50 factors
#nmf_types <- c(200,150,100,50)
#nmf_types <- c(100,50)
#nmf_types <- c(200,300)
nmf_types <- c(100)
tbl_types <- c("encycl","vis","fcn","all")
#memType <- c("lexical_CR","visual_CR","lexical_HR","visual_HR","lexical_FAR","visual_FAR")
memType <- c("lexical_CR","visual_CR") #,"lexical_HR","visual_HR","lexical_FAR","visual_FAR")


#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_200_150_100_50_nmf_whereIsTheKnee.xlsx")

#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_200_150_100_50_nmf_whereIsTheKnee_encyclVisFcnAll.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_percentile_100_50_nmf_whereIsTheKnee_encyclVisFcnAll.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_percentile_200_300_nmf_whereIsTheKnee_encyclVisFcnAll.xlsx")



# this is from the combined moderators
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf150.xlsx")
data <- results$x_m

for (nmf in nmf_types) {
  for (tbl_type in tbl_types) {
    for (mem_type in memType) {
    filtered_data <- subset(data,nmf_type==nmf & tbl==tbl_type & mem == mem_type)
  
    all_cum_adj_r2 <- filtered_data$cumulative_adj_rsq
  
    # Calculate the differences between successive elements
    differences <- diff(all_cum_adj_r2)
    
    # Define a threshold - this is subjective and might need adjustment
    threshold <- 0.0005
    # Find where the differences fall below the threshold
    #below_threshold <- which(differences < threshold)
    # Find where the differences are consistently below the threshold (so, where do the incremental gains fall)
    consistent_small_change <- which(rollapply(differences, width = 3, FUN = function(x) all(x < threshold), align = 'left', fill = NA))
    
    # The knee point is the first position where the difference falls below the threshold
    #knee_point <- ifelse(length(below_threshold) > 0, below_threshold[1], NA)
    knee_point <- if (length(consistent_small_change) > 0) consistent_small_change[1] else NA
    print(paste(nmf,tbl_type, mem_type,knee_point))
    } #memType
  } #tbl_type
} #nmf_type

#### (2) COMBINED into one

#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf228.xlsx")

# this is from the combined moderators
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf150.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf100.xlsx")

results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf100_addOneFromEachCat.xlsx")
#results <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/varExplained_R2change_from_X-M_encyclVisFcn_nmf_percentile_50_addOneFromEachCat.xlsx")

data <- results$x_m


all_cum_adj_r2 <- data$cumulative_adj_rsq

# Calculate the differences between successive elements
differences <- diff(all_cum_adj_r2)

# Define a threshold - this is subjective and might need adjustment
threshold <- 0.0001
# Find where the differences fall below the threshold
#below_threshold <- which(differences < threshold)
# Find where the differences are consistently below the threshold
consistent_small_change <- which(rollapply(differences, width = 3, FUN = function(x) all(x < threshold), align = 'left', fill = NA))

# The knee point is the first position where the difference falls below the threshold
#knee_point <- ifelse(length(below_threshold) > 0, below_threshold[1], NA)
knee_point <- if (length(consistent_small_change) > 0) consistent_small_change[1] else NA
print(knee_point)

# get the rightmost factor
data$latest_factor <- sapply(strsplit(data$num_factors, ", "), function(x) tail(x, 1))
# Convert latest_factor to a factor with levels in the order they appear
data$latest_factor <- factor(data$latest_factor, levels = unique(data$latest_factor))

data <- data[order(data$latest_factor), ] 
print(data$latest_factor)  # Check the order of factors
print(data$cumulative_adj_rsq)  # Check the order of corresponding values

ggplot(data, aes(x = latest_factor, y = cumulative_adj_rsq)) +
  geom_bar(stat = "identity", fill = "darkorchid1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(title = "Cumulative Adj R-Squared lexMem CR All Fac",
       x = "Factor",
       y = "Cumulative Adj R-Squared")


