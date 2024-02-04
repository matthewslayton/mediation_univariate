# i need scatter plots of CSA stats x STAMP performance and may as well throw in
# f01 x performance

# I got the item-wise performance from /Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP_scripts/PCs_betas_stampSubjMem.m


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


mem_data <- read.xlsx("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/stamp_IDs_mem_f01_forScatterPlot.xlsx")


# Function to calculate correlation and add text above each subplot
add_correlation_text_above <- function(data, x, y) {
  corr_value <- cor(data[[x]], data[[y]], use = "complete.obs")
  label_text <- sprintf("Correlation: %.2f", corr_value)
  data_summary <- data.frame(x = max(data[[x]]), y = max(data[[y]]), label = label_text)
  return(data_summary)
}

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(gridExtra)

# Remove rows with NaN values
mem_data <- na.omit(mem_data)

# Function to create scatter plot matrix for a variable
create_scatter_matrix <- function(data, variable, title) {
  melted_data <- melt(data, id.vars = c("ID", variable))
  
  correlation_text <- add_correlation_text_above(melted_data, "value", variable)
  
  scatter_plot <- ggplot(melted_data, aes(x = value, y = !!sym(variable))) +
    geom_point(size = 1, alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, size = 1.5) +
    labs(x = "Variable", y = variable, title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    theme(panel.spacing = unit(0, "lines"))
  
  scatter_plot <- scatter_plot +
    annotate("text", x = correlation_text$x, y = correlation_text$y, label = correlation_text$label, hjust = 1.05, vjust = 1.2, size = 3, color = "black")
  
  return(scatter_plot)
}

# Variables of interest and their human-readable names
variables_cmcm <- c("NMF_f1", "CS_adj", "MD", "slope", "frequency_adj", "name_agreement_adj", "NumFeat_adj")
variables_pmem <- c("NMF_f1", "CS_adj", "MD", "slope", "frequency_adj", "name_agreement_adj", "NumFeat_adj")
names_human_readable <- c("Factor 1", "Corr Strength", "Mean Dist", "CSxD", "Freq", "Name Agreement", "Num Feat")

# Create scatter plot matrix for CMEM
scatter_matrix_cmem <- lapply(variables_cmcm, function(variable) {
  create_scatter_matrix(mem_data[, c("ID", "CMEM", variable)], variable, names_human_readable[variable == variables_cmcm])
})

# Create scatter plot matrix for PMEM
scatter_matrix_pmem <- lapply(variables_pmem, function(variable) {
  create_scatter_matrix(mem_data[, c("ID", "PMEM", variable)], variable, names_human_readable[variable == variables_pmem])
})

# Arrange the plots in separate grids
grid.arrange(grobs = scatter_matrix_cmem, ncol = 3, top = "Average Conceptual Memory Performance and Image Feature Statistics")
grid.arrange(grobs = scatter_matrix_pmem, ncol = 3, top = "Average Perceptual Memory Performance and Image Feature Statistics")


# Assuming you have a data frame named 'your_data' with columns 'ID', 'mem', and 'F'

# Load the ggplot2 library
#library(ggplot2)

# # Create a scatter plot
# scatter_plot <- ggplot(mem_data, aes(x = CMEM, y = NMF_f1)) +
#   
#   # Customize the appearance of the points
#   geom_point(color = "#FF5733", size = 3, alpha = 0.7) +  # Adjust color, size, and transparency
#   
#   # Add a line of best fit
#   geom_smooth(method = "lm", se = FALSE, color = "#3366FF", size = 1.5) +  # Linear regression line
#   
#   # Customize the plot theme
#   theme_minimal() +  # or choose another theme
#   
#   # Add axis labels and a title
#   labs(x = "Conceptual Memory Performance", y = "NMF Factor 1", title = "Average Conceptual Memory vs. Factor 1") +
#   
#   # Display correlation value
#   annotate("text", x = max(mem_data$CMEM), y = max(mem_data$NMF_f1),
#            label = paste("Correlation:", round(cor(mem_data$CMEM, mem_data$NMF_f1), 2)),
#            hjust = 1, vjust = 1, size = 4, color = "#3366FF")  # Adjust position, size, and color
# 
# # Print the plot
# print(scatter_plot)
