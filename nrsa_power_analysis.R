######## need to do power analylsis for the effect
## we have two groups, MCI and sham, four days, and d-primes.
# For sure the DV is d-prime
# The IV would be group, so just make a binary variable where MCI is 1 and sham is 0
# A second IV can be days 1 through 4


#{r setup, include=FALSE}
#power analysis
#load simr
#install.packages("simr")
#install.packages("readxl")
library(simr)
library(readxl)
library(rio)
library(lme4)
library(ggplot2)
library(dplyr)

dprime <- import("/Users/matthewslayton/Documents/Duke/Simon_Lab/Analysis_files/power_analylsis.xlsx",which="data_for_power_analysis")

# Create a new data frame excluding columns with NaN values
dprime_clean <- dprime[,1:9]

# re-name Subj col
names(dprime_clean)[1] <- 'Subj'
names(dprime_clean)[2] <- 'iv1'
names(dprime_clean)[3] <- 'Day'
names(dprime_clean)[4] <- 'dv'

#dv <- dprime_clean$`CRET-dprime-all`
#iv1 <- dprime_clean$Group
#iv2 <- dprime_clean$Day
dv <- dprime_clean$dv
iv1 <- dprime_clean$iv1

# with Day variable
model_cret_all = lmer(dv~iv1+iv2+(1|Subj),data = dprime_clean)
summary(model_cret_all)
fixef(model_cret_all)["iv1"]
fixef(model_cret_all)["iv2"]
powerSim(model_cret_all)

# Group only
model_cret_all = lmer(dv~iv1+(1|Subj),data = dprime_clean)
summary(model_cret_all)
fixef(model_cret_all)["iv1"]
powerSim(model_cret_all)

### what about plotting a power curve?
# you need the effect size of interest, the range of sample sizes to consider, and the sig level of the test
# The effect size is a measure of how large the difference between the groups is, 
# while the power is the probability of detecting that difference with a given sample size and significance level.

# for CRET all, the effect size for Group is -0.76, so the shams did better? lol
# the power for group is 59.5%, meaning there is a 59.5% chance of rejecting the null hypothesis
# that there is no difference between groups
# to increase the power, you need to increase the effect size, increase the sample size, or decrease the sig level

# Function to add participants to a specific group
addParticipantsToGroup <- function(data, group_var, group_code, n) {
  # Select a template participant from the specified group
  template_participant <- data[which(data[[group_var]] == group_code)[1], , drop = FALSE]
  
  # Create new participants based on the template, replicating 4 times for each day
  new_participants <- template_participant[rep(1, n * 4), ]
  
  # Assign unique new Subject IDs, considering 4 rows per participant
  max_subj_id <- max(as.numeric(data$Subj))
  new_ids <- rep(seq(from = max_subj_id + 1, by = 1, length.out = n), each = 4)
  new_participants$Subj <- as.factor(new_ids)
  
  # Combine with the original dataset
  rbind(data, new_participants)
}

# Calculate initial sizes for treatment and sham groups
# have to divide by 4 because there are four values per subject
initial_treatment_size <- as.numeric(sum(dprime_clean$iv1 == 1)/4)  # '1' is the code for treatment
initial_sham_size <- as.numeric(sum(dprime_clean$iv1 == 0)/4)       # '0' is the code for sham

# Simulate different scenarios
results <- data.frame(scenario = character(), sampleSize = integer(), power = numeric())
scenarios <- list(
  list(treatment_add = 1, sham_add = 0),  # Scenario 1: 8 treatment, 4 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 2: 8 treatment, 5 sham
  list(treatment_add = 1, sham_add = 0),  # Scenario 3: 9 treatment, 5 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 4: 9 treatment, 6 sham
  list(treatment_add = 1, sham_add = 0),  # Scenario 5: 10 treatment, 6 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 6: 10 treatment, 7 sham
  list(treatment_add = 1, sham_add = 0),  # Scenario 7: 11 treatment, 7 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 8: 11 treatment, 8 sham
  list(treatment_add = 1, sham_add = 0),  # Scenario 7: 12 treatment, 8 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 8: 12 treatment, 9 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 8: 12 treatment, 10 sham
  list(treatment_add = 0, sham_add = 1),  # Scenario 8: 12 treatment, 11 sham
  list(treatment_add = 0, sham_add = 1)  # Scenario 8: 12 treatment, 12 sham
)

significance_level <- 0.05  # Common choice for significance level

adjusted_dataset <- dprime_clean
for (scenario in scenarios) {
  # Adjust the dataset for each scenario
  adjusted_dataset <- addParticipantsToGroup(adjusted_dataset, "iv1", 1, scenario$treatment_add)
  adjusted_dataset <- addParticipantsToGroup(adjusted_dataset, "iv1", 0, scenario$sham_add)
  # Check the structure and consistency of the adjusted dataset
  str(adjusted_dataset)
  print(table(adjusted_dataset$Subj))  # Check if new subjects are added correctly
  # Re-fit the model
  extended_model <- lmer(dv ~ iv1 + (1|Subj), data = adjusted_dataset)
  # Perform power analysis
  power_result <- powerSim(extended_model, nsim = 1000) # Adjust nsim as needed
  # Calculate power
  estimated_power <- mean(power_result$pval < significance_level)
  # Store the results
  results <- rbind(results, data.frame(
    scenario = paste("T+", scenario$treatment_add, "S+", scenario$sham_add),
    sampleSize = nrow(adjusted_dataset),
    power = estimated_power
  ))
}

# Plotting the power curve
#plot(results$sampleSize, results$power, type = "b", xlab = "Sample Size", ylab = "Power")

#library(ggplot2)
# Convert the 'results' data frame to a format suitable for ggplot2
results$scenario <- as.factor(results$scenario)
results <- na.omit(results)
# now remember, currently sampleSize should be 96 for 96 rows, but there are four days per participant
# so we have to divide by 4 so sampleSize equals people, at 24.
results$sampleSize <- results$sampleSize/4

# Create the plot
ggplot(results, aes(x = sampleSize, y = power, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +  # Line plot
  geom_point(size = 3) +  # Add points
  geom_hline(yintercept = 0.8, linetype = "dotted", color = "red", size = 1) +  # Dotted line for 0.8 power
  scale_x_continuous(breaks = seq(from = floor(min(results$sampleSize)/10)*10, 
                                  to = ceiling(max(results$sampleSize)/10)*10, 
                                  by = 2)) +
  scale_color_brewer(palette = "Set1", direction = -1,labels = legend_labels) +  # Updated color palette
  expand_limits(y = c(0, 1)) +  # Expand y-axis to show the full range
  labs(title = "Power Analysis Curve",
       x = "Sample Size",
       y = "Estimated Power",
       color = "Scenario") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = max(results$sampleSize), y = 0.81, label = "80%", color = "red", hjust = 1) +  # Add label to dotted line
  guides(color = guide_legend(title = NULL)) + # Remove legend title
  theme(legend.text = element_text(size = 8)) +
  + annotate("text", x = max(results$sampleSize), y = 0.9, label = "Effect Size: 0.76", color = "black", hjust = 1)





##################
## run the rest of these once I know I did the above correctly
dv <- dprime_clean$`PRET-dprime-all`
model_pret_all = lmer(dv~iv1+iv2+(1|Subj),data = dprime_clean)
summary(model_pret_all)
fixef(model_pret_all)["iv1"]
fixef(model_pret_all)["iv2"]
powerSim(model_pret_all)

dv <- dprime_clean$`CRET-dprime-high`
model_cret_high = lmer(dv~iv1+iv2+(1|Subj),data = dprime_clean)
summary(model_cret_high)
fixef(model_cret_high)["iv1"]
fixef(model_cret_high)["iv2"]
powerSim(model_cret_high)

dv <- dprime_clean$`PRET-dprime-high`
model_pret_high = lmer(dv~iv1+iv2+(1|Subj),data = dprime_clean)
summary(model_cret_low)
fixef(model_cret_low)["iv1"]
fixef(model_cret_low)["iv2"]
powerSim(model_cret_low)




data <- read.csv("Z:/Yu/STAMP/STAMP_Categories/Mixed effects/ROI/ROI_PHC_R/adjusted_beh/power_analysis.csv")
#run model with basic fixed effects (don't need any interactions)
model=lmer(NPS~submem+TPC+(1|Subject) + (1|STIM_1_ID)+(1|STIM_2_ID)+ (1|STIM_1_ID:STIM_2_ID),data = data)
#view fixef, use that for your power analysis if you don't have an a priori estimate of the slope
summary(model)
fixef(model)["submemboth_miss"]
#alternatively you could specify hypothosized slope for the most important fixed effect, usually based on another study.
fixef(model)["iv1"]<--0.05
#run 1000 simulations to estimate the power for iv1, you will want this > 80%
powerSim(model)


#fixef(model)["iv1"]<--0.05  approach and used the fixef(model)["submemboth_miss"] 

#power analysis
#load simr
library(simr)
#run model with basic fixed effects (don't need any interactions)
model=lmer(dv~iv1+iv2+(1|randintercept1) + (1|randintercept2,data = tbl))
#view fixef, use that for your power analysis if you don't have an a priori estimate of the slope
fixef(model)["iv1"]
#alternatively you could specify hypothosized slope for the most important fixed effect, usually based on another study.
fixef(model)["iv1"]<--0.05
#run 1000 simulations to estimate the power for iv1, you will want this > 80%
powerSim(model)

print(pc)