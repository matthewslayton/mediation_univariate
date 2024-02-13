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
#fcn <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_fcn_additionalROIs_unilateral.xlsx")
memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")

# let's check the 300 the Ps actually saw. How could the full 995 be better?
# encycl_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
# vis_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")

# what about remembered and forgotten?
# encycl_remembered <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
# encycl_forgotten <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
# encycl_300_remembered <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
# encycl_300_forgotten <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
# vis_remembered <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
# vis_forgotten <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
# vis_300_remembered <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")
# vis_300_forgotten <- importlist("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")


#tbl_names <- c("encycl","vis","fcn")
tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR")



#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {
# tbl <- "encycl"
# mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'


for (tbl in tbl_names) {
  for (mem in memType) {
    for (ROI_name in ROI_name_variable) {
      
      # (0) get your variables
      #curr_fac_tbl <- fac_tbl[[ROI_name]] # or curr_fac_tbl <- fac_tbl
      curr_tbl <- get(tbl) #get encycl, vis, or fcn
      curr_fac_tbl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
      hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
      ROI <- gsub('mask_','',ROI_name)
      
      # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
      curr_fac_tbl$visual_CR <- memColWithNaN$Sheet1$visual_CR
      curr_fac_tbl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
      
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
      
      # (2) clean those NaN rows
      Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID)
      Data_mixed_clean <- na.omit(Data_mixed)
      
      ### clustering by subject doesn't work.      
      ### we have to cluster by ItemID
      # At level one it will calculate the item intercepts for you to test at level 2
      # We don't have a valid level 2 because X or M1 or whatever have 200ish unique values per Subject
      # however there ARE unique values per item
      ### also remember that multilevel/mixed-effects are the same in this context.
      # just two ways of talking about the same things. Using mixed effects models is great
      # but if you can get to the same end with levels, then so much the better
      
      # Cortney They are equivalent in this context because X and M don't also vary by item and subject
      # The “mixed” part is referring to the fact that we have both random and fixed effects. 
      # In multilevel models you can have both, but only the random effects are required
      # So in this context (since we have nested data and are using both fixed and random effects) they are mathematically equivalent
      # Well at least in theory. Probably the package implementations are slightly different
      
      # lavaan is giving us a true multilevel model, just it’s not calculating the random 
      # and fixed effect in the same model (it isn’t mixed, only multilevel)
      
      ### why do the R^2's look wrong? Could be that the way I'm extracting them is wrong. I use indices
      # and maybe they're the wrong values. Also, clustering can interfere with the estimation of standard errors
      # might be better to calculate R^2 using individual regressions to check


      # model221 <- '
      # level:1
      # Y~1
      # level:2
      # Y~b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 + b7*M7 + b8*M8 +c*X
      # M1~a1*X
      # M2~a2*X
      # M3~a3*X
      # M4~a4*X
      # M5~a5*X
      # M6~a6*X
      # M7~a7*X
      # M8~a8*X
      # 
      # #indirect and total effects
      # ab1:=a1*b1
      # ab2:=a2*b2
      # ab3:=a3*b3
      # ab4:=a4*b4
      # ab5:=a5*b5
      # ab6:=a6*b6
      # ab7:=a7*b7
      # ab8:=a8*b8
      # 
      # # Total indirect effect
      # totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5 + ab6 + ab7 + ab8
      # 
      # # Total effect
      # totalEffect := totalIndirect + c
      # '
      # 
      # test_model <- lm(Y~M1*X + M2*X + M3*X + M4*X + M5*X + M6*X + M7*X + M8*X, data=Data_mixed_clean)
      # test_summary <- summary(test_model,rsq=TRUE)
      # 
      # average_Y_by_Subj <- Data_mixed_clean %>%
      #   group_by(Subj) %>%
      #   summarize(Average_Y = mean(Y, na.rm = TRUE))
      
      average_values_by_Item<- Data_mixed_clean %>%
        group_by(ItemID) %>%
        summarize(
          Average_X = mean(X, na.rm = TRUE),
          Average_Y = mean(Y, na.rm = TRUE),
          Average_M1 = mean(M1, na.rm = TRUE),
          Average_M2 = mean(M2, na.rm = TRUE),
          Average_M3 = mean(M3, na.rm = TRUE),
          Average_M4 = mean(M4, na.rm = TRUE),
          Average_M5 = mean(M5, na.rm = TRUE),
          Average_M6 = mean(M6, na.rm = TRUE),
          Average_M7 = mean(M7, na.rm = TRUE),
          Average_M8 = mean(M8, na.rm = TRUE)
        )
      

      #test_model <- lm(average_Y_by_Subj~M1*X + M2*X + M3*X + M4*X + M5*X + M6*X + M7*X + M8*X, data=Data_mixed_clean)
      #test_summary <- summary(test_model,rsq=TRUE)
      
      # test_Average_Model <- lm(Average_Y~Average_M1*Average_X + Average_M2*Average_X + Average_M3*Average_X + 
      #                            Average_M4*Average_X + Average_M5*Average_X + Average_M6*Average_X + Average_M7*Average_X + 
      #                            Average_M8*Average_X, data=average_values_by_Item)
      
      test_Average_Model <- lm(Average_Y~Average_M1*Average_X, data=average_values_by_Item)
      
      # zero-center
      average_values_by_Item$M1_centered <- average_values_by_Item$Average_M1 - mean(average_values_by_Item$Average_M1, na.rm = TRUE)
      # Example regression model using the centered M1 variable
      model_centered <- lm(Average_Y ~ M1_centered*Average_X, data = average_values_by_Item)
      
      test_Average_Model <- model_centered
      
      test_avg_summary <- summary(test_Average_Model,rsq=TRUE)
      rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y",test_avg_summary$r.squared)
      adj_rsq_y <- c(tbl,mem,ROI,hemisphere,"adj_rsq_y",test_avg_summary$adj.r.squared)
      
      # Shapiro-Wilk test for normality
      shapiro_test <- shapiro.test(residuals(test_Average_Model))
      W_statistic <- c(tbl,mem,ROI,hemisphere,"W_stat",shapiro_test$statistic)
      p_value <- c(tbl,mem,ROI,hemisphere,"pval",shapiro_test$p.value)
      
      mediation_output_df <- rbind(mediation_output_df,rsq_y,adj_rsq_y,W_statistic,p_value)
      
      # # Y is continuous and M1 etc are counts
      # # Add a log-transformed version of M1 (adding 1 to avoid log(0))
      # Data_mixed_clean$M1_log <- log(Data_mixed_clean$M1 + 1)
      # 
      # # Add a binary indicator for zeros in M1
      # Data_mixed_clean$M1_zero <- as.numeric(Data_mixed_clean$M1 == 0)
      # 
      # # Fit a linear model with the original and transformed predictors
      # model_log <- lm(Y ~ M1 + M1_log, data = Data_mixed_clean)
      # model_zero <- lm(Y ~ M1 + M1_zero , data = Data_mixed_clean)
      # model_regular <- lm(Y ~ M1, data = Data_mixed_clean)
      # # adding a predictor can do a lot of things.
      # # it might account for part of the variance that M1 didn't do on its own
      # # that would increase the R^2
      # # it might take away from M1 if they're correlated. That's bad from the 
      # # perspective of wanting significant result, but good because it keeps you honest
      # 
      # # Check the summary of the model
      # summary(model_log)
      # summary(model_zero)
      # 
      # # Diagnostics plots for the model
      # par(mfrow = c(2, 2))
      # plot(model_log)
      # 
      # par(mfrow=c(2,2))
      # plot(model_regular)
      
      # logistic model for zero vs positive outcomes
      
      # Step 1: Logistic Regression for Zero vs. Positive
      # coeffs tell you how changes in M1 are associated with log odds of Y being positive or zero
      # positive means higher M1 values gives higher chance of positive Y
      # significance is whether the association is statistically reliable
  
    #   Data_mixed_clean$Y_binary <- as.numeric(Data_mixed_clean$Y > 0)  # Binary indicator: 1 if Y > 0, 0 otherwise
    #   logistic_model <- glm(Y_binary ~ M1, data = Data_mixed_clean, family = binomial())
    #   #(Intercept)           M1  
    #   #-0.199743    -0.002221  
    #   # As M1 increases, the likelihood at Y is positive goes down, but the effect size is small
    #   # intercept is the value of Y when M1 is 0.
    #   
    #   # Step 2: Linear Regression for Non-Zero Values of Y
    #   Data_nonzero <- subset(Data_mixed_clean, Y != 0)
    #   linear_model_nonzero <- lm(Y ~ M1, data = Data_nonzero)
    #  # (Intercept)           M1  
    #  #   -0.241529    -0.001321  
    # # slight negative association
    # 
    #   install.packages("cplm")
    #   library(cplm)
    #   
    #   # Tweedie regression model
    #   tweedie_model <- cpglm(Y ~ M1, data = Data_mixed_clean, link.power = 0, var.power = 1.5)  # Tweedie distribution
    #   
      
      
      
      
      
      
      
      
      
      # # https://www.statology.org/histogram-of-residuals-in-r/
      # resid <- lm(Average_Y ~ Average_M1, data=average_values_by_Item)
      # resid_summary <- summary(resid)
      # 
      # ggplot(data = average_values_by_Item, aes(x = resid_summary$residuals)) +
      #   geom_histogram(fill = 'steelblue', color = 'black') +
      #   labs(title = 'Histogram of Residuals for MVOC L: Y~M1', x = 'Residuals', y = 'Frequency')
      # 
      # 
      # # model results
      # rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y",results_summary$pe$est[50])
      # rsq_m1 <- c(tbl,mem,ROI,hemisphere,"rsq_m1",results_summary$pe$est[51])
      # rsq_m2 <- c(tbl,mem,ROI,hemisphere,"rsq_m2",results_summary$pe$est[52])
      # rsq_m3 <- c(tbl,mem,ROI,hemisphere,"rsq_m3",results_summary$pe$est[53])
      # rsq_m4 <- c(tbl,mem,ROI,hemisphere,"rsq_m4",results_summary$pe$est[54])
      # rsq_m5 <- c(tbl,mem,ROI,hemisphere,"rsq_m5",results_summary$pe$est[55])
      # rsq_m6 <- c(tbl,mem,ROI,hemisphere,"rsq_m6",results_summary$pe$est[56])
      # rsq_m7 <- c(tbl,mem,ROI,hemisphere,"rsq_m7",results_summary$pe$est[57])
      # rsq_m8 <- c(tbl,mem,ROI,hemisphere,"rsq_m8",results_summary$pe$est[58])
      
      ##we're not capturing mediation.
      # The R^2 captures the models.
      # in the most complex model of Y, how much variance do X and M explain
      # it's information about the items as a predictor (because it's item averaged)
      
      #### model is too complex for the data so we're getting overfitting.
      # it's fitting noise, which inflates the R^2's. 
      #### (1) might be better to do just the mediators independently
      #### (2) stepwise, and have two mediators compete at a time and sort by highest to lowest effect
      
      
      # library(car)
      # # Calculate VIFs for the model -- tells us if there is multicollinearity
      # # but we do run each mediator separately so we get a better estimate, but
      # # it's something to consider when we think about overfitting
      # # you need R^2's when the ROI was significant
      # # and you need them for the most complex path, which is X and M on , the ab path
      # vif_values <- vif(test_Average_Model,type='predictor')
      # # Print the VIF values
      # print(vif_values)
      # plot(allEffects(test_Average_Model))
      
      # fit221 <- sem(model221,
      #               data = Data_mixed_clean,
      #               cluster = "ItemID",
      #               se = "robust.huber.white")

      
      
      # results_summary <- summary(fit221,fit.measures=TRUE, rsq=TRUE)
      # # Extract parameter estimates with robust standard errors
      # pe_results <- parameterEstimates(fit221, standardized = TRUE)
      # rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y",results_summary$pe$est[15])

      # # model results
      # rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y",results_summary$pe$est[50])
      # rsq_m1 <- c(tbl,mem,ROI,hemisphere,"rsq_m1",results_summary$pe$est[51])
      # rsq_m2 <- c(tbl,mem,ROI,hemisphere,"rsq_m2",results_summary$pe$est[52])
      # rsq_m3 <- c(tbl,mem,ROI,hemisphere,"rsq_m3",results_summary$pe$est[53])
      # rsq_m4 <- c(tbl,mem,ROI,hemisphere,"rsq_m4",results_summary$pe$est[54])
      # rsq_m5 <- c(tbl,mem,ROI,hemisphere,"rsq_m5",results_summary$pe$est[55])
      # rsq_m6 <- c(tbl,mem,ROI,hemisphere,"rsq_m6",results_summary$pe$est[56])
      # rsq_m7 <- c(tbl,mem,ROI,hemisphere,"rsq_m7",results_summary$pe$est[57])
      # rsq_m8 <- c(tbl,mem,ROI,hemisphere,"rsq_m8",results_summary$pe$est[58])
      
      # model_m1_m2_m3_m4_m5_m6_m7_m8 <- lm(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + (1|ItemID), data = Data_mixed_clean)
      # # Get the summary of the model
      # model_summary <- summary(model_m1_m2_m3_m4_m5_m6_m7_m8)
      # # Extract the R-squared value
      # r_squared_full_model <- model_summary$r.squared
      
     # rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y", r_squared_full_model)
      
      # rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y", rsq_y)
      # rsq_m1_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m1", rsq_m1)
      # rsq_m2_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m2", rsq_m2)
      # rsq_m3_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m3", rsq_m3)
      # rsq_m4_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m4", rsq_m4)
      # rsq_m5_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m5", rsq_m5)
      # rsq_m6_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m6", rsq_m6)
      # rsq_m7_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m7", rsq_m7)
      # rsq_m8_vector <- c(tbl, mem, ROI, hemisphere, "rsq_m8", rsq_m8)
      
     # mediation_output_df <- rbind(mediation_output_df,rsq_y,adj_rsq_y)
      
      
    } #end ROI_name_variable
  } #end memType
} #end tbl_names


colnames(mediation_output_df) <- c('tbl','mem','ROI','hemisphere','resultType','estimate')  

# fix the weird data type issues
if (class(mediation_output_df$estimate) == "character")
{
  mediation_output_df$estimate = as.numeric(mediation_output_df$estimate)
}

# Step 1: Initialize the 'adjusted' column with NAs
mediation_output_df$adjusted <- NA
# Step 2: Extract p-values where resultType is 'pval'
original_pvals <- mediation_output_df$estimate[mediation_output_df$resultType == "pval"]
# Step 3: Apply Benjamini-Hochberg correction to these p-values
adjusted_pvals <- p.adjust(original_pvals, method = "BH")
# Step 4: Fill in the 'adjusted' column with adjusted p-values for 'pval' rows
mediation_output_df$adjusted[mediation_output_df$resultType == "pval"] <- adjusted_pvals

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_M1_only_andShapiroWilk.xlsx'
write_xlsx(mediation_output_df, filename)



#### cross-validation splits the data into train and test sets so we can compare model performance
# Assuming Data_mixed_clean is your dataset
set.seed(123)  # For reproducibility
# Create a vector of unique ItemIDs
item_ids <- unique(Data_mixed_clean$ItemID)

# Randomly split ItemIDs into training and testing sets
train_ids <- sample(item_ids, size = length(item_ids) * 0.8)
test_ids <- setdiff(item_ids, train_ids)

# Create training and testing datasets based on split ItemIDs
train_data <- Data_mixed_clean[Data_mixed_clean$ItemID %in% train_ids, ]
test_data <- Data_mixed_clean[Data_mixed_clean$ItemID %in% test_ids, ]


fit_train <- sem(model221, data = train_data, cluster = "ItemID", se = "robust.huber.white")
results_summary_train <- summary(fit_train,fit.measures=TRUE, rsq=TRUE)
fit_test <- sem(model221, data = test_data, cluster = "ItemID", se = "robust.huber.white")
results_summary_test<- summary(fit_test,fit.measures=TRUE, rsq=TRUE)










### let's look at individual regressions.
# Regression for mediator M1
model_m1 <- lm(M1 ~ X, data = Data_mixed_clean)
summary(model_m1)$r.squared  # R² for the regression of M1 on X

# Regression for outcome Y with M1 and X
model_y <- lm(Y ~ M1 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M2 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M3 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M4 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M5 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M6 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M7 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X
model_y <- lm(Y ~ M8 + X, data = Data_mixed_clean)
summary(model_y)$r.squared  # R² for the regression of Y on M1 and X

### remember, you don't sum the Y~M1+X, Y~M2+X, etc for a few reasons. 
# First, R^2 can't be added, and they might explain overlapping variance

## one approach would be to do nested model comparisons.
# you start with just Y~X and incrementally add the mediators.
# Then do an anova to see if adding mediators increases the explanatory power of the model

# Base model with only X
base_model <- lm(Y ~ X, data = Data_mixed_clean)

# Adding mediators one by one
model_m1 <- lm(Y ~ X + M1, data = Data_mixed_clean)
model_m1_m2 <- lm(Y ~ X + M1 + M2, data = Data_mixed_clean)
model_m1_m2_m3 <- lm(Y ~ X + M1 + M2 + M3, data = Data_mixed_clean)
model_m1_m2_m3_m4 <- lm(Y ~ X + M1 + M2 + M3 + M4, data = Data_mixed_clean)
model_m1_m2_m3_m4_m5 <- lm(Y ~ X + M1 + M2 + M3 + M4 + M5, data = Data_mixed_clean)
model_m1_m2_m3_m4_m5_m6 <- lm(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6, data = Data_mixed_clean)
model_m1_m2_m3_m4_m5_m6_m7 <- lm(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6 + M7, data = Data_mixed_clean)
model_m1_m2_m3_m4_m5_m6_m7_m8 <- lm(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8, data = Data_mixed_clean)
# Get the summary of the model
model_summary <- summary(model_m1_m2_m3_m4_m5_m6_m7_m8)
# Extract the R-squared value
r_squared_full_model <- model_summary$r.squared
# # # Compare each model to its predecessor using an F-test
# anova(base_model, model_m1)  # Base model vs. model with M1
# anova(model_m1, model_m1_m2)  # Model with M1 vs. model with M1 and M2
# anova(model_m1_m2, model_m1_m2_m3)  # Model with M1 and M2 vs. model with M1, M2, and M3
# anova(model_m1_m2_m3, model_m1_m2_m3_m4)  # and so on...
# anova(model_m1_m2_m3_m4, model_m1_m2_m3_m4_m5)
# anova(model_m1_m2_m3_m4_m5, model_m1_m2_m3_m4_m5_m6)
# anova(model_m1_m2_m3_m4_m5_m6, model_m1_m2_m3_m4_m5_m6_m7)
# anova(model_m1_m2_m3_m4_m5_m6_m7, model_m1_m2_m3_m4_m5_m6_m7_m8)


# Initialize a data frame to store the results
comparison_df <- data.frame(Model_Comparison = character(), F_Value = numeric(), 
                            P_Value = numeric(), FDR_P_Value = numeric(), 
                            Significant = character(), stringsAsFactors = FALSE)

# List of model names for labeling comparisons
model_names <- c("Base", "M1", "M1+M2", "M1+M2+M3", "M1+M2+M3+M4", "M1+M2+M3+M4+M5", 
                 "M1+M2+M3+M4+M5+M6", "M1+M2+M3+M4+M5+M6+M7", "M1+M2+M3+M4+M5+M6+M7+M8")

# Perform ANOVA comparisons and store results
for (i in 2:length(models)) {
  comparison <- anova(models[[i-1]], models[[i]])
  # Extract the last row which contains the comparison of interest
  comparison_result <- comparison[nrow(comparison), ]
  # Construct the label for the model comparison
  model_comparison_label <- paste(model_names[i-1], "vs", model_names[i])
  # Append to the data frame
  comparison_df <- rbind(comparison_df, data.frame(Model_Comparison = model_comparison_label,
                                                   F_Value = comparison_result["F"], 
                                                   P_Value = comparison_result["Pr(>F)"],
                                                   stringsAsFactors = FALSE))
}

# Set column names for the data frame
colnames(comparison_df) <- c("Model_Comparison", "F_Value", "P_Value")

# Apply FDR correction
comparison_df$FDR_P_Value <- p.adjust(comparison_df$P_Value, method = "fdr")
# Determine significance based on FDR corrected p-values (assuming alpha = 0.05)
comparison_df$Significant <- comparison_df$FDR_P_Value < 0.05

#### all false after correction, so adding mediators doesn't make the model better?

### mediator models (M1~X, M2~X)
### outcome models (Y~X+M1, Y~X+M2)
# Assuming Data_mixed_clean is your dataset and it contains Y, X, and M1 to M8

# Initialize vectors to store R² values
r_squared_mediators <- numeric(8)
r_squared_outcomes <- numeric(8)

# Model names for plotting
mediator_names <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")

# Calculate R² for mediator models (M ~ X)
for (i in 1:8) {
  mediator_model <- lm(as.formula(paste(mediator_names[i], "~ X")), data = Data_mixed_clean)
  r_squared_mediators[i] <- summary(mediator_model)$r.squared
}

# Calculate R² for outcome models (Y ~ M + X)
for (i in 1:8) {
  outcome_model <- lm(as.formula(paste("Y ~", mediator_names[i], "+ X")), data = Data_mixed_clean)
  r_squared_outcomes[i] <- summary(outcome_model)$r.squared
}

# Plot R² values for mediator models
barplot(r_squared_mediators, names.arg = mediator_names, main = "R² for Mediator Models (M ~ X)",
        ylab = "R² Value", col = "skyblue", ylim = c(0, max(r_squared_mediators, r_squared_outcomes) + 0.05))
text(x = seq_along(mediator_names), y = r_squared_mediators, label = round(r_squared_mediators, 3), pos = 3, cex = 0.8)







### troubleshooting the NAs for rsq_y in some ROIs

#### any NaNs in the Y col?
#na_indices <- which(is.na(Data_mixed_clean$Y))

# Function to calculate VIF for lavaan SEM model
calculate_vif_sem <- function(model) {
  exog_vars <- names(model@model$Exog)
  endog_vars <- names(model@model$Endog)
  
  vif_values <- sapply(exog_vars, function(var) {
    lm_model <- lm(as.formula(paste(var, "~", paste(endog_vars, collapse = " + "))), data = model@data)
    viflm(lm_model)
  })
  
  return(vif_values)
}

# Calculate VIF for the entire model
vif_results <- calculate_vif_sem(fit221)


library(interactions)

# Assuming M1 is the moderator variable
# Check if columns X and M1 are present in the dataset
colnames(Data_mixed_clean)

# Create interaction term in the dataset
Data_mixed_clean$X_M1_interaction <- Data_mixed_clean$X * Data_mixed_clean$M1

# Subset the data and set row names
Data_moderation_clean <- na.omit(Data_mixed_clean[, c("Y", "X", "M1", "X_M1_interaction")])
rownames(Data_moderation_clean) <- NULL

# Model with the interaction term
model_moderation <- '
  Y ~ c * X + d1 * X_M1_interaction
'

# Fit the moderation model without clustering
fit_moderation <- sem(model_moderation, data = Data_moderation_clean)

# Display the results
summary(fit_moderation, fit.measures = TRUE, rsq = TRUE)













