# The X-M variance explained seems low. Maybe there's moderation?
# don't bother with moderated mediation right now. Just do moderation/interaction

#https://gabriellajg.github.io/EPSY-579-R-Cookbook-for-SEM/lavaan-lab-3-moderation-and-conditional-effects.html


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
library(interactions)

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



#tbl_names <- c("encycl","vis","fcn")
tbl_names <- c("encycl","vis")
#tbl_names <- c("encycl","vis","encycl_300","vis_300")
# alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
#                    "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR")



#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
moderation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric(),ci_lower=numeric(),ci_upper=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

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
      
      # (3) Create product term in data set
      Data_mixed_clean$XxM1 <- Data_mixed_clean$X * Data_mixed_clean$M1
      Data_mixed_clean$XxM2 <- Data_mixed_clean$X * Data_mixed_clean$M2
      Data_mixed_clean$XxM3 <- Data_mixed_clean$X * Data_mixed_clean$M3
      Data_mixed_clean$XxM4 <- Data_mixed_clean$X * Data_mixed_clean$M4
      Data_mixed_clean$XxM5 <- Data_mixed_clean$X * Data_mixed_clean$M5
      Data_mixed_clean$XxM6 <- Data_mixed_clean$X * Data_mixed_clean$M6
      Data_mixed_clean$XxM7 <- Data_mixed_clean$X * Data_mixed_clean$M7
      Data_mixed_clean$XxM8 <- Data_mixed_clean$X * Data_mixed_clean$M8
      
      # you can also make a composite moderator. Could make sense since they're all semantic factors
      Data_mixed_clean$CompMod = rowMeans(Data_mixed_clean[, c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")])
      Data_mixed_clean$XxCompMod = Data_mixed_clean$X * Data_mixed_clean$CompMod
      
      # (4) Interaction models
      #interactionModel <- lm(formula = Y~X*M1+X*M2+X*M3+X*M4+X*M5+X*M6+X*M7+X*M8, data = Data_mixed_clean)
      
      interactionAll <- "
        #Regression
        Y ~ b1*X + b2*M1 + b3*M2 + b4*M3 + b5*M4 + b6*M5 + b7*M6 + b8*M7 + b9*M8 + b10*XxM1 + b11*XxM2 + b12*XxM3 + b13*XxM4 + b14*XxM5 + b15*XxM6 + b16*XxM7 + b17*XxM8
      "
      inter_fit1 <- lavaan::sem(model = interactionAll, 
                                data = Data_mixed_clean, 
                                fixed.x = FALSE,
                                meanstructure = TRUE)
      
      interactionComp <- "
        #Regression
        Y ~ b1*X + b2*CompMod + b3*XxCompMod
      "
      
      inter_fit2 <- lavaan::sem(model = interactionComp, 
                                data = Data_mixed_clean, 
                                fixed.x = FALSE,
                                meanstructure = TRUE)
      
      ##### you can also make a composite moderator. Could make sense since they're all semantic factors
      #Data_mixed_clean$CompositeMod = rowMeans(Data_mixed_clean[, c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")])
      #interactionModelComp <- lm(Y ~ X * CompositeMod, data = Data_mixed_clean)
      #interact_plot(interactionModelComp, pred = "X", modx = "CompositeMod")
      #interact_plot(interactionModel,pred="X",modx="M1")
      
      # Regression coeffs/estimates and pvals tell you if there's a moderation effect or not
      # Covariances tells you how much the variables change together, so don't bear on moderation
      # Intercepts is the expected value of Y when all predictors are 0. It's a baseline value
      # Variances are the variability of the predictors and the DV.
      
      #### so, to assess moderation, you look at the interaction term's coeff and its p-value
      # for inter_fit2, there is a composite moderator, so it's just one line
      
      # (5) Get results
      #### INDIVIDUAL M's
      results_summary <- summary(inter_fit1,fit.measures=TRUE)
      pe_results <- parameterEstimates(inter_fit1,standardized=TRUE)
      
      XxM1_est <- results_summary$pe$est[10]
      XxM2_est <- results_summary$pe$est[11]
      XxM3_est <- results_summary$pe$est[12]
      XxM4_est <- results_summary$pe$est[13]
      XxM5_est <- results_summary$pe$est[14]
      XxM6_est <- results_summary$pe$est[15]
      XxM7_est <- results_summary$pe$est[16]
      XxM8_est <- results_summary$pe$est[17]
      XxM1_stdErr <- results_summary$pe$se[10]
      XxM2_stdErr <- results_summary$pe$se[11]
      XxM3_stdErr <- results_summary$pe$se[12]
      XxM4_stdErr <- results_summary$pe$se[13]
      XxM5_stdErr <- results_summary$pe$se[14]
      XxM6_stdErr <- results_summary$pe$se[15]
      XxM7_stdErr <- results_summary$pe$se[16]
      XxM8_stdErr <- results_summary$pe$se[17]
      XxM1_zval <- results_summary$pe$z[10]
      XxM2_zval <- results_summary$pe$z[11]
      XxM3_zval <- results_summary$pe$z[12]
      XxM4_zval <- results_summary$pe$z[13]
      XxM5_zval <- results_summary$pe$z[14]
      XxM6_zval <- results_summary$pe$z[15]
      XxM7_zval <- results_summary$pe$z[16]
      XxM8_zval <- results_summary$pe$z[17]
      XxM1_pval <- results_summary$pe$pvalue[10]
      XxM2_pval <- results_summary$pe$pvalue[11]
      XxM3_pval <- results_summary$pe$pvalue[12]
      XxM4_pval <- results_summary$pe$pvalue[13]
      XxM5_pval <- results_summary$pe$pvalue[14]
      XxM6_pval <- results_summary$pe$pvalue[15]
      XxM7_pval <- results_summary$pe$pvalue[16]
      XxM8_pval <- results_summary$pe$pvalue[17]
      
      # Define the interaction term names
      interaction_terms <- paste("XxM", 1:8, sep="")
      
      # Initialize lists to store confidence intervals
      ci_lower_list <- list()
      ci_upper_list <- list()
      
      # Loop through each interaction term and extract confidence intervals
      for (term in interaction_terms) {
        # Filter for the interaction term in the regression part
        filtered_result <- pe_results[pe_results$rhs == term & pe_results$lhs == "Y",]
        ci_lower_list[[term]] <- filtered_result$ci.lower
        ci_upper_list[[term]] <- filtered_result$ci.upper
        
      }
    
      XxM1 <- c(tbl, mem, ROI, hemisphere, "XxM1", XxM1_est, XxM1_stdErr, XxM1_zval, XxM1_pval, ci_lower_list$XxM1, ci_upper_list$XxM1)
      XxM2 <- c(tbl, mem, ROI, hemisphere, "XxM2", XxM2_est, XxM2_stdErr, XxM2_zval, XxM2_pval, ci_lower_list$XxM2, ci_upper_list$XxM2)
      XxM3 <- c(tbl, mem, ROI, hemisphere, "XxM3", XxM3_est, XxM3_stdErr, XxM3_zval, XxM3_pval, ci_lower_list$XxM3, ci_upper_list$XxM3)
      XxM4 <- c(tbl, mem, ROI, hemisphere, "XxM4", XxM4_est, XxM4_stdErr, XxM4_zval, XxM4_pval, ci_lower_list$XxM4, ci_upper_list$XxM4)
      XxM5 <- c(tbl, mem, ROI, hemisphere, "XxM5", XxM5_est, XxM5_stdErr, XxM5_zval, XxM5_pval, ci_lower_list$XxM5, ci_upper_list$XxM5)
      XxM6 <- c(tbl, mem, ROI, hemisphere, "XxM6", XxM6_est, XxM6_stdErr, XxM6_zval, XxM6_pval, ci_lower_list$XxM6, ci_upper_list$XxM6)
      XxM7 <- c(tbl, mem, ROI, hemisphere, "XxM7", XxM7_est, XxM7_stdErr, XxM7_zval, XxM7_pval, ci_lower_list$XxM7, ci_upper_list$XxM7)
      XxM8 <- c(tbl, mem, ROI, hemisphere, "XxM8", XxM8_est, XxM8_stdErr, XxM8_zval, XxM8_pval, ci_lower_list$XxM8, ci_upper_list$XxM8)
      
      #### Composite M
      results_summary_2 <- summary(inter_fit2,fit.measures=TRUE)
      pe_results_2 <- parameterEstimates(inter_fit2,standardized=TRUE)
      
      XxCompMod_est <- results_summary_2$pe$est[3]
      XxCompMod_stdErr <- results_summary_2$pe$se[3]
      XxCompMod_zval <- results_summary_2$pe$z[3]
      XxCompMod_pval <- results_summary_2$pe$pvalue[3]
      
      # Filter for the predictor of interest
      filtered_result <- pe_results_2[pe_results_2$rhs == "XxCompMod" & pe_results_2$lhs == "Y",]
      
      # Extract confidence intervals
      ci_lower <- filtered_result$ci.lower
      ci_upper <- filtered_result$ci.upper
      
      XxCompMod <- c(tbl,mem,ROI,hemisphere,"XxCompMod",XxCompMod_est,XxCompMod_stdErr,XxCompMod_zval,XxCompMod_pval,ci_lower,ci_upper)
  
      # add to big dataframe
      moderation_output_df <- rbind(moderation_output_df,XxM1,XxM2,XxM3,XxM4,XxM5,XxM6,XxM7,XxM8,XxCompMod)

      
    } #end ROI_name_variable
  } #end memType
} #end tbl_names


# (6) End stuff. Fix up the data types, add significance col, and write to xlsx
# make sure those col names are correct
colnames(moderation_output_df) <- c('tbl','mem','ROI','hemisphere','resultType','estimate','std_err','zval','pval','ci_lower','ci_upper')  

# fix the weird data type issues
if (class(moderation_output_df$estimate) == "character")
{
  moderation_output_df$estimate = as.numeric(moderation_output_df$estimate)
}
if (class(moderation_output_df$std_err) == "character")
{
  moderation_output_df$std_err = as.numeric(moderation_output_df$std_err)
}
if (class(moderation_output_df$zval) == "character")
{
  moderation_output_df$zval = as.numeric(moderation_output_df$zval)
}
if (class(moderation_output_df$pval) == "character")
{
  moderation_output_df$pval = as.numeric(moderation_output_df$pval)
}
if (class(moderation_output_df$ci_lower) == "character")
{
  moderation_output_df$ci_lower = as.numeric(moderation_output_df$ci_lower)
}
if (class(moderation_output_df$ci_upper) == "character")
{
  moderation_output_df$ci_upper = as.numeric(moderation_output_df$ci_upper)
}

# Loop through each row and add the 'significant' column based on CI values
moderation_output_df$significant <- NA  #Initialize the column with NA values
colnames(moderation_output_df) <- c('tbl','mem','ROI','hemisphere','resultType','estimate','std_err','zval','pval','ci_lower','ci_upper','significant')  

for (row in 1:nrow(moderation_output_df)) {
  # Check if the confidence interval excludes zero
  moderation_output_df$significant[row] <- moderation_output_df$ci_lower[row] > 0 & moderation_output_df$ci_upper[row] > 0 | moderation_output_df$ci_lower[row] < 0 & moderation_output_df$ci_upper[row] < 0
}

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/moderation.xlsx'
write_xlsx(moderation_output_df, filename)









