# why not switch mem and fac and see what happens?


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

factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

ROI_name_variable <- c('mask_AG_L','mask_AG_R','mask_ATL_L','mask_ATL_R',
                       'mask_FuG_L','mask_FuG_R','mask_Hipp_L',
                       'mask_Hipp_R','mask_IFG_L','mask_IFG_R',
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
encycl_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
vis_300 <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")

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
mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric(),ci_lower=numeric(),ci_upper=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

#tbl <- "encycl"
# tbl <- "encycl_300"
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
      
      X = curr_fac_tbl[["F01"]]
      Y = curr_fac_tbl$AvgROI
      # note, lexical_CR is a character object. need to convert to double
      # given how I'm converting earlier in the loop it shouldn't matter by this point
      M1 <- curr_fac_tbl[[mem]]
      if (class(M1) == "character")
      {
        M1 = as.numeric(M1)
      }
      
      
      # M2 <- curr_fac_tbl[["F02"]]
      # M3 <- curr_fac_tbl[["F03"]]
      # M4 <- curr_fac_tbl[["F04"]]
      # M5 <- curr_fac_tbl[["F05"]]
      # M6 <- curr_fac_tbl[["F06"]]
      # M7 <- curr_fac_tbl[["F07"]]
      # M8 <- curr_fac_tbl[["F08"]]
      
      Subj <- curr_fac_tbl$Subj
      ItemID <- curr_fac_tbl$ItemID
      
      # (2) clean those NaN rows
      #Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID)
      Data_mixed <- data.frame(X=X, Y=Y, M1=M1,Subj=Subj,ItemID=ItemID)
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
      
      
      model221 <- '
      level:1
      Y~1
      level:2
      Y~b1*M1 +c*X
      M1~a1*X
 
      
      #indirect and total effects
      ab1:=a1*b1

      
      # Total indirect effect
      totalIndirect := ab1
      
      # Total effect
      totalEffect := totalIndirect + c
      '
      
      #fit221<-sem(model221,data=Data_mixed_clean ,cluster="ItemID")
      #summary(fit221,fit.measures=TRUE)
      
      fit221 <- sem(model221, 
                    data = Data_mixed_clean,
                    cluster = "ItemID",
                    se = "robust.huber.white")
      
      results_summary <- summary(fit221,fit.measures=TRUE)
      
      # Extract parameter estimates with robust standard errors
      pe_results <- parameterEstimates(fit221, standardized = TRUE)
      
      
      # model results
      b1 <- c(tbl,mem,ROI,hemisphere,"b1",results_summary$pe$est[3],results_summary$pe$se[3],results_summary$pe$z[3],results_summary$pe$pvalue[3],pe_results$ci.lower[3],pe_results$ci.upper[3])
      c <- c(tbl,mem,ROI,hemisphere,"c",results_summary$pe$est[4],results_summary$pe$se[4],results_summary$pe$z[4],results_summary$pe$pvalue[4],pe_results$ci.lower[4],pe_results$ci.upper[4])
      a1 <- c(tbl,mem,ROI,hemisphere,"a1",results_summary$pe$est[5],results_summary$pe$se[5],results_summary$pe$z[5],results_summary$pe$pvalue[5],pe_results$ci.lower[5],pe_results$ci.upper[5])
      
      indirect1 <- c(tbl,mem,ROI,hemisphere, "indirect1", results_summary$pe$est[12], results_summary$pe$se[12], results_summary$pe$z[12], results_summary$pe$pvalue[12],pe_results$ci.lower[12],pe_results$ci.upper[12])
      totalIndirect <- c(tbl,mem,ROI,hemisphere, "totalIndirect", results_summary$pe$est[13], results_summary$pe$se[13], results_summary$pe$z[13], results_summary$pe$pvalue[13],pe_results$ci.lower[13],pe_results$ci.upper[13])
      totalEffect <- c(tbl,mem,ROI,hemisphere, "totalEffect", results_summary$pe$est[14], results_summary$pe$se[14], results_summary$pe$z[14], results_summary$pe$pvalue[14],pe_results$ci.lower[14],pe_results$ci.upper[14])
      
      mediation_output_df <- rbind(mediation_output_df,b1,c,a1,
                                   indirect1,totalIndirect,totalEffect)
      
      
      
    } #end ROI_name_variable
  } #end memType
} #end tbl_names

# Loop through each row and add the 'significant' column based on CI values
mediation_output_df$significant <- NA  #Initialize the column with NA values
colnames(mediation_output_df) <- c('tbl','mem','ROI','hemisphere','resultType','estimate','std_err','zval','pval','ci_lower','ci_upper','significant')  

for (row in 1:nrow(mediation_output_df)) {
  # Check if the confidence interval excludes zero
  mediation_output_df$significant[row] <- mediation_output_df$ci_lower[row] > 0 & mediation_output_df$ci_upper[row] > 0 | mediation_output_df$ci_lower[row] < 0 & mediation_output_df$ci_upper[row] < 0
}

# fix the weird data type issues
if (class(mediation_output_df$estimate) == "character")
{
  mediation_output_df$estimate = as.numeric(mediation_output_df$estimate)
}
if (class(mediation_output_df$std_err) == "character")
{
  mediation_output_df$std_err = as.numeric(mediation_output_df$std_err)
}
if (class(mediation_output_df$zval) == "character")
{
  mediation_output_df$zval = as.numeric(mediation_output_df$zval)
}
if (class(mediation_output_df$pval) == "character")
{
  mediation_output_df$pval = as.numeric(mediation_output_df$pval)
}
if (class(mediation_output_df$ci_lower) == "character")
{
  mediation_output_df$ci_lower = as.numeric(mediation_output_df$ci_lower)
}
if (class(mediation_output_df$ci_upper) == "character")
{
  mediation_output_df$ci_upper = as.numeric(mediation_output_df$ci_upper)
}

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_memAsMediator.xlsx'
write_xlsx(mediation_output_df, filename)

