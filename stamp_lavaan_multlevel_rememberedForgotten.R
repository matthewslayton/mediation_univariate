# same as stamp_lavaan_multLevel_nov14.R but for remembered and forgotten



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



# what about remembered and forgotten?
encycl_remembered <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
encycl_forgotten <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_encycl_additionalROIs_unilateral.xlsx")
encycl_300_remembered <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
encycl_300_forgotten <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_encycl_300_additionalROIs_unilateral.xlsx")
vis_remembered <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
vis_forgotten <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_vis_additionalROIs_unilateral.xlsx")
vis_300_remembered <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_remembered_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")
vis_300_forgotten <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/nmf_spreadsheets/avgActivity_forgotten_BNA_nnmf_vis_300_additionalROIs_unilateral.xlsx")

##### I NEED VERSIONS OF THESE OR TO FIND A DIFFERENT WAY TO CHANGE THE TYPE
### memColWithNaN <- import_list("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/correctMemColsWithNaNsNot999.xlsx")


#tbl_names <- c("encycl","vis","encycl_300","vis_300")
alt_tbl_names <- c("encycl_remembered","encycl_forgotten","vis_remembered","vis_forgotten",
                   "encycl_300_remembered","encycl_300_forgotten","vis_300_remembered","vis_300_forgotten")
memType <- c("lexical_CR","visual_CR")

#indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())
mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric(),ci_lower=numeric(),ci_upper=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

for (tbl in alt_tbl_names) {
  for (mem in memType) {
    for (ROI_name in ROI_name_variable) {
      
      # (0) get your variables
      #curr_fac_tbl <- fac_tbl[[ROI_name]] # or curr_fac_tbl <- fac_tbl
      curr_tbl <- get(tbl) #get encycl, vis, or fcn
      curr_fac_tbl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
      hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
      ROI <- gsub('mask_','',ROI_name)
      
      #### don't need to do this because I took the opportunity to make the switch in matlab 
      #### and it loads in R with NA automatically now
      # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
      #curr_fac_tbl$visual_CR <- memColWithNaN$Sheet1$visual_CR
      #curr_fac_tbl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
      
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
      b2 <- c(tbl,mem,ROI,hemisphere,"b2",results_summary$pe$est[4],results_summary$pe$se[4],results_summary$pe$z[4],results_summary$pe$pvalue[4],pe_results$ci.lower[4],pe_results$ci.upper[4])
      b3 <- c(tbl,mem,ROI,hemisphere,"b3",results_summary$pe$est[5],results_summary$pe$se[5],results_summary$pe$z[5],results_summary$pe$pvalue[5],pe_results$ci.lower[5],pe_results$ci.upper[5])
      b4 <- c(tbl,mem,ROI,hemisphere,"b4",results_summary$pe$est[6],results_summary$pe$se[6],results_summary$pe$z[6],results_summary$pe$pvalue[6],pe_results$ci.lower[6],pe_results$ci.upper[6])
      b5 <- c(tbl,mem,ROI,hemisphere,"b5",results_summary$pe$est[7],results_summary$pe$se[7],results_summary$pe$z[7],results_summary$pe$pvalue[7],pe_results$ci.lower[7],pe_results$ci.upper[7])
      b6 <- c(tbl,mem,ROI,hemisphere,"b6",results_summary$pe$est[8],results_summary$pe$se[8],results_summary$pe$z[8],results_summary$pe$pvalue[8],pe_results$ci.lower[8],pe_results$ci.upper[8])
      b7 <- c(tbl,mem,ROI,hemisphere,"b7",results_summary$pe$est[9],results_summary$pe$se[9],results_summary$pe$z[9],results_summary$pe$pvalue[9],pe_results$ci.lower[9],pe_results$ci.upper[9])
      b8 <- c(tbl,mem,ROI,hemisphere,"b8",results_summary$pe$est[10],results_summary$pe$se[10],results_summary$pe$z[10],results_summary$pe$pvalue[10],pe_results$ci.lower[10],pe_results$ci.upper[10])
      c <- c(tbl,mem,ROI,hemisphere,"c",results_summary$pe$est[11],results_summary$pe$se[11],results_summary$pe$z[11],results_summary$pe$pvalue[11],pe_results$ci.lower[11],pe_results$ci.upper[11])
      a1 <- c(tbl,mem,ROI,hemisphere,"a1",results_summary$pe$est[12],results_summary$pe$se[12],results_summary$pe$z[12],results_summary$pe$pvalue[12],pe_results$ci.lower[12],pe_results$ci.upper[12])
      a2 <- c(tbl,mem,ROI,hemisphere,"a2",results_summary$pe$est[13],results_summary$pe$se[13],results_summary$pe$z[13],results_summary$pe$pvalue[13],pe_results$ci.lower[13],pe_results$ci.upper[13])
      a3 <- c(tbl,mem,ROI,hemisphere,"a3",results_summary$pe$est[14],results_summary$pe$se[14],results_summary$pe$z[14],results_summary$pe$pvalue[14],pe_results$ci.lower[14],pe_results$ci.upper[14])
      a4 <- c(tbl,mem,ROI,hemisphere,"a4",results_summary$pe$est[15],results_summary$pe$se[15],results_summary$pe$z[15],results_summary$pe$pvalue[15],pe_results$ci.lower[15],pe_results$ci.upper[15])
      a5 <- c(tbl,mem,ROI,hemisphere,"a5",results_summary$pe$est[16],results_summary$pe$se[16],results_summary$pe$z[16],results_summary$pe$pvalue[16],pe_results$ci.lower[16],pe_results$ci.upper[16])
      a6 <- c(tbl,mem,ROI,hemisphere,"a6",results_summary$pe$est[17],results_summary$pe$se[17],results_summary$pe$z[17],results_summary$pe$pvalue[17],pe_results$ci.lower[17],pe_results$ci.upper[17])
      a7 <- c(tbl,mem,ROI,hemisphere,"a7",results_summary$pe$est[18],results_summary$pe$se[18],results_summary$pe$z[18],results_summary$pe$pvalue[18],pe_results$ci.lower[18],pe_results$ci.upper[18])
      a8 <- c(tbl,mem,ROI,hemisphere,"a8",results_summary$pe$est[19],results_summary$pe$se[19],results_summary$pe$z[19],results_summary$pe$pvalue[19],pe_results$ci.lower[19],pe_results$ci.upper[19])
      
      indirect1 <- c(tbl,mem,ROI,hemisphere, "indirect1", results_summary$pe$est[40], results_summary$pe$se[40], results_summary$pe$z[40], results_summary$pe$pvalue[40],pe_results$ci.lower[40],pe_results$ci.upper[40])
      indirect2 <- c(tbl,mem,ROI,hemisphere, "indirect2", results_summary$pe$est[41], results_summary$pe$se[41], results_summary$pe$z[41], results_summary$pe$pvalue[41],pe_results$ci.lower[41],pe_results$ci.upper[41])
      indirect3 <- c(tbl,mem,ROI,hemisphere, "indirect3", results_summary$pe$est[42], results_summary$pe$se[42], results_summary$pe$z[42], results_summary$pe$pvalue[42],pe_results$ci.lower[42],pe_results$ci.upper[42])
      indirect4 <- c(tbl,mem,ROI,hemisphere, "indirect4", results_summary$pe$est[43], results_summary$pe$se[43], results_summary$pe$z[43], results_summary$pe$pvalue[43],pe_results$ci.lower[43],pe_results$ci.upper[43])
      indirect5 <- c(tbl,mem,ROI,hemisphere, "indirect5", results_summary$pe$est[44], results_summary$pe$se[44], results_summary$pe$z[44], results_summary$pe$pvalue[44],pe_results$ci.lower[44],pe_results$ci.upper[44])
      indirect6 <- c(tbl,mem,ROI,hemisphere, "indirect6", results_summary$pe$est[45], results_summary$pe$se[45], results_summary$pe$z[45], results_summary$pe$pvalue[45],pe_results$ci.lower[45],pe_results$ci.upper[45])
      indirect7 <- c(tbl,mem,ROI,hemisphere, "indirect7", results_summary$pe$est[46], results_summary$pe$se[46], results_summary$pe$z[46], results_summary$pe$pvalue[46],pe_results$ci.lower[46],pe_results$ci.upper[46])
      indirect8 <- c(tbl,mem,ROI,hemisphere, "indirect8", results_summary$pe$est[47], results_summary$pe$se[47], results_summary$pe$z[47], results_summary$pe$pvalue[47],pe_results$ci.lower[47],pe_results$ci.upper[47])
      totalIndirect <- c(tbl,mem,ROI,hemisphere, "totalIndirect", results_summary$pe$est[48], results_summary$pe$se[48], results_summary$pe$z[48], results_summary$pe$pvalue[48],pe_results$ci.lower[48],pe_results$ci.upper[48])
      totalEffect <- c(tbl,mem,ROI,hemisphere, "totalEffect", results_summary$pe$est[49], results_summary$pe$se[49], results_summary$pe$z[49], results_summary$pe$pvalue[49],pe_results$ci.lower[49],pe_results$ci.upper[49])
      
      mediation_output_df <- rbind(mediation_output_df,b1,b2,b3,b4,b5,b6,b7,b8,c,
                                   a1,a2,a3,a4,a5,a6,a7,a8,
                                   indirect1,indirect2,indirect3,indirect4,indirect5,indirect6,indirect7,indirect8,totalIndirect,totalEffect)
      
      
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
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_with300_remembered_forgotten.xlsx'
write_xlsx(mediation_output_df, filename)

