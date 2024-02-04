# the goal here is simple. I want to put the mariam stats and NMF factors into one big model


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

mariamVals_df <- read.xlsx("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/mariamMemVals.xlsx")

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
      
      ### now that curr_fac_tbl is complete, let's add the additional cols with the mariam mem data
      # first make a col with the row order so we can go back after the merge
      # Add a new column to the first data frame with the row order
      curr_fac_tbl$order <- 1:nrow(curr_fac_tbl)
      # merge based on ItemID
      merged_df <- merge(curr_fac_tbl, mariamVals_df, by="ItemID")
      # put the order back
      merged_df <- merged_df[order(merged_df$order),]
      
      X = merged_df[[mem]]
      Y = merged_df$AvgROI
      # note, lexical_CR is a character object. need to convert to double
      # given how I'm converting earlier in the loop it shouldn't matter by this point
      if (class(X) == "character")
      {
        X = as.numeric(X)
      }
      
      M1 <- merged_df[["F01"]]
      M2 <- merged_df[["F02"]]
      M3 <- merged_df[["F03"]]
      M4 <- merged_df[["F04"]]
      M5 <- merged_df[["F05"]]
      M6 <- merged_df[["F06"]]
      M7 <- merged_df[["F07"]]
      M8 <- merged_df[["F08"]]
      M9 <- merged_df[["CS_adj"]]
      M10 <- merged_df[["MD"]]
      M11 <- merged_df[["slope"]]
      M12 <- merged_df[["frequency_adj"]]
      M13 <- merged_df[["NumFeat_adj"]]
      M14 <- merged_df[["name_agreement_adj"]]
      
      Subj <- c(curr_fac_tbl$Subj, merged_df$Subj)
      ItemID <- c(curr_fac_tbl$ItemID, merged_df$ItemID)
      
      # (2) clean those NaN rows
      Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,M9=M9,M10=M10,M11=M11,M12=M12,M13=M13,M14=M14,Subj=Subj,ItemID=ItemID)
      Data_mixed_clean <- na.omit(Data_mixed)
      
      
    model221 <- '
    level:1
    Y~1
    level:2
    Y~b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 + b7*M7 + b8*M8 + b9*M9 + b10*M10 + b11*M11 + b12*M12 + b13*M13 + b14*M14 + c*X
    M1~a1*X
    M2~a2*X
    M3~a3*X
    M4~a4*X
    M5~a5*X
    M6~a6*X
    M7~a7*X
    M8~a8*X
    M9~a9*X
    M10~a10*X
    M11~a11*X
    M12~a12*X
    M13~a13*X
    M14~a14*X
    
    #indirect and total effects
    ab1:=a1*b1
    ab2:=a2*b2
    ab3:=a3*b3
    ab4:=a4*b4
    ab5:=a5*b5
    ab6:=a6*b6
    ab7:=a7*b7
    ab8:=a8*b8
    ab9:=a9*b9
    ab10:=a10*b10
    ab11:=a11*b11
    ab12:=a12*b12
    ab13:=a13*b13
    ab14:=a14*b14
    
    # Total indirect effect
    totalIndirect := ab1 + ab2 + ab3 + ab4 + ab5 + ab6 + ab7 + ab8 + ab9 + ab10 + ab11 + ab12 + ab13 + ab14
    
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
      b9 <- c(tbl,mem,ROI,hemisphere,"b9",results_summary$pe$est[11],results_summary$pe$se[11],results_summary$pe$z[11],results_summary$pe$pvalue[11],pe_results$ci.lower[11],pe_results$ci.upper[11])
      b10 <- c(tbl,mem,ROI,hemisphere,"b10",results_summary$pe$est[12],results_summary$pe$se[12],results_summary$pe$z[12],results_summary$pe$pvalue[12],pe_results$ci.lower[12],pe_results$ci.upper[12])
      b11 <- c(tbl,mem,ROI,hemisphere,"b11",results_summary$pe$est[13],results_summary$pe$se[13],results_summary$pe$z[13],results_summary$pe$pvalue[13],pe_results$ci.lower[13],pe_results$ci.upper[13])
      b12 <- c(tbl,mem,ROI,hemisphere,"b12",results_summary$pe$est[14],results_summary$pe$se[14],results_summary$pe$z[14],results_summary$pe$pvalue[14],pe_results$ci.lower[14],pe_results$ci.upper[14])
      b13 <- c(tbl,mem,ROI,hemisphere,"b13",results_summary$pe$est[15],results_summary$pe$se[15],results_summary$pe$z[15],results_summary$pe$pvalue[15],pe_results$ci.lower[15],pe_results$ci.upper[15])
      b14 <- c(tbl,mem,ROI,hemisphere,"b14",results_summary$pe$est[16],results_summary$pe$se[16],results_summary$pe$z[16],results_summary$pe$pvalue[16],pe_results$ci.lower[16],pe_results$ci.upper[16])

      
      c <- c(tbl,mem,ROI,hemisphere,"c",results_summary$pe$est[17],results_summary$pe$se[17],results_summary$pe$z[17],results_summary$pe$pvalue[17],pe_results$ci.lower[17],pe_results$ci.upper[17])
      a1 <- c(tbl,mem,ROI,hemisphere,"a1",results_summary$pe$est[18],results_summary$pe$se[18],results_summary$pe$z[18],results_summary$pe$pvalue[18],pe_results$ci.lower[18],pe_results$ci.upper[18])
      a2 <- c(tbl,mem,ROI,hemisphere,"a2",results_summary$pe$est[19],results_summary$pe$se[19],results_summary$pe$z[19],results_summary$pe$pvalue[19],pe_results$ci.lower[19],pe_results$ci.upper[19])
      a3 <- c(tbl,mem,ROI,hemisphere,"a3",results_summary$pe$est[20],results_summary$pe$se[20],results_summary$pe$z[20],results_summary$pe$pvalue[20],pe_results$ci.lower[20],pe_results$ci.upper[20])
      a4 <- c(tbl,mem,ROI,hemisphere,"a4",results_summary$pe$est[21],results_summary$pe$se[21],results_summary$pe$z[21],results_summary$pe$pvalue[21],pe_results$ci.lower[21],pe_results$ci.upper[21])
      a5 <- c(tbl,mem,ROI,hemisphere,"a5",results_summary$pe$est[22],results_summary$pe$se[22],results_summary$pe$z[22],results_summary$pe$pvalue[22],pe_results$ci.lower[22],pe_results$ci.upper[22])
      a6 <- c(tbl,mem,ROI,hemisphere,"a6",results_summary$pe$est[23],results_summary$pe$se[23],results_summary$pe$z[23],results_summary$pe$pvalue[23],pe_results$ci.lower[23],pe_results$ci.upper[23])
      a7 <- c(tbl,mem,ROI,hemisphere,"a7",results_summary$pe$est[24],results_summary$pe$se[24],results_summary$pe$z[24],results_summary$pe$pvalue[24],pe_results$ci.lower[24],pe_results$ci.upper[24])
      a8 <- c(tbl,mem,ROI,hemisphere,"a8",results_summary$pe$est[25],results_summary$pe$se[25],results_summary$pe$z[25],results_summary$pe$pvalue[25],pe_results$ci.lower[25],pe_results$ci.upper[25])
      a9 <- c(tbl,mem,ROI,hemisphere,"a3",results_summary$pe$est[26],results_summary$pe$se[26],results_summary$pe$z[26],results_summary$pe$pvalue[26],pe_results$ci.lower[26],pe_results$ci.upper[26])
      a10 <- c(tbl,mem,ROI,hemisphere,"a4",results_summary$pe$est[27],results_summary$pe$se[27],results_summary$pe$z[27],results_summary$pe$pvalue[27],pe_results$ci.lower[27],pe_results$ci.upper[27])
      a11 <- c(tbl,mem,ROI,hemisphere,"a5",results_summary$pe$est[28],results_summary$pe$se[28],results_summary$pe$z[28],results_summary$pe$pvalue[28],pe_results$ci.lower[28],pe_results$ci.upper[28])
      a12 <- c(tbl,mem,ROI,hemisphere,"a6",results_summary$pe$est[29],results_summary$pe$se[29],results_summary$pe$z[29],results_summary$pe$pvalue[29],pe_results$ci.lower[29],pe_results$ci.upper[29])
      a13 <- c(tbl,mem,ROI,hemisphere,"a7",results_summary$pe$est[30],results_summary$pe$se[30],results_summary$pe$z[30],results_summary$pe$pvalue[30],pe_results$ci.lower[30],pe_results$ci.upper[30])
      a14 <- c(tbl,mem,ROI,hemisphere,"a8",results_summary$pe$est[31],results_summary$pe$se[31],results_summary$pe$z[31],results_summary$pe$pvalue[31],pe_results$ci.lower[31],pe_results$ci.upper[31])
      
      indirect1 <- c(tbl,mem,ROI,hemisphere, "indirect1", results_summary$pe$est[64], results_summary$pe$se[64], results_summary$pe$z[64], results_summary$pe$pvalue[64],pe_results$ci.lower[64],pe_results$ci.upper[64])
      indirect2 <- c(tbl,mem,ROI,hemisphere, "indirect2", results_summary$pe$est[65], results_summary$pe$se[65], results_summary$pe$z[65], results_summary$pe$pvalue[65],pe_results$ci.lower[65],pe_results$ci.upper[65])
      indirect3 <- c(tbl,mem,ROI,hemisphere, "indirect3", results_summary$pe$est[66], results_summary$pe$se[66], results_summary$pe$z[66], results_summary$pe$pvalue[66],pe_results$ci.lower[66],pe_results$ci.upper[66])
      indirect4 <- c(tbl,mem,ROI,hemisphere, "indirect4", results_summary$pe$est[67], results_summary$pe$se[67], results_summary$pe$z[67], results_summary$pe$pvalue[67],pe_results$ci.lower[67],pe_results$ci.upper[67])
      indirect5 <- c(tbl,mem,ROI,hemisphere, "indirect5", results_summary$pe$est[68], results_summary$pe$se[68], results_summary$pe$z[68], results_summary$pe$pvalue[68],pe_results$ci.lower[68],pe_results$ci.upper[68])
      indirect6 <- c(tbl,mem,ROI,hemisphere, "indirect6", results_summary$pe$est[69], results_summary$pe$se[69], results_summary$pe$z[69], results_summary$pe$pvalue[69],pe_results$ci.lower[69],pe_results$ci.upper[69])
      indirect7 <- c(tbl,mem,ROI,hemisphere, "indirect7", results_summary$pe$est[70], results_summary$pe$se[70], results_summary$pe$z[70], results_summary$pe$pvalue[70],pe_results$ci.lower[70],pe_results$ci.upper[70])
      indirect8 <- c(tbl,mem,ROI,hemisphere, "indirect8", results_summary$pe$est[71], results_summary$pe$se[71], results_summary$pe$z[71], results_summary$pe$pvalue[71],pe_results$ci.lower[71],pe_results$ci.upper[71])
      indirect9 <- c(tbl,mem,ROI,hemisphere, "indirect9", results_summary$pe$est[72], results_summary$pe$se[72], results_summary$pe$z[72], results_summary$pe$pvalue[72],pe_results$ci.lower[72],pe_results$ci.upper[72])
      indirect10 <- c(tbl,mem,ROI,hemisphere, "indirect10", results_summary$pe$est[73], results_summary$pe$se[73], results_summary$pe$z[73], results_summary$pe$pvalue[73],pe_results$ci.lower[73],pe_results$ci.upper[73])
      indirect11 <- c(tbl,mem,ROI,hemisphere, "indirect11", results_summary$pe$est[74], results_summary$pe$se[74], results_summary$pe$z[74], results_summary$pe$pvalue[74],pe_results$ci.lower[74],pe_results$ci.upper[74])
      indirect12 <- c(tbl,mem,ROI,hemisphere, "indirect12", results_summary$pe$est[75], results_summary$pe$se[75], results_summary$pe$z[75], results_summary$pe$pvalue[75],pe_results$ci.lower[75],pe_results$ci.upper[75])
      indirect13 <- c(tbl,mem,ROI,hemisphere, "indirect13", results_summary$pe$est[76], results_summary$pe$se[76], results_summary$pe$z[76], results_summary$pe$pvalue[76],pe_results$ci.lower[76],pe_results$ci.upper[76])
      indirect14 <- c(tbl,mem,ROI,hemisphere, "indirect14", results_summary$pe$est[77], results_summary$pe$se[77], results_summary$pe$z[77], results_summary$pe$pvalue[77],pe_results$ci.lower[77],pe_results$ci.upper[77])
      
      totalIndirect <- c(tbl,mem,ROI,hemisphere, "totalIndirect", results_summary$pe$est[78], results_summary$pe$se[78], results_summary$pe$z[78], results_summary$pe$pvalue[78],pe_results$ci.lower[78],pe_results$ci.upper[78])
      totalEffect <- c(tbl,mem,ROI,hemisphere, "totalEffect", results_summary$pe$est[79], results_summary$pe$se[79], results_summary$pe$z[79], results_summary$pe$pvalue[79],pe_results$ci.lower[79],pe_results$ci.upper[79])
      
      mediation_output_df <- rbind(mediation_output_df,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,
                                   a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,
                                   indirect1,indirect2,indirect3,indirect4,indirect5,indirect6,indirect7,indirect8,indirect9,indirect10,indirect11,indirect12,indirect13,indirect14,totalIndirect,totalEffect)
      
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
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_mariam_and_nmf.xlsx'
write_xlsx(mediation_output_df, filename)