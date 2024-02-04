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
      
      fit221 <- sem(model221, 
                    data = Data_mixed_clean,
                    cluster = "ItemID",
                    se = "robust.huber.white")
      
      results_summary <- summary(fit221,fit.measures=TRUE, rsq=TRUE)
      
      # Extract parameter estimates with robust standard errors
      pe_results <- parameterEstimates(fit221, standardized = TRUE)
      
      
      
      # model results
      rsq_y <- c(tbl,mem,ROI,hemisphere,"rsq_y",results_summary$pe$est[50])
      rsq_m1 <- c(tbl,mem,ROI,hemisphere,"rsq_m1",results_summary$pe$est[51])
      rsq_m2 <- c(tbl,mem,ROI,hemisphere,"rsq_m2",results_summary$pe$est[52])
      rsq_m3 <- c(tbl,mem,ROI,hemisphere,"rsq_m3",results_summary$pe$est[53])
      rsq_m4 <- c(tbl,mem,ROI,hemisphere,"rsq_m4",results_summary$pe$est[54])
      rsq_m5 <- c(tbl,mem,ROI,hemisphere,"rsq_m5",results_summary$pe$est[55])
      rsq_m6 <- c(tbl,mem,ROI,hemisphere,"rsq_m6",results_summary$pe$est[56])
      rsq_m7 <- c(tbl,mem,ROI,hemisphere,"rsq_m7",results_summary$pe$est[57])
      rsq_m8 <- c(tbl,mem,ROI,hemisphere,"rsq_m8",results_summary$pe$est[58])
      
      
      mediation_output_df <- rbind(mediation_output_df,rsq_y,rsq_m1,rsq_m2,rsq_m3,
                                   rsq_m4,rsq_m5,rsq_m6,rsq_m7,rsq_m8)
      
      
      
    } #end ROI_name_variable
  } #end memType
} #end tbl_names


colnames(mediation_output_df) <- c('tbl','mem','ROI','hemisphere','resultType','estimate')  


# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_varianceExplained.xlsx'
write_xlsx(mediation_output_df, filename)

