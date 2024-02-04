# we have mediation and moderation, which is tricky.
# this suggests that while item memorability influences brain activity via the item's semantics,
# the relationship between memorability and semantics is contingent on unknown factors.
# I'd like to know which items 'work' for or against mediation and moderation.
# Maybe we can identify trends for which items support the mediation or moderation

# the chatbot has suggested a few promising options, and I'd like to start with three

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
library(tictoc)
library(reshape2)
library(broom)

factor_names <- c('F01','F02','F03','F04','F05','F06','F07','F08')

# add mask_Hipp_A and mask_Hipp_P
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
#mediation_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric(),ci_lower=numeric(),ci_upper=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

all_reduced_cat_data_by_ROI <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),path=character(),animal=numeric(),home_tool=numeric(),outside=numeric(),food=numeric(),home=numeric())
# all_cat_data_by_ROI <- data.frame(tbl=character(),mem=character(),ROI=character(),
#   hemisphere=character(),path=character(), appliances=character(),baby=character(),
#   birds=character(),body_parts=character(),building=character(),category=character(),
#   clothing=character(),decorative=character(),electronics=character(),food=character(),
#   fruit=character(),furniture=character(),holiday=character(),house_parts=character(),
#   household_items=character(),insect=character(),kitchen_items=character(),mammal=character(),
#   military=character(),musical_instruments=character(),office_tool=character(),
#   plant=character(),reptile=character(),sea=character(),sports=character(),
#   street_items=character(),tool=character(),toys=character(),
#   vegetable=character(),vehicle=character())
### wow, it turns out that when you look at the actual data, there are 12 unique categories in the big group

all_cat_data_by_ROI <- data.frame(tbl=character(),mem=character(),ROI=character(),
    hemisphere=character(),path=character(),birds=character(),building=character(),
    clothing=character(),food=character(),fruit=character(),furniture=character(),
    mammal=character(),musical_instruments=character(),street_items=character(),
    tool=character(),vegetable=character(),vehicle=character())

#all_summary_data <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),resultType=character(),mean=numeric(),sd=numeric(),min=numeric(),max=numeric())


## need to add category cols
ref_tbl <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/itemIDs.xlsx",sheet="Sheet1")
names(ref_tbl)[names(ref_tbl) == "id"] <- "ItemID"

tbl <- "encycl"
mem <- "lexical_CR"
# ROI_name <- 'mask_AG_L'


#for (tbl in tbl_names) {
#  for (mem in memType) {
    for (ROI_name in ROI_name_variable) {
      
      tic()
      
      # (0) get your variables
      #curr_fac_tbl <- fac_tbl[[ROI_name]] # or curr_fac_tbl <- fac_tbl
      curr_tbl <- get(tbl) #get encycl, vis, or fcn
      curr_fac_tbl <- curr_tbl[[ROI_name]] #get the specific sheet for each ROI
      hemisphere <- str_sub(ROI_name,-1,-1) #get the L or R
      ROI <- gsub('mask_','',ROI_name)
      
      # (1) fac_tbl might load with 999 instead of NaNs. Need to replace first
      curr_fac_tbl$visual_CR <- memColWithNaN$Sheet1$visual_CR
      curr_fac_tbl$lexical_CR <- memColWithNaN$Sheet1$lexical_CR
      
      # add the category column
      curr_fac_tbl <- left_join(curr_fac_tbl, ref_tbl[, c("ItemID", "category","reduced_category")], by = "ItemID")
      
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
      category <- curr_fac_tbl$category
      reduced_category <- curr_fac_tbl$reduced_category
      
      # (2) clean those NaN rows
      Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj,ItemID=ItemID,category=category,reduced_category=reduced_category)
      Data_mixed_clean <- na.omit(Data_mixed)
      
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
      # (3) Make lists of categories and run the SEM with one category removed at a time
      # do this twice. Once for the "reduced_category" col that has five categories and
      # again for the "category col" that has thirty
      
      ### (3a) reduced_category
      unique_reduced_categories <- unique(Data_mixed_clean$reduced_category)
      impact_assessment_reduced_categories <- vector("list", length(unique_reduced_categories))
      errors_categories <- vector("list", length(unique_reduced_categories))
      
      for (i in seq_along(unique_reduced_categories)) {
        category_to_remove <- unique_reduced_categories[i]
        
        # Create a subset of the data without the current item's category
        modified_data <- subset(Data_mixed_clean, reduced_category != category_to_remove)
        
        # Re-fit the model
        modified_fit <- try(sem(model221, 
                            data = modified_data,
                            cluster = "ItemID",
                            se = "robust.huber.white"),
                            silent=TRUE)
        
        if (class(modified_fit) == "try-error") {
          impact_assessment_reduced_categories[[i]] <- NULL
          errors_categories[[i]] <- paste("Error with category:", category_to_remove)
        } else {
          impact_assessment_reduced_categories[[i]] <- summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
          errors_categories[[i]] <- NA  # No error
        }
        # Store the summary of the model
        impact_assessment_reduced_categories[[i]] <- summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
      }
      
      
      #results_summary <- summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
      #Or can do a separate call: fit_measures <- lavInspect(modified_fit, "fit.measures")
      
      ### (3b) category
      unique_categories <- unique(Data_mixed_clean$category)
      impact_assessment_categories <- vector("list", length(unique_categories))
      errors_categories <- vector("list", length(unique_categories))
      
      for (i in seq_along(unique_categories)) {
        category_to_remove <- unique_categories[i]
        
        # Create a subset of the data without the current item's category
        modified_data <- subset(Data_mixed_clean, category != category_to_remove)
        
        # Re-fit the model
        modified_fit <- try(sem(model221, 
                                data = modified_data,
                                cluster = "ItemID",
                                se = "robust.huber.white"),
                            silent=TRUE)
        
        if (class(modified_fit) == "try-error") {
          impact_assessment_categories[[i]] <- NULL
          errors_categories[[i]] <- paste("Error with category:", category_to_remove)
        } else {
          impact_assessment_categories[[i]] <-summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
          errors_categories[[i]] <- NA  # No error
        }
        # Store the summary of the model
        impact_assessment_categories[[i]] <- summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
      }
      

      # (4) Get the mediation estimates out of there
      ### (4a) reduced_category
      impact_assessment <- impact_assessment_reduced_categories
      # Extracting estimates for the a-paths
      a_paths <- paste("a", 1:8, sep="")
      a_estimates <- matrix(NA, nrow = length(a_paths), ncol = length(impact_assessment))
      b_paths <- paste("a", 1:8, sep="")
      b_estimates <- matrix(NA, nrow = length(b_paths), ncol = length(impact_assessment))
      indirect_paths <- paste("ab",1:8, sep="")
      indirect_estimates <- matrix(NA, nrow = length(indirect_paths), ncol = length(impact_assessment))
      # get c, totalIndirect, and totalEffect too
      c_totalInd_totalEff <- matrix(NA, nrow = 3, ncol = length(impact_assessment))
      rsqr_total <- matrix(NA, nrow = 9, ncol = length(impact_assessment))
      
      for (i in seq_along(impact_assessment)) {
        if ("lavaan.summary" %in% class(impact_assessment[[i]])) {
          for (j in seq_along(a_paths)) {
            a_path <- a_paths[j]
            curr_est <- impact_assessment[[i]]
            ests <- curr_est$pe
            a_estimate <- ests[ests$op == "~" & ests$label == a_path, "est"]
            if (length(a_estimate) > 0) {
              a_estimates[j, i] <- a_estimate
            } else {
              a_estimates[j, i] <- NA
            }
            b_path <- b_paths[j]
            b_estimate <- ests[ests$op == "~" & ests$label == b_path, "est"]
            if (length(b_estimate) > 0) {
              b_estimates[j, i] <- b_estimate
            } else {
              b_estimates[j, i] <- NA
            }
            
            indirect <- indirect_paths[j]
            indirect_estimate <- ests[ests$label == indirect, "est"]
            if (length(indirect_estimate) > 0) {
              indirect_estimates[j, i] <- indirect_estimate
            } else {
              indirect_estimates[j, i] <- NA
            }
          } #end of j loop
          c_est <- ests[ests$op == "~" & ests$label == "c", "est"]
          totalInd_est <- ests[ests$label == "totalIndirect", "est"]
          totalEff_est <- ests[ests$label == "totalEffect", "est"]
          rsqr <- ests[ests$op == "r2","est"]
          
          if (length(c_est) > 0) {
            c_totalInd_totalEff[1, i] <- c_est
            c_totalInd_totalEff[2, i] <- totalInd_est
            c_totalInd_totalEff[3, i] <- totalEff_est
            rsqr_total[,i] <- rsqr
          } else {
            c_totalInd_totalEff[1, i] <- NA
            c_totalInd_totalEff[2, i] <- NA
            c_totalInd_totalEff[3, i] <- NA
            rsqr_total[,i] <- NA
          }
        }
      }
      # Converting to a data frame for easier handling
      # Creating labels
      a_labels <- paste("a", 1:8, sep="")
      b_labels <- paste("b", 1:8, sep="")
      ab_labels <- paste("ab", 1:8, sep="")
      # Assuming only one each for these paths
      other_labels <- c("c", "totalIndirect", "totalEffect")
      rsq_labels <- c("rsq_y", "rsq_m1", "rsq_m2", "rsq_m3", "rsq_m4", "rsq_m5", "rsq_m6", "rsq_m7", "rsq_m8")
      # Combine all labels
      all_labels <- c(a_labels, b_labels, ab_labels, other_labels,rsq_labels)
      all_df_reduced_categories <- rbind(a_estimates,b_estimates,indirect_estimates,c_totalInd_totalEff,rsqr_total)
      all_df_reduced_categories <- as.data.frame(all_df_reduced_categories)
      all_df_reduced_categories$path <- all_labels
      
      # add outer loop info
      all_df_reduced_categories$tbl <- tbl
      all_df_reduced_categories$mem <- mem
      all_df_reduced_categories$ROI <- ROI
      all_df_reduced_categories$hemisphere <- hemisphere
      
      # rename data columns to category names
      names(all_df_reduced_categories)[1:5] <- c("animal","home_tool", "outside","food","home")
      
      # Reordering columns
      all_df_reduced_categories <- all_df_reduced_categories[, c(7:10,6, 1:5)]
      
      all_reduced_cat_data_by_ROI <- rbind(all_reduced_cat_data_by_ROI,all_df_reduced_categories)
      
      ### (4b) category
      impact_assessment <- impact_assessment_categories
      # Extracting estimates for the a-paths
      a_paths <- paste("a", 1:8, sep="")
      a_estimates <- matrix(NA, nrow = length(a_paths), ncol = length(impact_assessment))
      b_paths <- paste("a", 1:8, sep="")
      b_estimates <- matrix(NA, nrow = length(b_paths), ncol = length(impact_assessment))
      indirect_paths <- paste("ab",1:8, sep="")
      indirect_estimates <- matrix(NA, nrow = length(indirect_paths), ncol = length(impact_assessment))
      # get c, totalIndirect, and totalEffect too
      c_totalInd_totalEff <- matrix(NA, nrow = 3, ncol = length(impact_assessment))
      rsqr_total <- matrix(NA, nrow = 9, ncol = length(impact_assessment))
      
      for (i in seq_along(impact_assessment)) {
        if ("lavaan.summary" %in% class(impact_assessment[[i]])) {
          for (j in seq_along(a_paths)) {
            a_path <- a_paths[j]
            curr_est <- impact_assessment[[i]]
            ests <- curr_est$pe
            a_estimate <- ests[ests$op == "~" & ests$label == a_path, "est"]
            if (length(a_estimate) > 0) {
              a_estimates[j, i] <- a_estimate
            } else {
              a_estimates[j, i] <- NA
            }
            b_path <- b_paths[j]
            b_estimate <- ests[ests$op == "~" & ests$label == b_path, "est"]
            if (length(b_estimate) > 0) {
              b_estimates[j, i] <- b_estimate
            } else {
              b_estimates[j, i] <- NA
            }
            indirect <- indirect_paths[j]
            indirect_estimate <- ests[ests$label == indirect, "est"]
            if (length(indirect_estimate) > 0) {
              indirect_estimates[j, i] <- indirect_estimate
            } else {
              indirect_estimates[j, i] <- NA
            }
          } #end of j loop
          c_est <- ests[ests$op == "~" & ests$label == "c", "est"]
          totalInd_est <- ests[ests$label == "totalIndirect", "est"]
          totalEff_est <- ests[ests$label == "totalEffect", "est"]
          rsqr <- ests[ests$op == "r2","est"]
          
          if (length(c_est) > 0) {
            c_totalInd_totalEff[1, i] <- c_est
            c_totalInd_totalEff[2, i] <- totalInd_est
            c_totalInd_totalEff[3, i] <- totalEff_est
            rsqr_total[,i] <- rsqr
          } else {
            c_totalInd_totalEff[1, i] <- NA
            c_totalInd_totalEff[2, i] <- NA
            c_totalInd_totalEff[3, i] <- NA
            rsqr_total[,i] <- NA
          }
        }
      }
      
      # Converting to a data frame for easier handling
      # Creating labels
      a_labels <- paste("a", 1:8, sep="")
      b_labels <- paste("b", 1:8, sep="")
      ab_labels <- paste("ab", 1:8, sep="")
      # Assuming only one each for these paths
      other_labels <- c("c", "totalIndirect", "totalEffect")
      rsq_labels <- c("rsq_y", "rsq_m1", "rsq_m2", "rsq_m3", "rsq_m4", "rsq_m5", "rsq_m6", "rsq_m7", "rsq_m8")
      # Combine all labels
      all_labels <- c(a_labels, b_labels, ab_labels, other_labels,rsq_labels)
      all_df_categories <- rbind(a_estimates,b_estimates,indirect_estimates,c_totalInd_totalEff,rsqr_total)
      all_df_categories <- as.data.frame(all_df_categories)
      all_df_categories$path <- all_labels
      
      # add outer loop info
      all_df_categories$tbl <- tbl
      all_df_categories$mem <- mem
      all_df_categories$ROI <- ROI
      all_df_categories$hemisphere <- hemisphere
      
      # rename data columns to category names
      # names(all_df_categories)[1:30] <- c("appliances","baby","birds","body_parts",
      # "building","category","clothing","decorative","electronics","food","fruit",
      # "furniture","holiday","house_parts","household_items","insect","kitchen_items",
      # "mammal","military","musical_instruments","office_tool","plant","reptile",
      # "sea","sports","street_items","tool","toys","vegetable","vehicle")
      
      names(all_df_categories)[1:12] <- c("birds","building","clothing","food","
      fruit","furniture","mammal","musical_instruments","street_items","tool",
      "vegetable","vehicle")
      
      # Reordering columns THESE NUMBERS WILL HAVE TO CHANGE
      all_df_categories <- all_df_categories[, c(14:17,13,1:12)] #THESE NUMBERS WILL HAVE TO CHANGE
      
      all_cat_data_by_ROI <- rbind(all_cat_data_by_ROI,all_df_categories)
      
      
    } #end ROI_name_variable
toc()
#  } #end memType
#} #end tbl_names

# write to spreadsheet
filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_reduced_category_atATime.xlsx'
write_xlsx(all_reduced_cat_data_by_ROI, filename)

filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_category_atATime.xlsx'
write_xlsx(all_cat_data_by_ROI, filename)

# (5) Get some summary statistics for these values to see what's going on when we add or remove categories
# can do the same for items if I want
totalIndirect_data <- subset(all_df_categories, path == "totalIndirect")
totalIndirect_mean <- colMeans(totalIndirect_data[, c("animal", "home_tool", "outside", "food", "home")], na.rm = TRUE)
totalIndirect_sd <- apply(totalIndirect_data[, c("animal", "home_tool", "outside", "food", "home")], MARGIN = 2, sd, na.rm = TRUE)
totalIndirect_min <- sapply(totalIndirect_data[, c("animal", "home_tool", "outside", "food", "home")], min, na.rm = TRUE)
totalIndirect_max <- sapply(totalIndirect_data[, c("animal", "home_tool", "outside", "food", "home")], max, na.rm = TRUE)



allStats <- c(totalIndirect_data,totalIndirect_mean,totalIndirect_sd,totalIndirect_min,totalIndirect_max)


##### Continue quantifying results
one_cat <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_category_atATime.xlsx")
one_reduced_cat <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_reduced_category_atATime.xlsx")
med_analysis <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_with_Hipp_A_and_P.xlsx")
rsq <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_multilevel_unilateral_varianceExplained.xlsx")

data_from_previous_step <- read_excel("/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/remove_one_category_atATime.xlsx")

# (5a) fun with paired t-tests
### the goal here is to get the mediation and rsq results from the mediation analysis 
### that uses all categories and compare that data with the results when we
### removed one category at a time.

# now, I'm interested in total indirect effect, c path, rsq_y, rsq_m1
all_cat_totalInd <- subset(med_analysis,resultType=="totalIndirect" & tbl=="encycl" & mem=="lexical_CR")
all_cat_c <- subset(med_analysis,resultType=="c" & tbl=="encycl" & mem=="lexical_CR")
all_cat_rsq_y <- subset(rsq,resultType=="rsq_y" & tbl=="encycl" & mem=="lexical_CR")
all_cat_rsq_m1 <- subset(rsq,resultType=="rsq_m1" & tbl=="encycl" & mem=="lexical_CR")
all_cat_rsq_m2 <- subset(rsq, resultType == "rsq_m2" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m3 <- subset(rsq, resultType == "rsq_m3" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m4 <- subset(rsq, resultType == "rsq_m4" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m5 <- subset(rsq, resultType == "rsq_m5" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m6 <- subset(rsq, resultType == "rsq_m6" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m7 <- subset(rsq, resultType == "rsq_m7" & tbl == "encycl" & mem == "lexical_CR")
all_cat_rsq_m8 <- subset(rsq, resultType == "rsq_m8" & tbl == "encycl" & mem == "lexical_CR")

one_cat_totalInd <- subset(one_cat,path=="totalIndirect" & tbl=="encycl" & mem=="lexical_CR")
one_cat_c <- subset(one_cat,path=="c" & tbl=="encycl" & mem=="lexical_CR")
one_cat_rsq_y <- subset(one_cat,path=="rsq_y" & tbl=="encycl" & mem=="lexical_CR")
one_cat_rsq_m1 <- subset(one_cat,path=="rsq_m1" & tbl=="encycl" & mem=="lexical_CR")
one_cat_rsq_m2 <- subset(one_cat, path == "rsq_m2" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m3 <- subset(one_cat, path == "rsq_m3" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m4 <- subset(one_cat, path == "rsq_m4" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m5 <- subset(one_cat, path == "rsq_m5" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m6 <- subset(one_cat, path == "rsq_m6" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m7 <- subset(one_cat, path == "rsq_m7" & tbl == "encycl" & mem == "lexical_CR")
one_cat_rsq_m8 <- subset(one_cat, path == "rsq_m8" & tbl == "encycl" & mem == "lexical_CR")


one_reduced_cat_totalInd <- subset(one_reduced_cat,path=="totalIndirect" & tbl=="encycl" & mem=="lexical_CR")
one_reduced_cat_c <- subset(one_reduced_cat,path=="c" & tbl=="encycl" & mem=="lexical_CR")
one_reduced_cat_rsq_y <- subset(one_reduced_cat,path=="rsq_y" & tbl=="encycl" & mem=="lexical_CR")
one_reduced_cat_rsq_m1 <- subset(one_reduced_cat,path=="rsq_m1" & tbl=="encycl" & mem=="lexical_CR")
one_reduced_cat_rsq_m2 <- subset(one_reduced_cat, path == "rsq_m2" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m3 <- subset(one_reduced_cat, path == "rsq_m3" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m4 <- subset(one_reduced_cat, path == "rsq_m4" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m5 <- subset(one_reduced_cat, path == "rsq_m5" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m6 <- subset(one_reduced_cat, path == "rsq_m6" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m7 <- subset(one_reduced_cat, path == "rsq_m7" & tbl == "encycl" & mem == "lexical_CR")
one_reduced_cat_rsq_m8 <- subset(one_reduced_cat, path == "rsq_m8" & tbl == "encycl" & mem == "lexical_CR")


# now sort by ROI so they're in the same order
all_cat_totalInd_sorted <- all_cat_totalInd[order(all_cat_totalInd$ROI), ]
all_cat_c_sorted <- all_cat_c[order(all_cat_c$ROI), ]
all_cat_rsq_y_sorted <- all_cat_rsq_y[order(all_cat_rsq_y$ROI), ]
all_cat_rsq_m1_sorted <- all_cat_rsq_m1[order(all_cat_rsq_m1$ROI), ]
all_cat_rsq_m2_sorted <- all_cat_rsq_m2[order(all_cat_rsq_m2$ROI), ]
all_cat_rsq_m3_sorted <- all_cat_rsq_m3[order(all_cat_rsq_m3$ROI), ]
all_cat_rsq_m4_sorted <- all_cat_rsq_m4[order(all_cat_rsq_m4$ROI), ]
all_cat_rsq_m5_sorted <- all_cat_rsq_m5[order(all_cat_rsq_m5$ROI), ]
all_cat_rsq_m6_sorted <- all_cat_rsq_m6[order(all_cat_rsq_m6$ROI), ]
all_cat_rsq_m7_sorted <- all_cat_rsq_m7[order(all_cat_rsq_m7$ROI), ]
all_cat_rsq_m8_sorted <- all_cat_rsq_m8[order(all_cat_rsq_m8$ROI), ]


one_cat_totalInd_sorted <- one_cat_totalInd[order(one_cat_totalInd$ROI), ]
one_cat_c_sorted <- one_cat_c[order(one_cat_c$ROI), ]
one_cat_rsq_y_sorted <- one_cat_rsq_y[order(one_cat_rsq_y$ROI), ]
one_cat_rsq_m1_sorted <- one_cat_rsq_m1[order(one_cat_rsq_m1$ROI), ]
one_cat_rsq_m2_sorted <- one_cat_rsq_m2[order(one_cat_rsq_m2$ROI), ]
one_cat_rsq_m3_sorted <- one_cat_rsq_m3[order(one_cat_rsq_m3$ROI), ]
one_cat_rsq_m4_sorted <- one_cat_rsq_m4[order(one_cat_rsq_m4$ROI), ]
one_cat_rsq_m5_sorted <- one_cat_rsq_m5[order(one_cat_rsq_m5$ROI), ]
one_cat_rsq_m6_sorted <- one_cat_rsq_m6[order(one_cat_rsq_m6$ROI), ]
one_cat_rsq_m7_sorted <- one_cat_rsq_m7[order(one_cat_rsq_m7$ROI), ]
one_cat_rsq_m8_sorted <- one_cat_rsq_m8[order(one_cat_rsq_m8$ROI), ]


one_reduced_cat_totalInd_sorted <- one_reduced_cat_totalInd[order(one_reduced_cat_totalInd$ROI), ]
one_reduced_cat_c_sorted <- one_reduced_cat_c[order(one_reduced_cat_c$ROI), ]
one_reduced_cat_rsq_y_sorted <- one_reduced_cat_rsq_y[order(one_reduced_cat_rsq_y$ROI), ]
one_reduced_cat_rsq_m1_sorted <- one_reduced_cat_rsq_m1[order(one_reduced_cat_rsq_m1$ROI), ]
one_reduced_cat_rsq_m2_sorted <- one_reduced_cat_rsq_m2[order(one_reduced_cat_rsq_m2$ROI), ]
one_reduced_cat_rsq_m3_sorted <- one_reduced_cat_rsq_m3[order(one_reduced_cat_rsq_m3$ROI), ]
one_reduced_cat_rsq_m4_sorted <- one_reduced_cat_rsq_m4[order(one_reduced_cat_rsq_m4$ROI), ]
one_reduced_cat_rsq_m5_sorted <- one_reduced_cat_rsq_m5[order(one_reduced_cat_rsq_m5$ROI), ]
one_reduced_cat_rsq_m6_sorted <- one_reduced_cat_rsq_m6[order(one_reduced_cat_rsq_m6$ROI), ]
one_reduced_cat_rsq_m7_sorted <- one_reduced_cat_rsq_m7[order(one_reduced_cat_rsq_m7$ROI), ]
one_reduced_cat_rsq_m8_sorted <- one_reduced_cat_rsq_m8[order(one_reduced_cat_rsq_m8$ROI), ]


# paired t-test is if you're comparing the absence of one item to one other case, such as all the items included
#t.test(df$category1, df$category2, paired = TRUE)

### get my data cols
#these are from the regular mediation
curr_col_totalInd_all <- all_cat_totalInd_sorted[,6] 
curr_col_c_all <- all_cat_c_sorted[,6]
curr_col_rsq_y_all <- all_cat_rsq_y_sorted[,6]
curr_col_rsq_m1_all <- all_cat_rsq_m1_sorted[,6]
curr_col_rsq_m2_all <- all_cat_rsq_m2_sorted[,6]
curr_col_rsq_m3_all <- all_cat_rsq_m3_sorted[,6]
curr_col_rsq_m4_all <- all_cat_rsq_m4_sorted[,6]
curr_col_rsq_m5_all <- all_cat_rsq_m5_sorted[,6]
curr_col_rsq_m6_all <- all_cat_rsq_m6_sorted[,6]
curr_col_rsq_m7_all <- all_cat_rsq_m7_sorted[,6]
curr_col_rsq_m8_all <- all_cat_rsq_m8_sorted[,6]

#these are from the 12 cat case
totalInd_12cat <- one_cat_totalInd_sorted[,6:17]
c_12cat <- one_cat_c_sorted[,6:17]
rsq_y_12cat <- one_cat_rsq_y_sorted[,6:17]
rsq_m1_12cat <- one_cat_rsq_m1_sorted[,6:17]
rsq_m2_12cat <- one_cat_rsq_m2_sorted[,6:17]
rsq_m3_12cat <- one_cat_rsq_m3_sorted[,6:17]
rsq_m4_12cat <- one_cat_rsq_m4_sorted[,6:17]
rsq_m5_12cat <- one_cat_rsq_m5_sorted[,6:17]
rsq_m6_12cat <- one_cat_rsq_m6_sorted[,6:17]
rsq_m7_12cat <- one_cat_rsq_m7_sorted[,6:17]
rsq_m8_12cat <- one_cat_rsq_m8_sorted[,6:17]

#these are from the 5 cat case
totalInd_5cat <- one_reduced_cat_totalInd_sorted[,c(7,8,10)] 
c_5cat <- one_reduced_cat_c_sorted[,c(7,8,10)] 
rsq_y_5cat <- one_reduced_cat_rsq_y_sorted[,c(7,8,10)] 
rsq_m1_5cat <- one_reduced_cat_rsq_m1_sorted[,c(7,8,10)] 
rsq_m2_5cat <- one_reduced_cat_rsq_m2_sorted[,c(7,8,10)]
rsq_m3_5cat <- one_reduced_cat_rsq_m3_sorted[,c(7,8,10)]
rsq_m4_5cat <- one_reduced_cat_rsq_m4_sorted[,c(7,8,10)]
rsq_m5_5cat <- one_reduced_cat_rsq_m5_sorted[,c(7,8,10)]
rsq_m6_5cat <- one_reduced_cat_rsq_m6_sorted[,c(7,8,10)]
rsq_m7_5cat <- one_reduced_cat_rsq_m7_sorted[,c(7,8,10)]
rsq_m8_5cat <- one_reduced_cat_rsq_m8_sorted[,c(7,8,10)]

### set up storage df
# category_names <- colnames(totalInd_12cat)
# t_test_results <- data.frame(
#   Category = rep(category_names, each = 4),
#   ResultType = rep(c("totalInd", "c", "rsq_y", "rsq_m1"), times = 12),
#   P_Value = numeric(48),
#   FDR_P_Value = numeric(48),  # Added column for FDR corrected p-values
#   stringsAsFactors = FALSE
# )

### set up t-tests for both 12cat and 5cat with the all category results
category_names <- colnames(totalInd_12cat)
result_types <- c("totalInd", "c", "rsq_y", "rsq_m1", "rsq_m2", "rsq_m3", "rsq_m4", "rsq_m5", "rsq_m6", "rsq_m7", "rsq_m8")

t_test_results <- data.frame(
  Category = rep(category_names, each = length(result_types)),
  ResultType = rep(result_types, times = length(category_names)),
  P_Value = numeric(length(category_names) * length(result_types)),
  FDR_P_Value = numeric(length(category_names) * length(result_types)),
  stringsAsFactors = FALSE
)

### run t-tests
for (i in 1:length(category_names)) {
  # T-test for totalInd
  test_result_totalInd <- t.test(totalInd_12cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * 4 + 1] <- test_result_totalInd$p.value
  # T-test for c
  test_result_c <- t.test(c_12cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * 4 + 2] <- test_result_c$p.value
  # T-test for rsq_y
  test_result_rsq_y <- t.test(rsq_y_12cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * 4 + 3] <- test_result_rsq_y$p.value
  # T-test for rsq_m1
  test_result_rsq_m1 <- t.test(rsq_m1_12cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * 4 + 4] <- test_result_rsq_m1$p.value
  # T-test for rsq_m2
  test_result_rsq_m2 <- t.test(rsq_m2_12cat[, i], curr_col_rsq_m2_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 5] <- test_result_rsq_m2$p.value
  # T-test for rsq_m3
  test_result_rsq_m3 <- t.test(rsq_m3_12cat[, i], curr_col_rsq_m3_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 6] <- test_result_rsq_m3$p.value
  # T-test for rsq_m4
  test_result_rsq_m4 <- t.test(rsq_m4_12cat[, i], curr_col_rsq_m4_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 7] <- test_result_rsq_m4$p.value
  # T-test for rsq_m5
  test_result_rsq_m5 <- t.test(rsq_m5_12cat[, i], curr_col_rsq_m5_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 8] <- test_result_rsq_m5$p.value
  # T-test for rsq_m6
  test_result_rsq_m6 <- t.test(rsq_m6_12cat[, i], curr_col_rsq_m6_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 9] <- test_result_rsq_m6$p.value
  # T-test for rsq_m7
  test_result_rsq_m7 <- t.test(rsq_m7_12cat[, i], curr_col_rsq_m7_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 10] <- test_result_rsq_m7$p.value
  # T-test for rsq_m8
  test_result_rsq_m8 <- t.test(rsq_m8_12cat[, i], curr_col_rsq_m8_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 11] <- test_result_rsq_m8$p.value
}

t_test_results$FDR_P_Value <- p.adjust(t_test_results$P_Value, method = "fdr")

# filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ttest_anova_compare_cat_to_allMed.xlsx'
# # Create a new workbook or load an existing one
# if (file.exists(filename)) {
#   wb <- loadWorkbook(filename)
# } else {
#   wb <- createWorkbook()
# }
# sheet_name <- "12cat_results"  # Name this according to your data
# addWorksheet(wb, sheet_name)
# writeData(wb, sheet_name, t_test_results)
# # Save the workbook
# saveWorkbook(wb, filename, overwrite = TRUE)

filename <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ttest_anova_compare_cat_to_allMed.xlsx"
sheet_name <- "12cat_results"  

# Load the workbook if it exists, otherwise create a new one
if (file.exists(filename)) {
  wb <- loadWorkbook(filename)
} else {
  wb <- createWorkbook()
}
# Manually check if the sheet exists in the workbook
sheet_names <- names(wb$worksheets)
if (sheet_name %in% sheet_names) {
  # Remove the existing sheet
  removeWorksheet(wb, sheet_name)
}
# Add the sheet (new or replaced)
addWorksheet(wb, sheet_name)
# Write data to the worksheet
writeData(wb, sheet_name, t_test_results)
# Save the workbook
saveWorkbook(wb, filename, overwrite = TRUE)



##### next run t-tests for 5cat

result_types <- c("totalInd", "c", "rsq_y", "rsq_m1", "rsq_m2", "rsq_m3", "rsq_m4", "rsq_m5", "rsq_m6", "rsq_m7", "rsq_m8")
category_names <- colnames(totalInd_5cat)

t_test_results <- data.frame(
  Category = rep(category_names, each = length(result_types)),
  ResultType = rep(result_types, times = length(category_names)),
  P_Value = numeric(length(category_names) * length(result_types)),
  FDR_P_Value = numeric(length(category_names) * length(result_types)),
  stringsAsFactors = FALSE
)

### set up storage df
# category_names <- colnames(totalInd_5cat)
# t_test_results <- data.frame(
#   Category = rep(category_names, each = 4),
#   ResultType = rep(c("totalInd", "c", "rsq_y", "rsq_m1"), times = 3),
#   P_Value = numeric(12),
#   FDR_P_Value = numeric(12),  # Added column for FDR corrected p-values
#   stringsAsFactors = FALSE
# )

# ### run t-tests
# for (i in 1:length(category_names)) {
#   # T-test for totalInd
#   test_result_totalInd <- t.test(totalInd_5cat[, i], curr_col_totalInd_all[, 1])
#   t_test_results$P_Value[(i - 1) * 4 + 1] <- test_result_totalInd$p.value
#   
#   # T-test for c
#   test_result_c <- t.test(c_5cat[, i], curr_col_totalInd_all[, 1])
#   t_test_results$P_Value[(i - 1) * 4 + 2] <- test_result_c$p.value
#   
#   # T-test for rsq_y
#   test_result_rsq_y <- t.test(rsq_y_5cat[, i], curr_col_totalInd_all[, 1])
#   t_test_results$P_Value[(i - 1) * 4 + 3] <- test_result_rsq_y$p.value
#   
#   # T-test for rsq_m1
#   test_result_rsq_m1 <- t.test(rsq_m1_5cat[, i], curr_col_totalInd_all[, 1])
#   t_test_results$P_Value[(i - 1) * 4 + 4] <- test_result_rsq_m1$p.value
# }

# Assuming you have corresponding comparison columns for each result type
comparison_columns <- list(
  totalInd = curr_col_totalInd_all,
  c = curr_col_c_all,
  rsq_y = curr_col_rsq_y_all,
  rsq_m1 = curr_col_rsq_m1_all,
  rsq_m2 = curr_col_rsq_m2_all,
  rsq_m3 = curr_col_rsq_m3_all,
  rsq_m4 = curr_col_rsq_m4_all,
  rsq_m5 = curr_col_rsq_m5_all,
  rsq_m6 = curr_col_rsq_m6_all,
  rsq_m7 = curr_col_rsq_m7_all,
  rsq_m8 = curr_col_rsq_m8_all
)

for (i in 1:length(category_names)) {
  # T-test for totalInd
  test_result_totalInd <- t.test(totalInd_5cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 1] <- test_result_totalInd$p.value
  # T-test for c
  test_result_c <- t.test(c_5cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 2] <- test_result_c$p.value
  # T-test for rsq_y
  test_result_rsq_y <- t.test(rsq_y_5cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 3] <- test_result_rsq_y$p.value
  # T-test for rsq_m1
  test_result_rsq_m1 <- t.test(rsq_m1_5cat[, i], curr_col_totalInd_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 4] <- test_result_rsq_m1$p.value
  # T-test for rsq_m2
  test_result_rsq_m2 <- t.test(rsq_m2_5cat[, i], curr_col_rsq_m2_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 5] <- test_result_rsq_m2$p.value
  # T-test for rsq_m3
  test_result_rsq_m3 <- t.test(rsq_m3_5cat[, i], curr_col_rsq_m3_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 6] <- test_result_rsq_m3$p.value
  # T-test for rsq_m4
  test_result_rsq_m4 <- t.test(rsq_m4_5cat[, i], curr_col_rsq_m4_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 7] <- test_result_rsq_m4$p.value
  # T-test for rsq_m5
  test_result_rsq_m5 <- t.test(rsq_m5_5cat[, i], curr_col_rsq_m5_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 8] <- test_result_rsq_m5$p.value
  # T-test for rsq_m6
  test_result_rsq_m6 <- t.test(rsq_m6_5cat[, i], curr_col_rsq_m6_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 9] <- test_result_rsq_m6$p.value
  # T-test for rsq_m7
  test_result_rsq_m7 <- t.test(rsq_m7_5cat[, i], curr_col_rsq_m7_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 10] <- test_result_rsq_m7$p.value
  # T-test for rsq_m8
  test_result_rsq_m8 <- t.test(rsq_m8_5cat[, i], curr_col_rsq_m8_all[, 1])
  t_test_results$P_Value[(i - 1) * length(result_types) + 11] <- test_result_rsq_m8$p.value
}

t_test_results$FDR_P_Value <- p.adjust(t_test_results$P_Value, method = "fdr")

filename <- "/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ttest_anova_compare_cat_to_allMed.xlsx"
sheet_name <- "5cat_results"

# Load the workbook if it exists, otherwise create a new one
if (file.exists(filename)) {
  wb <- loadWorkbook(filename)
} else {
  wb <- createWorkbook()
}
# Check if the sheet exists in the workbook
if (sheet_name %in% names(wb$worksheets)) {
  # Remove the existing sheet
  removeWorksheet(wb, sheet_name)
}
# Add the sheet (new or replaced)
addWorksheet(wb, sheet_name)
# Write data to the worksheet
writeData(wb, sheet_name, t_test_results)
# Save the workbook
saveWorkbook(wb, filename, overwrite = TRUE)


# 
# # Create a new workbook or load an existing one
# if (file.exists(filename)) {
#   wb <- loadWorkbook(filename)
# } else {
#   wb <- createWorkbook()
# }
# sheet_name <- "5cat_results"  # Name this according to your data
# addWorksheet(wb, sheet_name)
# writeData(wb, sheet_name, t_test_results)
# # Save the workbook
# saveWorkbook(wb, filename, overwrite = TRUE)


### error with extra characters in a string. Here's how to remove them
# there's weird stuff in "fruit" so let's remove it
clean_colnames <- function(name) {
  name <- gsub("\r", "", name)  # Remove carriage return
  name <- gsub("\n", "", name)  # Remove newline
  name <- gsub("^[[:space:]]+|[[:space:]]+$", "", name)  # Remove leading/trailing spaces
  name
}

colnames(totalInd_12cat) <- sapply(colnames(totalInd_12cat), clean_colnames)

curr_col_totalInd_all <- as.vector(unlist((curr_col_totalInd_all)))
curr_col_c_all <- as.vector(unlist(curr_col_c_all))
curr_col_rsq_y_all <- as.vector(unlist(curr_col_rsq_y_all))
curr_col_rsq_m1_all <- as.vector(unlist(curr_col_rsq_m1_all))


### (5b) ANOVA
####### 12 CAT
# Adding curr_col_totalInd_all as a new column to totalInd_12cat
totalInd_12cat$curr_col_totalInd_all <- curr_col_totalInd_all

#library(tidyr)

# List of columns to convert, excluding 'curr_col_totalInd_all'
columns_to_convert <- setdiff(names(totalInd_12cat), "curr_col_totalInd_all")
# Converting columns to numeric
totalInd_12cat[columns_to_convert] <- lapply(totalInd_12cat[columns_to_convert], function(x) as.numeric(as.character(x)))
# Check if conversion was successful
str(totalInd_12cat)

# For c_12cat
columns_to_convert_c <- setdiff(names(c_12cat), "curr_col_c_all")
c_12cat[columns_to_convert_c] <- lapply(c_12cat[columns_to_convert_c], function(x) as.numeric(as.character(x)))

# For rsq_y_12cat
columns_to_convert_rsq_y <- setdiff(names(rsq_y_12cat), "curr_col_rsq_y_all")
rsq_y_12cat[columns_to_convert_rsq_y] <- lapply(rsq_y_12cat[columns_to_convert_rsq_y], function(x) as.numeric(as.character(x)))

# For rsq_m1_12cat
columns_to_convert_rsq_m1 <- setdiff(names(rsq_m1_12cat), "curr_col_rsq_m1_all")
rsq_m1_12cat[columns_to_convert_rsq_m1] <- lapply(rsq_m1_12cat[columns_to_convert_rsq_m1], function(x) as.numeric(as.character(x)))



# Reshaping data to long format
long_data <- pivot_longer(totalInd_12cat, 
                          cols = -curr_col_totalInd_all, 
                          names_to = "Category", 
                          values_to = "Value")

totalInd_anova <- aov(Value ~ Category + curr_col_totalInd_all, data = long_data)
summary(totalInd_anova)

c_12cat$curr_col_c_all <- curr_col_c_all
long_c_data <- pivot_longer(c_12cat, 
                            cols = -curr_col_c_all, 
                            names_to = "Category", 
                            values_to = "Value")
c_anova <- aov(Value ~ Category + curr_col_c_all, data = long_c_data)
summary(c_anova)

rsq_y_12cat$curr_col_rsq_y_all <- curr_col_rsq_y_all
long_rsq_y_data <- pivot_longer(rsq_y_12cat, 
                                cols = -curr_col_rsq_y_all, 
                                names_to = "Category", 
                                values_to = "Value")
rsq_y_anova <- aov(Value ~ Category + curr_col_rsq_y_all, data = long_rsq_y_data)
summary(rsq_y_anova)

rsq_m1_12cat$curr_col_rsq_m1_all <- curr_col_rsq_m1_all
long_rsq_m1_data <- pivot_longer(rsq_m1_12cat, 
                                 cols = -curr_col_rsq_m1_all, 
                                 names_to = "Category", 
                                 values_to = "Value")
rsq_m1_anova <- aov(Value ~ Category + curr_col_rsq_m1_all, data = long_rsq_m1_data)
summary(rsq_m1_anova)

totalInd_anova_df <- broom::tidy(totalInd_anova)
c_anova_df <- broom::tidy(c_anova)
rsq_y_anova_df <- broom::tidy(rsq_y_anova)
rsq_m1_anova_df <- broom::tidy(rsq_m1_anova)

totalInd_anova_df$ResultType <- "totalInd"
c_anova_df$ResultType <- "c"
rsq_y_anova_df$ResultType <- "rsq_y"
rsq_m1_anova_df$ResultType <- "rsq_m1"

combined_anova_df <- rbind(totalInd_anova_df, c_anova_df, rsq_y_anova_df, rsq_m1_anova_df)

combined_anova_df$FDR_P_Value <- NA  # Initialize the column with NA
combined_anova_df$FDR_P_Value <- p.adjust(combined_anova_df$p.value, method = "fdr")

filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ttest_anova_compare_cat_to_allMed.xlsx'
# Create a new workbook or load an existing one
if (file.exists(filename)) {
  wb <- loadWorkbook(filename)
} else {
  wb <- createWorkbook()
}
sheet_name <- "12cat_anova"  # Name this according to your data
addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, combined_anova_df)
# Save the workbook
saveWorkbook(wb, filename, overwrite = TRUE)


####### 5 CAT
# re-define these
#these are from the regular mediation
curr_col_totalInd_all <- all_cat_totalInd_sorted[,6] 
curr_col_c_all <- all_cat_c_sorted[,6]
curr_col_rsq_y_all <- all_cat_rsq_y_sorted[,6]
curr_col_rsq_m1_all <- all_cat_rsq_m1_sorted[,6]
# fix data type
curr_col_totalInd_all <- as.vector(unlist((curr_col_totalInd_all)))
curr_col_c_all <- as.vector(unlist(curr_col_c_all))
curr_col_rsq_y_all <- as.vector(unlist(curr_col_rsq_y_all))
curr_col_rsq_m1_all <- as.vector(unlist(curr_col_rsq_m1_all))

# List of columns to convert, excluding 'curr_col_totalInd_all'
columns_to_convert <- setdiff(names(totalInd_5cat), "curr_col_totalInd_all")
# Converting columns to numeric
totalInd_5cat[columns_to_convert] <- lapply(totalInd_5cat[columns_to_convert], function(x) as.numeric(as.character(x)))
# Check if conversion was successful
str(totalInd_5cat)

# For c_5cat
columns_to_convert_c <- setdiff(names(c_5cat), "curr_col_c_all")
c_5cat[columns_to_convert_c] <- lapply(c_5cat[columns_to_convert_c], function(x) as.numeric(as.character(x)))
str(c_5cat)

# For rsq_y_5cat
columns_to_convert_rsq_y <- setdiff(names(rsq_y_5cat), "curr_col_rsq_y_all")
rsq_y_5cat[columns_to_convert_rsq_y] <- lapply(rsq_y_5cat[columns_to_convert_rsq_y], function(x) as.numeric(as.character(x)))
str(rsq_y_5cat)

# For rsq_m1_5cat
columns_to_convert_rsq_m1 <- setdiff(names(rsq_m1_5cat), "curr_col_rsq_m1_all")
rsq_m1_5cat[columns_to_convert_rsq_m1] <- lapply(rsq_m1_5cat[columns_to_convert_rsq_m1], function(x) as.numeric(as.character(x)))
str(rsq_m1_5cat)

# Ensure the length of curr_col_totalInd_all matches the number of rows in totalInd_5cat
if(length(curr_col_totalInd_all) == nrow(totalInd_5cat)) {
  totalInd_5cat$curr_col_totalInd_all <- curr_col_totalInd_all
} else {
  stop("Length of curr_col_totalInd_all does not match the number of rows in totalInd_5cat")
}

totalInd_5cat$curr_col_totalInd_all <- curr_col_totalInd_all
c_5cat$curr_col_c_all <- curr_col_c_all
rsq_y_5cat$curr_col_rsq_y_all <- curr_col_rsq_y_all
rsq_m1_5cat$curr_col_rsq_m1_all <- curr_col_rsq_m1_all


# Reshaping data to long format
long_data <- pivot_longer(totalInd_5cat, 
                          cols = -curr_col_totalInd_all, 
                          names_to = "Category", 
                          values_to = "Value")

totalInd_anova <- aov(Value ~ Category + curr_col_totalInd_all, data = long_data)
summary(totalInd_anova)

c_5cat$curr_col_c_all <- curr_col_c_all
long_c_data <- pivot_longer(c_5cat, 
                            cols = -curr_col_c_all, 
                            names_to = "Category", 
                            values_to = "Value")
c_anova <- aov(Value ~ Category + curr_col_c_all, data = long_c_data)
summary(c_anova)

rsq_y_5cat$curr_col_rsq_y_all <- curr_col_rsq_y_all
long_rsq_y_data <- pivot_longer(rsq_y_5cat, 
                                cols = -curr_col_rsq_y_all, 
                                names_to = "Category", 
                                values_to = "Value")
rsq_y_anova <- aov(Value ~ Category + curr_col_rsq_y_all, data = long_rsq_y_data)
summary(rsq_y_anova)

rsq_m1_5cat$curr_col_rsq_m1_all <- curr_col_rsq_m1_all
long_rsq_m1_data <- pivot_longer(rsq_m1_5cat, 
                                 cols = -curr_col_rsq_m1_all, 
                                 names_to = "Category", 
                                 values_to = "Value")
rsq_m1_anova <- aov(Value ~ Category + curr_col_rsq_m1_all, data = long_rsq_m1_data)
summary(rsq_m1_anova)

totalInd_anova_df <- broom::tidy(totalInd_anova)
c_anova_df <- broom::tidy(c_anova)
rsq_y_anova_df <- broom::tidy(rsq_y_anova)
rsq_m1_anova_df <- broom::tidy(rsq_m1_anova)

totalInd_anova_df$ResultType <- "totalInd"
c_anova_df$ResultType <- "c"
rsq_y_anova_df$ResultType <- "rsq_y"
rsq_m1_anova_df$ResultType <- "rsq_m1"

combined_anova_df <- rbind(totalInd_anova_df, c_anova_df, rsq_y_anova_df, rsq_m1_anova_df)

combined_anova_df$FDR_P_Value <- NA  # Initialize the column with NA
combined_anova_df$FDR_P_Value <- p.adjust(combined_anova_df$p.value, method = "fdr")

filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/ttest_anova_compare_cat_to_allMed.xlsx'
# Create a new workbook or load an existing one
if (file.exists(filename)) {
  wb <- loadWorkbook(filename)
} else {
  wb <- createWorkbook()
}
sheet_name <- "5cat_anova"  # Name this according to your data
addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, combined_anova_df)
# Save the workbook
saveWorkbook(wb, filename, overwrite = TRUE)




# effect size is for paired t-tests
cohen.d(df$category1, df$category2, paired = TRUE)

# eta squared for ANOVA
# Extracting the Sum of Squares
ss_total <- sum(res.aov[["model"]][["Sum Sq"]])
ss_effect <- res.aov[["model"]][["Sum Sq"]][1]  # Assuming 'category' is the first factor

# Calculating Eta Squared
eta_squared <- ss_effect / ss_total
eta_squared





# Create a subset of the data without the current item's category
modified_data <- subset(Data_mixed_clean, reduced_category != category_to_remove)


# want to check variance explained too
#results_summary <- summary(modified_fit, fit.measures = TRUE, rsq = TRUE)
#Or can do a separate call: fit_measures <- lavInspect(modified_fit, "fit.measures")


# ###### the many-categories col
# unique_categories <- unique(Data_mixed_clean$category)
# impact_assessment_categories <- vector("list", length(unique_categories))
# errors_categories <- vector("list", length(unique_categories))
# 
# for (i in seq_along(unique_categories)) {
#   category_to_remove <- unique_categories[i]
#   
#   # Create a subset of the data without the current item's category
#   modified_data <- subset(Data_mixed_clean, category != category_to_remove)
#   
#   # Re-fit the model
#   modified_fit <- try(sem(model221, 
#                           data = modified_data,
#                           cluster = "ItemID",
#                           se = "robust.huber.white"),
#                       silent=TRUE)
#   
#   if (class(modified_fit) == "try-error") {
#     impact_assessment_categories[[i]] <- NULL
#     errors_categories[[i]] <- paste("Error with category:", category_to_remove)
#   } else {
#     impact_assessment_categories[[i]] <- summary(modified_fit)
#     errors_categories[[i]] <- NA  # No error
#   }
#   # Store the summary of the model
#   impact_assessment_categories[[i]] <- summary(modified_fit)
# }
# 
# ###### CATEGORIES
# impact_assessment <- impact_assessment_categories
# # Extracting estimates for the a-paths
# a_paths <- paste("a", 1:8, sep="")
# a_estimates <- matrix(NA, nrow = length(a_paths), ncol = length(impact_assessment))
# b_paths <- paste("a", 1:8, sep="")
# b_estimates <- matrix(NA, nrow = length(b_paths), ncol = length(impact_assessment))
# indirect_paths <- paste("ab",1:8, sep="")
# indirect_estimates <- matrix(NA, nrow = length(indirect_paths), ncol = length(impact_assessment))
# # get c, totalIndirect, and totalEffect too
# c_totalInd_totalEff <- matrix(NA, nrow = 3, ncol = length(impact_assessment))
# 
# for (i in seq_along(impact_assessment)) {
#   if ("lavaan.summary" %in% class(impact_assessment[[i]])) {
#     for (j in seq_along(a_paths)) {
#       a_path <- a_paths[j]
#       curr_est <- impact_assessment[[i]]
#       ests <- curr_est$pe
#       a_estimate <- ests[ests$op == "~" & ests$label == a_path, "est"]
#       if (length(a_estimate) > 0) {
#         a_estimates[j, i] <- a_estimate
#       } else {
#         a_estimates[j, i] <- NA
#       }
#       b_path <- b_paths[j]
#       b_estimate <- ests[ests$op == "~" & ests$label == b_path, "est"]
#       if (length(b_estimate) > 0) {
#         b_estimates[j, i] <- b_estimate
#       } else {
#         b_estimates[j, i] <- NA
#       }
#       
#       indirect <- indirect_paths[j]
#       indirect_estimate <- ests[ests$label == indirect, "est"]
#       if (length(indirect_estimate) > 0) {
#         indirect_estimates[j, i] <- indirect_estimate
#       } else {
#         indirect_estimates[j, i] <- NA
#       }
#     } #end of j loop
#     c_est <- ests[ests$op == "~" & ests$label == "c", "est"]
#     totalInd_est <- ests[ests$label == "totalIndirect", "est"]
#     totalEff_est <- ests[ests$label == "totalEffect", "est"]
#     
#     if (length(c_est) > 0) {
#       c_totalInd_totalEff[1, i] <- c_est
#       c_totalInd_totalEff[2, i] <- totalInd_est
#       c_totalInd_totalEff[3, i] <- totalEff_est
#     } else {
#       c_totalInd_totalEff[1, i] <- NA
#       c_totalInd_totalEff[2, i] <- NA
#       c_totalInd_totalEff[3, i] <- NA
#     }
#     
#   }
# }
# 
# # Converting to a data frame for easier handling
# # Creating labels
# a_labels <- paste("a", 1:8, sep="")
# b_labels <- paste("b", 1:8, sep="")
# ab_labels <- paste("ab", 1:8, sep="")
# # Assuming only one each for these paths
# other_labels <- c("c", "totalIndirect", "totalEffect")
# # Combine all labels
# all_labels <- c(a_labels, b_labels, ab_labels, other_labels)
# all_df_categories <- rbind(a_estimates,b_estimates,indirect_estimates,c_totalInd_totalEff)
# all_df_categories <- as.data.frame(all_df_categories)
# all_df_categories$path <- all_labels
# 
# # add outer loop info
# all_df_categories$tbl <- tbl
# all_df_categories$mem <- mem
# all_df_categories$ROI <- ROI
# all_df_categories$hemisphere <- hemisphere
# 
# # rename data columns to category names
# names(all_df_categories)[1:5] <- c("animal","home_tool", "outside","food","home")


# unique_items <- unique(Data_mixed_clean$ItemID)
# impact_assessment_items <- vector("list", length(unique_items))
# errors_items <- vector("list", length(unique_items))
# 
# for (i in seq_along(unique_items)) {
#   item_to_remove <- unique_items[i]
#   
#   # Create a subset of the data without the current item's category
#   modified_data <- subset(Data_mixed_clean, ItemID != item_to_remove)
#   
#   # Re-fit the model
#   modified_fit <- try(sem(model221, 
#                           data = modified_data,
#                           cluster = "ItemID",
#                           se = "robust.huber.white"),
#                       silent=TRUE)
#   
#   if (class(modified_fit) == "try-error") {
#     impact_assessment_items[[i]] <- NULL
#     errors_items[[i]] <- paste("Error with category:", item_to_remove)
#   } else {
#     impact_assessment_items[[i]] <- summary(modified_fit)
#     errors_items[[i]] <- NA  # No error
#   }
#   # Store the summary of the model
#   impact_assessment_items[[i]] <- summary(modified_fit)
# }

###### INDIVIDUAL ITEMS
# impact_assessment <- impact_assessment_items
# # Extracting estimates for the a-paths
# a_paths <- paste("a", 1:8, sep="")
# a_estimates <- matrix(NA, nrow = length(a_paths), ncol = length(impact_assessment))
# b_paths <- paste("a", 1:8, sep="")
# b_estimates <- matrix(NA, nrow = length(b_paths), ncol = length(impact_assessment))
# indirect_paths <- paste("ab",1:8, sep="")
# indirect_estimates <- matrix(NA, nrow = length(indirect_paths), ncol = length(impact_assessment))
# # get c, totalIndirect, and totalEffect too
# c_totalInd_totalEff <- matrix(NA, nrow = 3, ncol = length(impact_assessment))
# 
# for (i in seq_along(impact_assessment)) {
#   if ("lavaan.summary" %in% class(impact_assessment[[i]])) {
#     for (j in seq_along(a_paths)) {
#       a_path <- a_paths[j]
#       curr_est <- impact_assessment[[i]]
#       ests <- curr_est$pe
#       a_estimate <- ests[ests$op == "~" & ests$label == a_path, "est"]
#       if (length(a_estimate) > 0) {
#         a_estimates[j, i] <- a_estimate
#       } else {
#         a_estimates[j, i] <- NA
#       }
#       b_path <- b_paths[j]
#       b_estimate <- ests[ests$op == "~" & ests$label == b_path, "est"]
#       if (length(b_estimate) > 0) {
#         b_estimates[j, i] <- b_estimate
#       } else {
#         b_estimates[j, i] <- NA
#       }
#       
#       indirect <- indirect_paths[j]
#       indirect_estimate <- ests[ests$label == indirect, "est"]
#       if (length(indirect_estimate) > 0) {
#         indirect_estimates[j, i] <- indirect_estimate
#       } else {
#         indirect_estimates[j, i] <- NA
#       }
#     } #end of j loop
#     c_est <- ests[ests$op == "~" & ests$label == "c", "est"]
#     totalInd_est <- ests[ests$label == "totalIndirect", "est"]
#     totalEff_est <- ests[ests$label == "totalEffect", "est"]
#     
#     if (length(c_est) > 0) {
#       c_totalInd_totalEff[1, i] <- c_est
#       c_totalInd_totalEff[2, i] <- totalInd_est
#       c_totalInd_totalEff[3, i] <- totalEff_est
#     } else {
#       c_totalInd_totalEff[1, i] <- NA
#       c_totalInd_totalEff[2, i] <- NA
#       c_totalInd_totalEff[3, i] <- NA
#     }
#     
#   }
# }
# 
# # Converting to a data frame for easier handling
# # Creating labels
# a_labels <- paste("a", 1:8, sep="")
# b_labels <- paste("b", 1:8, sep="")
# ab_labels <- paste("ab", 1:8, sep="")
# # Assuming only one each for these paths
# other_labels <- c("c", "totalIndirect", "totalEffect")
# # Combine all labels
# all_labels <- c(a_labels, b_labels, ab_labels, other_labels)
# all_df_items <- rbind(a_estimates,b_estimates,indirect_estimates,c_totalInd_totalEff)
# all_df_items <- as.data.frame(all_df_items)
# all_df_items$Path <- all_labels






### (2) cook's distance
model <- lm(Y ~ X * M, data = your_data)  # Your actual model
cooks_dist <- cooks.distance(model)

# Plotting Cook's Distance
plot(cooks_dist, type = "h", main = "Cook's Distance")
abline(h = 4 / length(cooks_dist), col = "red")  # Threshold line



### (3) Bootstrapping
library(mediation)

med_model <- lm(M ~ X, data = your_data)  # Mediator model
out_model <- lm(Y ~ X + M, data = your_data)  # Outcome model

med_effect <- mediate(med_model, out_model, treat = "X", mediator = "M", 
                      boot = TRUE, sims = 1000)  # Increase sims for more accuracy

summary(med_effect)







