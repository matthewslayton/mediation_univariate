### work in progress. want single-mediator and multiple-mediator mediation analysis with mixed effects models

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
#mediation_output_df <- data.frame(ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {

#tbl_names <- c("encycl","vis","fcn")
tbl_names <- c("encycl","vis")
memType <- c("lexical_CR","visual_CR")

indirectEffects_output_df <- data.frame(tbl=character(),mem=character(),ROI=character(),hemisphere=character(),sigPred=character(),b_type=character(),a_path=numeric(),b_path=numeric(),c_path=numeric(),indirectEffect=numeric(),LL=numeric(), UL=numeric(), significant=logical())

#tbl <- "encycl"
#mem <- "lexical_CR"
#ROI_name <- 'mask_AG_L'

for (tbl in tbl_names) {
  for (mem in memType) {
    
    #mediation_output_df <- data.frame(ROI=character(),hemisphere=character(),resultType=character(),estimate=numeric(),std_err=numeric(),zval=numeric(),pval=numeric()) #could replace character() with factor()for (ROI_name in ROI_name_variable) {
    
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
      
      X = curr_fac_tbl$lexical_CR
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
      
      # (2) clean those NaN rows
      Data_mixed <- data.frame(X=X, Y=Y, M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,Subj=Subj)
      Data_mixed_clean <- na.omit(Data_mixed)
      
      # Fit the model for the effect of IV on the mediator (a path)
      #model_a1 <- lmer(M1 ~ X + (1 | Subj), data = Data_mixed_clean)
      # Extract the coefficient for the IV (a) -- this is the raw regression coeff
      #coef_a1 <- fixef(model_a1)["X"]
      # Extract the variance-covariance matrix for the fixed effects
      #vcov_a1 <- vcov(model_a1)
      # The diagonal element corresponding to the IV gives you var(a) -- this is var_a, asymptotic sampling variance of a
      #var_a1 <- diag(vcov_a1)["X"]
      #var_a1 <- vcov_a1[2, 2]
      
      model_list <- list()
      a_coeff_list <- numeric(8)
      #a_coeff_list <- numeric(4)
      #a_coeff_list <- numeric(1)
      vcov_list <- list()
      a_var_list <- numeric(8)
      #var_list <- numeric(4)
      #var_list <- numeric(1)
      # Loop through mediator variables M1 to M8
      for (i in 1:8) {
      #for (i in 1:4) {
      #for (i in 1:1) {
        
        ### a-path: effects of the independent variable (X) on the mediators (M1 through M8), found one at a time
        
        # Dynamically create the formula to get a-path. **** this shouldn't have an effect of subject
        model_a_formula <- as.formula(paste("M", i, "~ X + (1|Subj)", sep = ""))
        # Fit the model
        model_list[[i]] <- suppressMessages(suppressWarnings(lmer(model_a_formula, data = Data_mixed_clean)))
        # Extract the coefficient for X
        a_coeff_list[i] <- fixef(model_list[[i]])["X"]
        # Extract the variance-covariance matrix
        vcov_list[[i]] <- vcov(model_list[[i]])
        # Extract the variance for the coefficient of X
        a_var_list[i] <- vcov_list[[i]]["X", "X"]
      }
      
      
      ### b-path: effects of the mediators (M1 through M8) on the dependent variable (Y) while controlling for the independent variable (X).
      # even in multiple mediation, you need to find eight coefficients. We model the b-path with all eight mediators
      # and there's one coefficient for each
      
      ##### HM, this code gives me one value for coef_b, not eight
      #model_b <- suppressMessages(suppressWarnings(lmer(Y ~ M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + X + (1|Subj), data = Data_mixed_clean)))
      #model_b <- suppressWarnings(lmer(Y ~ X + M1 + M2 + M3 + M4 + (1|Subj), data = Data_mixed_clean))
      #model_b <- suppressWarnings(lmer(Y ~ X + M1 + (1|Subj), data = Data_mixed_clean))
      #coef_b <- fixef(model_b)["X"]
      #vcov_b <- vcov(model_b)
      #var_b<- vcov_b[2,2]      
      
      ###### find the coeffs one at a time
      #b_coeff_list <- numeric(8)
      b_coeff_list_oneAtATime <- numeric(8)
      b_coeff_list_allAtOnce <- numeric(8)
      b_var_list_oneAtATime <- numeric(8)
      b_var_list_allAtOnce <- numeric(8)
      for (i in 1:length(b_coeff_list_allAtOnce)) {
        model_b_formula <- as.formula(paste("Y ~ M", i, "+ X + (1|Subj)", sep = ""))
        model_b <- lmer(model_b_formula, data = Data_mixed_clean)
        b_coeff_list_oneAtATime[i] <- fixef(model_b)[paste("M", i, sep = "")]
    
        # Extract the variance-covariance matrix for the fixed effects
        vcov_b <- vcov(model_b)
        # Extract the variance for the coefficient of Mi
        # It's important to match the correct name in the vcov matrix, which is typically the name of the mediator
        b_var_list_oneAtATime[i] <- vcov_b[paste("M", i, sep = ""), paste("M", i, sep = "")]
      }

      ##### find the coeffs from one big model
      # Define the model that includes all mediators and X
      model_b <- lmer(Y ~ M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + X + (1|Subj), data = Data_mixed_clean)
      # Now, extract the 'b' path coefficients for each mediator using sapply
      b_coeff_list_allAtOnce <- sapply(paste0("M", 1:8), function(mediator) fixef(model_b)[mediator])
      # Extract the variance-covariance matrix
      vcov_b <- vcov(model_b)
      # Create a numeric vector for the mediator variances
      mediator_indices <- match(paste0("M", 1:8), rownames(vcov_b))
      b_var_list_allAtOnce <- diag(vcov_b)[mediator_indices]


      ## i had this. Is this wrong? 
      # model_b <- suppressMessages(suppressWarnings(lmer(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + (1|Subj), data = Data_mixed_clean)))
      
      ### c-path: direct effect of the independent variable (X) on the dependent variable (Y), controlling for the mediators (M1 through M8)
      model_c <- suppressMessages(suppressWarnings(lmer(Y ~ X + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + (1|Subj), data = Data_mixed_clean)))
      #model_c <- suppressWarnings(lmer(Y ~ X + M1 + M2 + M3 + M4 + (1|Subj), data = Data_mixed_clean))
      #model_c <- suppressWarnings(lmer(Y ~ X + M1 + (1|Subj), data = Data_mixed_clean))
      c_coeff <- fixef(model_c)["X"]
      vcov_c <- vcov(model_c)
      var_c <- vcov_c[2,2]

      require(MASS)
      
      # I believed that coef_b and var_b are already defined, so I can just use b=coef_b
      # now it seems that whether you calculate them one at a time or all at once, 
      # there will be eight individual coefficients, one for each mediator
      
      #a_counter <- 1
      #b_typeCounter <- 1
      
      for (a_counter in 1:length(a_coeff_list)) {
        
        curr_a = a_coeff_list[a_counter]
        curr_var_a = a_var_list[a_counter]
        a=curr_a
        #b=coef_b
        
        #two ways to calculate b. each mediator one at a time or all mediators at once
        for (b_typeCounter in 1:2) { 
          
          # use the a_counter because it goes a1-b1, a2-b2, etc
          curr_b_oneAtATime <- b_coeff_list_oneAtATime[a_counter]
          curr_b_allAtOnce <- b_coeff_list_allAtOnce[a_counter]
          curr_var_b_oneAtATime <- b_var_list_oneAtATime[a_counter]
          curr_var_b_allAtOnce <- b_var_list_allAtOnce[a_counter]
            if (b_typeCounter == 1) {
              b <- curr_b_oneAtATime
              curr_var_b <- curr_var_b_oneAtATime
              curr_b_type <- 'oneAtATime'
            } else if (b_typeCounter == 2) {
              b <- curr_b_allAtOnce
              curr_var_b <- curr_var_b_allAtOnce
              curr_b_type <- 'allAtOnce'
            }
    
            rep=20000 #replications
            conf=95 #confidence interval
            pest=c(a,b) #point estimates for a and b
            acov <- matrix(c(
              curr_var_a, 0,
              0, curr_var_b
            ),2,2) #variance-covariance mat of estimates a and b
            mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE) #matrix of simulated values for a and b based on multivariate normal distribution
            ab <- mcmc[,1]*mcmc[,2] #vector of product of a and b for each replication (so, simulated indirect effects)
            low=(1-conf/100)/2 #lower bound for CI based on conf value
            upp=((1-conf/100)/2)+(conf/100)
            LL=quantile(ab,low)
            UL=quantile(ab,upp)
            LL4=format(LL,digits=4)
            UL4=format(UL,digits=4)
            ################################################
            # The number of columns in the histogram can   #
            # be changed by replacing 'FD' below with      #
            # an integer value.                            #
            ################################################
            # hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
            #      main='Distribution of Indirect Effect')
            # don't really need histogram. Need significance.
            # indirect effect is significant if 0 is not included in the confidence interval?
            
            # This checks if the 95% CI excludes zero, which would indicate a significant indirect effect
            significant <- LL > 0 & UL > 0 | LL < 0 & UL < 0
            # get fac name
            curr_pred <- paste0("F0", sprintf("%01d", a_counter))
            
            indirectEffects_output_df <- rbind(indirectEffects_output_df, data.frame(tbl=tbl,mem=mem,ROI=ROI,hemisphere=hemisphere,sigPred=curr_pred,b_type=curr_b_type,a_path=a,b_path=b,c_path=c_coeff,indirectEffect=a*b,LL=LL,UL=UL, significant=significant))
            
            #colnames(indirectEffects_output_df) <- c('tbl','mem','ROI','hemisphere','sigPred','a-path','b-type','b-path','c-path','indirectEffect','CI_lower','CI_upper','significant')
            
          } # end b_TypeCounter 
        } #end a_counter loop
    } #end ROI_name_variable
  } #end memType
} #end tbl_names

filename <- '/Users/matthewslayton/Library/CloudStorage/OneDrive-DukeUniversity/STAMP/medAnalysis_eightFac_monteCarlo_indirectEffect_unilateral.xlsx'
write_xlsx(indirectEffects_output_df, filename)

# output indirect effect: a*b

#you could measure the proportion of sims that fall above 0 to get your p value (or just report the CI)

### Paul suggestion

# for the most part, with multilevel regressions, if your a path is significant and b path is significant, 
# then the indirect effect is significant (you can test mediation in multilevel regressions using 
# that Monte Carlo method by Preacher, which roughly works out to what i said, where 
# if you have p < .01 in both, then your mediation will be p < .05)
# for multiple mediators, each a path will be the same as if you are testing a single mediation
# i.e., regress M_ on X (edited) 
# then for the b path, you should do a multiple regression, regressing Y on X, M1, M2, etc.
# you can then test for the indirect effect X-M_-Y using the monte carlo method
# i would estimate each indirect effect separately basically

# ok, the monte carlo method to testing mediation is pretty easy. Preacher has a website set up to help with this:
# https://www.quantpsy.org/medmc/medmc.htm
# you just enter in your values from your lmer results, then the website generates R code 
# you can run to test for whether the indirect effect is significant using monte carlo simulations

# to be clear about what i mean by testing each separately: you test X-M_ with a separate 
# regression for each, you test M_-Y all together with a big multiple regression, 
# and then you test the significance of each indirect effect separately with this monte carlo method

# Separartely run: M1 ~ X, M2 ~ X, etc.
# then Y ~ X + M1 + M2 + M3 …


# chatgpt
### (1) fit multilevel models
# library(lme4)
# 
# # Fit the model for the effect of X on M1 (path a1)
# model_a1 <- lmer(M1 ~ X + (1 | Subj), data = Data_mixed_clean)
# model_a2 <- lmer(M2 ~ X + (1 | Subj), data = Data_mixed_clean)
# model_a3 <- lmer(M3 ~ X + (1 | Subj), data = Data_mixed_clean)
# model_a4 <- lmer(M4 ~ X + (1 | Subj), data = Data_mixed_clean)
# 
# # Fit the model for the effect of M1 on Y controlling for X (path b1)
# model_b1 <- lmer(Y ~ X + M1 + (1 | Subj), data = Data_mixed_clean)
# model_b2 <- lmer(Y ~ X + M2 + (1 | Subj), data = Data_mixed_clean)
# model_b3 <- lmer(Y ~ X + M3 + (1 | Subj), data = Data_mixed_clean)
# model_b4 <- lmer(Y ~ X + M4 + (1 | Subj), data = Data_mixed_clean)
# 
# ### (2) calculate the indirect effects
# # Get the coefficients for path the a's
# coef_a1 <- fixef(model_a1)["X"]
# coef_a2 <- fixef(model_a2)["X"]
# coef_a3 <- fixef(model_a3)["X"]
# coef_a4 <- fixef(model_a4)["X"]
# 
# # Get the coefficients for path the b's
# coef_b1 <- fixef(model_b1)["M1"]
# coef_b2 <- fixef(model_b2)["M1"]
# coef_b3 <- fixef(model_b3)["M1"]
# coef_b4 <- fixef(model_b4)["M1"]
# 
# # Calculate the indirect effect for M's
# indirect_effect_M1 <- coef_a1 * coef_b1
# indirect_effect_M2 <- coef_a2 * coef_b2
# indirect_effect_M3 <- coef_a3 * coef_b3
# indirect_effect_M4 <- coef_a4 * coef_b4
# 
# ### (3) conduct monte carlo simulations
# library(mediation)
# library(arm)
# 
# # Assuming we have a function to simulate the indirect effect called 'simulate_indirect_effect'
# # This function would generate datasets based on the parameters estimated and calculate indirect effects
# # You would write this function to simulate the indirect effect, taking into account the random effects from your multilevel model
# 
# ### the site https://www.quantpsy.org/medmc/medmc.htm wants me to load a number of variables
# # into the GUI. a, b, var(a), var(b), cov(a,b)
# 
# # For model_a1
# vcov_a1 <- vcov(model_a1)
# print(vcov_a1)
# 
# # For model_b1
# vcov_b1 <- vcov(model_b1)
# print(vcov_b1)
# 
# library(boot)
# 
# # A function to bootstrap the indirect effect
# boot_indirect_effect <- function(data, indices) {
#   # Sample the data
#   d <- data[indices, ]
#   
#   # Fit the models on the bootstrapped sample
#   fit_a <- lmer(M1 ~ X + (1 | Subj), data = d)
#   fit_b <- lmer(Y ~ X + M1 + (1 | Subj), data = d)
#   
#   # Extract the coefficients
#   coef_a <- fixef(fit_a)["X"]
#   coef_b <- fixef(fit_b)["M1"]
#   
#   # Return the indirect effect
#   return(coef_a * coef_b)
# }
# 
# # Perform the bootstrapping
# set.seed(123) # Set a seed for reproducibility
# boot_results <- boot(data = Data_mixed_clean, statistic = boot_indirect_effect, R = 1000)
# 
# # Extract the bootstrap estimates
# boot_estimates <- boot_results$t
# 
# # Calculate the covariance matrix of the bootstrap estimates
# cov_matrix <- cov(boot_estimates)
# print(cov_matrix)








# 
# ################################################
# # This code can be edited in this window and   #
# # submitted to Rweb, or for faster performance #
# # and a nicer looking histogram, submit        #
# # directly to R.                               #
# ################################################
# require(MASS)
# a=-2.47765
# b=0.04177169
# rep=20000
# conf=95
# pest=c(a,b)
# acov <- matrix(c(
#   0.04085856, 0,
#   0, 0.06168193
# ),2,2)
# mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
# ab <- mcmc[,1]*mcmc[,2]
# low=(1-conf/100)/2
# upp=((1-conf/100)/2)+(conf/100)
# LL=quantile(ab,low)
# UL=quantile(ab,upp)
# LL4=format(LL,digits=4)
# UL4=format(UL,digits=4)
# ################################################
# # The number of columns in the histogram can   #
# # be changed by replacing 'FD' below with      #
# # an integer value.                            #
# ################################################
# hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
#      main='Distribution of Indirect Effect')

