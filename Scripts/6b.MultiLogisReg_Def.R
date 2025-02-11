# ========================== MULTINOMIAL LOGISTIC REGRESSION (MLR) MODEL: DEF =========================
# Fitting an MLR-model towards finalizing its input space in modelling the transition rate: 
# Default to Performing, Default (baseline), Settlement, Write-off
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Fusion2.R
#
# -- Inputs:
#   - datCredit_train | Training set, created from subsampled set from 3b
#   - datCredit_valid | Validation set, created from subsampled set from 3b
#
# -- Outputs:
#   - <Analytics> | Input space
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Bespoke model evaluation function
evalMLR <- function(model, model_base, datGiven, targetFld, predClass) {
  require(data.table); require(scales)
  # - Test conditions
  # model <- modMLR; model_base <- modMLR_base; datGiven <- datCredit_train[MarkovStatus=="Def",]
  # targetFld = "Target_FromD"; predClass <- "Perf"
  result1 <- AIC(model) # 1164537 
  result2 <- coefDeter_glm(model, model_base) # 0.29%
  matPred <- predict(model, newdata=datGiven, type="probs")
  actuals <- ifelse(datGiven[[targetFld]] == predClass, 1,0)
  result3 <- roc(response=actuals, predictor = matPred[, predClass])
  objResults <- data.table(AIC=comma(result1), result2, AUC=percent(result3$auc,accuracy=0.01))
  return(objResults)
  # - Cleanup, if run interactively
  rm(result1, result2, matPred, actuals, result3, objResults, model, model_base, datGiven, targetFld, predClass)
}

# - Extreme down-sample the training data, used only for stepwise forward selection procedure and final confirmation
# of statistical significance
smp_size <- 50000 # Number of keys/loans during the subsampling step
# Implied sampling fraction for the downsampling step
smp_perc <- smp_size/datCredit_train[Counter==1,.N]
set.seed(6,kind="Mersenne-Twister")
# Training Key population
# Get unique subject IDs or keys from the full dataset
datKeys <- datCredit_train %>% subset(Counter==1, c("Date", "LoanID", "Date_Origination"))
# Use stratified random sampling to select at random some keys from which the training set will be populated 
datKeys_sampled <- datKeys %>% group_by(Date_Origination) %>% slice_sample(prop=smp_perc)
# Obtain the associated loan records in creating the subsampled dataset
datCredit_train_sub <- copy(datCredit_train %>% subset(LoanID %in% datKeys_sampled$LoanID))

# - Distributional analyses
describe(datCredit_train[MarkovStatus=="Perf",Target_FromD]) # Dominant class: Performing
describe(datCredit_train[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default
describe(datCredit_train_sub[MarkovStatus=="Perf",Target_FromD]) # Dominant class: Performing
describe(datCredit_train_sub[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base <- multinom(Target_FromD ~  1, 
                        data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
modMLR_base_sub <- multinom(Target_FromD ~  1, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)




### AB: yet to be run ----------------------------------------------------------------------------------------------
# ------ 2a. Modelling theme: Portfolio-level delinquency-themed variables

# --- Single-factor models: g0_Delinq_Any_Aggr_Prop and lags
# Where, relevant, we build single-factor models to determine which lag-orders are best
# - g0_Delinq_Any_Aggr_Prop
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Any_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_12,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_Any_Aggr_Prop; g0_Delinq_Any_Aggr_Prop_Lag_1
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better



# --- Single-factor models: DefaultStatus1_Aggr_Prop and lags

# - DefaultStatus1_Aggr_Prop
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_3,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - DefaultStatus1_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# DefaultStatus1_Aggr_Prop; DefaultStatus1_Aggr_Prop_Lag_1
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better
# Relatively weaker variable than g0_Delinq_Any_Aggr_Prop.



# --- Single-factor models: Various delinquency-themed portfolio-level variables

# - g0_Delinq_Ave
modMLR <- multinom(Target_FromD ~  g0_Delinq_Ave, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - CuringEvents_Aggr_Prop
modMLR <- multinom(Target_FromD ~  CuringEvents_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - ArrearsToBalance_Aggr_Prop
modMLR <- multinom(Target_FromD ~  ArrearsToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_Ave; ArrearsToBalance_Aggr_Prop





# ------ 2b. Combining insights: Delinquency-themed portfolio-level variables

# --- Full-model
modMLR_full <- multinom(Target_FromD ~ ArrearsToBalance_Aggr_Prop + CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                          g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1 + 
                          DefaultStatus1_Aggr_Prop + DefaultStatus1_Aggr_Prop_Lag_1, 
                        data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Statistical significance: Wald-test
modMLR_full_sum <- summary(modMLR_full)
(z <- modMLR_full_sum$coefficients  / modMLR_full_sum$standard.errors) # test statistic
(p <- (1 - pnorm(abs(z), 0, 1)) * 2) # 2-tailed z test p-value
### RESULTS: Insignificant variables for state (l): g0_Delinq_Ave  (S); g0_Delinq_Any_Aggr_Prop (S)
# g0_Delinq_Any_Aggr_Prop_Lag_1  (W)
# Wald statistic is known to be inappropriate for large sample sizes since everything becomes significant.



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ ArrearsToBalance_Aggr_Prop +  CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                              g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1 + 
                              DefaultStatus1_Aggr_Prop + DefaultStatus1_Aggr_Prop_Lag_1, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Agrees very closely (aside from the AIC) with full-sample model
# Henceforth, we shall use only the subsampled variant as an expediency

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 45m
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4m
### RESULTS: All variables are significant
### FINAL: Selected variables: g0_Delinq_Ave; DefaultStatus1_Aggr_Prop; g0_Delinq_Any_Aggr_Prop;





# ------ 3a. Modelling theme: Other portfolio-level variables

# --- Single-factor models: InterestRate_Margin_Aggr_Med and lags

# - InterestRate_Margin_Aggr_Med
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Margin_Aggr_Med_1
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_1, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Margin_Aggr_Med_2
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_2, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Margin_Aggr_Med_3
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Margin_Aggr_Med_9
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# InterestRate_Margin_Aggr_Med; InterestRate_Margin_Aggr_Med_1;
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_1 + InterestRate_Margin_Aggr_Med_2 +
                              InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med_9, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.3h
### RESULTS: Selected variables: InterestRate_Margin_Aggr_Med; InterestRate_Margin_Aggr_Med_2; InterestRate_Margin_Aggr_Med_9;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  



# --- Single-factor models: Various portfolio-level variables

# - InstalmentToBalance_Aggr_Prop
modMLR <- multinom(Target_FromD ~  InstalmentToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - AgeToTerm_Aggr_Mean
modMLR <- multinom(Target_FromD ~  AgeToTerm_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - PerfSpell_Maturity_Aggr_Mean
modMLR <- multinom(Target_FromD ~  PerfSpell_Maturity_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - CreditLeverage_Aggr
modMLR <- multinom(Target_FromD ~  CreditLeverage_Aggr, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Ave_Margin_Aggr
modMLR <- multinom(Target_FromD ~  Ave_Margin_Aggr, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - NewLoans_Aggr_Prop
modMLR <- multinom(Target_FromD ~  NewLoans_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# CreditLeverage_Aggr; AgeToTerm_Aggr_Mean;





# ------ 3b. Combining insights: Other portfolio-level variables
# ---- Combining insights: Other portfolio-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_9 +
                              InstalmentToBalance_Aggr_Prop + AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean + 
                              CreditLeverage_Aggr + Ave_Margin_Aggr + NewLoans_Aggr_Prop , 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 
### RESULTS: All variables are significant
### FINAL: Selected variables: InterestRate_Margin_Aggr_Med_2; InstalmentToBalance_Aggr_Prop; 
# AgeToTerm_Aggr_Mean; NewLoans_Aggr_Prop; CreditLeverage_Aggr ; PerfSpell_Maturity_Aggr_Mean
# It is strange that the lagged variant of InterestRate_Margin_Aggr_Med was chosen over its non-lagged variant,
# even though the latter had better fit statistics in single-factor models





# ------ 3c. Combining insights: Complete set of portfolio-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_2 + InstalmentToBalance_Aggr_Prop + 
                              AgeToTerm_Aggr_Mean + NewLoans_Aggr_Prop + Ave_Margin_Aggr + CreditLeverage_Aggr +
                              PerfSpell_Maturity_Aggr_Mean, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.6h
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 8m
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_Ave; DefaultStatus1_Aggr_Prop; g0_Delinq_Any_Aggr_Prop; InstalmentToBalance_Aggr_Prop
# Additional variables manually selected using expert judgment: 
#   AgeToTerm_Aggr_Mean; CreditLeverage_Aggr; InterestRate_Margin_Aggr_Med



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromD ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              InstalmentToBalance_Aggr_Prop + AgeToTerm_Aggr_Mean + 
                              CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 13.5m
### RESULTS: Insignificant variables include: InstalmentToBalance_Aggr_Prop; AgeToTerm_Aggr_Mean; InterestRate_Margin_Aggr_Med;



# --- Model 2, subsampled set
modMLR_sub_cand <- multinom(Target_FromD ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 7.3m
### RESULTS: All variables are significant
### FINAL: Selected variables: g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
# CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med





# ------ 4a. Modelling theme: Loan-level delinquency-themed variables

# --- Single-factor models: Various information theoretic forms of delinquency
# - TimeInPerfSpell
modMLR <- multinom(Target_FromD ~  TimeInPerfSpell, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_fac
modMLR <- multinom(Target_FromD ~  g0_Delinq_fac, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq
modMLR <- multinom(Target_FromD ~  g0_Delinq, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_Num
modMLR <- multinom(Target_FromD ~  g0_Delinq_Num, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - TimeInDelinqState
modMLR <- multinom(Target_FromD ~  TimeInDelinqState, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - slc_acct_arr_dir_3
modMLR <- multinom(Target_FromD ~  slc_acct_arr_dir_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - slc_acct_roll_ever_24_imputed_mean
modMLR <- multinom(Target_FromD ~  slc_acct_roll_ever_24_imputed_mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics: 
# g0_Delinq_fac (over g0_Delinq) + slc_acct_arr_dir_3 + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num 
# It is remarkable the good fit of g0_Delinq_fac compared to other delinquency-themed variables, even though
# these loan-level variables are overall better fitting than the portfolio-level ones so far.



# --- Single-factor models: speed at which delinquency changes across various window lengths
# - g0_Delinq_SD_4
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_4, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_SD_5
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_5, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_SD_6
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_6, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_SD_9
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - g0_Delinq_SD_12
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_SD_6 + g0_Delinq_SD_5 + g0_Delinq_SD_4
# There appears to be a monotonic increasing trend across all fit statistics as the window length increases; 
# longer is better, but only up to 6-months, whereafter most fit statistics start to decrease again





# ------ 4b. Combining insights: Loan-level delinquency-themed variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + 
                              g0_Delinq_SD_6 + g0_Delinq_SD_5 + g0_Delinq_SD_4, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_fac + g0_Delinq_SD_6 + g0_Delinq_SD_4 + TimeInDelinqState + slc_acct_arr_dir_3 + 
# g0_Delinq_Num 



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromD ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables: 0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
# slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6





# ------ 5a. Modelling theme: Loan-level time-fixed variables

# --- Single-factor models: various time-fixed variables
# - Principal_Real
modMLR <- multinom(Target_FromD ~  Principal_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Margin
modMLR <- multinom(Target_FromD ~  InterestRate_Margin, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - LN_TPE
modMLR <- multinom(Target_FromD ~  LN_TPE, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - pmnt_method_grp
modMLR <- multinom(Target_FromD ~  pmnt_method_grp, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:
### FINAL: The highest-ranked variables across all fit statistics:
# pmnt_method_grp + InterestRate_Margin





# ------ 5b. Modelling theme: Loan-level time-varying variables

# --- Single-factor models: various time-varying variables
# - Balance_Real
modMLR <- multinom(Target_FromD ~  Balance_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Instalment_Real
modMLR <- multinom(Target_FromD ~  Instalment_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - InterestRate_Nom
modMLR <- multinom(Target_FromD ~  InterestRate_Nom, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - AgeToTerm
modMLR <- multinom(Target_FromD ~  AgeToTerm, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - BalanceToPrincipal
modMLR <- multinom(Target_FromD ~  BalanceToPrincipal, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - slc_acct_pre_lim_perc_imputed_med
modMLR <- multinom(Target_FromD ~  slc_acct_pre_lim_perc_imputed_med, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Nom





# ------ 5c. Combining insights: Loan-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ pmnt_method_grp + InterestRate_Margin + 
                              BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Nom, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + InterestRate_Nom





# ------ 5d. Modelling theme: Loan-level State spell variables

# --- Single-factor models: various state spell variables
# - Prev_Spell_Age
modMLR <- multinom(Target_FromD ~  Prev_Spell_Age, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - StateSpell_Num
modMLR <- multinom(Target_FromD ~  StateSpell_Num, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - StateSpell_Num_Total
modMLR <- multinom(Target_FromD ~  StateSpell_Num_Total, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - TimeInStateSpell
modMLR <- multinom(Target_FromD ~  TimeInStateSpell, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
### FINAL: The highest-ranked variables across all fit statistics:
# TimeInStateSpell + StateSpell_Num_Total





# ------ 5e. Combining insights: A complete set of loan-level non-delinquency variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + InterestRate_Nom + TimeInStateSpell + StateSpell_Num_Total, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + InterestRate_Nom + TimeInStateSpell + StateSpell_Num_Total





# ------ 5f. Combining insights: A complete set of loan-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + TimeInStateSpell + StateSpell_Num_Total + 
                              g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + StateSpell_Num_Total + g0_Delinq_fac + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + 
# g0_Delinq_Num + g0_Delinq_SD_6





# ------ 6a. Modelling theme: Repo rate

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Repo_Rate, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_Repo_Rate + M_Repo_Rate_1 + M_Repo_Rate_2 + M_Repo_Rate_3 +
                              M_Repo_Rate_6 + M_Repo_Rate_9 + M_Repo_Rate_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Repo_Rate + M_Repo_Rate_12  + M_Repo_Rate_2   



# --- Single-factor models: various lag orders
# - M_Repo_Rate_2
modMLR <- multinom(Target_FromD ~  M_Repo_Rate_2, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_Repo_Rate_12
modMLR <- multinom(Target_FromD ~  M_Repo_Rate_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_Repo_Rate




# ------ 6b. Modelling theme: Inflation growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Inflation_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_Inflation_Growth + M_Inflation_Growth_1 + M_Inflation_Growth_2 + M_Inflation_Growth_3 +
                              M_Inflation_Growth_6 + M_Inflation_Growth_9 + M_Inflation_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Inflation_Growth + M_Inflation_Growth_2 + M_Inflation_Growth_12



# --- Single-factor models: various lag orders
# - M_Inflation_Growth_2
modMLR <- multinom(Target_FromD ~  M_Inflation_Growth_2, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_Inflation_Growth_12
modMLR <- multinom(Target_FromD ~  M_Inflation_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_Inflation_Growth_2 + M_Inflation_Growth_12




# ------ 6c. Modelling theme: RealGDP growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_RealGDP_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_2 + M_RealGDP_Growth_3 +
                              M_RealGDP_Growth_6 + M_RealGDP_Growth_9 + M_RealGDP_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealGDP_Growth + M_RealGDP_Growth_9 + M_RealGDP_Growth_12



# --- Single-factor models: various lag orders
# - M_RealGDP_Growth_9
modMLR <- multinom(Target_FromD ~  M_RealGDP_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_RealGDP_Growth_12
modMLR <- multinom(Target_FromD ~  M_RealGDP_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_RealGDP_Growth_12




# ------ 6d. Modelling theme: RealIncome growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_RealIncome_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_2 + M_RealIncome_Growth_3 + 
                              M_RealIncome_Growth_6 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC: ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12



# --- Single-factor models: various lag orders
# - M_RealIncome_Growth_1
modMLR <- multinom(Target_FromD ~  M_RealIncome_Growth_1,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_RealIncome_Growth_9
modMLR <- multinom(Target_FromD ~  M_RealIncome_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_RealIncome_Growth_12
modMLR <- multinom(Target_FromD ~  M_RealIncome_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_RealIncome_Growth_12




# ------ 6e. Modelling theme: DTI growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_DTI_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_2 + M_DTI_Growth_3 + 
                              M_DTI_Growth_6 + M_DTI_Growth_9 + M_DTI_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_9 + M_DTI_Growth_12



# --- Single-factor models: various lag orders
# - M_DTI_Growth_1
modMLR <- multinom(Target_FromD ~  M_DTI_Growth_1, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_DTI_Growth_9
modMLR <- multinom(Target_FromD ~  M_DTI_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_DTI_Growth_12
modMLR <- multinom(Target_FromD ~  M_DTI_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_DTI_Growth + M_DTI_Growth_1





# ------ 6f. Modelling theme: Employment growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Emp_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_2 + M_Emp_Growth_3 + 
                              M_Emp_Growth_6 + M_Emp_Growth_9 + M_Emp_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_12



# --- Single-factor models: various lag orders
# - M_Emp_Growth_1
modMLR <- multinom(Target_FromD ~  M_Emp_Growth_1, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - M_Emp_Growth_12
modMLR <- multinom(Target_FromD ~  M_Emp_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# 'Strongest' variable: M_Emp_Growth_12 + M_Emp_Growth





# ------ 6g. Combining insights: A complete set of macroeconomic variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ M_Repo_Rate + M_Repo_Rate_12  + M_Repo_Rate_2 + 
                              M_Inflation_Growth + M_Inflation_Growth_2 + M_Inflation_Growth_12 + 
                              M_RealGDP_Growth + M_RealGDP_Growth_12 + 
                              M_RealIncome_Growth + M_RealIncome_Growth_12 + 
                              M_DTI_Growth + M_DTI_Growth_1 + 
                              M_Emp_Growth_12 + M_Emp_Growth, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_sub, scope = list(lower = modMLR_base_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Inflation_Growth_2 + M_Inflation_Growth_12 + M_Repo_Rate + 
# M_Emp_Growth + M_DTI_Growth + M_RealIncome_Growth
# Removed using expert judgement: M_Repo_Rate_2 + M_Repo_Rate_12





# ------ 7. Combining insights: A complete set of all input variables

# --- Base-level null model, subsampled set | Stepwise forward selection procedure
# Specifying the best-in-class variables per theme as the minimum model
modMLR_single_sub <- multinom(Target_FromD ~ g0_Delinq_fac + g0_Delinq_Ave, 
                              data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med + 
                              BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + StateSpell_Num_Total + g0_Delinq_fac + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + 
                              M_Inflation_Growth_2 + M_Inflation_Growth_12 + M_Repo_Rate + 
                              M_Emp_Growth + M_DTI_Growth + M_RealIncome_Growth
                            , 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
# BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Margin +
# g0_Delinq_Num + g0_Delinq_SD_6 + TimeInDelinqState +  g0_Delinq_fac + pmnt_method_grp + 
# StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + M_Emp_Growth



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromD ~ g0_Delinq_Ave +  DefaultStatus1_Aggr_Prop + 
                              BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + TimeInDelinqState +  g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate , 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  ; McFadden R^2:  ; AUC:  

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant