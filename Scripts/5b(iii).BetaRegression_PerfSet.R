# =================================== BETA REGRESSION MODEL: PERF-SET =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Performing to Settlement
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Roland Breedt (RB), Dr Arno Botha (AB)
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
#   - 5a.BetaRegression_DataFusion3.R
#
# -- Inputs:
#   - datAggr_train | Training set, created from subsampled set
#   - datAggr_valid | Validation set, created from subsampled set
#
# -- Outputs:
#   - <Analytics> | Input space
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)

# - Visual Plot of transition rate
plot(datAggr_train$Date,datAggr_train$Y_PerfToSet,type="l",ylab="Transition proportions", xlab="Date",main="Performance to performance transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_PerfToSet,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))




# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_PerfToSet~Prev_PS,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2158984 ***
AIC(Delinq_Model) # AIC = -1896.961


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01836934 ***
AIC(Delinq_Model) # AIC = -1833.028

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01573565
AIC(Delinq_Model) # AIC = -1832.275

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01337552 ***
AIC(Delinq_Model) # AIC = -1831.619

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.009216663
AIC(Delinq_Model) # AIC = -1830.58

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.001263914
AIC(Delinq_Model) # AIC = -1828.71

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0007000511
AIC(Delinq_Model) # AIC = -1828.582

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.001498532
AIC(Delinq_Model) # AIC = -1828.753
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop and g0_Delinq_Any_Aggr_Prop_Lag_2


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToSet~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1895954
AIC(Delinq_Model) # AIC = -1878.207

Delinq_Model<-betareg(Y_PerfToSet~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2018913
AIC(Delinq_Model) # AIC = -1882.447

Delinq_Model<-betareg(Y_PerfToSet~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2138063 ***
AIC(Delinq_Model) # AIC =-1886.819

Delinq_Model<-betareg(Y_PerfToSet~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2180954 ***
AIC(Delinq_Model) # AIC = -1866.224

Delinq_Model<-betareg(Y_PerfToSet~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1406856
AIC(Delinq_Model) # AIC = -1866.224
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_6 and DefaultStatus1_Aggr_Prop_Lag_3
# R2 is reasonably higher than g0_Delinq_Any_Aggr_Prop


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02198172
AIC(Delinq_Model) # AIC = -1833.993

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02347418
AIC(Delinq_Model) # AIC = -1834.344

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.008340927
AIC(Delinq_Model) # AIC = -1830.352

Delinq_Model<-betareg(Y_PerfToSet~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1863731 ***
AIC(Delinq_Model) # AIC = -1877.07

Delinq_Model<-betareg(Y_PerfToSet~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1738332 ***
AIC(Delinq_Model) # AIC = -1877.739
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_3_Ave and CuringEvents_Aggr_Prop
# R2 is a bit lower than DefaultStatus1_Aggr_Prop_Lag


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_PerfToSet~ Prev_PS + g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                             DefaultStatus1_Aggr_Prop_Lag_6 + DefaultStatus1_Aggr_Prop_Lag_3 +
                             g0_Delinq_3_Ave + CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3168849
AIC(Delinq_Model_Full) # AIC = -1919.388

# Remove CuringEvents_Aggr_Prop, DefaultStatus1_Aggr_Prop_Lag_6 and DefaultStatus1_Aggr_Prop_Lag_3
Delinq_Model_Full<-betareg(Y_PerfToSet~ Prev_PS + g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                             g0_Delinq_3_Ave, data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3119188
AIC(Delinq_Model_Full) # AIC = -1923.182
### RESULTS: The previous P to S transition rate is a good predictor for the next month's transition rate, this is likely because it informs the general level of 
# transitions into settlement or put differently, the maturity of the loan book. Again the g0_delinq variables seem to be great candidate input variables
# for modelling the transitions rate; even for transition rates other than P to D for which it was originally engineered.


# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02364601 ***
AIC(Macro_Model) # AIC = -1834.436

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01634039 ***
AIC(Macro_Model) # AIC = -1832.516

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008129828
AIC(Macro_Model) # AIC = -1830.425

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.006069186
AIC(Macro_Model) # AIC = -1829.888

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0000386104
AIC(Macro_Model) # AIC = -1828.437

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003932483
AIC(Macro_Model) # AIC = -1829.271

Macro_Model<-betareg(Y_PerfToSet~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01523381
AIC(Macro_Model) # AIC = -1831.584
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Repo_Rate and M_Repo_Rate_1


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009756412
AIC(Macro_Model) # AIC = -1830.752

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.001470416
AIC(Macro_Model) # AIC = -1828.774

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.000121873
AIC(Macro_Model) # AIC = -1828.457

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002612575
AIC(Macro_Model) # AIC = -1829.022

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01645074
AIC(Macro_Model) # AIC = -1832.01

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0478319 ***
AIC(Macro_Model) # AIC = -1838.865

Macro_Model<-betareg(Y_PerfToSet~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07798439 ***
AIC(Macro_Model) # AIC = -1845.556
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Inflation_Growth_12 and M_Inflation_Growth_9
# R2 is higher than for the repo rate


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00433077
AIC(Macro_Model) # AIC = -1829.459

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00607023 ***
AIC(Macro_Model) # AIC = -1829.851

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.005660262
AIC(Macro_Model) # AIC = -1829.739

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003916113
AIC(Macro_Model) # AIC = -1829.324

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00008698379
AIC(Macro_Model) # AIC = -1828.448

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001733427
AIC(Macro_Model) # AIC = -1828.802

Macro_Model<-betareg(Y_PerfToSet~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01708179 ***
AIC(Macro_Model) # AIC = -1832.129
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth_12 and M_DTI_Growth_1
# R2 is slightly less than for the repo rate


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07291319 ***
AIC(Macro_Model) # AIC = -1849.333

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06374721
AIC(Macro_Model) # AIC = -1846.763

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06266856
AIC(Macro_Model) # AIC = -1846.406

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06399419 ***
AIC(Macro_Model) # AIC = -1846.916

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04707756
AIC(Macro_Model) # AIC = -1841.923

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02589653
AIC(Macro_Model) # AIC = -1835.181

Macro_Model<-betareg(Y_PerfToSet~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02521913
AIC(Macro_Model) # AIC = -1834.788
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth and M_RealIncome_Growth_3
# R2 is more than for the repo rate


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05588459 ***
AIC(Macro_Model) # AIC = -1843.568

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04074621 ***
AIC(Macro_Model) # AIC = -1839.537

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.03214332
AIC(Macro_Model) # AIC = -1837.157

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02346093
AIC(Macro_Model) # AIC = -1811.991

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01092233
AIC(Macro_Model) # AIC = -1834.711

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008725277
AIC(Macro_Model) # AIC = -1830.649

Macro_Model<-betareg(Y_PerfToSet~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0162609
AIC(Macro_Model) # AIC = -1832.468
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth and M_Emp_Growth_1
# R2 is more than for the repo rate


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03356167 ***
AIC(Macro_Model) # AIC = -1837.942

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02582826 ***
AIC(Macro_Model) # AIC = -1835.744

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02358045
AIC(Macro_Model) # AIC = -1835.069

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02336433
AIC(Macro_Model) # AIC = -1835.035

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01324626
AIC(Macro_Model) # AIC = -1832.111

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.005507538
AIC(Macro_Model) # AIC = -1829.821

Macro_Model<-betareg(Y_PerfToSet~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01189832
AIC(Macro_Model) # AIC = -1831.349
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth and M_RealGDP_Growth_1
# R2 is slightly more than for the repo rate


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_PerfToSet~M_Repo_Rate + M_Repo_Rate_1+M_Inflation_Growth_12 + M_Inflation_Growth_9+
                            M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                            M_Emp_Growth + M_Emp_Growth_1+M_RealGDP_Growth + M_RealGDP_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2977061
AIC(Macro_Model_Full) # AIC = -1901.477

# - Remove M_RealGDP_Growth and M_RealGDP_Growth_1
Macro_Model_Full<-betareg(Y_PerfToSet~M_Repo_Rate + M_Repo_Rate_1+M_Inflation_Growth_12 + M_Inflation_Growth_9+
                            M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                            M_Emp_Growth + M_Emp_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2799728
AIC(Macro_Model_Full) # AIC = -1898.053

# - Remove M_Inflation_Growth_12 and M_Repo_Rate_1
Macro_Model_Full<-betareg(Y_PerfToSet~M_Repo_Rate + M_Inflation_Growth_9+
                            M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                            M_Emp_Growth + M_Emp_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2716636
AIC(Macro_Model_Full) # AIC = -1899.107

# - Remove M_Repo_Rate and M_Inflation_Growth_9
Macro_Model_Full<-betareg(Y_PerfToSet~M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                            M_Emp_Growth + M_Emp_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2671706
AIC(Macro_Model_Full) # AIC = -1900.804
### RESULTS: Interestingly if a covariate is included in the model, so is another lagged version thereof. Clearly there exists some interactive effect between
# the differently lagged covariates of the same macroeconomic variable. Moreover, it is the case for all macroeconomic covariates that if one coefficient has a positive 
# sign, then its counterpart has a negative sign.


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.0006776344
AIC(Aggr_IR_Model) # AIC = -1828.58

Aggr_IR_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.001004362
AIC(Aggr_IR_Model) # AIC = -1828.652

Aggr_IR_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.001210535 ***
AIC(Aggr_IR_Model) # AIC = -1828.697

Aggr_IR_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 =  0.001543999 ***
AIC(Aggr_IR_Model) # AIC = -1828.77

Aggr_IR_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.00118828
AIC(Aggr_IR_Model) # AIC = -1828.689

Aggr_IR_Model<-betareg(Y_PerfToSet~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.0004568806
AIC(Aggr_IR_Model) # AIC = -1828.535
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med_3 and InterestRate_Margin_Aggr_Med_2


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_PerfToSet~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1332811 ***
AIC(Aggr_NewLoans) # AIC = -1862.138

Aggr_NewLoans<-betareg(Y_PerfToSet~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1105665
AIC(Aggr_NewLoans) # AIC = -1857.351

Aggr_NewLoans<-betareg(Y_PerfToSet~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1479392 ***
AIC(Aggr_NewLoans) # AIC = -1870.821

Aggr_NewLoans<-betareg(Y_PerfToSet~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1103742
AIC(Aggr_NewLoans) # AIC = -1858.505

Aggr_NewLoans<-betareg(Y_PerfToSet~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1227686
AIC(Aggr_NewLoans) # AIC = -1863.776
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_3 and NewLoans_Aggr_Prop


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med_2+
                           NewLoans_Aggr_Prop_3+NewLoans_Aggr_Prop+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.3928943
AIC(Aggr_Full_Model) # AIC = -1941.095

# - Remove InterestRate_Margin_Aggr_Med_2, NewLoans_Aggr_Prop, ArrearsToBalance_Aggr_Prop, InstalmentToBalance_Aggr_Prop and CuringEvents_Aggr_Prop
Aggr_Full_Model<-betareg(Y_PerfToSet~InterestRate_Margin_Aggr_Med_3 +
                           NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.3807734
AIC(Aggr_Full_Model) # AIC = -1944.987
### RESULTS: The general portfolio level covariates give better fit statistics than the macroeconomic variabes. This could be due to the fact that
# P to S transitions are less sensitive to the general macroeconomic environment, but more a function of information such as credit leverage etc.
# Again, the P to S model for the general portfolio level variables has a lower R2 than for the P to D general portfolio level input model.


# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
PS_BR_Full<-betareg(Y_PerfToSet~Prev_PS + g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                      g0_Delinq_3_Ave+M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                      M_Emp_Growth + M_Emp_Growth_1 + InterestRate_Margin_Aggr_Med_3 +
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PS_BR_Full)
PS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.4696749
AIC(PS_BR_Full) # AIC = -1965.592

# - Remove InterestRate_Margin_Aggr_Med_3 (very large std error)
PS_BR_Full<-betareg(Y_PerfToSet~Prev_PS + g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                      g0_Delinq_3_Ave+M_DTI_Growth_12 + M_DTI_Growth_1+M_RealIncome_Growth + M_RealIncome_Growth_3+
                      M_Emp_Growth + M_Emp_Growth_1 + 
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PS_BR_Full)
PS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.4316819
AIC(PS_BR_Full) # AIC = -1954.267

# - Remove Prev_PS, M_RealIncome_Growth, M_RealIncome_Growth_3, M_DTI_Growth_12, M_DTI_Growth_1, NewLoans_Aggr_Prop_3 and g0_Delinq_Any_Aggr_Prop
PS_BR_Full<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                      g0_Delinq_3_Ave+
                      M_Emp_Growth + M_Emp_Growth_1 + 
                      CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PS_BR_Full)
PS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.4110498
AIC(PS_BR_Full) # AIC = -1959.108
### RESULTS: Again, it does not seem as if the macroeconomic environment have such a big impact on the P to S transition rate, given that
# employment growth was the only macroeconomic variable that made it to the final model. Moreover, it is plausible that something like employment growth
# has more of an effect on settlement when compared to other macroeconomic variables such as the inflation rate.




# ------ 3. Finalised input space of the model | Mu
PS_BR_Full<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                          g0_Delinq_3_Ave+
                          M_Emp_Growth + M_Emp_Growth_1 + 
                          CreditLeverage_Aggr+
                          AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PS_BR_Full)
PS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.4110498
AIC(PS_BR_Full) # AIC = -1959.108




# ------ 4. Modelling themes | Phi
# --- Obtain the 3 strongest candidate input variables to model phi (keep input space for mu constant)
string1<-"Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                      g0_Delinq_3_Ave+
                      M_Emp_Growth + M_Emp_Growth_1 + 
                      CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|"
Phi_Set <- as.data.table(colnames(datAggr_train)[17:127])
colnames(Phi_Set)<-"InputV_Phi"
Phi_Set[,R2 := {
  # Construct the formula string dynamically for each row of Phi_Set
  formula_string <- paste(string1, InputV_Phi)
  # Fit the betareg model and extract the pseudo R-squared value
  betareg(as.formula(formula_string), data = datAggr_train)$pseudo.r.squared
}, by = InputV_Phi]

Phi_Set[,p_val := {
  # Construct the formula string dynamically for each row of Phi_Set
  formula_string <- paste(string1, InputV_Phi)
  # Fit the betareg model and extract the p-value
  summary(betareg(as.formula(formula_string), data = datAggr_train))$coefficients$precision[2, "Pr(>|z|)"]
}, by = InputV_Phi]

# Order best input variables for phi
Ordered_Phi_Set<-Phi_Set[order(R2, decreasing=TRUE),]
Ordered_Phi_Set[p_val<0.1,]
### RESULTS - best inputs to phi using single factor models for its input space is
# M_Repo_Rate, InstalmentToBalance_Aggr_Prop, M_Repo_Rate_2


# - Start of with the 3 best phi inputs
PS_Phi<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                          g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                          CreditLeverage_Aggr+AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|M_Repo_Rate+
                          InstalmentToBalance_Aggr_Prop+M_Repo_Rate_2, data=datAggr_train)
summary(PS_Phi)
PS_Phi$pseudo.r.squared # Pseudo R2 = 0.4101771
AIC(PS_Phi) # AIC = -1969.606

# - Remove InstalmentToBalance_Aggr_Prop and M_Repo_Rate_2
PS_Phi<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                  g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                  CreditLeverage_Aggr+AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|M_Repo_Rate, data=datAggr_train)
summary(PS_Phi)
PS_Phi$pseudo.r.squared # Pseudo R2 = 0.4111419
AIC(PS_Phi) # AIC = -1959.427
### RESULTS: Modelling phi as dynamic improves the R2 slightly by 0.24%. It is observed that when we use a lot of variables as a starting point
# for the input space to phi, the algorithm fails to converge, hence we only investigate the three best single factor inputs. All input variables
# for modelling mu stay significant using the repo rate to model phi dynamically.




# ------ 5. Finalised input space of the model
# --- Constant Phi
PD_Final_Cnst_Phi<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                             g0_Delinq_3_Ave+
                             M_Emp_Growth + M_Emp_Growth_1 + 
                             CreditLeverage_Aggr+
                             AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PD_Final_Cnst_Phi)
PD_Final_Cnst_Phi$pseudo.r.squared # Pseudo R2 = 0.4110498
AIC(PD_Final_Cnst_Phi) # AIC = -1959.108



# --- Dynamic Phi
PD_Final_Dyn_Phi<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 +
                            g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                            CreditLeverage_Aggr+AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|M_Repo_Rate, data=datAggr_train)
summary(PD_Final_Dyn_Phi)
PD_Final_Dyn_Phi$pseudo.r.squared # Pseudo R2 = 0.4111419
AIC(PD_Final_Dyn_Phi) # AIC = -1959.427
### RESULTS: The dynamic phi model has a better Pseudo R2 and AIC than the constant phi model. We therefore
# choose the dynamic phi model as our best.




# --- Final
PS_Final<-betareg(Y_PerfToSet~ g0_Delinq_Any_Aggr_Prop_Lag_2 + g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                                      CreditLeverage_Aggr+AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|M_Repo_Rate, data=datAggr_train)
summary(PS_Final)
PS_Final$pseudo.r.squared # Pseudo R2 = 0.4111419
AIC(PS_Final) # AIC = -1959.427
cat("MAE = ",round(mean(abs(predict(PS_Final,datAggr_valid)-datAggr_valid$Y_PerfToSet)),7)*100,"%",sep="","\n") # MAE =  0.09501%

# - Link function on final mu input space
link_func_stats<-rbind(sapply(c("logit", "probit", "cloglog", "loglog"), function(x) AIC(update(PS_Final, link = x))),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) round(mean(abs(predict(update(PS_Final, link = x),datAggr_valid)-datAggr_valid$Y_PerfToSet)),7)*100),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) update(PS_Final, link = x)$pseudo.r.squared))
rownames(link_func_stats)<-c("AIC","MAE","Pseudo R^2")
link_func_stats
optimal_link<-"loglog"
### RESULTS - Ranked links based on Pseudo R2 for links (similar results hold for other measures):
# 1) loglog; 2) probit; 3) logit; cloglog
# Results are quite similar as the range of the pseudo r2 is [0.4104036, 0.4687433]

# - Update link function
# Also remove intercept as it is no longer statistically significant
PS_Final<-betareg(Y_PerfToSet~ -1 + g0_Delinq_Any_Aggr_Prop_Lag_2 + g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                    CreditLeverage_Aggr + AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | M_Repo_Rate, data=datAggr_train, link=optimal_link)
summary(PS_Final)
PS_Final$pseudo.r.squared # Pseudo R2 = 0.4632961
AIC(PS_Final) # AIC = -1958.794
cat("MAE = ",round(mean(abs(predict(PS_Final,datAggr_valid)-datAggr_valid$Y_PerfToSet)),7)*100,"%",sep="","\n") # MAE =  0.09473%




# ------ 6. Cooks Distance Adjustment
# - Interactive runs are done to observe the best combinations of observations to leave out
# Cooks Distance Plot
plot(PS_Final, which = 2, type = "pearson",xlab="obs")
# Obtain observations with the biggest CD values
sort(round(cooks.distance(PS_Final),4))
# Specify training points to remove
Leave_Out<-c(191,187)
# Retrain model on new training set
PS_Adj<-update(PS_Final,subset=-Leave_Out)
cat("Pseudo R^2 before adjustment = ",PS_Final$pseudo.r.squared," --- ","Pseudo R^2 after adjustment = ",PS_Adj$pseudo.r.squared,"\n",sep="")
# Plot
plot(datAggr_valid$Date,predict(PS_Adj,datAggr_valid),type="l",col="red",lwd=2,ylim=c(0.001,0.015),xlab="Date",ylab="Transition probability",main="Constant Phi after Cooks adjustment")
lines(datAggr_valid$Date,as.numeric(datAggr_valid$Y_PerfToSet),type="l")
MAEval<-round(mean(abs(predict(PS_Adj,datAggr_valid)-as.numeric(datAggr_valid$Y_PerfToSet))),7)*100
legend(x="topright",paste("MAE = ",MAEval,"%"))
cat("MAE of Cooks Distance adjusted model= ",MAEval,"%","\n",sep="")
### RESULTS: Cooks distance adjustment improved the model fit:
# Pseudo R2 before CD = 0.4632961; After CD = 0.599667

# --- Save Model
PS_Final<-PS_Adj
pack.ffdf(paste0(genObjPath,"BR_P_To_S"), PS_Final)
