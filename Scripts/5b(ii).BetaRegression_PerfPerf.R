# =================================== BETA REGRESSION MODEL: PERF-PERF =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Performing to Performing
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
plot(datAggr_train$Date,datAggr_train$Y_PerfToPerf,type="l",ylab="Transition proportions", xlab="Date",main="Performance to performance transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_PerfToPerf,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))




# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_PerfToPerf~Prev_PP,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.04091737
AIC(Delinq_Model) # AIC = -1809.873


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3503165 ***
AIC(Delinq_Model) # AIC = -1890.236

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3378107 ***
AIC(Delinq_Model) # AIC = -1884.948

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3204438
AIC(Delinq_Model) # AIC = -1878.377

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2863877
AIC(Delinq_Model) # AIC = -1867.492

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2164443
AIC(Delinq_Model) # AIC = -1847.191

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2164443
AIC(Delinq_Model) # AIC = -1847.191

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1326723
AIC(Delinq_Model) # AIC = -1826.931
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop and g0_Delinq_Any_Aggr_Prop_Lag_1


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToPerf~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.07363079
AIC(Delinq_Model) # AIC = -1815.873

Delinq_Model<-betareg(Y_PerfToPerf~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.09903899
AIC(Delinq_Model) # AIC = -1821.431

Delinq_Model<-betareg(Y_PerfToPerf~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1246361
AIC(Delinq_Model) # AIC = -1827.284

Delinq_Model<-betareg(Y_PerfToPerf~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1813973 ***
AIC(Delinq_Model) # AIC = -1841.037

Delinq_Model<-betareg(Y_PerfToPerf~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.230083 ***
AIC(Delinq_Model) # AIC = -1854.12
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_6 and DefaultStatus1_Aggr_Prop_Lag_12
# R2 slightly lower than g0_Delinq_Any_Aggr_Prop


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3703922 ***
AIC(Delinq_Model) # AIC = -1897.275

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3549533 ***
AIC(Delinq_Model) # AIC = -1892.183

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1669628
AIC(Delinq_Model) # AIC = -1836.534

Delinq_Model<-betareg(Y_PerfToPerf~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.04296098
AIC(Delinq_Model) # AIC = -1809.474

Delinq_Model<-betareg(Y_PerfToPerf~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1584538
AIC(Delinq_Model) # AIC = -1836.266
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Ave and g0_Delinq_1_Ave
# R2 a bit higher than DefaultStatus1_Aggr_Prop_Lag


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave +
                             DefaultStatus1_Aggr_Prop_Lag_6 + DefaultStatus1_Aggr_Prop_Lag_12 +
                             g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1
                             ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.5836474
AIC(Delinq_Model_Full) # AIC = -1982.634

# Remove DefaultStatus1_Aggr_Prop_Lag_6 and DefaultStatus1_Aggr_Prop_Lag_12
Delinq_Model_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave +
                             g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1
                             ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.5793622
AIC(Delinq_Model_Full) # AIC = -1984.508
### RESULTS: Shorter lagged versions of the g0_delinq variable seems to be predictive of the P to P transition rate. All variables except
# g0_Delinq_Any_Aggr_Prop have a negative sign, which implies lower prevalence of the g0_delinq variables (any level) implies a higher predicted
# P to P transition rate.


# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2704033 ***
AIC(Macro_Model) # AIC = -1868.532

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2548303 ***
AIC(Macro_Model) # AIC = -1863.342

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2330989
AIC(Macro_Model) # AIC = -1856.519

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2221349
AIC(Macro_Model) # AIC = -1852.695

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1519902
AIC(Macro_Model) # AIC = -1833.099

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.09220357
AIC(Macro_Model) # AIC = -1818.857

Macro_Model<-betareg(Y_PerfToPerf~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05628521
AIC(Macro_Model) # AIC = -1811.266
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Repo_Rate and M_Repo_Rate_1


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1891624 ***
AIC(Macro_Model) # AIC = -1843.76

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1591731 ***
AIC(Macro_Model) # AIC = -1835.918

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1365091
AIC(Macro_Model) # AIC = -1830.328

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1156993
AIC(Macro_Model) # AIC = -1825.189

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05955385
AIC(Macro_Model) # AIC = -1812.633

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01080846
AIC(Macro_Model) # AIC = -1802.919

Macro_Model<-betareg(Y_PerfToPerf~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0002193991
AIC(Macro_Model) # AIC = -1800.966
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Inflation_Growth and M_Inflation_Growth_1
# R2 a bit lower than for the repo rate


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1958029 ***
AIC(Macro_Model) # AIC = -1843.956

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1874177 ***
AIC(Macro_Model) # AIC = -1841.4

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1744751
AIC(Macro_Model) # AIC = -1838.121

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1597908
AIC(Macro_Model) # AIC = -1834.428

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1075213
AIC(Macro_Model) # AIC = -1822.334

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08529881
AIC(Macro_Model) # AIC = -1817.414

Macro_Model<-betareg(Y_PerfToPerf~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02388756
AIC(Macro_Model) # AIC = -1805.347
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth and M_DTI_Growth_1
# R2 is slightly less than the repo rate


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0776734
AIC(Macro_Model) # AIC = -1818.081

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08366012
AIC(Macro_Model) # AIC = -1819.591

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.09785061
AIC(Macro_Model) # AIC = -1822.888

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1133722
AIC(Macro_Model) # AIC = -1826.712

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.139954
AIC(Macro_Model) # AIC = -1833.618

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1681412 ***
AIC(Macro_Model) # AIC = -1839.517

Macro_Model<-betareg(Y_PerfToPerf~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2282748 ***
AIC(Macro_Model) # AIC = -1854.754
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth_12 and M_RealIncome_Growth_9
# There appears to be a linear relationship between lag order and R2


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0518965
AIC(Macro_Model) # AIC = -1811.984

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05052069
AIC(Macro_Model) # AIC = -1811.754

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.05216724
AIC(Macro_Model) # AIC = -1812.15

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05155729
AIC(Macro_Model) # AIC = -1811.991

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05820415
AIC(Macro_Model) # AIC = -1813.314

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08401119 ***
AIC(Macro_Model) # AIC = -1819.052

Macro_Model<-betareg(Y_PerfToPerf~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1427224 ***
AIC(Macro_Model) # AIC = -1831.838
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth_12 and M_Emp_Growth_9
# There appears to be a linear relationship between lag order and R2


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07231851
AIC(Macro_Model) # AIC = -1817.341

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07367526
AIC(Macro_Model) # AIC = -1817.747

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0822135
AIC(Macro_Model) # AIC = -1819.764

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.09173896
AIC(Macro_Model) # AIC = -1822.169

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.106527
AIC(Macro_Model) # AIC = -1825.86

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1317421 ***
AIC(Macro_Model) # AIC = -1830.737

Macro_Model<-betareg(Y_PerfToPerf~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.191895 ***
AIC(Macro_Model) # AIC = -1845.017
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth_12 and M_RealGDP_Growth_9
# There appears to be a linear relationship between lag order and R2


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+M_Repo_Rate_1+M_Inflation_Growth+M_Inflation_Growth_1+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.4370609
AIC(Macro_Model_Full) # AIC = -1903.628

# Remove M_Emp_Growth_9
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+M_Repo_Rate_1+M_Inflation_Growth+M_Inflation_Growth_1+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.4366784
AIC(Macro_Model_Full) # AIC = -1905.096

# Remove M_Repo_Rate_1
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+M_Inflation_Growth+M_Inflation_Growth_1+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.4344379
AIC(Macro_Model_Full) # AIC = -1906.198

# Remove M_DTI_Growth_1
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+M_Inflation_Growth+M_Inflation_Growth_1+
                            M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.4344379
AIC(Macro_Model_Full) # AIC = -1906.198

# Remove M_Inflation_Growth_1
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+M_Inflation_Growth+
                            M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.425598
AIC(Macro_Model_Full) # AIC = -1906.352

# Remove M_Inflation_Growth
Macro_Model_Full<-betareg(Y_PerfToPerf~M_Repo_Rate+
                            M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.423214
AIC(Macro_Model_Full) # AIC = -1907.955
### RESULTS: The repo rate again proves to be a significant variable in predicting transition rates of a portfolio of loans; this time for the P to P
# transition rate. The repo rate, debt and income growth seems to have an immediate effect on the transition rate, whereas real income, employment growth
# and real GDP have more of a lagged effect on the transition rate. The pseudo R2 for the P to P macro model is lower than the pseudo R2 for the P to D
# macro model, implying the latter has a better goodness-of-fit when only using the macroeconomic variables in the input space.


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.063968 ***
AIC(Aggr_IR_Model) # AIC = -1813.118

Aggr_IR_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.05900986
AIC(Aggr_IR_Model) # AIC = -1812.115

Aggr_IR_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.05559841
AIC(Aggr_IR_Model) # AIC = -1811.409

Aggr_IR_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.05160014
AIC(Aggr_IR_Model) # AIC = -1810.605

Aggr_IR_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.03966417
AIC(Aggr_IR_Model) # AIC = -1808.217

Aggr_IR_Model<-betareg(Y_PerfToPerf~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.09383204 ***
AIC(Aggr_IR_Model) # AIC = -1819.887
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med and Ave_Margin_Aggr


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_PerfToPerf~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1026794
AIC(Aggr_NewLoans) # AIC = -1823.054

Aggr_NewLoans<-betareg(Y_PerfToPerf~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1152348
AIC(Aggr_NewLoans) # AIC = -1826.137

Aggr_NewLoans<-betareg(Y_PerfToPerf~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.2304023 ***
AIC(Aggr_NewLoans) # AIC = -1855.643

Aggr_NewLoans<-betareg(Y_PerfToPerf~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1886718
AIC(Aggr_NewLoans) # AIC = -1843.52

Aggr_NewLoans<-betareg(Y_PerfToPerf~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.2194807 ***
AIC(Aggr_NewLoans) # AIC = -1853.414
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_3 and NewLoans_Aggr_Prop_5, though these single factor models
# produce a low pseudo R2.


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_PerfToPerf~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                           NewLoans_Aggr_Prop_3+NewLoans_Aggr_Prop_5+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.5847956
AIC(Aggr_Full_Model) # AIC = -1974.182

# - Remove InterestRate_Margin_Aggr_Med, NewLoans_Aggr_Prop_5, CuringEvents_Aggr_Prop
Aggr_Full_Model<-betareg(Y_PerfToPerf~Ave_Margin_Aggr+
                           NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.5758032
AIC(Aggr_Full_Model) # AIC = -1974.979
### RESULTS: The general portfolio level model seems to be better than the macroeconomic model for P to P transitions. ArrearsToBalance_Aggr_Prop has a
# large negative coefficient and a large std error, which will have to be investigated (possibly removed later in the script).


# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6483146
AIC(PP_BR_Full) # AIC = -1996.54
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10135%

# - Remove InstalmentToBalance_Aggr_Prop
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6483124
AIC(PP_BR_Full) # AIC = -1998.533
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10137%

# - Remove ArrearsToBalance_Aggr_Prop
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_DTI_Growth+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6480651
AIC(PP_BR_Full) # AIC = -2000.4153
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10127%

# - Remove M_DTI_Growth+
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6478077
AIC(PP_BR_Full) # AIC = -2002.255
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10143%

# - Remove M_Emp_Growth_12
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6478077
AIC(PP_BR_Full) # AIC = -2002.255
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10143%

# - Remove M_Emp_Growth_12
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+Ave_Margin_Aggr+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6477395
AIC(PP_BR_Full) # AIC = -2004.097
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10121%

# - Remove Ave_Margin_Aggr
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+M_Repo_Rate+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.646386
AIC(PP_BR_Full) # AIC = -2005.269
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.1008%

# - Remove M_Repo_Rate
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6444219
AIC(PP_BR_Full) # AIC = -2005.062
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10289%

# - Remove AgeToTerm_Aggr_Mean
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6434349
AIC(PP_BR_Full) # AIC = -2005.661
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10202%
### RESULTS: The delinquency and macroeconomic themed variables make up the majority of the input space. The repo and inflation rate 
# did not make it to the final model, implying they are less predictive for P to P transitions. High levels of g0_Delinq_Ave, g0_Delinq_1_Ave, and
# g0_Delinq_Any_Aggr_Prop_Lag_1 result in a lower predicted P to P transition rate as expected. There seems to be latent interaction effects
# between the covariates as some of the coefficients have different directions for the same (but differently lagged) covariate.




# ------ 3. Finalised input space of the model | Mu
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_BR_Full)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6434349
AIC(PP_BR_Full) # AIC = -2005.661
cat("MAE = ",round(mean(abs(predict(PP_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10202%




# ------ 4. Modelling themes | Phi
# --- Macroeconomic & General portfolio-level theme
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | M_RealIncome_Growth_12, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6425489
AIC(PP_BR_Full) # AIC = -2007.081
# Note using M_RealIncome_Growth_9 as input to phi couldn't make the algorithm converge.

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | M_RealGDP_Growth_9, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6429578 ***
AIC(PP_BR_Full) # AIC = -2004.485

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | M_RealGDP_Growth_12, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6433461 ***
AIC(PP_BR_Full) # AIC = -2003.724

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | NewLoans_Aggr_Prop_3, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6391091
AIC(PP_BR_Full) # AIC = -2029.11

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | CreditLeverage_Aggr, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6402846
AIC(PP_BR_Full) # AIC = -2027.46

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6317893
AIC(PP_BR_Full) # AIC = -2057.623
### RESULTS: Based on the single factor models, the M_RealGDP_Growth_12 and M_RealGDP_Growth_9 are the best covariates for modelling phi.


# --- Delinquency themed
PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.642424 ***
AIC(PP_BR_Full) # AIC = -2008.403

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | g0_Delinq_1_Ave, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6421974
AIC(PP_BR_Full) # AIC = -2009.4

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Any_Aggr_Prop, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6422862 ***
AIC(PP_BR_Full) # AIC = -2009.156

PP_BR_Full<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Any_Aggr_Prop_Lag_1, data=datAggr_train)
PP_BR_Full$pseudo.r.squared # Pseudo R2 = 0.6421846
AIC(PP_BR_Full) # AIC = -2010.758
### RESULTS: Based on the single factor models, the g0_Delinq_Ave and g0_Delinq_Any_Aggr_Prop are the best covariates for modelling phi.

# --- Fusion step
# Combine insights mined from previous themes
PP_Final_Phi<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean | M_RealGDP_Growth_9 + M_RealGDP_Growth_12 + g0_Delinq_Ave + g0_Delinq_Any_Aggr_Prop, data=datAggr_train)
summary(PP_Final_Phi)
PP_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.6334611
AIC(PP_Final_Phi) # AIC = -2024.419
cat("MAE = ",round(mean(abs(predict(PP_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10288%

# Remove M_RealGDP_Growth_9, M_RealGDP_Growth_12 and g0_Delinq_Any_Aggr_Prop
PP_Final_Phi<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                        g0_Delinq_Any_Aggr_Prop_Lag_1+
                        M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                        M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                        NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                        PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave , data=datAggr_train)
summary(PP_Final_Phi)
PP_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.642424
AIC(PP_Final_Phi) # AIC = -2008.403
cat("MAE = ",round(mean(abs(predict(PP_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10214%
cat("MAE = ",round(mean(abs(predict(PD_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03837%
### RESULTS: g0_Delinq_2_Ave also a great input to help model phi. Removing M_DTI_Growth (which was also significant) further improved the model
# results. Furthermore, we expect the coefficient direction of g0_Delinq_2_Ave to be negative when modelling phi, given that this implies high values of
# g0_Delinq_2_Ave will result in a higher Var(y_i)




# ------ 5. Finalised input space of the model
# --- Constant Phi
PP_Final_Cnst_Phi<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                      g0_Delinq_Any_Aggr_Prop_Lag_1+
                      M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(PP_Final_Cnst_Phi)
PP_Final_Cnst_Phi$pseudo.r.squared # Pseudo R2 = 0.6434349
AIC(PP_Final_Cnst_Phi) # AIC = -2005.661
cat("MAE = ",round(mean(abs(predict(PP_Final_Cnst_Phi,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10202%

# --- Dynamic Phi
PP_Final_Dyn_Phi<-betareg(Y_PerfToPerf~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop+
                        g0_Delinq_Any_Aggr_Prop_Lag_1+
                        M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                        M_RealGDP_Growth_9+M_RealGDP_Growth_12+
                        NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                        PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave , data=datAggr_train)
summary(PP_Final_Dyn_Phi)
PP_Final_Dyn_Phi$pseudo.r.squared # Pseudo R2 = 0.642424
AIC(PP_Final_Dyn_Phi) # AIC = -2008.403
cat("MAE = ",round(mean(abs(predict(PP_Final_Dyn_Phi,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),7)*100,"%",sep="","\n") # MAE = 0.10214%





Phi_Set <- as.data.table(colnames(datAggr_train)[17:127])
colnames(Phi_Set)<-"InputV_Phi"
Phi_Set[,R2 := {
  # Construct the formula string dynamically for each row
  formula_string <- paste("Y_PerfToPerf ~ g0_Delinq_Ave + g0_Delinq_1_Ave + g0_Delinq_Any_Aggr_Prop +",
                          "g0_Delinq_Any_Aggr_Prop_Lag_1 +",
                          "M_RealIncome_Growth_9 + M_RealIncome_Growth_12 +",
                          "M_RealGDP_Growth_9 + M_RealGDP_Growth_12 +",
                          "NewLoans_Aggr_Prop_3 + CreditLeverage_Aggr +",
                          "PerfSpell_Maturity_Aggr_Mean |", InputV_Phi)
  # Fit the betareg model and extract the pseudo R-squared value
  betareg(as.formula(formula_string), data = datAggr_train)$pseudo.r.squared
}, by = InputV_Phi]

Ordered_Phi_Set<-Phi_Set[order(R2, decreasing=TRUE),]

