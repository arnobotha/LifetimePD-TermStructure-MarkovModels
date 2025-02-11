# =================================== BETA REGRESSION MODEL: DEF-SET =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Default to Settlement
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
plot(datAggr_train$Date,datAggr_train$Y_DefToSet,type="l",ylab="Transition proportions", xlab="Date",main="Default to settlement transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_DefToSet,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))




# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_DefToSet~Prev_DS,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3483524 ***
AIC(Delinq_Model) # AIC = -1447.777


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.009510545 ***
AIC(Delinq_Model) # AIC = -1355.235

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.006470466
AIC(Delinq_Model) # AIC = -1354.574

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.004965394
AIC(Delinq_Model) # AIC = -1354.248

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.002469521
AIC(Delinq_Model) # AIC = -1353.757

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0001705077
AIC(Delinq_Model) # AIC = -1353.329

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0051507
AIC(Delinq_Model) # AIC = -1354.186

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.008834295 ***
AIC(Delinq_Model) # AIC = -1354.803
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop and g0_Delinq_Any_Aggr_Prop_Lag_12


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_DefToSet~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3512272
AIC(Delinq_Model) # AIC = -1438.786

Delinq_Model<-betareg(Y_DefToSet~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3605962 ***
AIC(Delinq_Model) # AIC = -1442.758

Delinq_Model<-betareg(Y_DefToSet~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3667394 ***
AIC(Delinq_Model) # AIC = -1445.744

Delinq_Model<-betareg(Y_DefToSet~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3524018
AIC(Delinq_Model) # AIC = -1443.132

Delinq_Model<-betareg(Y_DefToSet~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2196281
AIC(Delinq_Model) # AIC = -1402.575
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_2 and DefaultStatus1_Aggr_Prop_Lag_3


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01229852
AIC(Delinq_Model) # AIC = -1355.842

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0141956
AIC(Delinq_Model) # AIC = -1356.213

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.04579614
AIC(Delinq_Model) # AIC = -1362.13

Delinq_Model<-betareg(Y_DefToSet~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.3521223 ***
AIC(Delinq_Model) # AIC = -1438.776

Delinq_Model<-betareg(Y_DefToSet~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2582567 ***
AIC(Delinq_Model) # AIC = -1419.027
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_3_Ave and CuringEvents_Aggr_Prop


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_DefToSet~ Prev_DS + g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                             g0_Delinq_3_Ave + CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.483324
AIC(Delinq_Model_Full) # AIC = -1493.691

# Remove CuringEvents_Aggr_Prop g0_Delinq_Any_Aggr_Prop
Delinq_Model_Full<-betareg(Y_DefToSet~ Prev_DS + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                             g0_Delinq_3_Ave
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.4825346
AIC(Delinq_Model_Full) # AIC = -1497.23
### RESULTS: The previous D to S transition rate is a good predictor for the next month's transition rate, this is likely because it informs the general level of 
# transitions into settlement from default.


# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07875276 ***
AIC(Macro_Model) # AIC = -1368.286

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06631814 ***
AIC(Macro_Model) # AIC = -1365.573

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04799776
AIC(Macro_Model) # AIC = -1361.944

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03767234
AIC(Macro_Model) # AIC = -1359.908

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008106514
AIC(Macro_Model) # AIC = 0.008106514

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0001411793
AIC(Macro_Model) # AIC = -1353.321

Macro_Model<-betareg(Y_DefToSet~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009534666
AIC(Macro_Model) # AIC = -1354.725
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Repo_Rate and M_Repo_Rate_1


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008980804
AIC(Macro_Model) # AIC = -1354.896

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.0029912
AIC(Macro_Model) # AIC = -1353.817

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0002920834
AIC(Macro_Model) # AIC = -1353.35

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0003025094
AIC(Macro_Model) # AIC = -1353.35

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01177746
AIC(Macro_Model) # AIC = -1355.128

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04070381 ***
AIC(Macro_Model) # AIC = -1359.561

Macro_Model<-betareg(Y_DefToSet~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07047231 ***
AIC(Macro_Model) # AIC = -1364.241
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Inflation_Growth_12 and M_Inflation_Growth_9
# R2 is similar to that of the repo rate


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.004046665
AIC(Macro_Model) # AIC = -1354.111

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003876697
AIC(Macro_Model) # AIC = -1354.059

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.004720906
AIC(Macro_Model) # AIC = -1354.203

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.007887017
AIC(Macro_Model) # AIC = -1354.775

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01925533
AIC(Macro_Model) # AIC = -1356.737

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0328715 ***
AIC(Macro_Model) # AIC = -1358.926

Macro_Model<-betareg(Y_DefToSet~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06291662 ***
AIC(Macro_Model) # AIC = -1363.956
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth_12 and M_DTI_Growth_9
# R2 is slightly lower than the repo rate


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1488635 ***
AIC(Macro_Model) # AIC = -1386.964

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1331515 ***
AIC(Macro_Model) # AIC = -1383.561

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1275128
AIC(Macro_Model) # AIC = -1382.408

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1295277
AIC(Macro_Model) # AIC = -1383.119

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1067277
AIC(Macro_Model) # AIC = -1377.795

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06581769
AIC(Macro_Model) # AIC = -1367.154

Macro_Model<-betareg(Y_DefToSet~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04860336
AIC(Macro_Model) # AIC = -1363.24
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth and M_RealIncome_Growth_1
# R2 is higher than for the repo rate variables


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1372483 ***
AIC(Macro_Model) # AIC = -1381.26

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1183733 ***
AIC(Macro_Model) # AIC = -1377.612

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1037608
AIC(Macro_Model) # AIC = -1374.527

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.09522944
AIC(Macro_Model) # AIC = -1372.487

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06378844
AIC(Macro_Model) # AIC = -1365.723

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0400875
AIC(Macro_Model) # AIC = -1360.917

Macro_Model<-betareg(Y_DefToSet~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02849291
AIC(Macro_Model) # AIC = -1358.705
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth and M_Emp_Growth_1
# R2 is higher than for the repo rate variables


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08908118 ***
AIC(Macro_Model) # AIC = -1372.408

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.07509598 ***
AIC(Macro_Model) # AIC = -1369.348

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06806296
AIC(Macro_Model) # AIC = -1367.781

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.066846
AIC(Macro_Model) # AIC = -1367.523

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04565699
AIC(Macro_Model) # AIC = -1362.84

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02329367
AIC(Macro_Model) # AIC = -1357.843

Macro_Model<-betareg(Y_DefToSet~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01972899
AIC(Macro_Model) # AIC = -1357.104
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth and M_RealGDP_Growth_1
# R2 is slightly higher than for the repo rate variables

# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_DefToSet~ M_Repo_Rate + M_Repo_Rate_1 + M_Inflation_Growth_12 + M_Inflation_Growth_9 +
                            M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                            M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth + M_RealGDP_Growth_1 
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3811512
AIC(Macro_Model_Full) # AIC = -1434.043

# - Remove M_Repo_Rate and M_Inflation_Growth_12
Macro_Model_Full<-betareg(Y_DefToSet~ M_Repo_Rate_1 +  M_Inflation_Growth_9 +
                            M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                            M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth + M_RealGDP_Growth_1 
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3812151
AIC(Macro_Model_Full) # AIC = -1437.966

# - Remove M_RealGDP_Growth  M_RealGDP_Growth_1 
Macro_Model_Full<-betareg(Y_DefToSet~ M_Repo_Rate_1 +  M_Inflation_Growth_9 +
                            M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                            M_Emp_Growth + M_Emp_Growth_1 
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3820214
AIC(Macro_Model_Full) # AIC = -1441.59

# - Remove M_Emp_Growth and M_Emp_Growth_1
Macro_Model_Full<-betareg(Y_DefToSet~ M_Repo_Rate_1 +  M_Inflation_Growth_9 +
                            M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3778616
AIC(Macro_Model_Full) # AIC = -1444.864

# - Remove M_Inflation_Growth_9
Macro_Model_Full<-betareg(Y_DefToSet~ M_Repo_Rate_1 + 
                            M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.3689096
AIC(Macro_Model_Full) # AIC = -1444.269
### RESULTS: Both the DTI and real income growth macroeconomic variables has two variants in the final ,acroeconmic model. 
# These variables again seem to have some interactive effect given that the direction of the coefficient sign is positive for the one lag and 
# negative for the other. The repo rate lagged one month is significant without its original form (or another lagged variant thereof) included.


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01282025
AIC(Aggr_IR_Model) # AIC = -1355.511

Aggr_IR_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01389946
AIC(Aggr_IR_Model) # AIC = -1355.688

Aggr_IR_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01416921 ***
AIC(Aggr_IR_Model) # AIC = -1355.716

Aggr_IR_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01399389 ***
AIC(Aggr_IR_Model) # AIC = -1355.674

Aggr_IR_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01194827
AIC(Aggr_IR_Model) # AIC = -1355.284

Aggr_IR_Model<-betareg(Y_DefToSet~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.001080527
AIC(Aggr_IR_Model) # AIC = -1353.495
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med_2 and InterestRate_Margin_Aggr_Med_3


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_DefToSet~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1781244
AIC(Aggr_NewLoans) # AIC = -1388.103

Aggr_NewLoans<-betareg(Y_DefToSet~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1820889 ***
AIC(Aggr_NewLoans) # AIC = -1391.775

Aggr_NewLoans<-betareg(Y_DefToSet~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.188348 ***
AIC(Aggr_NewLoans) # AIC = -1396.469

Aggr_NewLoans<-betareg(Y_DefToSet~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1629141
AIC(Aggr_NewLoans) # AIC = -1387.032

Aggr_NewLoans<-betareg(Y_DefToSet~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.1694843
AIC(Aggr_NewLoans) # AIC = -1392.156
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_3 and NewLoans_Aggr_Prop_1


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_DefToSet~InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_3+
                           NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_1+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.4970207
AIC(Aggr_Full_Model) # AIC = -1489.862

# - Remove InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_2, NewLoans_Aggr_Prop_1, CuringEvents_Aggr_Prop and ArrearsToBalance_Aggr_Prop
Aggr_Full_Model<-betareg(Y_DefToSet~NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                           InstalmentToBalance_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.3692341
AIC(Aggr_Full_Model) # AIC = -1436.39
### RESULTS: The macroeconomic model vs the general portfolio level model have more or less the same fit statistics. InstalmentToBalance_Aggr_Prop has a large standard
# error relative to the rest of the covariates and might need to be removed towards obtaining a more robust model.

# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                      g0_Delinq_3_Ave +  M_Repo_Rate_1 + 
                      M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      NewLoans_Aggr_Prop_3+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5527834
AIC(DS_BR_Full) # AIC = -1507.705
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.4132%

# Remove NewLoans_Aggr_Prop_3
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                      g0_Delinq_3_Ave +  M_Repo_Rate_1 + 
                      M_DTI_Growth_12 + M_DTI_Growth_9 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5527267
AIC(DS_BR_Full) # AIC = -1509.662
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41413%

# Remove M_DTI_Growth_9 
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                      g0_Delinq_3_Ave +  M_Repo_Rate_1 + 
                      M_DTI_Growth_12 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5523418
AIC(DS_BR_Full) # AIC = -1511.607
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41366%

# Remove  M_Repo_Rate_1
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                      g0_Delinq_3_Ave + 
                      M_DTI_Growth_12 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5518548
AIC(DS_BR_Full) # AIC = -1513.526
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41425%

# Remove g0_Delinq_Any_Aggr_Prop_Lag_12
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS +
                      g0_Delinq_3_Ave + 
                      M_DTI_Growth_12 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5500201
AIC(DS_BR_Full) # AIC = -1515.211
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41601%

# Remove CreditLeverage_Aggr
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS +
                      g0_Delinq_3_Ave + 
                      M_DTI_Growth_12 + M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5479652
AIC(DS_BR_Full) # AIC = -1516.11
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41714%

# Remove M_DTI_Growth_12
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS +
                      g0_Delinq_3_Ave + 
                      M_RealIncome_Growth + M_RealIncome_Growth_1 +
                      InstalmentToBalance_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5462142
AIC(DS_BR_Full) # AIC = -1516.893
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41613%

# Remove InstalmentToBalance_Aggr_Prop
DS_BR_Full<-betareg(Y_DefToSet~Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1+
                      g0_Delinq_3_Ave + 
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_BR_Full)
DS_BR_Full$pseudo.r.squared # Pseudo R2 = 0.5390409
AIC(DS_BR_Full) # AIC = -1515.548
cat("MAE = ",round(mean(abs(predict(DS_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41804%
### RESULTS: The only macroeconomic covariate that made it to the final model is real income growth. There seems to be latent interaction effects
# between the covariates. Interestingly, the D to S model has a better goodness-of-fit than the P to S model, when looking at the pseudo R2.



# ------ 4. Modelling themes | Phi
string1<-"Y_DefToSet~Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1+
                      g0_Delinq_3_Ave + 
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
# g0_Delinq_Ave, g0_Delinq_Any_Aggr_Prop, M_Repo_Rate_3

# - Start off with the 3 best phi inputs
DS_Phi<-betareg(Y_DefToSet~Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1+
                      g0_Delinq_3_Ave + 
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|g0_Delinq_Ave+g0_Delinq_Any_Aggr_Prop+M_Repo_Rate_3, data=datAggr_train)
summary(DS_Phi)
DS_Phi$pseudo.r.squared # Pseudo R2 = 0.5325348
AIC(DS_Phi) # AIC = -1530.674
cat("MAE = ",round(mean(abs(predict(DS_Phi,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.42062%

# - Remove M_Repo_Rate_3
DS_Phi<-betareg(Y_DefToSet~Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1+
                  g0_Delinq_3_Ave + 
                  AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|g0_Delinq_Ave+g0_Delinq_Any_Aggr_Prop, data=datAggr_train)
summary(DS_Phi)
DS_Phi$pseudo.r.squared # Pseudo R2 = 0.5324038
AIC(DS_Phi) # AIC = -1532.107
cat("MAE = ",round(mean(abs(predict(DS_Phi,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.42051%

# - Remove g0_Delinq_Any_Aggr_Prop
DS_Phi<-betareg(Y_DefToSet~Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1+
                  g0_Delinq_3_Ave + 
                  AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean|g0_Delinq_Ave, data=datAggr_train)
summary(DS_Phi)
DS_Phi$pseudo.r.squared # Pseudo R2 = 0.5391357
AIC(DS_Phi) # AIC = -1516.948
cat("MAE = ",round(mean(abs(predict(DS_Phi,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41854%
### RESULTS: When modelling the phi parameter, the g0_Delinq_Ave covariate gives the best R2 value. It is observed that when we use a lot of variables as a starting point
# for the input space to phi, the algorithm fails to converge, hence we only investigate the three best single factor inputs.





# ------ 5. Finalised input space of the model
# --- Constant Phi
DS_Final_Cnst_Phi<-betareg(Y_DefToSet ~ Prev_DS + M_RealIncome_Growth + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                             AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DS_Final_Cnst_Phi)
DS_Final_Cnst_Phi$pseudo.r.squared # Pseudo R2 = 0.5390409
AIC(DS_Final_Cnst_Phi) # AIC = -1515.548
cat("MAE = ",round(mean(abs(predict(DS_Final_Cnst_Phi,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE =  0.41804%



# --- Dynamic Phi
DS_Final_Dyn_Phi<-betareg(Y_DefToSet ~ Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                            AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train)
summary(DS_Final_Dyn_Phi)
DS_Final_Dyn_Phi$pseudo.r.squared # Pseudo R2 = 0.5391357
AIC(DS_Final_Dyn_Phi) # AIC = -1516.948
cat("MAE = ",round(mean(abs(predict(DS_Final_Dyn_Phi,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE =  0.41854%
### RESULTS: The dynamic phi model has a better Pseudo R2 and AIC than the constant phi model. The MAE's are basically the same, hence we
# choose the dynamic phi model as our best.


# --- Final
DS_Final<-betareg(Y_DefToSet ~ Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                    AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train)
summary(DS_Final)
DS_Final$pseudo.r.squared # Pseudo R2 = 0.5391357
AIC(DS_Final) # AIC = -1516.948
cat("MAE = ",round(mean(abs(predict(DS_Final,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.41854%

# - Link function on final mu input space
link_func_stats<-rbind(sapply(c("logit", "probit", "cloglog", "loglog"), function(x) AIC(update(DS_Final, link = x))),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) round(mean(abs(predict(update(DS_Final, link = x),datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) update(DS_Final, link = x)$pseudo.r.squared))
rownames(link_func_stats)<-c("AIC","MAE","Pseudo R^2")
link_func_stats
optimal_link<-"loglog"
### RESULTS - Ranked links based on Pseudo R2 for links (similar results hold for other measures):
# 1) loglog; 2) probit; 3) logit; cloglog
# Results are quite similar as the range of the pseudo r2 is 0.5378441, 0.581419]

# - Update link function
DS_Final<-betareg(Y_DefToSet ~ Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                    AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train, link=optimal_link)
summary(DS_Final)
DS_Final$pseudo.r.squared # Pseudo R2 = 0.581419
AIC(DS_Final) # AIC = -1515.873
cat("MAE = ",round(mean(abs(predict(DS_Final,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.42069%



# ------ 6. Cooks Distance Adjustment
# - Interactive runs are done to observe the best combinations of observations to leave out
# Cooks Distance Plot
plot(DS_Final, which = 2, type = "pearson",xlab="obs")
# Obtain observations with the biggest CD values
sort(round(cooks.distance(DS_Final),4))
# Specify training points to remove
Leave_Out<-c(191)
# Retrain model on new training set
DS_Adj<-update(DS_Final,subset=-Leave_Out)
cat("Pseudo R^2 before adjustment = ",DS_Final$pseudo.r.squared," --- ","Pseudo R^2 after adjustment = ",DS_Adj$pseudo.r.squared,"\n",sep="")
# Plot
plot(datAggr_valid$Date,predict(DS_Adj,datAggr_valid),type="l",col="red",lwd=2,ylim=c(0.001,0.06),xlab="Date",ylab="Transition probability",main="Constant Phi after Cooks adjustment")
lines(datAggr_valid$Date,as.numeric(datAggr_valid$Y_DefToSet),type="l")
MAEval<-round(mean(abs(predict(DS_Adj,datAggr_valid)-as.numeric(datAggr_valid$Y_DefToSet))),7)*100
legend(x="topright",paste("MAE = ",MAEval,"%"))
cat("MAE of Cooks Distance adjusted model= ",MAEval,"%","\n",sep="") # MAE = 0.42294%
### RESULTS: Cooks distance adjustment improved the model fit:
# MAE before CD = 0.42069%; After CD = 0.42294%
# Pseudo R2 before CD = 0.581419; After CD = 0.6346869

# --- Save Model
DS_Final<-DS_Adj
pack.ffdf(paste0(genObjPath,"BR_D_To_S"), DS_Final)