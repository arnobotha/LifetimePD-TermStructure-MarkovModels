# =================================== BETA REGRESSION MODEL: DEF-DEF =================================
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
plot(datAggr_train$Date,datAggr_train$Y_DefToDef,type="l",ylab="Transition proportions", xlab="Date",main="Performance to performance transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_DefToDef,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))




# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_DefToDef~Prev_DD,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.001158885
AIC(Delinq_Model) # AIC = -1208.818


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02462205
AIC(Delinq_Model) # AIC = -1213.319

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0294279 ***
AIC(Delinq_Model) # AIC = -1214.248

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02871259 ***
AIC(Delinq_Model) # AIC = -1214.093

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02605927
AIC(Delinq_Model) # AIC = -1213.551

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01247089
AIC(Delinq_Model) # AIC = -1210.964

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.005899623
AIC(Delinq_Model) # AIC = -1209.744

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.001944933
AIC(Delinq_Model) # AIC = -1209
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop_Lag_1 and g0_Delinq_Any_Aggr_Prop_Lag_2


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_DefToDef~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.000009466048
AIC(Delinq_Model) # AIC = -1208.63

Delinq_Model<-betareg(Y_DefToDef~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.001154208
AIC(Delinq_Model) # AIC = -1208.856

Delinq_Model<-betareg(Y_DefToDef~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.00422545
AIC(Delinq_Model) # AIC = -1209.47

Delinq_Model<-betareg(Y_DefToDef~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01633771 ***
AIC(Delinq_Model) # AIC = -1211.982

Delinq_Model<-betareg(Y_DefToDef~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.06743802 ***
AIC(Delinq_Model) # AIC = -1222.569
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_12 and DefaultStatus1_Aggr_Prop_Lag_6


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02802763
AIC(Delinq_Model) # AIC = -1214.001

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01714926
AIC(Delinq_Model) # AIC = -1211.877

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.07273778 ***
AIC(Delinq_Model) # AIC = -1222.876

Delinq_Model<-betareg(Y_DefToDef~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.007345528
AIC(Delinq_Model) # AIC = -1210.078

Delinq_Model<-betareg(Y_DefToDef~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.05311064 ***
AIC(Delinq_Model) # AIC = -1219.475
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_2_Ave and CuringEvents_Aggr_Prop


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_DefToDef~ Prev_DD + g0_Delinq_Any_Aggr_Prop_Lag_1 + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                             DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                             g0_Delinq_2_Ave + CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2810744
AIC(Delinq_Model_Full) # AIC = -1260.281

# Remove Prev_DD, g0_Delinq_Any_Aggr_Prop_Lag_1, g0_Delinq_Any_Aggr_Prop_Lag_2 g0_Delinq_2_Ave
Delinq_Model_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                              CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1990574
AIC(Delinq_Model_Full) # AIC = -1248.41
### RESULTS: 
#
#

# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0002471133
AIC(Macro_Model) # AIC = -1208.672

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0000349655
AIC(Macro_Model) # AIC = -1208.634

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0007283011
AIC(Macro_Model) # AIC = -1208.758

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002356348
AIC(Macro_Model) # AIC = -1209.052

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008403981 ***
AIC(Macro_Model) # AIC = -1210.168

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.005730257
AIC(Macro_Model) # AIC = -1209.701

Macro_Model<-betareg(Y_DefToDef~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00604185 ***
AIC(Macro_Model) # AIC = -1209.784
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Repo_Rate_6 and M_Repo_Rate_12


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009346494
AIC(Macro_Model) # AIC = -1210.343

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.01238121
AIC(Macro_Model) # AIC = -1210.882

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01291621
AIC(Macro_Model) # AIC = -1210.98

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01317526 ***
AIC(Macro_Model) # AIC = -1211.015

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01752671 ***
AIC(Macro_Model) # AIC = -1211.798

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003548661
AIC(Macro_Model) # AIC = -1209.266

Macro_Model<-betareg(Y_DefToDef~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001415372
AIC(Macro_Model) # AIC = -1208.896
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Inflation_Growth_3 and M_Inflation_Growth_6


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01248877
AIC(Macro_Model) # AIC = -1211.054

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008781478 ***
AIC(Macro_Model) # AIC = -1210.332

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.004229222 ***
AIC(Macro_Model) # AIC = -1209.442

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001572726
AIC(Macro_Model) # AIC = -1208.928

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001633149
AIC(Macro_Model) # AIC = -1208.938

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0009916532
AIC(Macro_Model) # AIC = -1208.814

Macro_Model<-betareg(Y_DefToDef~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.007813186
AIC(Macro_Model) # AIC = -1210.097
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth_1 and M_DTI_Growth_2


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02426125 ***
AIC(Macro_Model) # AIC = -1213.142

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0100851
AIC(Macro_Model) # AIC = -1210.511

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002301134
AIC(Macro_Model) # AIC = -1209.063

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00004934425
AIC(Macro_Model) # AIC = -1208.637

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00583114
AIC(Macro_Model) # AIC = -1209.774

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009229938
AIC(Macro_Model) # AIC = -1210.439

Macro_Model<-betareg(Y_DefToDef~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01449624 ***
AIC(Macro_Model) # AIC = -1211.455
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth and M_RealIncome_Growth_12


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06547958 ***
AIC(Macro_Model) # AIC = -1220.715

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03881825 ***
AIC(Macro_Model) # AIC = -1215.827

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01714484
AIC(Macro_Model) # AIC = -1211.822

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.005425811
AIC(Macro_Model) # AIC = -1209.642

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003092014
AIC(Macro_Model) # AIC = -1209.222

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00975265
AIC(Macro_Model) # AIC = -1210.486

Macro_Model<-betareg(Y_DefToDef~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02619687
AIC(Macro_Model) # AIC = -1213.695
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth and M_Emp_Growth_1


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03295484 ***
AIC(Macro_Model) # AIC = -1214.637

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01780901 ***
AIC(Macro_Model) # AIC = -1211.883

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.007189446
AIC(Macro_Model) # AIC = -1209.953

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001903115
AIC(Macro_Model) # AIC = -1208.981

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001879172
AIC(Macro_Model) # AIC = -1208.985

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00441843
AIC(Macro_Model) # AIC = -1209.467

Macro_Model<-betareg(Y_DefToDef~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.006406755
AIC(Macro_Model) # AIC = -1209.842
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth and M_RealGDP_Growth_1


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_DefToDef~M_Repo_Rate_6 + M_Repo_Rate_12 + M_Inflation_Growth_3 + M_Inflation_Growth_6+
                            M_DTI_Growth_1 + M_DTI_Growth_2 + M_RealIncome_Growth + M_RealIncome_Growth_12+
                            M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth + M_RealGDP_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.248569
AIC(Macro_Model_Full) # AIC = -1236.273

# - Remove M_Repo_Rate_6, M_RealIncome_Growth_12
Macro_Model_Full<-betareg(Y_DefToDef~ M_Repo_Rate_12 + M_Inflation_Growth_3 + M_Inflation_Growth_6+
                            M_DTI_Growth_1 + M_DTI_Growth_2 + M_RealIncome_Growth+
                            M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth + M_RealGDP_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2482531
AIC(Macro_Model_Full) # AIC = -1240.05

# - M_DTI_Growth_1 and M_DTI_Growth_2 
Macro_Model_Full<-betareg(Y_DefToDef~ M_Repo_Rate_12 + M_Inflation_Growth_3 + M_Inflation_Growth_6+
                            M_RealIncome_Growth+
                            M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth + M_RealGDP_Growth_1
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2024412
AIC(Macro_Model_Full) # AIC = -1235.086

# - M_Emp_Growth_1 and  M_RealGDP_Growth_1
Macro_Model_Full<-betareg(Y_DefToDef~ M_Repo_Rate_12 + M_Inflation_Growth_3 + M_Inflation_Growth_6+
                            M_RealIncome_Growth+
                            M_Emp_Growth  + M_RealGDP_Growth
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1619485
AIC(Macro_Model_Full) # AIC = -1231.484

# - M_Inflation_Growth_6
Macro_Model_Full<-betareg(Y_DefToDef~ M_Repo_Rate_12 + M_Inflation_Growth_3 +
                            M_RealIncome_Growth+
                            M_Emp_Growth  + M_RealGDP_Growth
                          ,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.156236
AIC(Macro_Model_Full) # AIC = -1232.202
### RESULTS:
#
#
#


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.009268354
AIC(Aggr_IR_Model) # AIC = -1210.41

Aggr_IR_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.009483165
AIC(Aggr_IR_Model) # AIC = -1210.452

Aggr_IR_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01008596
AIC(Aggr_IR_Model) # AIC = -1210.566

Aggr_IR_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 =  0.01166321
AIC(Aggr_IR_Model) # AIC = -1210.87

Aggr_IR_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01570294 ***
AIC(Aggr_IR_Model) # AIC = -1211.66

Aggr_IR_Model<-betareg(Y_DefToDef~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.01322869 ***
AIC(Aggr_IR_Model) # AIC = -1211.143
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med_9 and Ave_Margin_Aggr


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_DefToDef~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.0004267222
AIC(Aggr_NewLoans) # AIC = -1208.706

Aggr_NewLoans<-betareg(Y_DefToDef~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.0001811567
AIC(Aggr_NewLoans) # AIC = -1208.66

Aggr_NewLoans<-betareg(Y_DefToDef~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.0009183099
AIC(Aggr_NewLoans) # AIC = -1208.801

Aggr_NewLoans<-betareg(Y_DefToDef~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.01765617 ***
AIC(Aggr_NewLoans) # AIC = -1211.801

Aggr_NewLoans<-betareg(Y_DefToDef~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.006649233 ***
AIC(Aggr_NewLoans) # AIC = -1209.91
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_4 and NewLoans_Aggr_Prop_5


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_DefToDef~InterestRate_Margin_Aggr_Med_9 + Ave_Margin_Aggr+
                           NewLoans_Aggr_Prop_4+NewLoans_Aggr_Prop_5+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.2855356
AIC(Aggr_Full_Model) # AIC = -1252.05

# Remove CreditLeverage_Aggr AgeToTerm_Aggr_Mean NewLoans_Aggr_Prop_4 NewLoans_Aggr_Prop_5 PerfSpell_Maturity_Aggr_Mean InterestRate_Margin_Aggr_Med_9
Aggr_Full_Model<-betareg(Y_DefToDef~ Ave_Margin_Aggr+
                           ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.2427593
AIC(Aggr_Full_Model) # AIC = -1253.557




# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 + M_Repo_Rate_12 + M_Inflation_Growth_3 +
                      M_RealIncome_Growth + M_Emp_Growth  + M_RealGDP_Growth +
                      Ave_Margin_Aggr + ArrearsToBalance_Aggr_Prop +
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop, data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3446756
AIC(DD_BR_Full) # AIC = -1268.218
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.80621%

# Remove  M_RealGDP_Growth
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                      M_Repo_Rate_12 + M_Inflation_Growth_3 +
                      M_RealIncome_Growth + M_Emp_Growth +
                      Ave_Margin_Aggr + ArrearsToBalance_Aggr_Prop +
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop, data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3440221
AIC(DD_BR_Full) # AIC = -1269.739
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.80557%

# Remove M_RealIncome_Growth
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                      CuringEvents_Aggr_Prop + M_Repo_Rate_12 + M_Inflation_Growth_3 +
                      M_Emp_Growth +
                      Ave_Margin_Aggr + ArrearsToBalance_Aggr_Prop +
                      InstalmentToBalance_Aggr_Prop, data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3437055
AIC(DD_BR_Full) # AIC = -1271.684
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.80464%

# Remove Ave_Margin_Aggr
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                      M_Repo_Rate_12 + M_Inflation_Growth_3 +
                      M_Emp_Growth +
                      ArrearsToBalance_Aggr_Prop +
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop, data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.337089
AIC(DD_BR_Full) # AIC = -1272.093
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.80805%

# Remove M_Inflation_Growth_3
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                      CuringEvents_Aggr_Prop + M_Repo_Rate_12 +
                      M_Emp_Growth +
                      ArrearsToBalance_Aggr_Prop +
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop, data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3333937
AIC(DD_BR_Full) # AIC = -1273.416
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.81239%

# Remove InstalmentToBalance_Aggr_Prop
DD_BR_Full<-betareg(Y_DefToDef~ DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                      CuringEvents_Aggr_Prop + M_Repo_Rate_12 +
                      M_Emp_Growth +
                      ArrearsToBalance_Aggr_Prop , data=datAggr_train)
summary(DD_BR_Full)
DD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3251043
AIC(DD_BR_Full) # AIC = -1273.086
cat("MAE = ",round(mean(abs(predict(DD_BR_Full,datAggr_valid)-datAggr_valid$Y_DefToDef)),7)*100,"%",sep="","\n") # MAE = 0.81987%