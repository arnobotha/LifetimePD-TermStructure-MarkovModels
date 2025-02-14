# =================================== BETA REGRESSION MODEL: DEF-DEF =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Default to Write-off
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
plot(datAggr_train$Date,datAggr_train$Y_DefToWO,type="l",ylab="Transition proportions", xlab="Date",main="Default to Write-Off transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_DefToWO,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))

# - Ensure target variable is restricted to (0,1)
cat('Nr of y targets where y=0 is', sum(datAggr_train$Y_DefToWO==0),"\n") # 1 case
datAggr_train[,Y_DefToWO:=ifelse(Y_DefToWO==0,0.00001,Y_DefToWO)]

# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_DefToWO~Prev_DW,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.2011908 ***
AIC(Delinq_Model) # AIC = -1475.099


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.005605378
AIC(Delinq_Model) # AIC = -1413.488

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.008393413
AIC(Delinq_Model) # AIC = -1414.17

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01066302
AIC(Delinq_Model) # AIC = -1414.769

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01284079
AIC(Delinq_Model) # AIC = -1415.364

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.02148021
AIC(Delinq_Model) # AIC = -1417.863

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.03633441 ***
AIC(Delinq_Model) # AIC = -1422.233

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.05871232 ***
AIC(Delinq_Model) # AIC = -1428.87
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop_Lag_12 and g0_Delinq_Any_Aggr_Prop_Lag_9


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_DefToWO~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1277018
AIC(Delinq_Model) # AIC = -1457.777

Delinq_Model<-betareg(Y_DefToWO~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1313752
AIC(Delinq_Model) # AIC = -1459.826

Delinq_Model<-betareg(Y_DefToWO~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1347126
AIC(Delinq_Model) # AIC = -1461.634

Delinq_Model<-betareg(Y_DefToWO~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 =0.1466666 ***
AIC(Delinq_Model) # AIC = -1467.505

Delinq_Model<-betareg(Y_DefToWO~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1505787 ***
AIC(Delinq_Model) # AIC = -1469.744
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_12 and DefaultStatus1_Aggr_Prop_Lag_6


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.004052098
AIC(Delinq_Model) # AIC = -1413.123

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.003904084
AIC(Delinq_Model) # AIC = -1413.094

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01788031
AIC(Delinq_Model) # AIC = -1416.795

Delinq_Model<-betareg(Y_DefToWO~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1408532 ***
AIC(Delinq_Model) # AIC = -1460.581

Delinq_Model<-betareg(Y_DefToWO~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.03086083 ***
AIC(Delinq_Model) # AIC = -1420.626
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_3_Ave and CuringEvents_Aggr_Prop


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_DefToWO~ Prev_DW + g0_Delinq_Any_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_12 +
                             DefaultStatus1_Aggr_Prop_Lag_12 + DefaultStatus1_Aggr_Prop_Lag_6 +
                             g0_Delinq_3_Ave + CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2748481
AIC(Delinq_Model_Full) # AIC = -1505.714

# Remove g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_3_Ave, DefaultStatus1_Aggr_Prop_Lag_12 and g0_Delinq_Any_Aggr_Prop_Lag_12
Delinq_Model_Full<-betareg(Y_DefToWO~ Prev_DW +
                             DefaultStatus1_Aggr_Prop_Lag_6 +
                             CuringEvents_Aggr_Prop
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.2630963
AIC(Delinq_Model_Full) # AIC = -1508.278
### RESULTS: The previous month's transition rate is significant and has a positive coefficient. This result is similar to what was found for the other transitions
# that were modelled.

# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003555786
AIC(Macro_Model) # AIC = -1412.975

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.006181655
AIC(Macro_Model) # AIC = -1413.576

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.007664837
AIC(Macro_Model) # AIC = -1413.941

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01033836
AIC(Macro_Model) # AIC = -1414.597

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01761551
AIC(Macro_Model) # AIC = -1416.626

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03705944 ***
AIC(Macro_Model) # AIC = -1422.216

Macro_Model<-betareg(Y_DefToWO~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05343079 ***
AIC(Macro_Model) # AIC = -1427.554
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Repo_Rate_9 and M_Repo_Rate_12


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00128295
AIC(Macro_Model) # AIC = -1412.507

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.001008358
AIC(Macro_Model) # AIC = -1412.435

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0001990589
AIC(Macro_Model) # AIC = -1412.229

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00003615193
AIC(Macro_Model) # AIC = -1412.186

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.006957974
AIC(Macro_Model) # AIC = -1414.134

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0289935 ***
AIC(Macro_Model) # AIC = -1420.72

Macro_Model<-betareg(Y_DefToWO~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04287844 ***
AIC(Macro_Model) # AIC = -1426.232
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Inflation_Growth_12 and M_Inflation_Growth_9


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02287704
AIC(Macro_Model) # AIC = -1418.005

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02586452
AIC(Macro_Model) # AIC = -1418.896

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03380256
AIC(Macro_Model) # AIC = -1421.123

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04231568
AIC(Macro_Model) # AIC = -1423.511

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05163863
AIC(Macro_Model) # AIC = -1426.533

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.06153529 ***
AIC(Macro_Model) # AIC = -1429.897

Macro_Model<-betareg(Y_DefToWO~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08320458 ***
AIC(Macro_Model) # AIC = -1437.875
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth_9 and M_DTI_Growth_12


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00002225037
AIC(Macro_Model) # AIC = -1412.181

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0003022993
AIC(Macro_Model) # AIC = -1412.245

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0008063468
AIC(Macro_Model) # AIC = -1412.366

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00119135
AIC(Macro_Model) # AIC = -1412.46

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00000477035
AIC(Macro_Model) # AIC = -1412.177

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002120419 ***
AIC(Macro_Model) # AIC = -1412.669

Macro_Model<-betareg(Y_DefToWO~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003388501 ***
AIC(Macro_Model) # AIC = -1412.973
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth_9 and M_RealIncome_Growth_12


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009997786 ***
AIC(Macro_Model) # AIC = -1414.579

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.005286546 ***
AIC(Macro_Model) # AIC = -1413.471

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001401516
AIC(Macro_Model) # AIC = -1412.527

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.00006814556
AIC(Macro_Model) # AIC = -1412.193

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001348939
AIC(Macro_Model) # AIC = -1412.524

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003785089
AIC(Macro_Model) # AIC = -1413.132

Macro_Model<-betareg(Y_DefToWO~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002483864
AIC(Macro_Model) # AIC = -1412.787
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth and M_Emp_Growth_1


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002188003
AIC(Macro_Model) # AIC = -1412.644

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.001393762
AIC(Macro_Model) # AIC = -1412.482

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.000912146
AIC(Macro_Model) # AIC = -1412.384

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0006071181
AIC(Macro_Model) # AIC = -1412.317

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.003394399
AIC(Macro_Model) # AIC = -1412.956

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.009256708 ***
AIC(Macro_Model) # AIC = -1414.269

Macro_Model<-betareg(Y_DefToWO~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.01224369 ***
AIC(Macro_Model) # AIC = -1414.965
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth_12 and M_RealGDP_Growth_9


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_DefToWO~M_Repo_Rate_9 + M_Repo_Rate_12 + M_Inflation_Growth_12 + M_Inflation_Growth_9+
                                              M_DTI_Growth_9 + M_DTI_Growth_12 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12+
                                              M_Emp_Growth + M_Emp_Growth_1 + M_RealGDP_Growth_12 + M_RealGDP_Growth_9,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1508713
AIC(Macro_Model_Full) # AIC = -1439.954

# - Remove M_RealGDP_Growth_9, M_RealGDP_Growth_12
Macro_Model_Full<-betareg(Y_DefToWO~M_Repo_Rate_9 + M_Repo_Rate_12 + M_Inflation_Growth_12 + M_Inflation_Growth_9+
                                              M_DTI_Growth_9 + M_DTI_Growth_12 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12+
                                              M_Emp_Growth + M_Emp_Growth_1,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1514986
AIC(Macro_Model_Full) # AIC = -1443.385

# - Remove  M_RealIncome_Growth_9, M_RealIncome_Growth_12
Macro_Model_Full<-betareg(Y_DefToWO~M_Repo_Rate_9 + M_Repo_Rate_12 + M_Inflation_Growth_12 + M_Inflation_Growth_9+
                                              M_DTI_Growth_9 + M_DTI_Growth_12+
                                              M_Emp_Growth + M_Emp_Growth_1,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1515398
AIC(Macro_Model_Full) # AIC = -1446.506

# - Remove M_Emp_Growth_1, M_Inflation_Growth_9 and M_Inflation_Growth_12
Macro_Model_Full<-betareg(Y_DefToWO~M_Repo_Rate_9 + M_Repo_Rate_12+
                                              M_DTI_Growth_9 + M_DTI_Growth_12+
                                              M_Emp_Growth,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.1464024
AIC(Macro_Model_Full) # AIC = -1449.193
### RESULTS: The repo rate and DTI growth has a lagged effect on the D to W transition rate, whereas the employment
# growth have a more immediate effect on the transition rate. Moreover, employment growth is the only macroeconomic variable that 
# doesn't have a lagged counterpart also included in the final macroeconomic model.


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.09597667
AIC(Aggr_IR_Model) # AIC = -1441.426

Aggr_IR_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.09773059
AIC(Aggr_IR_Model) # AIC = -1442.08

Aggr_IR_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.1002803 ***
AIC(Aggr_IR_Model) # AIC = -1443.011

Aggr_IR_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.09882392
AIC(Aggr_IR_Model) # AIC = -1442.576

Aggr_IR_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.1043393 ***
AIC(Aggr_IR_Model) # AIC = -1445.244

Aggr_IR_Model<-betareg(Y_DefToWO~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.09053946
AIC(Aggr_IR_Model) # AIC = -1437.689
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med_9 and InterestRate_Margin_Aggr_Med_2


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_DefToWO~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.04209324 ***
AIC(Aggr_NewLoans) # AIC = -1427.615

Aggr_NewLoans<-betareg(Y_DefToWO~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.03284106
AIC(Aggr_NewLoans) # AIC = -1423.781

Aggr_NewLoans<-betareg(Y_DefToWO~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.02953095
AIC(Aggr_NewLoans) # AIC = -1423.561

Aggr_NewLoans<-betareg(Y_DefToWO~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.05889845 ***
AIC(Aggr_NewLoans) # AIC = -1432.769

Aggr_NewLoans<-betareg(Y_DefToWO~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.03987811
AIC(Aggr_NewLoans) # AIC = -1427.139
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_4 and NewLoans_Aggr_Prop


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_DefToWO~InterestRate_Margin_Aggr_Med_9 + InterestRate_Margin_Aggr_Med_2+
                           NewLoans_Aggr_Prop_4 + NewLoans_Aggr_Prop+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.2521232
AIC(Aggr_Full_Model) # AIC = -1493.372

# - Remove NewLoans_Aggr_Prop, InterestRate_Margin_Aggr_Med_9,  InterestRate_Margin_Aggr_Med_2 and ArrearsToBalance_Aggr_Prop
Aggr_Full_Model<-betareg(Y_DefToWO~NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.2080404
AIC(Aggr_Full_Model) # AIC = -1481.609
### RESULTS: The general portfolio level theme leads to a model with a better goodness-of-fit than the macroeconomic model for the D to W transition rate.


# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+M_Repo_Rate_9 + M_Repo_Rate_12+
                      M_DTI_Growth_9 + M_DTI_Growth_12+M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3189135
AIC(DW_BR_Full) # AIC = -1513.587

# - Remove M_Repo_Rate_12
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+M_Repo_Rate_9+
                      M_DTI_Growth_9 + M_DTI_Growth_12+M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3190236
AIC(DW_BR_Full) # AIC = -1515.582

# - Remove M_DTI_Growth_9
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+M_Repo_Rate_9+
                      M_DTI_Growth_12+M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3196942
AIC(DW_BR_Full) # AIC = -1517.468

# - Remove M_Repo_Rate_9
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                      M_DTI_Growth_12+M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3194283
AIC(DW_BR_Full) # AIC = -1518.873

# - Remove M_DTI_Growth_12
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                      M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3152305
AIC(DW_BR_Full) # AIC = -1519.093

# - Remove InstalmentToBalance_Aggr_Prop
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                      M_Emp_Growth +
                      NewLoans_Aggr_Prop_4+CreditLeverage_Aggr+
                      CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3152305
AIC(DW_BR_Full) # AIC = -1518.873

# - Remove NewLoans_Aggr_Prop_4
DW_BR_Full<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                      M_Emp_Growth+CreditLeverage_Aggr+
                      CuringEvents_Aggr_Prop+
                      AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_BR_Full)
DW_BR_Full$pseudo.r.squared # Pseudo R2 = 0.3025425
AIC(DW_BR_Full) # AIC = -1515.571
### RESULTS: The general portfolio level themed variables make up the majority of the input space. There seems to be latent interaction effects
# between the covariates.


# ------ 4. Modelling themes | Phi
string1<-"Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                      M_Emp_Growth+CreditLeverage_Aggr+
                      CuringEvents_Aggr_Prop+
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
# M_RealGDP_Growth_3, DefaultStatus1_Aggr_Prop_Lag_1, M_DTI_Growth_12

# - Start of with the 3 best phi inputs
DW_Phi<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                  M_Emp_Growth+CreditLeverage_Aggr+
                  CuringEvents_Aggr_Prop+
                  AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean| M_RealGDP_Growth_3+DefaultStatus1_Aggr_Prop_Lag_1+M_DTI_Growth_12, data=datAggr_train)
summary(DW_Phi)
DW_Phi$pseudo.r.squared # Pseudo R2 = 0.3044386
AIC(DW_Phi) # AIC = -1521.058

# - Remove intercept
DW_Phi<-betareg(Y_DefToWO~ Prev_DW+DefaultStatus1_Aggr_Prop_Lag_6+CuringEvents_Aggr_Prop+
                  M_Emp_Growth+CreditLeverage_Aggr+
                  CuringEvents_Aggr_Prop+
                  AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean| -1 + M_RealGDP_Growth_3+DefaultStatus1_Aggr_Prop_Lag_1+M_DTI_Growth_12, data=datAggr_train)
summary(DW_Phi)
DW_Phi$pseudo.r.squared # Pseudo R2 = 0.3044324
AIC(DW_Phi) # AIC = -1523.041
# RESULTS: This is the only one of the 6 BR models where the best input space for modelling phi is more than one covariate.


# ------ 5. Finalised input space of the model
# --- Constant Phi
DW_Final_Cnst_Phi<-betareg(Y_DefToWO ~ Prev_DW + DefaultStatus1_Aggr_Prop_Lag_6 + CuringEvents_Aggr_Prop +
                             M_Emp_Growth + CreditLeverage_Aggr + CuringEvents_Aggr_Prop +
                             AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean, data=datAggr_train)
summary(DW_Final_Cnst_Phi)
DW_Final_Cnst_Phi$pseudo.r.squared # Pseudo R2 = 0.3025425
AIC(DW_Final_Cnst_Phi) # AIC = -1959.108


# --- Dynamic Phi
DW_Final_Dyn_Phi<-betareg(Y_DefToWO ~ Prev_DW + DefaultStatus1_Aggr_Prop_Lag_6 + CuringEvents_Aggr_Prop +
                            M_Emp_Growth + CreditLeverage_Aggr + CuringEvents_Aggr_Prop + AgeToTerm_Aggr_Mean + 
                            PerfSpell_Maturity_Aggr_Mean | -1 + M_RealGDP_Growth_3 + DefaultStatus1_Aggr_Prop_Lag_1 + M_DTI_Growth_12, data=datAggr_train)
summary(DW_Final_Dyn_Phi)
DW_Final_Dyn_Phi$pseudo.r.squared # Pseudo R2 = 0.3044324
AIC(DW_Final_Dyn_Phi) # AIC = -1515.571
### RESULTS: The dynamic phi model has a better Pseudo R2 and AIC than the constant phi model. We therefore use the dynamic phi model as our model to tweak
# going forward.


# --- Final
DW_Final<-betareg(Y_DefToWO ~ Prev_DW + DefaultStatus1_Aggr_Prop_Lag_6 + CuringEvents_Aggr_Prop +
                    M_Emp_Growth + CreditLeverage_Aggr + CuringEvents_Aggr_Prop + AgeToTerm_Aggr_Mean + 
                    PerfSpell_Maturity_Aggr_Mean | -1 + M_RealGDP_Growth_3 + DefaultStatus1_Aggr_Prop_Lag_1 + M_DTI_Growth_12, data=datAggr_train)
summary(DW_Final)
DW_Final$pseudo.r.squared # Pseudo R2 = 0.3044324
AIC(DW_Final) # AIC = -1523.041
cat("MAE = ",round(mean(abs(predict(DW_Final,datAggr_valid)-datAggr_valid$Y_DefToWO)),7)*100,"%",sep="","\n") # MAE = 0.36861%


# - Link function on final mu input space
link_func_stats<-rbind(sapply(c("logit", "probit", "cloglog", "loglog"), function(x) AIC(update(DW_Final, link = x))),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) round(mean(abs(predict(update(DW_Final, link = x),datAggr_valid)-datAggr_valid$Y_DefToWO)),7)*100),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) update(DW_Final, link = x)$pseudo.r.squared))
rownames(link_func_stats)<-c("AIC","MAE","Pseudo R^2")
link_func_stats
optimal_link<-"loglog"
### RESULTS - Ranked links based on Pseudo R2 for links (similar results hold for other measures):
# 1) loglog; 2) probit; 3) logit; cloglog
# Results are quite similar as the range of the pseudo r2 is [0.3044324,0.3910874]

# - Update link function
DW_Final <- betareg(Y_DefToWO ~ Prev_DW + DefaultStatus1_Aggr_Prop_Lag_6 + CuringEvents_Aggr_Prop +
                    M_Emp_Growth + CreditLeverage_Aggr + CuringEvents_Aggr_Prop + AgeToTerm_Aggr_Mean +
                    PerfSpell_Maturity_Aggr_Mean | -1 + M_RealGDP_Growth_3 + DefaultStatus1_Aggr_Prop_Lag_1 + M_DTI_Growth_12, 
                  data=datAggr_train, link=optimal_link)
summary(DW_Final)
DW_Final$pseudo.r.squared # Pseudo R2 = 0.3910887
AIC(DW_Final) # AIC = -1524.5
cat("MAE = ",round(mean(abs(predict(DW_Final,datAggr_valid)-datAggr_valid$Y_DefToWO)),7)*100,"%",sep="","\n") # MAE = 0.37035%




# ------ 6. Cooks Distance Adjustment
# - Interactive runs are done to observe the best combinations of observations to leave out
# Cooks Distance Plot
plot(DW_Final, which = 2, type = "pearson",xlab="obs")
# Obtain observations with the biggest CD values
sort(round(cooks.distance(DW_Final),4))
# Specify training points to remove
Leave_Out<-c(65)
# Retrain model on new training set
DW_Adj<-update(DW_Final,subset=-Leave_Out)
cat("Pseudo R^2 before adjustment = ",DW_Final$pseudo.r.squared," --- ","Pseudo R^2 after adjustment = ",DW_Adj$pseudo.r.squared,"\n",sep="")
# Plot
plot(datAggr_valid$Date,predict(DW_Adj,datAggr_valid),type="l",col="red",lwd=2,ylim=c(0,0.05),xlab="Date",ylab="Transition probability",main="Constant Phi after Cooks adjustment")
lines(datAggr_valid$Date,as.numeric(datAggr_valid$Y_DefToWO),type="l")
MAEval<-round(mean(abs(predict(DW_Adj,datAggr_valid)-as.numeric(datAggr_valid$Y_DefToWO))),7)*100
legend(x="topright",paste("MAE = ",MAEval,"%"))
cat("MAE of Cooks Distance adjusted model= ",MAEval,"%","\n",sep="")
### RESULTS: Cooks distance adjustment improved the model fit:
# Pseudo R2 before CD = 0.3910887; After CD = 0.3963285

# --- Save Model
DW_Final<-DW_Adj
pack.ffdf(paste0(genObjPath,"BR_D_To_W"), DW_Final)