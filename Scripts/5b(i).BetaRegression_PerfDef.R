# =================================== BETA REGRESSION MODEL: PERF-DEF =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Performing to Default
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
plot(datAggr_train$Date,datAggr_train$Y_PerfToDef,type="l",ylab="Transition proportions", xlab="Date",main="Performance to default transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_PerfToDef,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))




# ------ 2. Modelling themes | Mu
# --- Portfolio-level
# - Delinquency themed
Delinq_Model<-betareg(Y_PerfToDef~Prev_PD,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6085501
AIC(Delinq_Model) # AIC = -2248.076


# Comparison of lags in: g0_Delinq_Any_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6728586 ***
AIC(Delinq_Model) # AIC = -2295.275

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6495544 ***
AIC(Delinq_Model) # AIC = -2284.415

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6184806
AIC(Delinq_Model) # AIC = -2266.633

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.5860797
AIC(Delinq_Model) # AIC = -2245.511

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_9,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.5804986
AIC(Delinq_Model) # AIC = -2235.58

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Any_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.5458925
AIC(Delinq_Model) # AIC = -2215.17
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_Any_Aggr_Prop_Lag_1 and g0_Delinq_Any_Aggr_Prop_Lag_2


# Comparison of lags in: DefaultStatus1_Aggr_Prop
Delinq_Model<-betareg(Y_PerfToDef~DefaultStatus1_Aggr_Prop_Lag_1,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.0952838 ***
AIC(Delinq_Model) # AIC = -2095.553

Delinq_Model<-betareg(Y_PerfToDef~DefaultStatus1_Aggr_Prop_Lag_2,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.06681366 ***
AIC(Delinq_Model) # AIC = -2089.554

Delinq_Model<-betareg(Y_PerfToDef~DefaultStatus1_Aggr_Prop_Lag_3,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.04645225
AIC(Delinq_Model) # AIC = -2085.371

Delinq_Model<-betareg(Y_PerfToDef~DefaultStatus1_Aggr_Prop_Lag_6,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01098652
AIC(Delinq_Model) # AIC = -2078.244

Delinq_Model<-betareg(Y_PerfToDef~DefaultStatus1_Aggr_Prop_Lag_12,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.01937705
AIC(Delinq_Model) # AIC = -2079.946
### RESULTS: Based on the single factor model comparison the two best lags are:
# DefaultStatus1_Aggr_Prop_Lag_1 and DefaultStatus1_Aggr_Prop_Lag_2
# R2 much lower than g0_Delinq_Any_Aggr_Prop


# Comparison of mean delinquency levels: g0_Delinq
Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6821978 ***
AIC(Delinq_Model) # AIC = -2296.999

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_1_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.6351628
AIC(Delinq_Model) # AIC = -2273.591

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_2_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.7798485 ***
AIC(Delinq_Model) # AIC = -2371.328

Delinq_Model<-betareg(Y_PerfToDef~g0_Delinq_3_Ave,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.1684455
AIC(Delinq_Model) # AIC = -2111.693

Delinq_Model<-betareg(Y_PerfToDef~CuringEvents_Aggr_Prop,data=datAggr_train)
Delinq_Model$pseudo.r.squared # Pseudo R2 = 0.00269601
AIC(Delinq_Model) # AIC = -2076.607
### RESULTS: Based on the single factor model comparison the two best lags are:
# g0_Delinq_2_Ave and g0_Delinq_Ave
# R2 a bit higher than DefaultStatus1_Aggr_Prop_Lag


# - Combining best versions of the lags towards obtaining the most parsimonious model
Delinq_Model_Full<-betareg(Y_PerfToDef~ Prev_PD + g0_Delinq_Any_Aggr_Prop_Lag_1+g0_Delinq_Any_Aggr_Prop_Lag_2+
                             DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_Ave+g0_Delinq_2_Ave 
                             ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8224033
AIC(Delinq_Model_Full) # AIC = -2408.1

# Remove g0_Delinq_Ave, Prev_PD, g0_Delinq_Any_Aggr_Prop_Lag_2 and g0_Delinq_Any_Aggr_Prop_Lag_1
Delinq_Model_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                             g0_Delinq_2_Ave, data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8158307
AIC(Delinq_Model_Full) # AIC = -2411.391
### RESULTS: g0_Delinq_2_Ave is more significant than g0_Delinq_Ave, since the former represents 
# the proportion at delinquency level two, where greater levels thereof are 
# intuitively indicative of transitioning to default.
# Prev_PD, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1 is similarly discarded,
# since the risk information from Prev_PD is already embedded in g0_Delinq_2_Ave, while the latter two 
# variables had weak R2 values from the start.


# --- Macroeconomic-level
# Comparison of lags in: Repo rate
Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4000684
AIC(Macro_Model) # AIC = -2187.049

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4281664
AIC(Macro_Model) # AIC = -2197.604

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4561828 ***
AIC(Macro_Model) # AIC = -2208.446

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4694704 ***
AIC(Macro_Model) # AIC = -2213.044

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4543541 ***
AIC(Macro_Model) # AIC = -2201.871

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4292412
AIC(Macro_Model) # AIC = -2185.533

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4284043
AIC(Macro_Model) # AIC = -2179.45
### RESULTS: Based on the single factor model comparison the three best lags are:
# M_Repo_Rate_3, M_Repo_Rate_2 and M_Repo_Rate_6


# Comparison of lags in: Inflation
Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.3305069
AIC(Macro_Model) # AIC = -2162.722

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.3603861
AIC(Macro_Model) # AIC = -2171.1

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.3955963 ***
AIC(Macro_Model) # AIC = -2182.212

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4105961 ***
AIC(Macro_Model) # AIC = -2187.464

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.370894 ***
AIC(Macro_Model) # AIC = -2172.885

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2839135
AIC(Macro_Model) # AIC = -2139.424

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2005836
AIC(Macro_Model) # AIC = -2114.695
### RESULTS: Based on the single factor model comparison the three best lags are:
# M_Inflation_Growth_2, M_Inflation_Growth_3 and M_Inflation_Growth_6
# R2 a bit lower than for the repo rate


# Comparison of lags in: Debt to Income
Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.5298894 ***
AIC(Macro_Model) # AIC = -2215.935

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4816643 ***
AIC(Macro_Model) # AIC = -2197.705

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4511582
AIC(Macro_Model) # AIC = -2114.695

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4319762
AIC(Macro_Model) # AIC = -2189.007

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.441539
AIC(Macro_Model) # AIC = -2187.458

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4323245
AIC(Macro_Model) # AIC = -2183.526

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.310135
AIC(Macro_Model) # AIC = -2144.343
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_DTI_Growth and M_DTI_Growth_1
# R2 more or less similar than the repo rate, hence affordability is a good predictor for P to D transitions


# Comparison of lags in: Real Income Growth
Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002455657
AIC(Macro_Model) # AIC = -2076.54

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.008800571
AIC(Macro_Model) # AIC = -2077.694

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.01861681
AIC(Macro_Model) # AIC = -2079.488

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03069737
AIC(Macro_Model) # AIC = -2081.706

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08237212
AIC(Macro_Model) # AIC = -2081.706

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1611797 ***
AIC(Macro_Model) # AIC = -2108.816

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2684711 ***
AIC(Macro_Model) # AIC = -2137.445
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealIncome_Growth_12 and M_RealIncome_Growth_9
# There appears to be a linear relationship between lag order and R2


# Comparison of lags in: Employment Growth
Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.0007020733
AIC(Macro_Model) # AIC = -2076.226

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.004734697
AIC(Macro_Model) # AIC = -2076.955

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.01189863
AIC(Macro_Model) # AIC = -2078.27

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.02199064
AIC(Macro_Model) # AIC = -2080.164

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.05593202
AIC(Macro_Model) # AIC = -2087.005

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1099855 ***
AIC(Macro_Model) # AIC = -2098.469

Macro_Model<-betareg(Y_PerfToDef~M_Emp_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1773913 ***
AIC(Macro_Model) # AIC = -2112.099
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_Emp_Growth_12 and M_Emp_Growth_9
# There appears to be a linear relationship between lag order and R2


# Comparison of lags in: Real GDP Growth
Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03215402
AIC(Macro_Model) # AIC = -2082.298

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.04731634
AIC(Macro_Model) # AIC = -2085.366

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.06419555
AIC(Macro_Model) # AIC = -2088.815

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08016744
AIC(Macro_Model) # AIC = -2092.099

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1380261
AIC(Macro_Model) # AIC = -2112.099

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.21904 ***
AIC(Macro_Model) # AIC = -2104.729

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2974952 ***
AIC(Macro_Model) # AIC = -2148.367
### RESULTS: Based on the single factor model comparison the two best lags are:
# M_RealGDP_Growth_12 and M_RealGDP_Growth_9
# There appears to be a linear relationship between lag order and R2


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Repo_Rate_3+M_Repo_Rate_6+M_Inflation_Growth_2+M_Inflation_Growth_3+M_Inflation_Growth_6+
                       M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                       M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6908449
AIC(Macro_Model_Full) # AIC = -2288.299

# Remove M_Inflation_Growth_3
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Repo_Rate_3+M_Repo_Rate_6+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6908102
AIC(Macro_Model_Full) # AIC = -2290.287

# Remove M_Repo_Rate_3
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Repo_Rate_6+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6908798
AIC(Macro_Model_Full) # AIC = -2292.266

# Remove M_Repo_Rate_6
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_9+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6903012
AIC(Macro_Model_Full) # AIC = -2294.171

# Remove M_RealIncome_Growth_9
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6897679
AIC(Macro_Model_Full) # AIC = -2295.859

# Remove M_RealGDP_Growth_9
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6887006
AIC(Macro_Model_Full) # AIC = -2297.568

# Remove M_RealIncome_Growth_12
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+M_Inflation_Growth_6+
                            M_DTI_Growth+M_DTI_Growth_1+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6855891
AIC(Macro_Model_Full) # AIC = -2298.505

# Remove M_Inflation_Growth_6
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.6857029
AIC(Macro_Model_Full) # AIC = -2288.299
### RESULTS: Both the repo and inflation rate are significant in the final model and have positive coefficients
# as expected. Affordability (DTI) seems to be a strong predictor of the P to D transition rate. The real GDP and employment
# growth seems to have a longer lagged effect on defaulting behavior compared to the other macroeconomic variables.


# - General portfolio level variables
# General portfolio level sub-theme: Interest rate
Aggr_IR_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.3178974 ***
AIC(Aggr_IR_Model) # AIC = -2142.483

Aggr_IR_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med_1,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.3094378
AIC(Aggr_IR_Model) # AIC = -2140.28

Aggr_IR_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med_2,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.3019322
AIC(Aggr_IR_Model) # AIC = -2138.172

Aggr_IR_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med_3,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.2947336
AIC(Aggr_IR_Model) # AIC = -2142.483

Aggr_IR_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med_9,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.2372168
AIC(Aggr_IR_Model) # AIC = -2136.144

Aggr_IR_Model<-betareg(Y_PerfToDef~Ave_Margin_Aggr,data=datAggr_train)
Aggr_IR_Model$pseudo.r.squared # Pseudo R2 = 0.3207544 ***
AIC(Aggr_IR_Model) # AIC = -2145.665
### RESULTS: Based on the single factor model comparison the two best versions are:
# InterestRate_Margin_Aggr_Med and Ave_Margin_Aggr


# General portfolio level sub-theme: NewLoans_Aggr_Prop
Aggr_NewLoans<-betareg(Y_PerfToDef~NewLoans_Aggr_Prop,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.01121817
AIC(Aggr_NewLoans) # AIC = -2078.759

Aggr_NewLoans<-betareg(Y_PerfToDef~NewLoans_Aggr_Prop_1,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.000007243758
AIC(Aggr_NewLoans) # AIC = -2076.099

Aggr_NewLoans<-betareg(Y_PerfToDef~NewLoans_Aggr_Prop_3,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.02408323
AIC(Aggr_NewLoans) # AIC = -2080.748

Aggr_NewLoans<-betareg(Y_PerfToDef~NewLoans_Aggr_Prop_4,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.02556991 ***
AIC(Aggr_NewLoans) # AIC = -2080.954

Aggr_NewLoans<-betareg(Y_PerfToDef~NewLoans_Aggr_Prop_5,data=datAggr_train)
Aggr_NewLoans$pseudo.r.squared # Pseudo R2 = 0.03874277 ***
AIC(Aggr_NewLoans) # AIC = -2083.531
### RESULTS: Based on the single factor model comparison the two best versions are:
# NewLoans_Aggr_Prop_4 and NewLoans_Aggr_Prop_5, though these single factor models
# produce a low pseudo R2.


# - Full model of general portfolio level variables
Aggr_Full_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                           NewLoans_Aggr_Prop_4+NewLoans_Aggr_Prop_5+CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+
                           InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                           AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean
                           ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.6919601
AIC(Aggr_Full_Model) # AIC = -2298.34

# - Remove NewLoans_Aggr_Prop_4, NewLoans_Aggr_Prop_5, InstalmentToBalance_Aggr_Prop, CuringEvents_Aggr_Prop, AgeToTerm_Aggr_Mean and CreditLeverage_Aggr
Aggr_Full_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                           ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.6845527
AIC(Aggr_Full_Model) # AIC = -2304.097
### RESULTS: ArrearsToBalance_Aggr_Prop and Ave_Margin_Aggr seem to have a strong positive effect on P tot D transitions as expected. The 
# PerfSpell_Maturity_Aggr_Mean variable has a negative coefficient which is expected given that the larger the average performance spell age
# at a given date, the lower we expect the P to D transition rate to be. All these variables had strong one factor model results, hence why 
# they also made it to the final model of this theme.


# ---  Fusion step
# Combine insights mined from previous themes
# Process is run interactively in tweaking the final model
PD_BR_Full<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                      ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+M_Repo_Rate_2+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8581106
AIC(PD_BR_Full) # AIC = -2442.834
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03819%

# Remove M_RealGDP_Growth_12
PD_BR_Full<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                      CreditLeverage_Aggr+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+M_Repo_Rate_2+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8599235
AIC(PD_BR_Full) # AIC = -2444.054
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03824%

# Remove InterestRate_Margin_Aggr_Med
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                      ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+M_Repo_Rate_2+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8577103
AIC(PD_BR_Full) # AIC = -2445.577
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03789%

# Remove PerfSpell_Maturity_Aggr_Mean
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                      ArrearsToBalance_Aggr_Prop+M_Repo_Rate_2+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8572009
AIC(PD_BR_Full) # AIC = -2446.538
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03799%

# Remove ArrearsToBalance_Aggr_Prop because:
# std error is too high and results doesn't change significantly
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate_2+
                      M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8529192
AIC(PD_BR_Full) # AIC = -2442.483
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%

# - Last interactive checks on the repo rate given its importance in default risk modelling
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+
                      M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_BR_Full) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%
### RESULTS: The delinquency and macroeconomic themed variables make up the majority of the input space. Both the repo and inflation rate 
# are significant in the final model, which corroborates previous research that these variables are useful in credit risk models. g0_Delinq_2_Ave 
# had the strongest one factor model and has a strong possitive relationship to the P to D transition rate. There seems to be latent interaction effects
# between the covariates as some of the coefficients have different directions for the same (but differently lagged) covariate.




# ------ 3. Finalised input space of the model | Mu
PD_BR_Final_Mu<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                      M_Repo_Rate+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_BR_Final_Mu)
PD_BR_Final_Mu$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_BR_Final_Mu) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_BR_Final_Mu,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%




# ------ 4. Modelling themes | Phi
string1<-"Y_PerfToDef~Ave_Margin_Aggr+
                      M_Repo_Rate+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave|"
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
# g0_Delinq_2_Ave, NewLoans_Aggr_Prop_5, NewLoans_Aggr_Prop_3

# - Start off with the 3 best phi inputs
PD_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                  M_Repo_Rate+M_Inflation_Growth_2+
                  M_DTI_Growth+M_DTI_Growth_1+
                  M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                  g0_Delinq_2_Ave| g0_Delinq_2_Ave+NewLoans_Aggr_Prop_5+NewLoans_Aggr_Prop_3, data=datAggr_train)
summary(PD_Phi)
PD_Phi$pseudo.r.squared # Pseudo R2 = 0.8545452
AIC(PD_Phi) # AIC = -2450.682
cat("MAE = ",round(mean(abs(predict(PD_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03818%

# - Remove g0_Delinq_2_Ave
PD_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                  M_Repo_Rate+M_Inflation_Growth_2+
                  M_DTI_Growth+M_DTI_Growth_1+
                  M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                  g0_Delinq_2_Ave|NewLoans_Aggr_Prop_5+NewLoans_Aggr_Prop_3, data=datAggr_train)
summary(PD_Phi)
PD_Phi$pseudo.r.squared # Pseudo R2 = 0.8545452
AIC(PD_Phi) # AIC = -2450.682
cat("MAE = ",round(mean(abs(predict(PD_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03818%

# vs

# - Remove NewLoans_Aggr_Prop_5 and NewLoans_Aggr_Prop_3
PD_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                  M_Repo_Rate+M_Inflation_Growth_2+
                  M_DTI_Growth+M_DTI_Growth_1+
                  M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                  g0_Delinq_2_Ave|g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Phi)
PD_Phi$pseudo.r.squared # Pseudo R2 = 0.8550763
AIC(PD_Phi) # AIC = -2446.353
cat("MAE = ",round(mean(abs(predict(PD_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03837%
### RESULTS: g0_Delinq_2_Ave is also a great input to model phi. Removing M_DTI_Growth (which was also significant) further improved the model
# results. Furthermore, we expect the coefficient direction of g0_Delinq_2_Ave to be negative when modelling phi, given that this implies high values of
# g0_Delinq_2_Ave will result in a higher Var(y_i)




# ------ 5. Finalised input space of the model
# --- Constant Phi
PD_Final_Cnst_Phi<-betareg(Y_PerfToDef ~ Ave_Margin_Aggr + M_Repo_Rate + M_Inflation_Growth_2 +
                        M_DTI_Growth + M_DTI_Growth_1 + M_Emp_Growth_9 + M_Emp_Growth_12 + DefaultStatus1_Aggr_Prop_Lag_1 +
                        DefaultStatus1_Aggr_Prop_Lag_2 + g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Cnst_Phi)
PD_Final_Cnst_Phi$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_Final_Cnst_Phi) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_Final_Cnst_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%


# --- Dynamic Phi
PD_Final_Dyn_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                        M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                        DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Dyn_Phi)
PD_Final_Dyn_Phi$pseudo.r.squared # Pseudo R2 = 0.8550763
AIC(PD_Final_Dyn_Phi) # AIC = -2446.353
cat("MAE = ",round(mean(abs(predict(PD_Final_Dyn_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03837%
### RESULTS: The dynamic phi model has a better Pseudo R2 and AIC than the constant phi model. The MAE's are basically the same, hence we
# choose the dynamic phi model as our best.


# --- Final
PD_Final<-betareg(Y_PerfToDef ~ Ave_Margin_Aggr + M_Repo_Rate + M_Inflation_Growth_2 +
                            M_DTI_Growth + M_DTI_Growth_1 + M_Emp_Growth_9 + M_Emp_Growth_12 + DefaultStatus1_Aggr_Prop_Lag_1 +
                            DefaultStatus1_Aggr_Prop_Lag_2 + g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final)
PD_Final$pseudo.r.squared # Pseudo R2 = 0.8550763
AIC(PD_Final) # AIC = -2446.353
cat("MAE = ",round(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03837%


# - Link function on final mu input space
link_func_stats<-rbind(sapply(c("logit", "probit", "cloglog", "loglog"), function(x) AIC(update(PD_Final, link = x))),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) round(mean(abs(predict(update(PD_Final, link = x),datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) update(PD_Final, link = x)$pseudo.r.squared))
rownames(link_func_stats)<-c("AIC","MAE","Pseudo R^2")
link_func_stats
optimal_link<-"loglog"
### RESULTS - Ranked links based on Pseudo R2 for links (similar results hold for other measures):
# 1) loglog; 2) probit; 3) logit; cloglog
# Results are quite similar as the range of the pseudo r2 is [0.8549787,0.864546]

# - Update link function
PD_Final<-betareg(Y_PerfToDef ~ Ave_Margin_Aggr + M_Repo_Rate + M_Inflation_Growth_2 +
                    M_DTI_Growth + M_DTI_Growth_1 + M_Emp_Growth_9 + M_Emp_Growth_12 + DefaultStatus1_Aggr_Prop_Lag_1 +
                    DefaultStatus1_Aggr_Prop_Lag_2 + g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train, link = optimal_link)
summary(PD_Final)
PD_Final$pseudo.r.squared # Pseudo R2 = 0.864546
AIC(PD_Final) # AIC = -2447.781
cat("MAE = ",round(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03831%




# ------ 6. Cooks Distance Adjustment
# - Interactive runs are done to observe the best combinations of observations to leave out
# Cooks Distance Plot
plot(PD_Final, which = 2, type = "pearson",xlab="obs")
# Obtain observations with the biggest CD values
sort(round(cooks.distance(PD_Final),4))
# Specify training points to remove
Leave_Out<-c(66,2)
# Retrain model on new training set
PD_Adj<-update(PD_Final,subset=-Leave_Out)
cat("Pseudo R^2 before adjustment = ",PD_Final$pseudo.r.squared," --- ","Pseudo R^2 after adjustment = ",PD_Adj$pseudo.r.squared,"\n",sep="")
# Plot
plot(datAggr_valid$Date,predict(PD_Adj,datAggr_valid),type="l",col="red",lwd=2,ylim=c(0.001,0.009),xlab="Date",ylab="Transition probability",main="Constant Phi after Cooks adjustment")
lines(datAggr_valid$Date,as.numeric(datAggr_valid$Y_PerfToDef),type="l")
MAEval<-round(mean(abs(predict(PD_Adj,datAggr_valid)-as.numeric(datAggr_valid$Y_PerfToDef))),7)*100
legend(x="topright",paste("MAE = ",MAEval,"%"))
cat("MAE of Cooks Distance adjusted model= ",MAEval,"%","\n",sep="")
### RESULTS: Cooks distance adjustment improved the model fit:
# Pseudo R2 before CD = 0.864546; After CD = 0.8723582

# --- Save Model
PD_Final<-PD_Adj
pack.ffdf(paste0(genObjPath,"BR_P_To_D"), PD_Final)