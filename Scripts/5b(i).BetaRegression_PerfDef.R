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
#   - datCredit_train | Training set, created from subsampled set
#   - datCredit_valid | Validation set, created from subsampled set
#
# -- Outputs:
#   - <Analytics> | Input space
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)

# - Visual Plot of transition rate
plot(datAggr_train$Date,datAggr_train$Y_PerfToDef,type="l",ylab="Transition proportions", xlab="Date",main="Performance to performance transitions over time",lwd=2)
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
# R2 much lower than DefaultStatus1_Aggr_Prop

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

# Remove  g0_Delinq_Ave, Prev_PD, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1
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
### RESULTS: Based on the single factor model comparison the three best lags are:
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
AIC(Macro_Model) # AIC = -2076.54

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08237212
AIC(Macro_Model) # AIC = -2081.706

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1611797 ***
AIC(Macro_Model) # AIC = -2108.816

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2684711 ***
AIC(Macro_Model) # AIC = -2137.445
### RESULTS: Based on the single factor model comparison the three best lags are:
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
### RESULTS: Based on the single factor model comparison the three best lags are:
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
### RESULTS: Based on the single factor model comparison the three best lags are:
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
# as expected. Affordability (DTI) seems to be a strong predictor of the P to D transition rate. The real GDP and Employment
# growth seems to have a larger lag on defaulting behavior compared to the other variables.


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

# - NewLoans_Aggr_Prop_4 NewLoans_Aggr_Prop_5 InstalmentToBalance_Aggr_Prop CuringEvents_Aggr_Prop AgeToTerm_Aggr_Mean CreditLeverage_Aggr
Aggr_Full_Model<-betareg(Y_PerfToDef~InterestRate_Margin_Aggr_Med+Ave_Margin_Aggr+
                           ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean
                         ,data=datAggr_train)
summary(Aggr_Full_Model)
Aggr_Full_Model$pseudo.r.squared # Pseudo R2 = 0.6845527
AIC(Aggr_Full_Model) # AIC = -2304.097
### RESULTS: ArrearsToBalance_Aggr_Prop and Ave_Margin_Aggr seem to have a strong positive effect on P tot D transitions as expected. The 
# PerfSpell_Maturity_Aggr_Mean variable has a negative coefficient which is expected given that the larger the average performance spell age
# at a given date, the lower we expect the P to D transition rate to be.

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

# Remove ArrearsToBalance_Aggr_Prop
# Std error too high and results doesn't change too much
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate_2+
                      M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8529192
AIC(PD_BR_Full) # AIC = -2442.483
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%

# - Last interactive checks on the repo rate given its importance
PD_BR_Full<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+
                      M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_BR_Full) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%





# ------ 3. Finalised input space of the model | Mu
PD_BR_Final_Mu<-betareg(Y_PerfToDef~Ave_Margin_Aggr+
                      M_Repo_Rate+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+
                      M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave , data=datAggr_train)
summary(PD_BR_Final_Mu)
PD_BR_Final_Mu$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_BR_Final_Mu) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_BR_Final_Mu,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%
### RESULTS: g0_Delinq_2_Ave seems to be a great predictor of the transition rate as was also found in the single factor models.





# ------ 4. Modelling themes | Phi
# --- Macroeconomic & Portfolio-level theme
PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                          M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                          DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_Repo_Rate, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8540518
AIC(PD_Dynamic_Phi_Macro) # AIC = -2442.836


PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_Inflation_Growth_2, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8543009
AIC(PD_Dynamic_Phi_Macro) # AIC = -2442.74

PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_DTI_Growth, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8543273
AIC(PD_Dynamic_Phi_Macro) # AIC = -2442.451

PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_DTI_Growth_1, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8543392 ***
AIC(PD_Dynamic_Phi_Macro) # AIC = -2442.381

PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_Emp_Growth_9, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8536546
AIC(PD_Dynamic_Phi_Macro) # AIC = -2444.846

PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_Emp_Growth_12, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8539764
AIC(PD_Dynamic_Phi_Macro) # AIC = -2447.126

PD_Dynamic_Phi_Macro<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | Ave_Margin_Aggr, data=datAggr_train)
PD_Dynamic_Phi_Macro$pseudo.r.squared # Pseudo R2 = 0.8543216 ***
AIC(PD_Dynamic_Phi_Macro) # AIC = -2442.449
### RESULTS: Based on the single factor models, the M_DTI_Growth_1 and Ave_Margin_Aggr, are the best covariates for modelling phi.

# --- Delinquency themed
PD_Dynamic_Phi_Delinq<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | DefaultStatus1_Aggr_Prop_Lag_1, data=datAggr_train)
PD_Dynamic_Phi_Delinq$pseudo.r.squared # Pseudo R2 = 0.8541664
AIC(PD_Dynamic_Phi_Delinq) # AIC = -2442.592

PD_Dynamic_Phi_Delinq<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                 M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                 DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | DefaultStatus1_Aggr_Prop_Lag_2, data=datAggr_train)
PD_Dynamic_Phi_Delinq$pseudo.r.squared # Pseudo R2 = 0.8542119 ***
AIC(PD_Dynamic_Phi_Delinq) # AIC = -2442.553

PD_Dynamic_Phi_Delinq<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                 M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                 DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train)
PD_Dynamic_Phi_Delinq$pseudo.r.squared # Pseudo R2 = 0.8550763 ***
AIC(PD_Dynamic_Phi_Delinq) # AIC = -2446.353
### RESULTS: Based on the single factor models, the DefaultStatus1_Aggr_Prop_Lag_2 and g0_Delinq_2_Ave, are the best covariates for modelling phi.

# --- Fusion step
# Combine insights mined from previous themes
PD_Final_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                                 M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                                 DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_DTI_Growth + Ave_Margin_Aggr+ DefaultStatus1_Aggr_Prop_Lag_2 + g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Phi)
PD_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.8538607
AIC(PD_Final_Phi) # AIC = -2449.148
cat("MAE = ",round(mean(abs(predict(PD_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03875%

# Remove Ave_Margin_Aggr and DefaultStatus1_Aggr_Prop_Lag_2
PD_Final_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                    M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                    DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | M_DTI_Growth + g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Phi)
PD_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.8534394
AIC(PD_Final_Phi) # AIC = -2452.707
cat("MAE = ",round(mean(abs(predict(PD_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.0387%

# Remove M_DTI_Growth as a test
PD_Final_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                    M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                    DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Phi)
PD_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.8550763
AIC(PD_Final_Phi) # AIC = -2446.353
cat("MAE = ",round(mean(abs(predict(PD_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03837%

### VS No dynamic phi

PD_Final_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                        M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                        DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave, data=datAggr_train)
summary(PD_Final_Phi)
PD_Final_Phi$pseudo.r.squared # Pseudo R2 = 0.8543395
AIC(PD_Final_Phi) # AIC = -2444.381
cat("MAE = ",round(mean(abs(predict(PD_Final_Phi,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03833%


# --- Link function


# ------ 5. Finalised input space of the model
# --- Constant Phi
PD_Final_Cnst_Phi<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                        M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                        DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave, data=datAggr_train)
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

# --- Final
PD_Final<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                            DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train)
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

# - Update link function
PD_Final<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                    M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                    DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train, link = optimal_link)
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
P_D_Adj<-update(PD_Final,subset=-Leave_Out)
cat("Pseudo R^2 before adjustment = ",PD_Final$pseudo.r.squared," --- ","Pseudo R^2 after adjustment = ",P_D_Adj$pseudo.r.squared,"\n",sep="")
# Plot
plot(datAggr_valid$Date,predict(P_D_Adj,datAggr_valid),type="l",col="red",lwd=2,ylim=c(0.001,0.009),xlab="Date",ylab="Transition probability",main="Constant Phi after Cooks adjustment")
lines(datAggr_valid$Date,as.numeric(datAggr_valid$Y_PerfToDef),type="l")
MAEval<-round(mean(abs(predict(P_D_Adj,datAggr_valid)-as.numeric(datAggr_valid$Y_PerfToDef))),7)*100
legend(x="topright",paste("MAE = ",MAEval,"%"))
cat("MAE of Cooks Distance adjusted model= ",MAEval,"%","\n",sep="") # MAE = 0.03829%





