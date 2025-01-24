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


# - Combining best versions of the lags
Delinq_Model_Full<-betareg(Y_PerfToDef~ Prev_PD + g0_Delinq_Any_Aggr_Prop_Lag_1+g0_Delinq_Any_Aggr_Prop_Lag_2+
                             DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_Ave+g0_Delinq_2_Ave 
                             ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8224033
AIC(Delinq_Model_Full) # AIC = -2408.1

# Remove g0_Delinq_Ave
Delinq_Model_Full<-betareg(Y_PerfToDef~ Prev_PD + g0_Delinq_Any_Aggr_Prop_Lag_1+g0_Delinq_Any_Aggr_Prop_Lag_2+
                             DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave 
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8223609
AIC(Delinq_Model_Full) # AIC = -2410.088

# Remove Prev_PD
Delinq_Model_Full<-betareg(Y_PerfToDef~ g0_Delinq_Any_Aggr_Prop_Lag_1+g0_Delinq_Any_Aggr_Prop_Lag_2+
                             DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave 
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8192019
AIC(Delinq_Model_Full) # AIC = -2409.911

# Remove g0_Delinq_Any_Aggr_Prop_Lag_2
Delinq_Model_Full<-betareg(Y_PerfToDef~ g0_Delinq_Any_Aggr_Prop_Lag_1+
                             DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave 
                           ,data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8174124
AIC(Delinq_Model_Full) # AIC = -2409.727

# Remove  g0_Delinq_Any_Aggr_Prop_Lag_1
Delinq_Model_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                             g0_Delinq_2_Ave, data=datAggr_train)
summary(Delinq_Model_Full)
Delinq_Model_Full$pseudo.r.squared # Pseudo R2 = 0.8158307
AIC(Delinq_Model_Full) # AIC = -2411.391
### RESULTS: Significant input variables in delinquency theme:
# DefaultStatus1_Aggr_Prop_Lag_1
# DefaultStatus1_Aggr_Prop_Lag_2
# g0_Delinq_2_Ave



# --- Macroeconomic-level
# - Repo rate
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
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4543541
AIC(Macro_Model) # AIC = -2201.871

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4292412
AIC(Macro_Model) # AIC = -2185.533

Macro_Model<-betareg(Y_PerfToDef~M_Repo_Rate_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4284043
AIC(Macro_Model) # AIC = -2179.45


# - Inflation
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
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.370894
AIC(Macro_Model) # AIC = -2172.885

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2839135
AIC(Macro_Model) # AIC = -2139.424

Macro_Model<-betareg(Y_PerfToDef~M_Inflation_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2005836
AIC(Macro_Model) # AIC = -2114.695


# - Debt to Income
Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.5298894 ***
AIC(Macro_Model) # AIC = -2215.935

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4816643 ***
AIC(Macro_Model) # AIC = -2197.705

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4511582
AIC(Macro_Model) # AIC = -2189.007

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4319762
AIC(Macro_Model) # AIC = -2183.001

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.441539
AIC(Macro_Model) # AIC = -2187.458

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4323245
AIC(Macro_Model) # AIC = -2183.526

Macro_Model<-betareg(Y_PerfToDef~M_DTI_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.310135
AIC(Macro_Model) # AIC = -2144.343


# - Real Income Growth
Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.002455657
AIC(Macro_Model) # AIC = -2076.54

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.4816643 ***
AIC(Macro_Model) # AIC = -2077.694

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.01861681
AIC(Macro_Model) # AIC = -2079.488

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03069737
AIC(Macro_Model) # AIC = -2081.706

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08237212
AIC(Macro_Model) # AIC = -2091.662

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1611797
AIC(Macro_Model) # AIC = -2108.816

Macro_Model<-betareg(Y_PerfToDef~M_RealIncome_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2684711 ***
AIC(Macro_Model) # AIC = -2137.445


# - Employment Growth
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


# - Real GDP Growth
Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.03215402
AIC(Macro_Model) # AIC = -2082.298

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_1,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.004734697
AIC(Macro_Model) # AIC = -2076.955

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_2,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 =  0.04731634
AIC(Macro_Model) # AIC = -2085.366

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_3,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.08016744
AIC(Macro_Model) # AIC = -2092.099

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_6,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.1380261
AIC(Macro_Model) # AIC = -2104.729

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_9,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.21904 ***
AIC(Macro_Model) # AIC = -2124.799

Macro_Model<-betareg(Y_PerfToDef~M_RealGDP_Growth_12,data=datAggr_train)
Macro_Model$pseudo.r.squared # Pseudo R2 = 0.2974952 ***
AIC(Macro_Model) # AIC = -2148.367


# - Combining best versions of the lags
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Repo_Rate_3+M_Inflation_Growth_2+M_Inflation_Growth_3+
                       M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                       M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7095634
AIC(Macro_Model_Full) # AIC = -2302.775

# Remove M_Repo_Rate_3
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+M_Inflation_Growth_3+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7094414
AIC(Macro_Model_Full) # AIC = -2304.697

# Remove M_Inflation_Growth_3
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_9+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7091803
AIC(Macro_Model_Full) # AIC = -2306.605

# Remove M_RealGDP_Growth_9
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_9+M_Emp_Growth_12+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7057418
AIC(Macro_Model_Full) # AIC = -2306.909

# Remove M_Emp_Growth_9
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                            M_Emp_Growth_12+M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7040557
AIC(Macro_Model_Full) # AIC = -2307.296

# Remove M_Emp_Growth_12
Macro_Model_Full<-betareg(Y_PerfToDef~M_Repo_Rate_2+M_Inflation_Growth_2+
                            M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                            M_RealGDP_Growth_12,data=datAggr_train)
summary(Macro_Model_Full)
Macro_Model_Full$pseudo.r.squared # Pseudo R2 = 0.7010249
AIC(Macro_Model_Full) # AIC = -2307.385

### RESULTS: Significant input variables in macroeconomic theme:
# M_Repo_Rate_2
# M_Inflation_Growth_2
# M_DTI_Growth
# M_DTI_Growth_1
# M_RealIncome_Growth_1
# M_RealIncome_Growth_12
# M_RealGDP_Growth_12


# - Other portfolio level variables
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                   AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+InterestRate_Margin_Aggr_Med_1+InterestRate_Margin_Aggr_Med_2+
                   InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6964219
AIC(Port_Full) # AIC = -2295.137

# Remove AgeToTerm_Aggr_Mean
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+InterestRate_Margin_Aggr_Med_1+InterestRate_Margin_Aggr_Med_2+
                     InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6964219
AIC(Port_Full) # AIC = -2295.137

# Remove InterestRate_Margin_Aggr_Med_1
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     AgeToTerm_Aggr_Mean+PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+InterestRate_Margin_Aggr_Med_2+
                     InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.696568
AIC(Port_Full) # AIC = -2297.113

# Remove AgeToTerm_Aggr_Mean
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+InterestRate_Margin_Aggr_Med_2+
                     InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6965532
AIC(Port_Full) # AIC = -2299.109

# Remove InterestRate_Margin_Aggr_Med_2
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+InstalmentToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+
                     InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6965663
AIC(Port_Full) # AIC = -2301.043

# Remove InstalmentToBalance_Aggr_Prop
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+
                     InterestRate_Margin_Aggr_Med_3+InterestRate_Margin_Aggr_Med_9+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6944714
AIC(Port_Full) # AIC = -2302.518

# Remove InterestRate_Margin_Aggr_Med_9
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     PerfSpell_Maturity_Aggr_Mean+InterestRate_Margin_Aggr_Med+
                     InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6938698
AIC(Port_Full) # AIC = -2303.938

# Remove InterestRate_Margin_Aggr_Med
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+CuringEvents_Aggr_Prop+
                     PerfSpell_Maturity_Aggr_Mean+
                     InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.6946013
AIC(Port_Full) # AIC =  -2304.828

# Remove CuringEvents_Aggr_Prop
Port_Full<-betareg(Y_PerfToDef~ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                     InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr+Ave_Margin_Aggr,data=datAggr_train)
summary(Port_Full)
Port_Full$pseudo.r.squared # Pseudo R2 = 0.693613
AIC(Port_Full) # AIC =  -2306.411
### RESULTS: Significant input variables in general portfolio level theme:
# ArrearsToBalance_Aggr_Prop
# PerfSpell_Maturity_Aggr_Mean
# InterestRate_Margin_Aggr_Med_3
# CreditLeverage_Aggr
# Ave_Margin_Aggr



# ---  Fusion step
# Combine insights mined from previous themes
PD_BR_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                             g0_Delinq_2_Ave + M_Repo_Rate_2+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_12+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                      InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr+Ave_Margin_Aggr, data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8612443
AIC(PD_BR_Full) # AIC = -2448.991
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03786%

# Remove M_Repo_Rate_2
PD_BR_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_12+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                      InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr+Ave_Margin_Aggr, data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8611558
AIC(PD_BR_Full) # AIC = -2450.939
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.0379%

# Remove Ave_Margin_Aggr
PD_BR_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_12+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                      InterestRate_Margin_Aggr_Med_3+CreditLeverage_Aggr, data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8610815
AIC(PD_BR_Full) # AIC = -2451.044
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.0377%

# Remove InterestRate_Margin_Aggr_Med_3
PD_BR_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave+M_Inflation_Growth_2+
                      M_DTI_Growth+M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_12+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                      CreditLeverage_Aggr, data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8609873
AIC(PD_BR_Full) # AIC = -2452.857
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03765%

# Remove M_DTI_Growth
PD_BR_Full<-betareg(Y_PerfToDef~ DefaultStatus1_Aggr_Prop_Lag_1+DefaultStatus1_Aggr_Prop_Lag_2+
                      g0_Delinq_2_Ave+M_Inflation_Growth_2+
                      M_DTI_Growth_1+M_RealIncome_Growth_1+M_RealIncome_Growth_12+
                      M_RealGDP_Growth_12+ArrearsToBalance_Aggr_Prop+PerfSpell_Maturity_Aggr_Mean+
                      CreditLeverage_Aggr, data=datAggr_train)
summary(PD_BR_Full)
PD_BR_Full$pseudo.r.squared # Pseudo R2 = 0.8590264
AIC(PD_BR_Full) # AIC = -2452.23
cat("MAE = ",round(mean(abs(predict(PD_BR_Full,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03818%

##### MAYBE REMOVE BIG STD ERRORS #####

# --- Link function
# --- Look at link functions
link_func_stats<-rbind(sapply(c("logit", "probit", "cloglog", "loglog"), function(x) AIC(update(PD_BR_Full, link = x))),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) round(mean(abs(predict(update(PD_BR_Full, link = x),datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100),
                       sapply(c("logit", "probit", "cloglog", "loglog"), function(x) update(PD_BR_Full, link = x)$pseudo.r.squared))
rownames(link_func_stats)<-c("AIC","MAE","Pseudo R^2")
link_func_stats
optimal_link<-"cloglog"

# ------ 3. Finalised input space of the model | Mu

# call RolandFunction() only here on validation; not prior.





# ------ 4. Modelling themes | Phi

# --- Portfolio-level

# --- Macroeconomic-level

# - Unemployment: lags (which are best?)
# asdfasdfasdf

# ---  Fusion step
# Combine insights mined from previous themes

# --- Link function



# ------ 5. Finalised input space of the model | Phi
