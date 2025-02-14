# ================================= MODEL COMPARISON: TRANSITION RATES =================================
# Fit finalised models (Markov, BR, MLR) for a specific transition type towards comparing the 
# transition rate over calendar time
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Roland Breedt (RB)
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
#   - <Analytics> | Transition rate time graphs
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)





# ------ 2. Fit models to training data

# --- Markov chain model
matResults_MC <- Markov_TPM(DataSetMC=datCredit_smp,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))


# --- Beta regression (BR) models

# - Load fitted BR models from stored objects, post Cook's Distance adjustment
if(!exists('PD_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_D"), tempPath)
if(!exists('PP_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_P"), tempPath)
if(!exists('PS_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_S"), tempPath)
if(!exists('DD_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_D"), tempPath)
if(!exists('DS_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_S"), tempPath)
if(!exists('DW_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_W"), tempPath)



# --- Multinomial Logistic Regression (MLR) models
# - Performing
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + ns(CreditLeverage_Aggr,3) +
                          ns(BalanceToPrincipal,3) + ns(InterestRate_Margin,3) + 
                          ns(g0_Delinq_Num,5) + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + ns(slc_acct_roll_ever_24_imputed_mean,5) + 
                          M_Emp_Growth + M_Inflation_Growth_2 + ns(M_Repo_Rate,3) + 
                          ns(slc_acct_pre_lim_perc_imputed_med,4), 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)

# - Default
modMLR_def <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + CreditLeverage_Aggr + g0_Delinq_Ave + 
                         g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + 
                         ns(slc_acct_roll_ever_24_imputed_mean,6) + pmnt_method_grp + ns(InterestRate_Margin,3) + Principal_Real + 
                         BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                         M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth + M_Inflation_Growth_6, 
                       data = datCredit_train[MarkovStatus=="Def",],maxit=1000)





# ------ 3. Render predictions from fitted models

# --- Beta regression: Scaling approach
# NOTE: Scale by a single-factor z towards forcing row sums in transition matrix to equal 1

# - Calculate vector of denominators, whilst substituting missing BR-models with actual observations (PW + DP)
Denom_P <- predict(PD_Final,datAggr_valid) + predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid) + datAggr_train$Y_PerfToWO
Denom_D <- predict(DD_Final,datAggr_valid) + predict(DS_Final,datAggr_valid) + predict(DW_Final,datAggr_valid) + datAggr_train$Y_DefToPerf

# - Create data object of scaled predictions and actuals from the BR-models, having used the validation dataset
datPred_Scaled <- data.table(Date=datAggr_valid$Date,
                             a_PD=datAggr_valid$Y_PerfToDef, a_PS=datAggr_valid$Y_PerfToSet,
                             a_PP=datAggr_valid$Y_PerfToPerf, a_PW=datAggr_valid$Y_PerfToWO,
                             a_DD=datAggr_valid$Y_DefToDef, a_DW=datAggr_valid$Y_DefToWO,
                             a_DS=datAggr_valid$Y_DefToSet, a_DP=datAggr_valid$Y_DefToPerf,
                             p_PD=predict(PD_Final,datAggr_valid)/Denom_P, p_PS=predict(PS_Final,datAggr_valid)/Denom_P,
                             p_PP=predict(PP_Final,datAggr_valid)/Denom_P, p_PW=datAggr_train$Y_PerfToWO/Denom_P,
                             p_DD=predict(DD_Final,datAggr_valid), p_DD2=predict(DD_Final,datAggr_valid)/Denom_D,
                             p_DW=predict(DW_Final,datAggr_valid), p_DW2=predict(DW_Final,datAggr_valid)/Denom_D,
                             p_DS=predict(DS_Final,datAggr_valid), p_DS2=predict(DS_Final,datAggr_valid)/Denom_D,
                             p_DP=1-predict(DS_Final,datAggr_valid)-predict(DD_Final,datAggr_valid)-predict(DW_Final,datAggr_valid),
                             p_DP2=datAggr_train$Y_DefToPerf/Denom_D)


# --- MLR-models
datCredit_valid[MarkovStatus=="Perf", pred_MLR_PD := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")[,"Def"]]
datCredit_valid[MarkovStatus=="Perf", pred_MLR_PP := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")[,"Perf"]]
datCredit_valid[MarkovStatus=="Perf", pred_MLR_PS := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")[,"Set"]]
datCredit_valid[MarkovStatus=="Perf", pred_MLR_PW := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")[,"W_Off"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DD := predict(modMLR_def, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Def"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DW := predict(modMLR_def, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"W_Off"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DS := predict(modMLR_def, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Set"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DP := predict(modMLR_def, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Perf"]]






# ------ 3. Term Structure for PD-transition type
# Constructed for the cohort starting at the following given date
dteStart <- as.Date("2007-01-31")

# --- Data preparation for calculating term-structures

# - Aggregate actual transitions to portfolio-level transition rate
datAggr_sub <- datCredit_smp[Date<maxDate_observed & Date >= dteStart,
                      list(TransRate_PD = mean(Y_PerfToDef_Sub,na.rm=T), TransRate_PP = mean(Y_PerfToPerf_Sub,na.rm=T),
                           TransRate_PS = mean(Y_PerfToSet_Sub,na.rm=T), TransRate_PW = mean(Y_PerfToWO_Sub,na.rm=T),
                           TransRate_DP = mean(Y_DefToPerf_Sub,na.rm=T), TransRate_DD = mean(Y_DefToDef_Sub,na.rm=T),
                           TransRate_DS = mean(Y_DefToSet_Sub,na.rm=T), TransRate_DW = mean(Y_DefToWO_Sub,na.rm=T)),
                      by=list(Date)]

# - Aggregate expected transition probabilities to portfolio-level transition rate
datAggr_MLR <- datCredit_valid[Date<maxDate_observed & Date >= dteStart,
                             list(TransRate_PD_Exp = mean(pred_MLR_PD,na.rm=T), TransRate_PP_Exp = mean(pred_MLR_PP,na.rm=T),
                                  TransRate_PS_Exp = mean(pred_MLR_PS,na.rm=T), TransRate_PW_Exp = mean(pred_MLR_PW,na.rm=T),
                                  TransRate_DP_Exp = mean(pred_MLR_DP,na.rm=T), TransRate_DD_Exp = mean(pred_MLR_DD,na.rm=T),
                                  TransRate_DS_Exp = mean(pred_MLR_DS,na.rm=T), TransRate_DW_Exp = mean(pred_MLR_DW,na.rm=T)),
                             by=list(Date)]



# --- Calculate term-structures

# - Initialise transition matrices
matTrans_Act <- matrix(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1), nrow=4, byrow=T) # Actuals / MC
matTrans_MC <- copy(matTrans_Act)
matTrans_BR <- copy(matTrans_Act)
matTrans_MLR <- copy(matTrans_Act)

# - Initialise term structure
vTerm_Act <- numeric(datAggr_sub[,.N])
vTerm_MC <- copy(vTerm_Act)
vTerm_BR <- copy(vTerm_Act)
vTerm_MLR <- copy(vTerm_Act)


for (t in 1:NROW(vTerm_Act)) {
  # - Testing conditions
  # t <- 1
  # -- Assign matrix elements accordingly
  # - Actuals
  matTrans_Act[1,1] <- datAggr_sub[t, TransRate_PP]
  matTrans_Act[1,2] <- datAggr_sub[t, TransRate_PD]
  matTrans_Act[1,3] <- datAggr_sub[t, TransRate_PS]
  matTrans_Act[1,4] <- datAggr_sub[t, TransRate_PW]
  matTrans_Act[2,1] <- datAggr_sub[t, TransRate_DP]
  matTrans_Act[2,2] <- datAggr_sub[t, TransRate_DD]
  matTrans_Act[2,3] <- datAggr_sub[t, TransRate_DS]
  matTrans_Act[2,4] <- datAggr_sub[t, TransRate_DW]
  
  #matTrans_Act[1,1] <- datPred_Scaled[t, a_PP]
  #matTrans_Act[1,2] <- datPred_Scaled[t, a_PD]
  #matTrans_Act[1,3] <- datPred_Scaled[t, a_PS]
  #matTrans_Act[1,4] <- datPred_Scaled[t, a_PW]
  #matTrans_Act[2,1] <- datPred_Scaled[t, a_DP]
  #matTrans_Act[2,2] <- datPred_Scaled[t, a_DD]
  #matTrans_Act[2,3] <- datPred_Scaled[t, a_DS]
  #matTrans_Act[2,4] <- datPred_Scaled[t, a_DW]
  
  # - Beta regression (BR)
  matTrans_BR[1,1] <- datPred_Scaled[t, p_PP]
  matTrans_BR[1,2] <- datPred_Scaled[t, p_PD]
  matTrans_BR[1,3] <- datPred_Scaled[t, p_PS]
  matTrans_BR[1,4] <- datPred_Scaled[t, p_PW]
  matTrans_BR[2,1] <- datPred_Scaled[t, p_DP]
  matTrans_BR[2,2] <- datPred_Scaled[t, p_DD]
  matTrans_BR[2,3] <- datPred_Scaled[t, p_DS]
  matTrans_BR[2,4] <- datPred_Scaled[t, p_DW]
  
  # - Multinomial Logistic Regression (MLR)
  matTrans_MLR[1,1] <- datAggr_MLR[t, TransRate_PP_Exp]
  matTrans_MLR[1,2] <- datAggr_MLR[t, TransRate_PD_Exp]
  matTrans_MLR[1,3] <- datAggr_MLR[t, TransRate_PS_Exp]
  matTrans_MLR[1,4] <- datAggr_MLR[t, TransRate_PW_Exp]
  matTrans_MLR[2,1] <- datAggr_MLR[t, TransRate_DP_Exp]
  matTrans_MLR[2,2] <- datAggr_MLR[t, TransRate_DD_Exp]
  matTrans_MLR[2,3] <- datAggr_MLR[t, TransRate_DS_Exp]
  matTrans_MLR[2,4] <- datAggr_MLR[t, TransRate_DW_Exp]
  
  # -- Matrix multiplication
  if (t==1) {
    matTrans_Act_Cumul <- matTrans_Act
    matTrans_MC_Cumul <- matResults_MC/100 # Markov chain
    matTrans_BR_Cumul <- matTrans_BR
    matTrans_MLR_Cumul <- matTrans_MLR
  } else {
    matTrans_Act_Cumul <- matTrans_Act_Cumul %*% matTrans_Act
    matTrans_MC_Cumul <- matTrans_MC_Cumul %*% matResults_MC/100
    matTrans_BR_Cumul <- matTrans_BR_Cumul %*% matTrans_BR
    matTrans_MLR_Cumul <- matTrans_MLR_Cumul %*% matTrans_MLR
  }
  
  # -- Store cumulative transition rate of type PD
  vTerm_Act[t] <- matTrans_Act_Cumul[1,2]
  vTerm_MC[t] <- matTrans_MC_Cumul[1,2]
  vTerm_BR[t] <- matTrans_BR_Cumul[1,2]
  vTerm_MLR[t] <- matTrans_MLR_Cumul[1,2]
}

# Quick plot
plot(vTerm_Act, type="b")
lines(vTerm_MC, col="red")
lines(vTerm_BR, col="blue")
lines(vTerm_MLR, col="green")
### RESULTS: Seems promising, though the best results are obtained when using the validation set respective to each technique
# When using the universal datAggr_sub to constitute the "actuals", then the BR and MLR technique perform more similarly.
# Something to note in the article



# --- Graph term-structures

# - Create data object for graphing purposes
datGraph <- rbind(data.table(Date=datAggr_sub$Date, TransRate=vTerm_Act, Type="a_Actual"),
                  data.table(Date=datAggr_sub$Date, TransRate=vTerm_MC, Type="b_MC"),
                  data.table(Date=datAggr_sub$Date, TransRate=vTerm_BR, Type="c_BR"),
                  data.table(Date=datAggr_sub$Date, TransRate=vTerm_MLR, Type="d_MLR"))

# - Aesthetic engineering: Annotation - MAE - Average discrepancy (AD)
(MAE_MC <- mean(abs(vTerm_Act - vTerm_MC), na.rm=T))
(MAE_BR <- mean(abs(vTerm_Act - vTerm_BR), na.rm=T))
(MAE_MLR <- mean(abs(vTerm_Act - vTerm_MLR), na.rm=T))

# - Aesthetic engineering: Annotation - Average discrepancy ratio (ADR)
abs(1-mean(vTerm_MC / vTerm_Act, na.rm=T))
abs(1-mean(vTerm_BR / vTerm_Act, na.rm=T))
abs(1-mean(vTerm_MLR / vTerm_Act, na.rm=T))

# - Aesthetic engineering: Generic
datGraph[, FacetLabel := "Term-structure: Performing to default"]
chosenFont <- "Cambria"
vCol <- brewer.pal(9, name="Set1")[c(1,9,2,3)]
vSize <- c(0.6,0.3,0.3,0.3)
vShape <- c(16,20,15,17)
vLabel <- c("a_Actual"=bquote(italic(A[t*"'"])*': Actual'), "b_MC"=bquote(italic(C[t*"'"])*': Markov Chain'),
            "c_BR"=bquote(italic(B[t*"'"])*': Beta Regression'),"d_MLR"=bquote(italic(M[t*"'"])*': Multinomial Logistic Regression'))
annoX <- as.Date("2017-12-31")
annoY <- c(0.95,0.90,0.85)

# - Plot
# - Create graph
(g <- ggplot(datGraph, aes(x=Date, y=TransRate, group=Type)) + theme_minimal() + 
    labs(x=bquote("Calendar date (months) "*italic(t*"'")), y="Cumulative probability (%)", family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    geom_line(aes(colour=Type, linewidth=Type, linetype=Type)) + 
    geom_point(aes(colour=Type, shape=Type), size=1) + 
    # Annotations
    annotate(geom="text", x = annoX, y = max(datGraph$TransRate)*annoY[1], family=chosenFont, size=4,
             label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(C[t*\"'\"])*': '*",
                          str_sub(percent(MAE_MC, accuracy=0.001),1,-2), "*'%'"), parse=T) + 
    annotate(geom="text", x = annoX, y = max(datGraph$TransRate)*annoY[2], family=chosenFont, size=4,
             label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(B[t*\"'\"])*': '*",
                          str_sub(percent(MAE_BR, accuracy=0.001),1,-2), "*'%'"), parse=T) + 
    annotate(geom="text", x = annoX, y = max(datGraph$TransRate)*annoY[3], family=chosenFont, size=4,
             label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(M[t*\"'\"])*': '*",
                          str_sub(percent(MAE_MLR, accuracy=0.001),1,-2), "*'%'"), parse=T) +     
    # Facets & scales
    facet_grid(FacetLabel ~.) + 
    scale_color_manual(name="", values=vCol, labels=vLabel) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) +
    scale_shape_manual(name="", values=vShape, labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), labels=percent) + 
    guides(colour=guide_legend(nrow=2)))


dpi <- 220
ggsave(g, file=paste0(genFigPath, "TermStructure_PD.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white") 
