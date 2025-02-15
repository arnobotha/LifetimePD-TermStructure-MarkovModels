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





# ------ 3. Transition rates

# --- Custom graphing logic 
# data objects: datMC, datBR, datMLR.
# field names: gvnRef_From, gvnRef_To, gvnFldPred_BR, gvnFldPred_MLR, gvnFldAct_BR, gvnFldAct_MLR
# graphing options: chosenFont, vCol, vSize, vShape, vLabel, dpi
graphTransRate <- function(datMC, datBR, datMLR, dteStart, gvnRef_From, gvnRef_To, gvnFldPred_BR, gvnFldPred_MLR, gvnFldAct_BR, 
                           gvnFldAct_MLR, chosenFont="Cambria", vCol="", vSize="", vShape="", vLabel="", yLabel="", dpi=220, annoX="", annoY="", fileName="") {
  # - Testing conditions
  # datMC=matResults_MC; datBR=datPred_Scaled; datMLR=datCredit_valid; dteStart=as.Date("2007-06-30"); gvnRef_From="Def"
  # gvnRef_To="Perf"; gvnFldPred_BR="p_DP"; gvnFldPred_MLR="pred_MLR_DP"; gvnFldAct_BR="a_DP"; gvnFldAct_MLR="Y_DefToPerf_Sub"
  # vSize=""; vCol=""; vShape="";  vLabel=""; fileName=paste0(genFigPath, "TransRate_DP.png")
  
  require(RColorBrewer); require(scales); require(ggplot2); require(data.table)
  
  # - Preliminarnies
  if(all(vCol=="")) {
    vCol <- brewer.pal(9, name="Set1")[c(1,9,2,3)]
  }
  if(all(vSize=="")) {
    vSize <- c(0.6,0.3,0.3,0.3)
  }
  if (all(vShape=="")) {
    vShape <- c(16,20,15,17)
  }
  if (all(vLabel=="")) {
    vLabel <- c("a_Actual"=bquote(italic(A[t*"'"])*': Actual'), "b_MC"=bquote(italic(C[t*"'"])*': Markov Chain'),
      "c_BR"=bquote(italic(B[t*"'"])*': Beta Regression'),"d_MLR"=bquote(italic(M[t*"'"])*': Multinomial Logistic Regression'))
  }
  if (all(is.na(annoX)) | !is.na(all(annoX==""))) {
    annoX <- as.Date("2016-12-31")
  }
  if (all(annoY=="")) {
    annoY <- c(0.9, 0.86, 0.82)
  }
  
  
  # - Collate predictions into common format for graphing purposes
  # NOTE: The structure of the given data objects (datMC, datBR, datMLR) are assumed to be known, just as an expediency
  pred_MC <- datMC[gvnRef_From,gvnRef_To]/100
  pred_BR <- datBR[Date >= dteStart & Date<maxDate_observed, ][[gvnFldPred_BR]]
  pred_MLR <- datMLR[MarkovStatus==gvnRef_From & Date<maxDate_observed & Date >= dteStart, list(TransRate = mean(get(gvnFldPred_MLR),na.rm=T)), 
                              by=list(Date)]$TransRate
  
  
  # --- Graphing logic
  
  # - Aggregate to portfolio-level
  datAggr_sub <- datMLR[Date<maxDate_observed & Date >= dteStart & MarkovStatus==gvnRef_From,
                                 list(TransRate = mean(get(gvnFldAct_MLR),na.rm=T), Type="a_Actual"),by=list(Date)]
  
  # - Merge Actuals and Predictions
  datAggr <- rbind(datAggr_sub,
                   data.table(Date=datAggr_sub$Date, TransRate=pred_MC, Type="b_MC"),
                   data.table(Date=datAggr_sub$Date, TransRate=pred_BR, Type="c_BR"),
                   data.table(Date=datAggr_sub$Date, TransRate=pred_MLR, Type="d_MLR"))
  
  # - Actuals
  actual_sub <- datAggr_sub$TransRate
  actual_BR_PD <- datBR[Date<maxDate_observed & Date >= dteStart, get(gvnFldAct_BR)]
  
  # - Calculate MAEs
  (MAE_MC <- round(mean(abs(actual_sub-pred_MC)),7))
  (MAE_BR <- round(mean(abs(actual_BR_PD-pred_BR)),7))
  (MAE_MLR <- round(mean(abs(actual_sub-pred_MLR)),7))
  
  # - Create graph
  (g <- ggplot(datAggr, aes(x=Date, y=TransRate, group=Type)) + theme_minimal() + 
      labs(x=bquote("Calendar date (months) "*italic(t*"'")), y=paste0("1-month transition rate: ", yLabel), family=chosenFont) + 
      theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
            axis.text.x=element_text(angle=90), 
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
      geom_line(aes(colour=Type, linewidth=Type, linetype=Type)) + 
      geom_point(aes(colour=Type, shape=Type), size=1) + 
      # Annotations
      annotate(geom="text", x = annoX, y = max(datAggr_sub$TransRate)*annoY[1], family=chosenFont, size=4,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(C[t*\"'\"])*': '*",
                            str_sub(percent(MAE_MC, accuracy=0.001),1,-2), "*'%'"), parse=T) + 
      annotate(geom="text", x = annoX, y = max(datAggr_sub$TransRate)*annoY[2], family=chosenFont, size=4,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(B[t*\"'\"])*': '*",
                            str_sub(percent(MAE_BR, accuracy=0.001),1,-2), "*'%'"), parse=T) + 
      annotate(geom="text", x = annoX, y = max(datAggr_sub$TransRate)*annoY[3], family=chosenFont, size=4,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(M[t*\"'\"])*': '*",
                            str_sub(percent(MAE_MLR, accuracy=0.001),1,-2), "*'%'"), parse=T) +     
      # Facets & scales
      scale_color_manual(name="", values=vCol, labels=vLabel) + 
      scale_linewidth_manual(name="", values=vSize, labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) +
      scale_shape_manual(name="", values=vShape, labels=vLabel) + 
      scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
      scale_y_continuous(breaks=pretty_breaks(), labels=percent) + 
      guides(colour = guide_legend(nrow=2)))
  
  if (fileName != "") {
    ggsave(g, file=fileName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white") 
  }
  
  return(lst(MC=pred_MC, BR=pred_BR, MLR=pred_MLR,
             MC_MAE=percent(MAE_MC,accuracy=0.001), BR_MAE=percent(MAE_BR,accuracy=0.001), MLR_MAE=percent(MAE_MLR,accuracy=0.001), Plot=g,
             MC_MAE_raw = MAE_MC, BR_MAE_raw=MAE_BR, MLR_MAE_raw=MAE_MLR))
  
  # - Cleanup, useful when run interactively and during debugging
  rm(datMC,datBR,datMLR,gvnRef_From,gvnRef_To,gvnFldPred_BR,gvnFldPred_MLR,gvnFldAct_BR,gvnFldAct_MLR, g)
}



# --- Transition rate graphs

# -- Performing start state
# - Performing to Default
lstPred_PD <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=minDate_observed, gvnRef_From="Perf", 
                          gvnRef_To="Def", gvnFldPred_BR="p_PD", gvnFldPred_MLR="pred_MLR_PD", gvnFldAct_BR="a_PD", gvnFldAct_MLR="Y_PerfToDef_Sub",
                          fileName=paste0(genFigPath, "TransRate_PD.png"), annoX=as.Date("2018-01-31"), annoY=c(0.9, 0.85, 0.8), yLabel="PD")

# - Performing to Performing
lstPred_PP <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=minDate_observed, gvnRef_From="Perf", 
                             gvnRef_To="Perf", gvnFldPred_BR="p_PP", gvnFldPred_MLR="pred_MLR_PP", gvnFldAct_BR="a_PP", gvnFldAct_MLR="Y_PerfToPerf_Sub",
                             fileName=paste0(genFigPath, "TransRate_PP.png"), annoY=c(0.999,0.998,0.997), yLabel="PP")

# - Performing to Settlement
lstPred_PS <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=minDate_observed, gvnRef_From="Perf", 
                             gvnRef_To="Set", gvnFldPred_BR="p_PS", gvnFldPred_MLR="pred_MLR_PS", gvnFldAct_BR="a_PS", gvnFldAct_MLR="Y_PerfToSet_Sub",
                             fileName=paste0(genFigPath, "TransRate_PS.png"), annoY=c(0.97,0.91,0.85), yLabel="PS")

# - Performing to Write-off
lstPred_PW <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=minDate_observed, gvnRef_From="Perf", 
                             gvnRef_To="W_Off", gvnFldPred_BR="p_PW", gvnFldPred_MLR="pred_MLR_PW", gvnFldAct_BR="a_PW", gvnFldAct_MLR="Y_PerfToWO_Sub",
                             fileName=paste0(genFigPath, "TransRate_PW.png"), annoY=c(1.03,0.97,0.91), yLabel="PW")


# -- Default start state
# - Default to Default
lstPred_DD <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=as.Date("2007-06-30"), gvnRef_From="Def", 
                          gvnRef_To="Def", gvnFldPred_BR="p_DD", gvnFldPred_MLR="pred_MLR_DD", gvnFldAct_BR="a_DD", gvnFldAct_MLR="Y_DefToDef_Sub",
                          fileName=paste0(genFigPath, "TransRate_DD.png"), annoY=c(0.998,0.995,0.992), annoX=as.Date("2013-01-31"), yLabel="DD")

# - Default to Write-off
lstPred_DW <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=as.Date("2007-06-30"), gvnRef_From="Def", 
                          gvnRef_To="W_Off", gvnFldPred_BR="p_DW", gvnFldPred_MLR="pred_MLR_DW", gvnFldAct_BR="a_DW", gvnFldAct_MLR="Y_DefToWO_Sub",
                          fileName=paste0(genFigPath, "TransRate_DW.png"), annoY=c(0.99,0.93,0.87), yLabel="DW")

# - Default to Settlement
lstPred_DS <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=as.Date("2007-06-30"), gvnRef_From="Def", 
                          gvnRef_To="Set", gvnFldPred_BR="p_DS", gvnFldPred_MLR="pred_MLR_DS", gvnFldAct_BR="a_DS", gvnFldAct_MLR="Y_DefToSet_Sub",
                          fileName=paste0(genFigPath, "TransRate_DS.png"), annoY=c(0.99,0.93,0.87), annoX=as.Date("2012-01-31"), yLabel="DS")

# - Default to Performing
lstPred_DP <- graphTransRate(datMC=matResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=as.Date("2007-06-30"), gvnRef_From="Def", 
                          gvnRef_To="Perf", gvnFldPred_BR="p_DP", gvnFldPred_MLR="pred_MLR_DP", gvnFldAct_BR="a_DP", gvnFldAct_MLR="Y_DefToPerf_Sub",
                          fileName=paste0(genFigPath, "TransRate_DP.png"), annoY=c(0.2,0.14,0.08), annoX=as.Date("2019-05-31"), yLabel="DP")


# --- Improvement in MAE-based AD-statistics (average discrepancy)
# - Beta regression to Markov chain
mean( 1-lstPred_PD$BR_MAE_raw / lstPred_PD$MC_MAE_raw, # 59%
      1-lstPred_PP$BR_MAE_raw / lstPred_PP$MC_MAE_raw, # 34%
      1-lstPred_PS$BR_MAE_raw / lstPred_PS$MC_MAE_raw, # 29%
      1-lstPred_PW$BR_MAE_raw / lstPred_PW$MC_MAE_raw, # -9%
      1-lstPred_DP$BR_MAE_raw / lstPred_DP$MC_MAE_raw, # -26%
      1-lstPred_DD$BR_MAE_raw / lstPred_DD$MC_MAE_raw, # -7%
      1-lstPred_DS$BR_MAE_raw / lstPred_DS$MC_MAE_raw, # 22%
      1-lstPred_DW$BR_MAE_raw / lstPred_DW$MC_MAE_raw) # 29%
# 58.8% average improvement in AD


# - MLSto Markov chain
mean( 1-lstPred_PD$MLR_MAE_raw / lstPred_PD$MC_MAE_raw, # 64%
      1-lstPred_PP$MLR_MAE_raw / lstPred_PP$MC_MAE_raw, # 46%
      1-lstPred_PS$MLR_MAE_raw / lstPred_PS$MC_MAE_raw, # 34%
      1-lstPred_PW$MLR_MAE_raw / lstPred_PW$MC_MAE_raw, # 15%
      1-lstPred_DP$MLR_MAE_raw / lstPred_DP$MC_MAE_raw, # 28%
      1-lstPred_DD$MLR_MAE_raw / lstPred_DD$MC_MAE_raw, # 25%
      1-lstPred_DS$MLR_MAE_raw / lstPred_DS$MC_MAE_raw, # 38%
      1-lstPred_DW$MLR_MAE_raw / lstPred_DW$MC_MAE_raw) # 36%
# 64.1% average improvement in AD



# --- Cleanup
rm(datCredit_smp, datCredit_train, datCredit_valid, datAggr_train, datAggr_valid, matResults_MC, 
   modMLR_perf, modMLR_def, PP_Final, PD_Final, PS_Final, PW_Final, DD_Final, DP_Final, DS_Final, DW_Final,
   datPred_Scaled, lstPred_DP, lstPred_DD, lstPred_DS, lstPred_DW, lstPred_PD, lstPred_PP, lstPred_PS, lstPred_PW); gc()

