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
datResults_MC <- Markov_TPM(DataSetMC=datCredit_smp,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))


# --- Beta regression (BR) models

# - Load fitted BR models from stored objects, post Cook's Distance adjustment
if(!exists('PD_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_D"), tempPath)
if(!exists('PP_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_P"), tempPath)
if(!exists('PS_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_S"), tempPath)
if(!exists('DD_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_D"), tempPath)
if(!exists('DS_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_S"), tempPath)
if(!exists('DW_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_W"), tempPath)



# --- Multinomial Logistic Regression (MLR) model
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
                          ns(BalanceToPrincipal,3) + ns(InterestRate_Margin,3) + 
                          ns(g0_Delinq_Num,5) + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + ns(slc_acct_roll_ever_24_imputed_mean,5) + 
                          M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate + 
                          CreditLeverage_Aggr + ns(slc_acct_pre_lim_perc_imputed_med,4), 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)





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
datCredit_valid[MarkovStatus=="Def", pred_MLR_DD := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Def"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DW := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"W_Off"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DS := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Set"]]
datCredit_valid[MarkovStatus=="Def", pred_MLR_DP := predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")[,"Perf"]]




# ------ 3. Transition rates

# --- Custom graphing logic 
# data objects: datMC, datBR, datMLR.
# field names: gvnRef_From, gvnRef_To, gvnFldPred_BR, gvnFldPred_MLR, gvnFldPred_MLR, gvnFldAct_MLR
# graphing options: chosenFont, vCol, vSize, vShape, vLabel, dpi
graphTransRate <- function(datMC, datBR, datMLR, dteStart, gvnRef_From, gvnRef_To, gvnFldPred_BR, gvnFldPred_MLR, gvnFldAct_BR, 
                           gvnFldAct_MLR, chosenFont="Cambria", vCol="", vSize="", vShape="", vLabel="", dpi=200, fileName="") {
  # - Testing conditions
  # datMC=datResults_MC; datBR=datPred_Scaled; datMLR=datCredit_valid; dteStart=dteStart; gvnRef_From="Perf"
  # gvnRef_To="Def"; gvnFldPred_BR="p_PD"; gvnFldPred_MLR="pred_MLR_PD"; gvnFldAct_BR="a_PD"; gvnFldAct_MLR="Y_PerfToDef_Sub"
  
  require(RColorBrewer)
  
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
  
  
  # - Collate predictions into common format for graphing purposes
  # NOTE: The structure of the given data objects (datMC, datBR, datMLR) are assumed to be known, just as an expediency
  pred_MC <- datMC[gvnRef_From,gvnRef_To]/100
  pred_BR <- datBR[Date >= dteStart & Date<maxDate_observed, ][[gvnFldPred_BR]]
  pred_MLR <- datMLR[MarkovStatus==gvnRef_MRL & Date<maxDate_observed & Date >= dteStart, list(TransRate = mean(get(gvnFldPred_MLR))), 
                              by=list(Date)]$TransRate
  
  
  # --- Graphing logic
  
  # - Aggregate to portfolio-level
  datAggr_sub <- datMLR[Date<maxDate_observed & Date >= dteStart & MarkovStatus==gvnRef_MRL,
                                 list(TransRate = mean(get(gvnFldAct_MLR),na.rm=T), Type="a_Actual"),by=list(Date)]
  
  # - Merge Actuals and Predictions
  datAggr <- rbind(datAggr_sub,
                   data.table(Date=datAggr_sub$Date, TransRate=pred_MC, Type="b_MC"),
                   data.table(Date=datAggr_sub$Date, TransRate=pred_BR, Type="c_BR"),
                   data.table(Date=datAggr_sub$Date, TransRate=pred_MLR, Type="d_MLR"))
  
  # - Actuals
  actual_sub <- datAggr_sub$TransRate
  actual_BR_PD <- datBR[[gvnFldAct_BR]]
  
  # - Calculate MAEs
  (MAE_MC <- round(mean(abs(actual_sub-pred_MC)),7))
  (MAE_BR <- round(mean(abs(actual_BR_PD-pred_BR)),7))
  (MAE_MLR <- round(mean(abs(actual_sub-pred_MLR)),7))
  
  # - Create graph
  (g <- ggplot(datAggr, aes(x=Date, y=TransRate, group=Type)) + theme_minimal() + 
      labs(x=bquote("Calendar date (months) "*italic(t*"'")), y="1-month transition rate: Performing to Default", family=chosenFont) + 
      theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
            axis.text.x=element_text(angle=90), 
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
      geom_line(aes(colour=Type, linewidth=Type, linetype=Type)) + 
      geom_point(aes(colour=Type, shape=Type), size=1) + 
      # Annotations
      annotate(geom="text", x = as.Date("2015-07-31"), y = max(datAggr_sub$TransRate)*0.9, family=chosenFont, size=3,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(C[t*\"'\"])*': '*",sprintf("%.4f", MAE_MC*100), "*'%'"), parse=T) + 
      annotate(geom="text", x = as.Date("2015-07-31"), y = max(datAggr_sub$TransRate)*0.86, family=chosenFont, size=3,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(B[t*\"'\"])*': '*",sprintf("%.4f", MAE_BR*100), "*'%'"), parse=T) + 
      annotate(geom="text", x = as.Date("2015-07-31"), y = max(datAggr_sub$TransRate)*0.82, family=chosenFont, size=3,
               label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(M[t*\"'\"])*': '*",sprintf("%.4f", MAE_MLR*100), "*'%'"), parse=T) +     
      # Facets & scales
      scale_color_manual(name="", values=vCol, labels=vLabel) + 
      scale_linewidth_manual(name="", values=vSize, labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) +
      scale_shape_manual(name="", values=vShape, labels=vLabel) + 
      scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
      scale_y_continuous(breaks=pretty_breaks(), labels=percent))
  
  return(lst(MC=pred_MC, BR=pred_BR, MLR=pred_MLR,
             MC_MAE=MAE_MC, BR_MAE=MAE_BR, MLR_MAE=MAE_MLR, Plot=g))
  
  # - Cleanup, useful when run interactively and during debugging
  rm(datMC,datBR,datMLR,gvnRef_From,gvnRef_To,gvnFldPred_BR,gvnFldPred_MLR,gvnFldAct_BR,gvnFldAct_MLR, g)
}



# --- Transition rate graphs
lstPred <- graphTransRate(datMC=datResults_MC, datBR=datPred_Scaled, datMLR=datCredit_valid, dteStart=dteStart, gvnRef_From="Perf", 
                          gvnRef_To="Def", gvnFldPred_BR="p_PD", gvnFldPred_MLR="pred_MLR_PD", gvnFldAct_BR="a_PD", gvnFldAct_MLR="Y_PerfToDef_Sub",
                          fileName=paste0(genFigPath, "TransRate_PD.png"))








# - Save graph
ggsave(g, file=, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Cleanup
rm(datCredit_smp, datCredit_train, datCredit_valid, datAggr_train, datAggr_valid, datResults_MC, PD_Final,
   modMLR_perf)

expression('"MAE between "*italic(A[t])*" and "*italic(C[t])*": "*')
