# ==================================== MODEL COMPARISON: MLR-MODELS  ===================================
# Fit finalised MLR-models, calculate diagnostics, and analyse overall results (discriminatory power)
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
#   - <Analytics> | Diagnostics
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Distributional analyses
describe(datCredit_train[MarkovStatus=="Perf",Target_FromP]) # Dominant class: Performing
# Value         Perf     Def     Set   W_Off
# Frequency  9020554   27184   66643     434
describe(datCredit_train[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default
# Value         Def   Perf    Set  W_Off
# Frequency  457448  12810   7108   6000

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base_perf <- multinom(Target_FromP ~  1, 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
modMLR_base_def <- multinom(Target_FromD ~  1, 
                             data = datCredit_train[MarkovStatus=="Def",],maxit=1000)

# - Bespoke ROC-graphing function
plotROC <- function(actuals, predictions, label, chosenFont="Cambria", fileName="", dpi=200, roundDigits=3){
  # - Testing conditions
  # actuals <- actuals_perf; predictions <- matPred[, "Perf"]; label <- "Performing to Performing"
  # fileName <- ""; roundDigits <- 3; fileName<-paste0(genFigPath, "ROC-PerfPerf_DV")
  
  # - Perform ROC-analysis
  roc_Obj <- roc(actuals~predictions,plot=FALSE,print.auc=T, ci=T)
  err <- percent(as.numeric(roc_Obj$ci[2]-roc_Obj$ci[1]), accuracy=0.001)
  AUC <- percent(as.numeric(roc_Obj$a),accuracy=0.01)
  
  # - Plot, if specified
  if (fileName != "") {
    
    # - Aesthetic engineering
    vCol <- brewer.pal(8, name="Paired")[c(1,2)]
    
    # - Prepare graphing dataset
    datGraph <- unique(data.table(sens=round(roc_Obj$sensitivities, digits=roundDigits), 
                           spec=1-round(roc_Obj$specificities, digits=roundDigits), 
                           Facet_Label=label))
    
    # - Create graph
    (g1 <- ggplot(datGraph, aes(y = sens, x = spec)) + theme_minimal() + 
      labs(x="False positive rate", y="True positive rate", family=chosenFont) +
      theme(text=element_text(family=chosenFont),legend.position = "none",
            axis.text.x=element_text(angle=0), strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=12, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
      geom_ribbon(aes(ymin = spec, ymax = sens),fill=vCol[1],alpha=0.7) + geom_line(color = vCol[2]) +
      geom_segment(y = 0, yend = 1, x = 0, xend = 1, color = vCol[2],linetype="dashed",linewidth=0.8) + 
      annotate("label", x = 0.5, y = 0.5, label = paste0("AUC =", AUC, " ± ", err),family=chosenFont, size=4) + 
      facet_grid(Facet_Label ~ .) + 
      scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent) )
    
    # - Save graph
    ggsave(g1, file=paste0(fileName, ".png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
    
    return(list(ROC=roc_Obj, Graph=g1, AUC_String=paste0("AUC =", AUC, " ± ", err)))
  } else {
    return(list(ROC=roc_Obj, AUC_String=paste0("AUC =", AUC, " ± ", err)))
  }
  
  rm(roc_Obj, actuals, predictions, datGraph, g1, err, AUC)
}



# ------ 2. Fit finalised MLR-models

# --- Performing MLR-model
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + ns(CreditLeverage_Aggr,3) +
                          ns(BalanceToPrincipal,3) + ns(InterestRate_Margin,3) + 
                          ns(g0_Delinq_Num,5) + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + ns(slc_acct_roll_ever_24_imputed_mean,5) + 
                          M_Emp_Growth + M_Inflation_Growth_2 + ns(M_Repo_Rate,3) + 
                          ns(slc_acct_pre_lim_perc_imputed_med,4), 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)

# - AIC & McFadden R^2
(result1 <- comma(AIC(modMLR_perf))) # 853,356
(result2 <- coefDeter_glm(modMLR_perf, modMLR_base_perf))  # 26.95%


# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result_perf <- dropterm(modMLR_perf, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 7.5h
### RESULTS: All variables are significant

# - ROC-analyses & AUC-statistics: Preparation | In-sample
matPred <- predict(modMLR_perf, newdata=datCredit_train[MarkovStatus=="Perf",], type="probs")
actuals_perf <- ifelse(datCredit_train[MarkovStatus=="Perf",Target_FromP] == "Perf", 1,0)
actuals_def <- ifelse(datCredit_train[MarkovStatus=="Perf",Target_FromP] == "Def", 1,0)
actuals_stl <- ifelse(datCredit_train[MarkovStatus=="Perf",Target_FromP] == "Set", 1,0)
actuals_woff <- ifelse(datCredit_train[MarkovStatus=="Perf",Target_FromP] == "W_Off", 1,0)

# - ROC-analyses & AUC-statistics: Results | In-sample
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Performing to Performing")
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Performing to Default")
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Performing to Settlement")
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Performing to Write-off")
# AUC (P):  81.66% ± 0.141%; AUC (D):  98.20% ± 0.096%; AUC (S):  75.95% ± 0.169%; AUC (W):  93.42% ± 1.032%

# - ROC-analyses & AUC-statistics: Preparation | Out-of-sample
matPred <- predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")
actuals_perf <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Perf", 1,0)
actuals_def <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Def", 1,0)
actuals_stl <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Set", 1,0)
actuals_woff <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "W_Off", 1,0)

# - ROC-analyses & AUC-statistics: Results | Out-of-sample
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Performing to Performing", fileName=paste0(genFigPath, "ROC-PerfPerf_DV"))
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Performing to Default", fileName=paste0(genFigPath, "ROC-PerfDef_DV"))
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Performing to Settlement", fileName=paste0(genFigPath, "ROC-PerfSet_DV"))
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Performing to Write-off", fileName=paste0(genFigPath, "ROC-PerfWOff_DV"))
# AUC (P):  81.62% ± 0.204%; AUC (D):  98.23% ± 0.136%; AUC (S):  76.10% ± 0.243%; AUC (W):  93.77% ± 1.381%



# --- Default MLR-model
modMLR_def <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + CreditLeverage_Aggr + g0_Delinq_Ave + 
                         g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + 
                         ns(slc_acct_roll_ever_24_imputed_mean,6) + pmnt_method_grp + ns(InterestRate_Margin,3) + Principal_Real + 
                         BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                         M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth + M_Inflation_Growth_6, 
                       data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_def, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")

# - AIC & McFadden R^2
(result1 <- comma(AIC(modMLR_def))) # 183,464
(result2 <- coefDeter_glm(modMLR_def, modMLR_base_def))  # 28.43%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_def, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 28.9m
### RESULTS: All variables are significant

# - ROC-analyses & AUC-statistics: Preparation | In-sample
matPred <- predict(modMLR_def, newdata=datCredit_train[MarkovStatus=="Def",], type="probs")
actuals_perf <- ifelse(datCredit_train[MarkovStatus=="Def",Target_FromP] == "Perf", 1,0)
actuals_def <- ifelse(datCredit_train[MarkovStatus=="Def",Target_FromP] == "Def", 1,0)
actuals_stl <- ifelse(datCredit_train[MarkovStatus=="Def",Target_FromP] == "Set", 1,0)
actuals_woff <- ifelse(datCredit_train[MarkovStatus=="Def",Target_FromP] == "W_Off", 1,0)

# - ROC-analyses & AUC-statistics: Results
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Default to Performing")
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Default to Default")
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Default to Settlement")
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Default to Write-off")
# AUC (P):  96.24% ± 0.082%; AUC (D):  78.23% ± 0.328%; AUC (S):  74.87% ± 0.570%; AUC (W):  79.64% ± 0.549%

# - ROC-analyses & AUC-statistics: Preparation | Out-of-sample
matPred <- predict(modMLR_def, newdata=datCredit_valid[MarkovStatus=="Def",], type="probs")
actuals_perf <- ifelse(datCredit_valid[MarkovStatus=="Def",Target_FromP] == "Perf", 1,0)
actuals_def <- ifelse(datCredit_valid[MarkovStatus=="Def",Target_FromP] == "Def", 1,0)
actuals_stl <- ifelse(datCredit_valid[MarkovStatus=="Def",Target_FromP] == "Set", 1,0)
actuals_woff <- ifelse(datCredit_valid[MarkovStatus=="Def",Target_FromP] == "W_Off", 1,0)

# - ROC-analyses & AUC-statistics: Results | Out-of-sample
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Default to Performing", fileName=paste0(genFigPath, "ROC-DefPerf_DV"))
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Default to Default", fileName=paste0(genFigPath, "ROC-DefDef_DV"))
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Default to Settlement", fileName=paste0(genFigPath, "ROC-DefSet_DV"))
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Default to Write-off", fileName=paste0(genFigPath, "ROC-DefWOff_DV"))
# AUC (P):  96.33% ± 0.119%; AUC (D):  77.94% ± 0.485%; AUC (S):  76.10% ± 0.243%; AUC (W):  93.77% ± 1.381%





# --- Cleanup
rm(rocResult, matPred, actuals_def, actuals_perf, actuals_stl, actuals_woff, result1, result2, modLR_Result_perf,
   modMLR_perf, modMLR_base_perf, modMLR_base_def, datCredit_train, datCredit_valid); gc()