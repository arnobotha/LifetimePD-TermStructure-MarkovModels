# ==================================== MODEL COMPARISON: MLR-MODELS  ===================================
# Fit finalised MLR-models, calculate diagnostics, and perform overall analysis of results
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
describe(datCredit_train[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default

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
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
                              BalanceToPrincipal + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate , 
                            data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)

# - AIC & McFadden R^2
(result1 <- comma(AIC(modMLR_perf))) # 897,469
(result2 <- coefDeter_glm(modMLR_perf, modMLR_base_perf))  # 23.16%


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

# - ROC-analyses & AUC-statistics: Results
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Performing to Performing")
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Performing to Default")
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Performing to Settlement")
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Performing to Write-off")
# AUC (P):  74.99% ± 0.188%; AUC (D):  97.48% ± 0.139%; AUC (S):  66.61% ± 0.226%; AUC (W):  91.94% ± 1.288%

# - ROC-analyses & AUC-statistics: Preparation | Out-of-sample
matPred <- predict(modMLR_perf, newdata=datCredit_valid[MarkovStatus=="Perf",], type="probs")
actuals_perf <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Perf", 1,0)
actuals_def <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Def", 1,0)
actuals_stl <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "Set", 1,0)
actuals_woff <- ifelse(datCredit_valid[MarkovStatus=="Perf",Target_FromP] == "W_Off", 1,0)

# - ROC-analyses & AUC-statistics: Results
rocResult <- plotROC(actuals=actuals_perf, predictions=matPred[,"Perf"], label="Performing to Performing", fileName=paste0(genFigPath, "ROC-PerfPerf_DV"))
rocResult <- plotROC(actuals=actuals_def, predictions=matPred[,"Def"], label="Performing to Default", fileName=paste0(genFigPath, "ROC-PerfDef_DV"))
rocResult <- plotROC(actuals=actuals_stl, predictions=matPred[,"Set"], label="Performing to Settlement", fileName=paste0(genFigPath, "ROC-PerfSet_DV"))
rocResult <- plotROC(actuals=actuals_woff, predictions=matPred[,"W_Off"], label="Performing to Write-off", fileName=paste0(genFigPath, "ROC-PerfWOff_DV"))
# AUC (P):  74.94% ± 0.270%; AUC (D):  97.53% ± 0.201%; AUC (S):  66.81% ± 0.324%; AUC (W):  92.16% ± 1.702%




# --- Cleanup
rm(rocResult, matPred, actuals_def, actuals_perf, actuals_stl, actuals_woff, result1, result2, modLR_Result_perf,
   modMLR_perf, modMLR_base_perf, modMLR_base_def, datCredit_train, datCredit_valid); gc()