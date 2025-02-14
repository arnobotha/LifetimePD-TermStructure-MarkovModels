# =================================== BETA REGRESSION ANALYSIS =================================
# Residual analysis on the 6 BR models and also scaling of predictions.
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
#   - 5b(i)-(iii) | Performing BR-models
#   - 5c(i)-(iii) | Default BR-models
#
# -- Inputs:
#   - datAggr_train | Training set, created from subsampled set
#   - datAggr_valid | Validation set, created from subsampled set
#   - Final BR model objects

# -- Outputs:
#   - <Analytics> | High-level analysis of fit statistics of BR-models
#   - <Analytics> | Pearson Residual analyses (graphs)
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries
chosenFont <- "Cambria"
dpi <- 180

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)

# - Load BR models from stored objects
if(!exists('PD_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_D"), tempPath)
if(!exists('PP_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_P"), tempPath)
if(!exists('PS_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_S"), tempPath)

if(!exists('DD_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_D"), tempPath)
if(!exists('DS_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_S"), tempPath)
if(!exists('DW_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_W"), tempPath)





# ------ 2. High-level analysis of BR-models (for reporting purposes)

# - Performing to Default
summary(PD_Final)
percent(PD_Final$pseudo.r.squared, accuracy=0.01) # 87.24%
AIC(PD_Final) # -2446.269
cat("MAE = ",percent(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),accuracy=0.00001),sep="","\n") # 0.03795%

# - Performing to Performing
summary(PP_Final)
percent(PP_Final$pseudo.r.squared, accuracy=0.01) # 70.04%
AIC(PP_Final) # -2446.269
cat("MAE = ",percent(mean(abs(predict(PP_Final,datAggr_valid)-datAggr_valid$Y_PerfToPerf)),accuracy=0.00001),sep="","\n") #  0.10263%

# - Performing to Settlement
summary(PS_Final)
percent(PS_Final$pseudo.r.squared, accuracy=0.01) # 59.97%
AIC(PS_Final) # -2030.63
cat("MAE = ",percent(mean(abs(predict(PS_Final,datAggr_valid)-datAggr_valid$Y_PerfToSet)),accuracy=0.00001),sep="","\n") #  0.09531%

# - Default to Default
summary(DD_Final)
percent(DD_Final$pseudo.r.squared, accuracy=0.01) # 33.68%
AIC(DD_Final) # -1269.948
cat("MAE = ",percent(mean(abs(predict(DD_Final,datAggr_valid)-datAggr_valid$Y_DefToDef)),accuracy=0.00001),sep="","\n") #  0.82782%

# - Default to Settlement
summary(DS_Final)
percent(DS_Final$pseudo.r.squared, accuracy=0.01) # 63.47%
AIC(DS_Final) # -1535.38
cat("MAE = ",percent(mean(abs(predict(DS_Final,datAggr_valid)-datAggr_valid$Y_DefToSet)),accuracy=0.00001),sep="","\n") #  0.42276%

# - Default to Write-off
summary(DW_Final)
percent(DW_Final$pseudo.r.squared, accuracy=0.01) # 39.63%
AIC(DW_Final) # -1530.126
cat("MAE = ",percent(mean(abs(predict(DW_Final,datAggr_valid)-datAggr_valid$Y_DefToWO)),accuracy=0.00001),sep="","\n") #  0.42276%






# ------ 3. Scaling approach
### AB: Move this entire section to script 7a since this object is not used anywhere else
# NOTE: Scale by a single-factor z towards forcing row sums in transition matrix to equal 1

# - Calculate vector of denominators, whilst substituting missing BR-models with actual observations (PW + DP)
Denom_P <- predict(PD_Final,datAggr_valid) + predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid) + datAggr_train$Y_PerfToWO
Denom_D <- predict(DD_Final,datAggr_valid) + predict(DS_Final,datAggr_valid) + predict(DW_Final,datAggr_valid) + datAggr_train$Y_DefToPerf

# - Create data object of scaled predictions from the BR-models, having used the validation dataset
datPred_Scaled <- data.table(Date=datAggr_valid$Date,
                             p_PD=predict(PD_Final,datAggr_valid)/Denom_P, p_PS=predict(PS_Final,datAggr_valid)/Denom_P,
                             p_PP=predict(PP_Final,datAggr_valid)/Denom_P, p_PW=datAggr_train$Y_PerfToWO/Denom_P,
                             p_DD=predict(DD_Final,datAggr_valid), p_DD2=predict(DD_Final,datAggr_valid)/Denom_D,
                             p_DW=predict(DW_Final,datAggr_valid), p_DW2=predict(DW_Final,datAggr_valid)/Denom_D,
                             p_DS=predict(DS_Final,datAggr_valid), p_DS2=predict(DS_Final,datAggr_valid)/Denom_D,
                             p_DP=1-predict(DS_Final,datAggr_valid)-predict(DD_Final,datAggr_valid)-predict(DW_Final,datAggr_valid),
                             p_DP2=datAggr_train$Y_DefToPerf/Denom_D)

# - Plot differences; closer to zero is 'better'
plot(datPred_Scaled$p_DD-datPred_Scaled$p_DD2, type="b")
lines(predict(PS_Final,datAggr_valid)-datPred_Scaled$p_PW, type="b", col="red")

# --- Save the scaled prediction scores
pack.ffdf(paste0(genPath,"datPred_Scaled"), datPred_Scaled)

# --- Sanity Check
datPred_Scaled[,SUMP:=round(p_PD+p_PS+p_PP+p_PW,8),by=.I]
datPred_Scaled[,SUMD:=round(p_DD+p_DW+p_DS+p_DP,8),by=.I]
datPred_Scaled[,SUMD2:=round(p_DD2+p_DW2+p_DS2+p_DP2,8),by=.I]
any(datPred_Scaled$SUMP!=1)
any(datPred_Scaled$SUMD!=1)
any(datPred_Scaled$SUMD2!=1)
### RESULTS: All rows of the TPM sum to 1





# ------ 3. Pearson Residual Analysis

# - Obtain pearson residuals from BR-models
residPD <- residuals(PD_Final,type="pearson")
residPP <- residuals(PP_Final,type="pearson")
residPS <- residuals(PS_Final,type="pearson")
residDD <- residuals(DD_Final,type="pearson")
residDW <- residuals(DW_Final,type="pearson")
residDS <- residuals(DS_Final,type="pearson")

# - Statistical moments of residuals (reporting purposes)
format(skewness(residPD), digits=3) # 0.687
format(skewness(residPP), digits=3) # 0.161
format(skewness(residPS), digits=3) # -0.862
format(skewness(residDD), digits=3) # -0.213
format(skewness(residDW), digits=3) # 0.857
format(skewness(residDS), digits=4) # 1.406


# --- Graphing logic: custom function
# NOTE: vCol is assumed to have 3 colours: 1 for historgram, 1 for empirical density, 1 for normal density
# x_vals & y_vals are positioning coordinates for the annotation (skewness & kurtosis)
plotPearsonResiduals <- function(residuals, facetLabel, vCol, x_vals, y_vals, fileName="", dpi=240) {
  # - Testing conditions
  # residuals <- residPD; facetLabel <- "BR-model: Performing to Default"
  # x_vals <- c(2.5,2.5); y_vals <- c(0.4, 0.3685)
  
  # - Compose data object for graphing purposes
  datGiven <- data.table(res=residuals, facetLabel= facetLabel)
  
  # - Perform Kolmorogove-Smirnov test for evaluating normality in residuals
  testResult <- ks.test(residuals, y="pnorm")
  
  # - Aesthetic engineering: Annotations
  dat_anno1 <- data.table(Label = c(paste0("Empirical kurtosis = ",round(kurtosis(residuals),3)),
                                    paste0("Empirical skewness = ", round(skewness(residuals),3)),
                                    paste0("KS-test p-value = ", percent(testResult$p.value,accuracy=0.01))),
                          x = x_vals, y = y_vals)
  
  # - Create plot
  g1 <- ggplot(datGiven, aes(x=res)) + theme_minimal() + 
    labs(x=bquote("Pearson Residuals "*italic(r)^{(P)}), y="Density") + 
    theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=0), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), 
          legend.margin = margin(t=-8)) + 
    # Overlay empirical histogram and density
    geom_histogram(aes(y=after_stat(density)),breaks = seq(-3, 4, by = 0.5) ,colour=vCol[2], fill=vCol[1], alpha=0.75, show.legend = F) + 
    geom_density(aes(colour="EMP", linetype="EMP", linewidth="EMP")) + 
    # Overlay normal normal density, with specified mean and standard deviation
    stat_function(fun = dnorm, args = list(mean = mean(datGiven$res), sd = sd(datGiven$res)),
                  aes(colour="NOR",linetype="NOR",linewidth="NOR")) +
    # facets & scale options
    facet_grid(facetLabel ~ .) +
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3) +
    scale_colour_manual(name = "", values =c('NOR'=vCol[3],'EMP'=vCol[2]), labels = c('Empirical density','Normal density')) +
    scale_linetype_manual(name = "",values=c('NOR'='dashed','EMP'='solid'),labels = c('Empirical density','Normal density'))+
    scale_linewidth_manual(name = "",values=c('NOR'=0.7,'EMP'=0.5),labels = c('Empirical density','Normal density')) +
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(limits=c(-4,4))
  
  # - Save plot
  if (fileName != "") {
    ggsave(g1, file=fileName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white") 
  }
  
  return(g1)
  
  # - Cleanup (relevant when running interactively)
  rm(residuals, facetLabel, x_vals, y_vals, testResult)
}

# - Aesthetic engineering
vCol <- brewer.pal(10, "Paired")[c(7,8,10)]
x_vals <- c(2.5, 2.55, 2.45); y_vals <- c(0.4, 0.37, 0.34)

plotPearsonResiduals(residPD, facetLabel="BR-model: Performing to Default", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_PD.png"))
plotPearsonResiduals(residPP, facetLabel="BR-model: Performing to Performing", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_PP.png"))
plotPearsonResiduals(residPS, facetLabel="BR-model: Performing to Settlement", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_PS.png"))
plotPearsonResiduals(residDD, facetLabel="BR-model: Default to Default", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_DD.png"))
plotPearsonResiduals(residDW, facetLabel="BR-model: Default to Write-off", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_DW.png"))
plotPearsonResiduals(residDS, facetLabel="BR-model: Default to Settlement", x_vals=x_vals, y_vals=y_vals, vCol=vCol,
                     fileName=paste0(genFigPath,"PearsonResiduals_DS.png"))


# - Cleanup
rm(datAggr_train, datAggr_valid, datPred_Scaled, DD_Final, DS_Final, DW_Final, PD_Final, PP_Final, PS_Final)