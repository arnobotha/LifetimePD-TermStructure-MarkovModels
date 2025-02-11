# ====================================== MODEL COMPARISON: PERF-DEF ====================================
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
#   - <Analytics> | Diagnostics
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


# --- Beta regression (BR) model
suppressWarnings(rm(PD_Final)); unpack.ffdf(paste0(genObjPath,"BR_P_To_D"), tempPath)
summary(PD_Final)
PD_Final$pseudo.r.squared # Pseudo R2 = 0.8723582
AIC(PD_Final) # AIC = -2446.269
cat("MAE = ",round(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03795%


# --- Multinomial Logistic Regression (MLR) model
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
                          BalanceToPrincipal + InterestRate_Margin + 
                          g0_Delinq_Num + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                          M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate +
                          CreditLeverage_Aggr + slc_acct_pre_lim_perc_imputed_med, 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)






# ------ 3. Transition rate graph

# --- Render predictions from fitted models
pred_MC <- datResults_MC["Perf","Def"]/100
pred_BR <- predict(PD_Final,datAggr_valid)
datCredit_smp[MarkovStatus=="Perf", pred_MLR := predict(modMLR_perf, newdata=datCredit_smp[MarkovStatus=="Perf",], type="probs")[,"Def"]]
pred_MLR <- datCredit_smp[MarkovStatus=="Perf", list(TransRate = mean(pred_MLR)), by=list(Date)]$TransRate


# --- Graphing logic

# - Aggregate to portfolio-level
datAggr_sub <- datCredit_valid[Date<maxDate_observed,list(TransRate = mean(Y_PerfToDef_Sub,na.rm=T), Type="a_Actual"),by=list(Date)]

# - Merge Actuals and Predictions
datAggr <- rbind(datAggr_sub,
                     data.table(Date=datAggr_sub$Date, TransRate=pred_MC, Type="b_MC"),
                     data.table(Date=datAggr_sub$Date, TransRate=pred_BR, Type="c_BR"),
                     data.table(Date=datAggr_sub$Date, TransRate=pred_MLR, Type="d_MLR"))

# - Calculate MAEs
MAE_MC <- round(mean(abs(datAggr_sub$TransRate-pred_MC)),7)
MAE_BR <- round(mean(abs(datAggr_sub$TransRate-pred_BR)),7)
MAE_MLR <- round(mean(abs(datAggr_sub$TransRate-pred_MLR)),7)

# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(9, name="Set1")[c(1,9,2,3)]
vSize <- c(0.6,0.3,0.3,0.3)
vShape <- c(16,20,15,17)
vLabel <-c("a_Actual"=bquote(italic(A[t*"'"])*': Actual'), "b_MC"=bquote(italic(C[t*"'"])*': Markov Chain'),
           "c_BR"=bquote(italic(B[t*"'"])*': Beta Regression'),"d_MLR"=bquote(italic(M[t*"'"])*': Multinomial Logistic Regression'))

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
           label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(C[t*\"'\"])*': '*",sprintf("%.3f", MAE_MC*100), "*'%'"), parse=T) + 
  annotate(geom="text", x = as.Date("2015-07-31"), y = max(datAggr_sub$TransRate)*0.86, family=chosenFont, size=3,
           label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(B[t*\"'\"])*': '*",sprintf("%.3f", MAE_BR*100), "*'%'"), parse=T) + 
  annotate(geom="text", x = as.Date("2015-07-31"), y = max(datAggr_sub$TransRate)*0.82, family=chosenFont, size=3,
           label=paste0("'MAE between '*italic(A[t*\"'\"])*' and '*italic(M[t*\"'\"])*': '*",sprintf("%.3f", MAE_MLR*100), "*'%'"), parse=T) +     
  # Facets & scales
  scale_color_manual(name="", values=vCol, labels=vLabel) + 
  scale_linewidth_manual(name="", values=vSize, labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) +
  scale_shape_manual(name="", values=vShape, labels=vLabel) + 
  scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
  scale_y_continuous(breaks=pretty_breaks(), labels=percent))


# - Save graph
ggsave(g, file=paste0(genFigPath, "TransRate_PD.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Cleanup
rm(datCredit_smp, datCredit_train, datCredit_valid, datAggr_train, datAggr_valid, datResults_MC, PD_Final,
   modMLR_perf)

expression('"MAE between "*italic(A[t])*" and "*italic(C[t])*": "*')
