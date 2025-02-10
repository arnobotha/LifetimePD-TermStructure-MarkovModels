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
#   - Scripts 5a-5c
#
# -- Inputs:
#   - datAggr_train | Training set, created from subsampled set
#   - datAggr_valid | Validation set, created from subsampled set
#   - Final BR models
# -- Outputs:
#   - <Analytics> | Prediction dataset
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries
chosenFont <- "Cambria"
dpi <- 180

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)

# - Ensure target variable is restricted to (0,1)
cat('Nr of y targets where y=0 is', sum(datAggr_train$Y_DefToWO==0),"\n") # 1 case
datAggr_train[,Y_DefToWO:=ifelse(Y_DefToWO==0,0.00001,Y_DefToWO)]

# - Load in BR models
if(!exists('PD_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_D"), tempPath)
if(!exists('PP_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_P"), tempPath)
if(!exists('PS_Final')) unpack.ffdf(paste0(genObjPath,"BR_P_To_S"), tempPath)

if(!exists('DD_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_D"), tempPath)
if(!exists('DS_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_S"), tempPath)
if(!exists('DW_Final')) unpack.ffdf(paste0(genObjPath,"BR_D_To_W"), tempPath)


# ------ 2. Multifactor Scaling
PRED_SCALED<-data.table(Date=datAggr_valid$Date,p_PD=predict(PD_Final,datAggr_valid)/(predict(PD_Final,datAggr_valid) +predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid)+datAggr_train$Y_PerfToWO),
                        p_PS=predict(PS_Final,datAggr_valid)/(predict(PD_Final,datAggr_valid) +predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid)+datAggr_train$Y_PerfToWO),
                        p_PP=predict(PP_Final,datAggr_valid)/(predict(PD_Final,datAggr_valid) +predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid)+datAggr_train$Y_PerfToWO),
                        p_PW=datAggr_train$Y_PerfToWO/(predict(PD_Final,datAggr_valid) +predict(PP_Final,datAggr_valid) + predict(PS_Final,datAggr_valid)+datAggr_train$Y_PerfToWO),
                        p_DD=predict(DD_Final,datAggr_valid),p_DW=predict(DW_Final,datAggr_valid),p_DS=predict(DS_Final,datAggr_valid),p_DP = 1-predict(DS_Final,datAggr_valid)-predict(DD_Final,datAggr_valid)-
                        predict(DW_Final,datAggr_valid))

# --- Save the scaled prediction scores
pack.ffdf(paste0(genPath,"PRED_SCALED"), PRED_SCALED)

# --- Sanity Check
PRED_SCALED[,SUMP:=round(p_PD+p_PS+p_PP+p_PW,8),by=.I]
PRED_SCALED[,SUMD:=round(p_DD+p_DW+p_DS+p_DP,8),by=.I]
any(PRED_SCALED$SUMP!=1)
any(PRED_SCALED$SUMD!=1)
### RESULTS: All rows of the TPM sum to 1

# ------ 3. Multifactor Scaling
# - Obtain pearson residuals
residPD<- residuals(PD_Final,type="pearson")
residPP<- residuals(PP_Final,type="pearson")
residPS<- residuals(PS_Final,type="pearson")

residDD<- residuals(DD_Final,type="pearson")
residDW<- residuals(DW_Final,type="pearson")
residDS<- residuals(DS_Final,type="pearson")

# --- Final Plots
# - Performing To Default
# - Some Statistics to show
start_y<-0.4
space<-0.0315
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residPD),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residPD),3),"'")),
                        x = rep(2.5,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residPD)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
gg_Residuals_PerfToDef <- ggplot(df, aes(x=res)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),breaks = seq(-3, 4, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
  stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
  geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
  # facets & scale options
  labs(x="Residuals", 
       y="Density") + 
  theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=0), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
  facet_grid("Performing to default" ~ .) +
  geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
  scale_colour_manual(name = ' ', 
                      values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
  scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
  scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
  scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
  scale_x_continuous(limits=c(-4,4))
(gg_Residuals_PerfToDef<-gg_Residuals_PerfToDef+theme(legend.key.width = unit(1,"cm")))


# --- Perf To Perf
# - Some Statistics to Show
start_y<-0.5
space<-0.0325
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residPP),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residPP),3),"'")),
                        x = rep(2.5,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residPP)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
gg_Residuals_PerfToPerf <- ggplot(df, aes(x=res)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),breaks = seq(-4, 4, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
  stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
  geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
  # facets & scale options
  labs(x="Residuals", 
       y="Density") + 
  theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=0), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
  facet_grid("Performing to performing" ~ .) +
  geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
  scale_colour_manual(name = ' ', 
                      values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
  scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
  scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
  scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
  scale_x_continuous(limits=c(-4.5,4.5))
(gg_Residuals_PerfToPerf<-gg_Residuals_PerfToPerf+theme(legend.key.width = unit(1,"cm")))


# --- Perf To Set
# - Some Statistics to Show
start_y<-0.5
space<-0.0333
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residPS),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residPS),3),"'")),
                        x = rep(2.025,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residPS)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
gg_Residuals_PerfToSet <- ggplot(df, aes(x=res)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),breaks = seq(-3, 3, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
  stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
  geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
  # facets & scale options
  labs(x="Residuals", 
       y="Density") + 
  theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=0), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
  facet_grid("Performing to settlement" ~ .) +
  geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
  scale_colour_manual(name = ' ', 
                      values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
  scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
  scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
  scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
  scale_x_continuous(limits=c(-4,4))

(gg_Residuals_PerfToSet<-gg_Residuals_PerfToSet+theme(legend.key.width = unit(1,"cm")))


# --- Default To Write-Off
# - Some Statistics to Show
start_y<-0.47
space<-0.0325
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residDW),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residDW),3),"'")),
                        x = rep(2.5,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residDW)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
gg_Residuals_DefToWO <- ggplot(df, aes(x=res)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),breaks = seq(-3, 4, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
  stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
  geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
  # facets & scale options
  labs(x="Residuals", 
       y="Density") + 
  theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=0), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
  facet_grid("Default to write-off" ~ .) +
  geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
  scale_colour_manual(name = ' ', 
                      values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
  scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
  scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
  scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
  scale_x_continuous(limits=c(-4,4))

(gg_Residuals_DefToWO<-gg_Residuals_DefToWO+theme(legend.key.width = unit(1,"cm")))


# --- Default To Default
# - Some Statistics to Show
start_y<-0.45
space<-0.0325
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residDD),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residDD),3),"'")),
                        x = rep(2.025,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residDD)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
(gg_Residuals_DefToDef <- ggplot(df, aes(x=res)) + theme_minimal() + 
    geom_histogram(aes(y=..density..),breaks = seq(-3, 3, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
    stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
    geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
    # facets & scale options
    labs(x="Residuals", 
         y="Density") + 
    theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=0), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
    facet_grid("Default to default" ~ .) +
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
    scale_colour_manual(name = ' ', 
                        values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
    scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
    scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(limits=c(-3.5,3.5)))
(gg_Residuals_DefToDef<-gg_Residuals_DefToDef+theme(legend.key.width = unit(1,"cm")))


# --- Default to settlement
start_y<-0.46
space<-0.0325
y_vals<-c(start_y,start_y-space,start_y-2*space)
dat_anno1 <- data.table(Label = c(paste0("'Empirical density statistics:'"),
                                  paste0("'Kurtosis = ",round(kurtosis(residDS),3),"'"),
                                  paste0("'Skewness = ", round(skewness(residDS),3),"'")),
                        x = rep(2.5,3),
                        y = y_vals)

# - Prepare data
df<-data.frame(res=residDS)

# - Set colours
col.v <- brewer.pal(8, "Accent")[c(1,2,3,8,5)]

# - Plot
gg_Residuals_DefToSet <- ggplot(df, aes(x=res)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),breaks = seq(-2.5, 4, by = 0.5) ,colour="black", fill=col.v[3], alpha=0.75) + 
  stat_function(aes(colour="NOR",linetype="solid",size="A"),fun = dnorm, args = list(mean = mean(df$res), sd = sd(df$res))) +
  geom_density(aes(colour="EMP",linetype="dashed",size="B"),show.legend = F) + 
  # facets & scale options
  labs(x="Residuals", 
       y="Density") + 
  theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=0), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-8)) + 
  facet_grid("Default to settlement" ~ .) +
  geom_text(data=dat_anno1, aes(x=x, y=y, label = Label),family=chosenFont, size=3, parse=T) +
  scale_colour_manual(name = ' ', 
                      values =c('NOR'=col.v[4],'EMP'=col.v[5]), labels = c('Empirical density','Normal density')) +
  scale_linetype_manual(name = ' ',values=c('solid'='solid','dashed'='dashed'),labels = c('Empirical density','Normal density'))+
  scale_size_manual(name = ' ',values=c('A'=0.8,'B'=0.8),labels = c('Empirical density','Normal density'))+
  scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
  scale_x_continuous(limits=c(-2.75,4))
(gg_Residuals_DefToSet<-gg_Residuals_DefToSet+theme(legend.key.width = unit(1,"cm")))

# - Final Plot
(all_resid<-grid.arrange(gg_Residuals_PerfToPerf,gg_Residuals_PerfToDef,gg_Residuals_PerfToSet,gg_Residuals_DefToDef,gg_Residuals_DefToSet,gg_Residuals_DefToWO,ncol=2))

