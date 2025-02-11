# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)




# ------ 2. Fit Beta regression model using finalised input space

# --- Performing to Default
PD_Final <- betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                    M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                    DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train, link = "loglog")
summary(PD_Final)
PD_Final$pseudo.r.squared # Pseudo R2 = 0.864546
AIC(PD_Final) # AIC = -2447.908
cat("MAE = ",round(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03814%



# --- Performing to Performing

# --- Performing to Settlement
PS_Final <- betareg(Y_PerfToSet~ -1 + g0_Delinq_Any_Aggr_Prop_Lag_2 + g0_Delinq_3_Ave+M_Emp_Growth + M_Emp_Growth_1 + 
                    CreditLeverage_Aggr + AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | M_Repo_Rate, data=datAggr_train, link="loglog")
summary(PS_Final)
PS_Final$pseudo.r.squared # Pseudo R2 = 0.4711473
AIC(PS_Final) # AIC = -1962.1
cat("MAE = ",round(mean(abs(predict(PS_Final,datAggr_valid)-datAggr_valid$Y_PerfToSet)),7)*100,"%",sep="","\n") # MAE =  0.09436%



# --- Default to Default



# --- Default to Write-off
DW_Final<-betareg(Y_DefToWO ~ Prev_DW + DefaultStatus1_Aggr_Prop_Lag_6 + CuringEvents_Aggr_Prop +
                    M_Emp_Growth + CreditLeverage_Aggr + CuringEvents_Aggr_Prop + AgeToTerm_Aggr_Mean +
                    PerfSpell_Maturity_Aggr_Mean | -1 + M_RealGDP_Growth_3 + DefaultStatus1_Aggr_Prop_Lag_1 + M_DTI_Growth_12, 
                  data=datAggr_train, link=optimal_link)
summary(DW_Final)
### AB: Problems
DW_Final$pseudo.r.squared # Pseudo R2 = 0.3910874
AIC(DW_Final) # AIC = -1524.5
cat("MAE = ",round(mean(abs(predict(DW_Final,datAggr_valid)-datAggr_valid$Y_DefToWO)),7)*100,"%",sep="","\n") # MAE = 0.37035%



# -- Default to Settlement
DS_Final<-betareg(Y_DefToSet ~ Prev_DS +  M_RealIncome_Growth  + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                    AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train, link=optimal_link)
summary(DS_Final)
DS_Final$pseudo.r.squared # Pseudo R2 = 0.5820605
AIC(DS_Final) # AIC = -1516.18
cat("MAE = ",round(mean(abs(predict(DS_Final,datAggr_valid)-datAggr_valid$Y_DefToSet)),7)*100,"%",sep="","\n") # MAE = 0.42069%





# ------ 3. Cook's Distance plot

# - Create graphing dataset
datGraph_PD <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(PD_Final),4),Facet_label="BR-model: Performing to Default")
datGraph_PS <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(PS_Final),4),Facet_label="BR-model: Performing to Settlement")
datGraph_DS <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(DS_Final),4),Facet_label="BR-model: Default to Settlement")

# - Aesthetic engineering: general
chosenFont <- "Cambria"; dpi <- 200
vCol = brewer.pal(10, name="Paired")

# - Aesthetic engineering: Encircle outliers
datGraph_PD[, Encircled := ifelse(CooksD/max(CooksD) >= 0.3, CooksD, NA)]
datGraph_PS[, Encircled := ifelse(CooksD/max(CooksD) >= 0.3, CooksD, NA)]
datGraph_DS[, Encircled := ifelse(CooksD/max(CooksD) >= 0.8, CooksD, NA)]


# - Create graph: Performing to Default
(g1 <- ggplot(datGraph_PD,aes(x=Date,y=CooksD,ymin=min(CooksD),ymax=CooksD)) + theme_minimal() +
  labs(x="Calendar date (months)", y=bquote("Cook's distance "~italic(D[C])), family=chosenFont) + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
        axis.text.x=element_text(angle=90), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
  geom_linerange(colour=vCol[1],linewidth=0.4) + geom_point(shape=16,colour=vCol[2],size=1) +
  geom_point(aes(x=Date, y=Encircled), shape=1,size=4, colour=vCol[6]) + # Encircled points
  facet_grid(Facet_label ~ .) +
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
  scale_y_continuous(breaks=pretty_breaks()))

# - Create graph: Performing to Settlement
(g3 <- ggplot(datGraph_PS,aes(x=Date,y=CooksD,ymin=min(CooksD),ymax=CooksD)) + theme_minimal() +
    labs(x="Calendar date (months)", y=bquote("Cook's distance "~italic(D[C])), family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    geom_linerange(colour=vCol[1],linewidth=0.4) + geom_point(shape=16,colour=vCol[2],size=1) +
    geom_point(aes(x=Date, y=Encircled), shape=1,size=4, colour=vCol[6]) + # Encircled points
    facet_grid(Facet_label ~ .) +
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks()))

# - Create graph: Performing to Settlement
(g6 <- ggplot(datGraph_DS,aes(x=Date,y=CooksD,ymin=min(CooksD),ymax=CooksD)) + theme_minimal() +
    labs(x="Calendar date (months)", y=bquote("Cook's distance "~italic(D[C])), family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    geom_linerange(colour=vCol[1],linewidth=0.4) + geom_point(shape=16,colour=vCol[2],size=1) +
    geom_point(aes(x=Date, y=Encircled), shape=1,size=4, colour=vCol[6]) + # Encircled points
    facet_grid(Facet_label ~ .) +
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks()))

# - Save graphs
ggsave(g1, file=paste0(genFigPath, "CooksDist_PD.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
ggsave(g3, file=paste0(genFigPath, "CooksDist_PS.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
ggsave(g6, file=paste0(genFigPath, "CooksDist_DS.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

