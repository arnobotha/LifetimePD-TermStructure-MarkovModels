# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)
chosenFont <- "Cambria"
dpi <- 180


# - Visual Plot of transition rate
plot(datAggr_train$Date, datAggr_train$Y_PerfToDef,type="l",ylab="Transition proportions", xlab="Date",main="Performance to performance transitions over time",lwd=2)
lines(datAggr_train$Date, datAggr_valid$Y_PerfToDef,col="orange")
legend(x="topright",legend=c("Training","Validation"),fill=c("black","orange"))

# - Build model
optimal_link<-"loglog"
PD_Final<-betareg(Y_PerfToDef~Ave_Margin_Aggr+M_Repo_Rate+M_Inflation_Growth_2+
                    M_DTI_Growth+M_DTI_Growth_1+M_Emp_Growth_9+M_Emp_Growth_12+DefaultStatus1_Aggr_Prop_Lag_1+
                    DefaultStatus1_Aggr_Prop_Lag_2+g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train, link = optimal_link)
summary(PD_Final)
PD_Final$pseudo.r.squared # Pseudo R2 = 0.864546
AIC(PD_Final) # AIC = -2447.781
cat("MAE = ",round(mean(abs(predict(PD_Final,datAggr_valid)-datAggr_valid$Y_PerfToDef)),7)*100,"%",sep="","\n") # MAE = 0.03831%


# --- Set Ci values
x<-1:191
y<-round(cooks.distance(PD_Final),4)

# - Create dataset
PLOT1<-data.table(i=1:191,cd=y ,Facet_label="Hypothetical Cook's distance plot")

# - Plot
(CD<-ggplot(PLOT1,aes(x=i,y=cd,ymin=min(cd),ymax=cd)) + theme_minimal() +
  labs(x="Observation "~italic(i), y=bquote("Cook's distance "~italic(C[i])), family=chosenFont) + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
        axis.text.x=element_text(angle=90), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
  geom_linerange(colour="black",linewidth=0.4) + geom_point(shape=16,colour="#377EB8",size=1) +
  facet_grid(Facet_label ~ .) +
  scale_x_continuous(breaks=seq(from=0, to=190, by=25)) +
  scale_y_continuous(breaks=pretty_breaks()))

x<-datAggr_train$Date
y<-cooks.distance(PD_Final)
PLOT2<-data.table(i=x,cd=y,Facet_label="Performing to default")


(CD2<-ggplot(PLOT2,aes(x=i,y=cd,ymin=min(cd),ymax=cd)) + theme_minimal() +
    labs(x="Calendar date (months)", y=bquote("Cook's distance "), family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    geom_linerange(colour="black",linewidth=0.4) + geom_point(shape=16,colour="#377EB8",size=1) +
    facet_grid(Facet_label ~ .) +
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks()))
ggsave(file = "CD2.png", plot = CD2, device = 'png', path = genFigPath, height = 750/dpi, width = 1200/dpi, dpi=400,bg="white")
