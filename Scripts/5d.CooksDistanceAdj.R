# ================================ BETA REGRESSION MODEL: COOK'S DISTANCE ==============================
# Identify influential observations within the training sample for each BR-model using Cook's distance
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Roland Breedt (RB), 
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
#
# -- Inputs:
#   - datAggr_train | Training set, created from subsampled set
#   - datAggr_valid | Validation set, created from subsampled set
#
# -- Outputs:
#   - <Analytics> | Graphs on Cook's D
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datAggr_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datAggr_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)




# ------ 2. Fit Beta regression model using finalised input space

# --- Performing to Default
PD_Final <- betareg(Y_PerfToDef ~ Ave_Margin_Aggr + M_Repo_Rate + M_Inflation_Growth_2 +
                    M_DTI_Growth + M_DTI_Growth_1 + M_Emp_Growth_9 + M_Emp_Growth_12 + DefaultStatus1_Aggr_Prop_Lag_1 +
                    DefaultStatus1_Aggr_Prop_Lag_2 + g0_Delinq_2_Ave | g0_Delinq_2_Ave, data=datAggr_train, link = "loglog")


# --- Performing to Performing



# --- Performing to Settlement
PS_Final <- betareg(Y_PerfToSet~ -1 + g0_Delinq_Any_Aggr_Prop_Lag_2 + g0_Delinq_3_Ave + M_Emp_Growth + M_Emp_Growth_1 + 
                    CreditLeverage_Aggr + AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | M_Repo_Rate, data=datAggr_train, link="loglog")


# --- Default to Default


# --- Default to Write-off


# -- Default to Settlement
DS_Final <- betareg(Y_DefToSet ~ Prev_DS + M_RealIncome_Growth + M_RealIncome_Growth_1 + g0_Delinq_3_Ave + 
                    AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean | g0_Delinq_Ave, data=datAggr_train, link="loglog")




# ------ 3. Cook's Distance plot

# --- Preparation for graphing
# - Create graphing dataset
datGraph_PD <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(PD_Final),4),Facet_label="BR-model: Performing to Default")
datGraph_PS <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(PS_Final),4),Facet_label="BR-model: Performing to Settlement")
datGraph_DS <- data.table(i=1:datAggr_train[,.N], Date=datAggr_train$Date,
                          CooksD=round(cooks.distance(DS_Final),4),Facet_label="BR-model: Default to Settlement")

# - Aesthetic engineering: general
chosenFont <- "Cambria"; dpi <- 200
vCol = brewer.pal(10, name="Paired")[c(1,2,6)]

# - Aesthetic engineering: Encircle outliers
datGraph_PD[, Encircled := ifelse(CooksD/max(CooksD) >= 0.3, CooksD, NA)]
datGraph_PS[, Encircled := ifelse(CooksD/max(CooksD) >= 0.3, CooksD, NA)]
datGraph_DS[, Encircled := ifelse(CooksD/max(CooksD) >= 0.8, CooksD, NA)]



# --- Graphing logic: custom function
# NOTE: vCol must have 3 colours: 1 for fill, 1 for line colour, 1 for encircled points
plotCooksD <- function(datGiven, vCol, chosenFont, dpi=200, fileName="") {
  (g1 <- ggplot(datGiven,aes(x=Date,y=CooksD,ymin=min(CooksD),ymax=CooksD)) + theme_minimal() +
     labs(x="Calendar date (months)", y=bquote("Cook's distance "~italic(D[C])), family=chosenFont) + 
     theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-8, 0, 0, 0),
           axis.text.x=element_text(angle=90), 
           strip.background=element_rect(fill="snow2", colour="snow2"),
           strip.text=element_text(size=10, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
     geom_linerange(colour=vCol[1],linewidth=0.4) + geom_point(shape=16,colour=vCol[2],size=1) +
     geom_point(aes(x=Date, y=Encircled), shape=1,size=4, colour=vCol[3]) + # Encircled points
     facet_grid(Facet_label ~ .) +
     scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
     scale_y_continuous(breaks=pretty_breaks()))
  
  if (fileName != "") {
    ggsave(g1, file=fileName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white") 
  }
  return(g1)
}



# --- Create graphs
plotCooksD(datGiven=datGraph_PD, vCol=vCol, chosenFont, dpi=200, fileName=paste0(genFigPath, "CooksDist_PD.png"))
plotCooksD(datGiven=datGraph_PS, vCol=vCol, chosenFont, dpi=200, fileName=paste0(genFigPath, "CooksDist_PS.png"))
plotCooksD(datGiven=datGraph_DS, vCol=vCol, chosenFont, dpi=200, fileName=paste0(genFigPath, "CooksDist_PS.png"))

