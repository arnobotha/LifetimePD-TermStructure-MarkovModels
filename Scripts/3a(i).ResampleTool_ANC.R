# ========================== SUBSAMPLING & RESAMPLING SCHEME FOR MARKOV MODELS =========================
# A tool for investigating subsampling & resampling parameters interactively towards obtaining a 
# training and validation dataset that is representative to the population.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Roland Breedt (RB)

# DESCRIPTION:
# This ancillary & exploratory script implements a given sample size by first subsampling raw data
# using stratified sampling before resampling into a basic cross-validation set (training:validation)
# controlled by the sampling fraction, also using the stratified sampling design.
# Representativeness is checked through estimating a First-Order Time-Homogeneous Markov Chain on the
# various datasets and also comparing the 12-month worst ever default rate over time.
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2e.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2e.
#
# -- Outputs:
#   - Event rate graph across resampled set
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Only keep relevant columns for sampling analysis
datCredit_real<-datCredit_real[,list(Date_Origination, Date, LoanID, Counter, DefaultStatus1, DefaultStatus1_lead_12_max,
                                     MarkovStatus, MarkovStatus_Future)]

# - Overview
cat("Nr of Loan Accounts in Dataset = ",comma(datCredit_real[!duplicated(LoanID),.N]),"\n",sep="")
cat("Nr of observations in Dataset = ",comma(datCredit_real[,.N]),"\n",sep="")
### RESULTS: Nr of Loan Accounts in Dataset = 650715 of which there 47 939 860 observations

# - Resampling, stratification, and other general parameters
confLevel <- 0.95
smp_size <- 200000 # Number of keys/loans during the subsampling step
# Implied sampling fraction for the downsampling step
smp_perc <- smp_size/datCredit_real[Counter==1, .N] 
cat("Implied sampling fraction = ", round(smp_perc*100,3),"% of loan accounts","\n",sep="")
smp_frac<-0.7 # sampling fraction during resampling step (training vs validation), after subsampling





# ------ 2. Subsampling scheme

# --- 0. Initial feasibility check of proposed stratifier: Date_Origination

DatPlot <- datCredit_real[,list(NrOrig=.N),by=list(Date_Origination)]
plot(DatPlot$Date_Origination,DatPlot$NrOrig,type="p"); rm(DatPlot)


# --- 1. Clustered subsampling scheme with 1-way stratified random sampling (Date_Origination)
# - Set seed for sampling
set.seed(6,kind="Mersenne-Twister")

# - Training Key population
# Get unique subject IDs or keys from the full dataset
datKeys <- datCredit_real %>% subset(Counter==1, c("Date", "LoanID", "Date_Origination"))

# Use stratified random sampling to select at random some keys from which the training set will be populated 
datKeys_sampled <- datKeys %>% group_by(Date_Origination) %>% slice_sample(prop=smp_perc)

# - Obtain the associated loan records in creating the subsampled dataset
datCredit_smp <- copy(datCredit_real %>% subset(LoanID %in% datKeys_sampled$LoanID))
cat("Nr of observations in Subset = ",comma(nrow(datCredit_smp)),"\n",sep="")





# ------ 3. Resampling scheme 

# - Set seed and training set proportion
set.seed(1,kind="Mersenne-Twister")

# - Use 1-way stratified sampling by the origination date given the previously samnpled keys (loan IDs)
dat_train_keys <- datKeys_sampled %>% group_by(Date_Origination) %>% slice_sample(prop=smp_frac)

# - Create the subset dataset
datCredit_train <- datCredit_smp %>% subset(LoanID %in% dat_train_keys$LoanID)
datCredit_valid <- datCredit_smp %>% subset(!(LoanID %in% dat_train_keys$LoanID))

# - [SANITY CHECK] Ensuring that the resampling scheme reconstitutes the full (subsampled) dataset
cat( (datCredit_smp[,.N] == datCredit_train[,.N] + datCredit_valid[,.N]) %?% "SAFE: Resampling scheme implemented successfully\n" %:%
       "WARNING: Resampling scheme not implemented successfully.\n")
cat("Nr of observations in Training Set = ",comma(datCredit_train[,.N]),"\n",sep="")
cat("Nr of observations in Validation Set = ",comma(datCredit_valid[,.N]),"\n",sep="")

# - Actual proportion of the training set
datCredit_train[Counter==1,.N]/datCredit_smp[Counter==1,.N]*100
### RESULTS: training set consists of 67% of the loans from the subsampled set





# ------ 4. Graphing event rates over time given resampled sets
# - Check representatives | dataset-level proportions should be similar
table(datCredit_smp[,DefaultStatus1_lead_12_max]) %>% prop.table()
table(datCredit_train[,DefaultStatus1_lead_12_max]) %>% prop.table()
table(datCredit_valid[,DefaultStatus1_lead_12_max]) %>% prop.table()

# - Merge samples together
datGraph <- rbind(datCredit_real[, list(Time=Date, MarkovStatus=DefaultStatus1, Target=DefaultStatus1_lead_12_max, Sample = "a_Full")],
                   datCredit_train[, list(Time=Date, MarkovStatus=DefaultStatus1, Target=DefaultStatus1_lead_12_max, Sample = "b_Train")],
                   datCredit_valid[, list(Time=Date, MarkovStatus=DefaultStatus1, Target=DefaultStatus1_lead_12_max, Sample = "c_Valid")])

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
def_StartDte <- min(datCredit_real[,Date], na.rm=T)
def_EndDte <- max(datCredit_real[,Date], na.rm=T)
maxDate <- def_EndDte - years(1) # A post-hoc filter, used for graphing purposes, given a 12-month outcome window

# - Aggregate to monthly level and observe up to given point
port.aggr <- datGraph[MarkovStatus==0, list(EventRate = sum(Target, na.rm=T)/.N),
             by=list(Sample, Time)][Time >= def_StartDte & Time <= maxDate,] %>% setkey(Time)

# - Aesthetics engineering
port.aggr[, Facet_label := "Worst-ever aggregation approach"]

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- port.aggr[Sample == "b_Train", mean(EventRate, na.rm=T)]
stdError_EventRate <- port.aggr[Sample == "b_Train", sd(EventRate, na.rm=T)] / sqrt(port.aggr[Sample == "b_Train", .N])
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Calculate MAE over time by sample
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Time), names_from = c(Sample), values_from = c(EventRate))
(diag.samplingRep.train <- mean(abs(port.aggr2$a_Full - port.aggr2$b_Train)) * 100)
(diag.samplingRep.valid <- mean(abs(port.aggr2$a_Full - port.aggr2$c_Valid)) * 100)
(diag.samplingRep.trainValid <- mean(abs(port.aggr2$b_Train - port.aggr2$c_Valid)) * 100)

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
smp_size<-nrow(datCredit_smp)
vCol <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
vLabel <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
             "b_Train"=bquote(italic(B)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(smp_frac*smp_size/1000))*"k)"),
             "c_Valid"=bquote(italic(C)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round((1-smp_frac)*smp_size/1000))*"k)"))

# - Create graph 1 (all sets)
(g2 <- ggplot(port.aggr, aes(x=Time, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (months)", y=bquote("Conditional 12-month default rate (%) across sample "*italic(bar(D)))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    #annotations
    annotate("text", x=as.Date("2013-02-28"), y=port.aggr[Time <= "2008-12-31", mean(EventRate)]*1.15, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(B[t]))*': ", sprintf("%1.2f", mean_EventRate*100), "% ± ", 
                          sprintf("%.2f", margin_EventRate*100),"%'"), parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Time <= "2008-12-31", mean(EventRate)]*1.05,
             label=paste0("'MAE between '*italic(A)[t]*' and '*italic(B)[t]*': ", sprintf("%.2f", diag.samplingRep.train),"%'"),
             family=chosenFont, size=3, parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Time <= "2008-12-31", mean(EventRate)]*1,
             label=paste0("'MAE between '*italic(A)[t]*' and '*italic(C)[t]*': ", sprintf("%.2f", diag.samplingRep.valid),"%'"),
             family=chosenFont, size=3, parse=T) +      
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Time <= "2008-12-31", mean(EventRate)]*0.95,
             label=paste0("'MAE between '*italic(B)[t]*' and '*italic(C)[t]*': ", sprintf("%.2f", diag.samplingRep.trainValid),"%'"),
             family=chosenFont, size=3, parse=T) +     
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=size.v, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g2, file=paste0(genFigPath, "DefaultRates_SampleRates_Subsample-", round(smp_size/1000),"k.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- Cleanup
suppressWarnings(rm(port.aggr, port.aggr2, datGraph, datCredit, datCredit_smp, datCredit_train, datCredit_valid, g2, datCredit_real,
                    datKeys, datKeys_sampled, dat_train_keys)); gc()
