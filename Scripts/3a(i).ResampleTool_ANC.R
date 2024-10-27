# ============================= SUBSAMPLING & RESAMPLING SCHEME FOR PD MODELS ===========================
# A tool for investigating subsampling & resampling parameters iteratively towards a PD-modelling setup 
# (cross-sectiona modellingl
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This ancillary & exploratory script implements a given sample size by first subsampling raw data
# using 2-way stratified sampling before resampling into a basic cross-validation set (training:validation)
# controlled by the sampling fraction, also using the same 2-way stratified sampling design.
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - Event rate graph across resampled set
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Confidence interval parameter
confLevel <- 0.95

# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
currStatusVar <- "DefaultStatus1"
timeVar <- "Date"

# - Subset given dataset accordingly; an efficiency enhancement
datCredit <- subset(datCredit_real, select=unique(c(stratifiers,targetVar,currStatusVar,timeVar))) %>% drop_na()
datCredit[is.na(get(targetVar)), .N] == 0 # should be true
rm(datCredit_real); gc()

# - Subsampling & resampling parameters
smp_size <- 1500000 # fixed size of downsampled set
train_prop <- 0.7 # sampling fraction for resampling scheme




# ------ 2. Subsampled resampling scheme: basic cross-validation with 2-way stratified random sampling

# - Preliminaries
smp_perc <- smp_size/datCredit[, .N] # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
#datCredit_smp <- datCredit %>% group_by(DefaultStatus1_lead_12_max, Date) %>% slice_sample(prop=train_prop) %>% as.data.table() # should yield same
datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
datCredit_train <- datCredit_smp %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=train_prop) %>% as.data.table()
#datCredit_train <- datCredit_smp %>% group_by(DefaultStatus1_lead_12_max, Date) %>% slice_sample(prop=train_prop) %>% as.data.table() # should yield same
datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table()




# ------ 3. Frequency analysis on n-way stratified inner sampling technique

# - Determine subsampling window given cross-sectional design
def_StartDte <- min(datCredit_smp[,get(timeVar)], na.rm=T)
def_EndDte <- max(datCredit_smp[,get(timeVar)], na.rm=T)
maxDate <- def_EndDte - years(1) # A post-hoc filter, used for graphing purposes, given a 12-month outcome window

# - Aggregate data according to the same n-way stratified sampling technique used within subsampling/resampling scheme
datStrata <- datCredit_train[,list(Time=get(timeVar), Target=factor(get(targetVar)))][
              Time >= def_StartDte & Time <= maxDate, list(Freq = .N), by=list(Target, Time)] %>% setkey(Time, Target)
datStrata[, Freq_Time := sum(Freq,na.rm=T), by=list(Time)]
datStrata[, Freq_Perc := Freq/sum(Freq,na.rm=T)]
datStrata[, Freq_Time_Perc := Freq/Freq_Time]
table(datCredit_train[get(timeVar) >= def_StartDte & get(timeVar) <= maxDate,get(targetVar)], 
      datCredit_train[get(timeVar) >= def_StartDte & get(timeVar) <= maxDate,get(timeVar)]) %>% prop.table() # should give same results

# - Aesthetics engineering
datStrata[, Facet_label := "Worst-ever aggregation approach"]

# - Create summaries for annotations within graph
table(datCredit_train[get(timeVar) >= def_StartDte & get(timeVar) <= maxDate, get(targetVar)]) %>% prop.table() # Prior probability P(D=1) over all observations: ~ 8.1%
mean(datStrata[Target==1, Freq_Time_Perc], na.rm=T) # average E_t( P(D=1) ) over all time t ~ 8.0%
datStrata_aggr <- datStrata[, list(StratumSize_N = .N, StratumSize_Min = min(Freq,na.rm=T), StratumSize_Mean = mean(Freq,na.rm=T),
                                   StratumSize_SD = sd(Freq,na.rm=T))]
datStrata_aggr[, StrataSize_Margin := qnorm(1-(1-confLevel)/2) * datStrata_aggr$StratumSize_SD / sqrt(datStrata_aggr$StratumSize_N)]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
vCol <- brewer.pal(8, "Dark2")[c(1,2)]
vFill <- brewer.pal(8, "Set2")[c(1,2)]

# - Create graph to evidence minimum strata sizes
(g0 <- ggplot(datStrata, aes(x=Time, y=Freq, group=Target)) + theme_minimal() + 
    labs(x=bquote("Reporting date (months) "*italic(t)), y=bquote("Account volumes in "*italic(D[T])~": "*.(round(train_prop*smp_size/1000))*"k observations")) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main bar graph
    geom_bar(position="stack", stat="identity", aes(colour=Target, fill=Target), linewidth=0.25) + 
    # annotations
    annotate("text", x=as.Date("2013-02-28"), y=datStrata_aggr$StratumSize_Mean, size=3, family=chosenFont,
             label=paste0(datStrata_aggr$StratumSize_N, " total strata with a mean cell size of ", 
                          comma(datStrata_aggr$StratumSize_Mean, accuracy=0.1),
                          " ± ", sprintf("%.1f", datStrata_aggr$StrataSize_Margin), " and a minimum size of ", 
                          sprintf("%.0f", datStrata_aggr$StratumSize_Min))) +     
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="Default outcome (12-months)", values=vCol) + 
    scale_fill_manual(name="Default outcome (12-months)", values=vFill) + 
    scale_y_continuous(breaks=pretty_breaks(), label=comma) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") )

# - Save graph
ggsave(g0, file=paste0(genFigPath, "StrataDesign_Train_", round(smp_size/1000),"k.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datStrata, datStrata_aggr, g0)



# ------ 4. Graphing event rates over time given resampled sets

# - Check representatives | dataset-level proportions should be similar
table(datCredit_smp[,get(targetVar)]) %>% prop.table()
table(datCredit_train[,get(targetVar)]) %>% prop.table()
table(datCredit_valid[,get(targetVar)]) %>% prop.table()

# - Merge samples together
datGraph <- rbind(datCredit[, list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar), Sample = "a_Full")],
                   datCredit_train[, list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar), Sample = "b_Train")],
                   datCredit_valid[, list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar), Sample = "c_Valid")])

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
def_StartDte <- min(datCredit[,get(timeVar)], na.rm=T)
def_EndDte <- max(datCredit[,get(timeVar)], na.rm=T)
maxDate <- def_EndDte - years(1) # A post-hoc filter, used for graphing purposes, given a 12-month outcome window

# - Aggregate to monthly level and observe up to given point
port.aggr <- datGraph[Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
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
### RESULTS: Sample-size dependent
# 100k-sample: Train: 0.57%; Validation: 0.87%
# 1m-sample: Train: 0.18%; Validation: 0.28%
# 1.5m-sample: Train: 0.16%; Validation: 0.22%
# 2m-sample: Train: 0.12%; Validation: 0.2%
# 4m-sample: Train: 0.08%; Validation: 0.14%

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
vCol <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
vLabel <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
             "b_Train"=bquote(italic(B)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(train_prop*smp_size/1000))*"k)"),
             "c_Valid"=bquote(italic(C)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round((1-train_prop)*smp_size/1000))*"k)"))

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
suppressWarnings(rm(port.aggr, port.aggr2, datGraph, datCredit, datCredit_smp, datCredit_train, datCredit_valid, g2))