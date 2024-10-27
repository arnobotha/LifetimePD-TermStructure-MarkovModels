# ============================= SUBSAMPLING & RESAMPLING SCHEME FOR PD MODELS ==========================
# Determining the effect of a wide range of subsample sizes within a resampling scheme amidst a 
# cross-sectional modelling setup (PD-modelling)
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This ancillary & exploratory script iterates across a given vector of subsample sizes towards
# resampling data into a basic cross-validation setup (training:validation), using 2-way stratified 
# sampling. Each chosen size is executed multiple times over various seed values to account for randomness
# in following a broader Monte Carlo setup. Thereafter, various error measures are calculated within 
# each iteration, whereupon these error values are appropriately aggregated and graphed into a 
# single cohesive graph.
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
#   - Graph: Mean error rates across subsample sizes
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - General
cpu.threads <- 6
confLevel <- 0.95

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Subsampling & resampling parameters
train_prop <- 0.7 # sampling fraction for resampling scheme
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
currStatusVar <- "DefaultStatus1"
timeVar <- "Date"

# - Subset given dataset accordingly; an efficiency enhancement
datCredit <- subset(datCredit_real, select=unique(c(stratifiers,targetVar,currStatusVar,timeVar))) %>% drop_na()
datCredit[is.na(get(targetVar)), .N] == 0 # should be true
rm(datCredit_real); gc()

# - Calculate prior probability of default event over all time on population. 
# NOTE: Precalculating this is merely a coding optimisation
prior_pop <- datCredit[, list(Target=get(targetVar))][Target==1,.N] / datCredit[,.N] 

# - Calculate 12-month conditional default rate on population.
# NOTE: Precalculating this is merely a coding optimisation
def_StartDte <- min(datCredit[,get(timeVar)], na.rm=T)
def_EndDte <- max(datCredit[,get(timeVar)], na.rm=T)
maxDate <- def_EndDte - years(1)
eventRate_pop <- datCredit[, list(Target=get(targetVar), 
                                       Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
                    by=list(Time)][Time >= def_StartDte & Time <= maxDate, ][order(Time), EventRate]
plot(eventRate_pop, type="b")


# - Iteration parameters
smp_size_v <- c(100000,150000,200000,250000,375000,500000,625000,750000,875000,1000000,1250000,1500000,1750000,2000000,
                2500000,3000000,3500000,4000000,4500000,5000000,6000000,7000000,8000000,9000000,10000000,12500000,15000000,20000000)
seed_v <- c(1:100)



# ------ 2. Subsampled resampling scheme: basic cross-validation with random sampling

# --- Defines function for applying a subsampled resampling scheme given parameters on given data
# This function serves as an "outer job" to be called within a multithreaded environment
# - Inputs: smp_size: Subsample size; train_prop: sampling fraction for resmpling scheme;
# stratifiers: vector of stratification field names for n-way stratified sampling inner technique;
# targetVar: outcome field name within cross-sectional modelling (also first element of [stratifiers]);
# currStatusVar: current status field name within cross-sectional modelling for event rate calculations
# timeVar: field name of date for event rate calculations; seed: specific seed value
# prior_pop: pre-calculated prior probability within population for error measurement
# eventRate_pop: pre-calculated event rates over time within population for error measurement
# datGiven: given dataset from which to subsample and resample
subSmp_strat <- function(smp_size, train_prop, stratifiers=NA, targetVar=NA, currStatusVar=NA, timeVar=NA, seed=123, 
                         prior_pop=NA, eventRate_pop=NA, datGiven) {
  
  # - Preliminaries: Error Checks
  if (any(is.na(stratifiers)) & is.na(targetVar)) { stop("Stratifiers and target variables are unspecified! Must at least include the target variable")}
  if (any(is.na(stratifiers)) & !is.na(targetVar)) { stratifiers <- targetVar}
  if (any(!is.na(stratifiers)) & is.na(targetVar)) { targetVar <- stratifiers[1] }
  
  # - Preliminaries: assignments
  smp_perc <- smp_size/datGiven[, .N] # Implied sampling fraction for downsampling step
  
  # - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
  set.seed(seed)
  datCredit_smp <- datGiven %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
  datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme
  
  # - Implement resampling scheme using given main sampling fraction
  set.seed(seed)
  datCredit_train <- datCredit_smp %>% group_by(across(all_of(stratifiers)))  %>% slice_sample(prop=train_prop) %>% as.data.table()
  datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table()
  
  
  # --- Calculate error measure 1: Difference in prior probabilities between population and training (as subsampled + resampled)
  # Calculate prior probabilities within each relevant dataset, e.g., proportion of defaults across all time
  
  # - Population
  if (is.na(prior_pop)) {
    prior_pop <- datGiven[, list(Target=get(targetVar))][Target==1,.N] / datGiven[,.N] # population-level 
  }
  
  # - Subsampled + resampled training set
  prior_train <- datCredit_train[, list(Target=get(targetVar))][Target==1,.N] / datCredit_train[,.N] # training set level
  
  # - Compare population with training set using chosen error measure
  err_priorProb_AE <- abs(prior_pop - prior_train) # absolute error
  err_priorProb_SqrdErr <- (prior_pop - prior_train)^2 # squared error
  
  # - create output table (preliminary)
  datTemp <- data.table("SubSampleSize"=smp_size, "SampleFrac"=train_prop, "Stratifiers"=paste(stratifiers, collapse="; "),
                        "Seed"=seed, "Err_PriorProb_AE" = err_priorProb_AE, "Err_PriorProb_SqrdErr" = err_priorProb_SqrdErr)
  
  
  # --- Calculate error measure 2: MAE between 2 time series of the event rate between population/training and training/validation (as subsampled + resampled)
  # NOTE: event rate is a 12-month conditional event rate, e.g., k-month default rate at t+k given that event has not happened at t
  # This is an optional error measure
  if (!is.na(currStatusVar) & !is.na(timeVar)) {

    # - Population
    if (any(is.na(eventRate_pop))) {
      def_StartDte <- min(datGiven[,get(timeVar)], na.rm=T)
      def_EndDte <- max(datGiven[,get(timeVar)], na.rm=T)
      maxDate <- def_EndDte - years(1)
      eventRate_pop <- datGiven[, list(Target=get(targetVar), 
                                             Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
               by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    }
    
    # - Subsampled + resampled training set
    def_StartDte <- min(datCredit_train[,get(timeVar)], na.rm=T)
    def_EndDte <- max(datCredit_train[,get(timeVar)], na.rm=T)
    maxDate <- def_EndDte - years(1)
    eventRate_train <- datCredit_train[, list(Target=get(targetVar), 
                                           Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
              by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    
    
    # - Subsampled + resampled validation set
    eventRate_valid <- datCredit_valid[, list(Target=get(targetVar), 
                                              Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
               by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    
    
    # - Compare event rates across different sets using chosen error measure
    err_eventRate_MAE_train <- mean(abs(eventRate_pop - eventRate_train), na.rm=t) # mean absolute error
    err_eventRate_MAE_valid <- mean(abs(eventRate_pop - eventRate_valid), na.rm=t) # mean absolute error
    err_eventRate_MAE_trainvalid <- mean(abs(eventRate_train - eventRate_valid), na.rm=t) # mean absolute error
    
    # - Append error value to output table
    datTemp <- data.table(datTemp, "Err_EventRate_PopTrain_MAE" = err_eventRate_MAE_train, 
                          "Err_EventRate_PopValid_MAE" = err_eventRate_MAE_valid,
                          "Err_EventRate_TrainValid_MAE" = err_eventRate_MAE_trainvalid)
  }

  # Return value of chosen error measure
  return(datTemp)
} # end of function


# - Testing function call
ptm <- proc.time() #IGNORE: for computation time calculation
subSmp_strat(smp_size=1500000, train_prop=train_prop, seed=1, 
             stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar, 
             prior_pop=prior_pop, eventRate_pop=eventRate_pop, datGiven=datCredit)
proc.time() - ptm  #IGNORE: for computation time calculation



# --- Main Loop (outer function call)

cl.port <- makeCluster(cpu.threads)
registerDoParallel(cl.port)

cat(paste0("1 (", Sys.time(),"). Iterating across subsample sizes ..."),
    file="subsampleLoop.txt", append=F)

ptm <- proc.time() #IGNORE: for computation time calculation

# - Multithreaded looping procedure using the foreach-package
datResults <- foreach(it=1:(length(seed_v)*length(smp_size_v)), .combine='rbind', .verbose=F, .inorder=T, 
                         .packages=c('dplyr','data.table', 'lubridate', "scales"), .export=unique(c('subSmp_strat'))) %dopar%
  {
    # - Testing 
    #it <- 101
    
    # - Set indices
    iSeed <- (it-1) %% length(seed_v) + 1 # modulo operation
    iSize <- (it-1) %/% length(seed_v) + 1 # integer-valued division
    
    # - Iterate 
    temp <-subSmp_strat(smp_size=smp_size_v[iSize], train_prop=train_prop, seed=seed_v[iSeed],
                        stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar,
                        prior_pop=prior_pop, eventRate_pop=eventRate_pop, datGiven=datCredit)
    
    # - Reporting
    if (iSeed == length(seed_v)) {
      cat(paste0("\n2 (", Sys.time(),"). Subsample size: ", comma(smp_size_v[iSize]), " tested ",length(seed_v), " times."),
          file="subsampleLoop.txt", append=T) 
    }
    
    return(temp)
  }  

t <- proc.time() - ptm  #IGNORE: for computation time calculation
cat(paste0("\n3 (", Sys.time(),"). ForEach-loop done. Elapsed time: ", round(t[3]/60), " minutes."),
    file="subsampleLoop.txt", append=T)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath, "subSampleSizes"), datResults); gc()
stopCluster(cl.port)




# --- Graphing

# - Load in Dataset
if (!exists('datResults')) unpack.ffdf(paste0(genObjPath,"subSampleSizes"), tempPath)


# - Aggregate to subsample size level
datGraph <- datResults[, list(PriorProb_MAE = mean(Err_PriorProb_AE , na.rm=T), PriorProb_MAE_SD = sd(Err_PriorProb_AE , na.rm=T),
                  PriorProb_RMSE = sqrt(sum(Err_PriorProb_SqrdErr, na.rm=T)/.N), PriorProb_RMSE_SE = sd(Err_PriorProb_SqrdErr, na.rm=T),
                  EventRate_PopTrain_MAE_Mean = mean(Err_EventRate_PopTrain_MAE, na.rm=T), EventRate_PopTrain_MAE_SD = sd(Err_EventRate_PopTrain_MAE, na.rm=T),
                  EventRate_TrainValid_MAE_Mean = mean(Err_EventRate_TrainValid_MAE, na.rm=T), EventRate_TrainValid_MAE_SD = sd(Err_EventRate_TrainValid_MAE, na.rm=T),N=.N),
           by=list(SubSampleSize)]

# - Create 95% confidence interval for point estimate (mean) : Population-training set comparison
datGraph[, EventRate_MAE_PopTrain_Mean_ErrMargin := (qnorm(1-(1-confLevel)/2)*EventRate_PopTrain_MAE_SD/sqrt(N))]
datGraph[, EventRate_MAE_PopTrain_Mean_lower := EventRate_PopTrain_MAE_Mean - EventRate_MAE_PopTrain_Mean_ErrMargin]
datGraph[, EventRate_MAE_PopTrain_Mean_upper := EventRate_PopTrain_MAE_Mean + EventRate_MAE_PopTrain_Mean_ErrMargin]

# - Create 95% confidence interval for point estimate (mean) : Training-Validation set comparison
datGraph[, EventRate_MAE_TrainValid_Mean_ErrMargin := (qnorm(1-(1-confLevel)/2)*EventRate_TrainValid_MAE_SD/sqrt(N))]
datGraph[, EventRate_MAE_TrainValid_Mean_lower := EventRate_TrainValid_MAE_Mean - EventRate_MAE_TrainValid_Mean_ErrMargin]
datGraph[, EventRate_MAE_TrainValid_Mean_upper := EventRate_TrainValid_MAE_Mean + EventRate_MAE_TrainValid_Mean_ErrMargin]

# - Create summary table for annotations within graph
datAnnotate <- datGraph[SubSampleSize %in% c(100000,150000,250000,375000,500000,625000,750000,875000, 1000000,1250000,1500000,1750000,
                                             2000000,2500000,4000000,5000000,10000000), 
                        list(`"Size "*italic(n[s])`=comma(SubSampleSize), 
                             `italic(E)*"["*epsilon(italic(n[s]))*"] for "*italic(D):italic(D[T])`=paste0(sprintf("%.3f", EventRate_PopTrain_MAE_Mean*100),"%"),
                             `95% CI`=paste0("± ",sprintf("%.4f",EventRate_MAE_PopTrain_Mean_ErrMargin*100),"%"),
                              `italic(E)*"["*epsilon(italic(n[s]))*"] for "*italic(D[T]):italic(D[V])`=paste0(sprintf("%.3f", EventRate_TrainValid_MAE_Mean*100),"%"),
                             `95% CI`=paste0("± ",sprintf("%.4f",EventRate_MAE_TrainValid_Mean_ErrMargin*100),"%"))]

# SCRATCH
plot(x=datGraph$SubSampleSize, y=datGraph$PriorProb_MAE, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$PriorProb_RMSE, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_PopTrain_MAE_Mean, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_TrainValid_MAE_Mean, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_MAE_PopTrain_Mean_ErrMargin, type="b")

# - Pivot for graphing purposes: 2 different set comparisons using single error measure
datGraph2 <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_PopTrain_MAE_Mean, b_EventRate_TrainValid=EventRate_TrainValid_MAE_Mean)],
                          cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value") %>% as.data.table()

datGraph2_lower <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_MAE_PopTrain_Mean_lower, b_EventRate_TrainValid=EventRate_MAE_TrainValid_Mean_lower)],
                          cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value_Lower") %>% as.data.table()
datGraph2_upper <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_MAE_PopTrain_Mean_upper, b_EventRate_TrainValid=EventRate_MAE_TrainValid_Mean_upper)],
                                cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value_Upper") %>% as.data.table()
datGraph2_margins <- merge(datGraph2_lower, datGraph2_upper, by=c("SubSampleSize", "Set"))
datGraph3 <- merge(datGraph2, datGraph2_margins, by=c("SubSampleSize", "Set"))

# - Find elbow point where differential between subsequent error values becomes negligible
# I.e., find x where 2nd derivative of f(x) is near zero
datEventRate_PopTrain_1st <- datGraph[order(SubSampleSize), list(Gradient = diff(EventRate_PopTrain_MAE_Mean) / diff(SubSampleSize))]$Gradient
datEventRate_TrainValid_1st <- datGraph[order(SubSampleSize), list(Gradient = diff(EventRate_TrainValid_MAE_Mean) / diff(SubSampleSize))]$Gradient
# plot(diff(datEventRate_PopTrain_1st), type="b", main="2nd derivative") % 2nd derivative is not smooth
# plot(diff(datEventRate_TrainValid_1st), type="b", main="2nd derivative") # 2nd derivative is neither smooth nor monotonic
# Find index of stationary points x such that f''(x) <= epislon
statPoint_PopTrain <- which(diff(datEventRate_PopTrain_1st) < 10^(-10.25))[1]
statPoint_TrainValid <- which(diff(datEventRate_TrainValid_1st) < 10^(-10.5))[1] # 2nd derivative is neither smooth nor monotonic
statPoint_TrainValid <- which(abs(datEventRate_TrainValid_1st) < 10^(-9))[1]
# Find corresponding x-values at index
SubSamp_PopTrain <- datGraph$SubSampleSize[statPoint_PopTrain+1] # (x + 1 to correspond to indices of original vector)
SubSamp_TrainValid <- datGraph$SubSampleSize[statPoint_TrainValid+1] # (x + 1 to correspond to indices of original vector)
# Find corresponding y-values at index
Err_PopTrain <- datGraph$EventRate_PopTrain_MAE_Mean[statPoint_PopTrain+1] # (x + 1 to correspond to indices of original vector)
Err_TrainValid <- datGraph$EventRate_TrainValid_MAE_Mean[statPoint_TrainValid+1] # (x + 1 to correspond to indices of original vector)

# - Create elbow annotation object
datAnnotate_elbow <- data.table(Set=c("a_EventRate_PopTrain", "b_EventRate_TrainValid"), 
                                SubSampleSize=c(SubSamp_PopTrain, SubSamp_TrainValid), Value=c(Err_PopTrain,Err_TrainValid),
                                Label=paste0( c(comma(SubSamp_PopTrain/1000000), comma(SubSamp_TrainValid/1000000)), "m") )


# Aesthetic engineering
datGraph3[, Facet_label := factor("'Comparison of 12-month default rate series across sets: '*(italic(D):italic(D[T]))*' ; '*(italic(D[T]):italic(D[V]))")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
col.v <- brewer.pal(10, "Paired")[c(10,8)]
fill.v <- brewer.pal(10, "Paired")[c(9,7)]
linetype.v <- c("solid", "dotted")
label.v <- list(expression(italic(D)*" vs "*italic(D[T])),
                expression(italic(D[T])*" vs "*italic(D[V])) )
label.v2 <- list(expression(italic(D)*" vs "*italic(D[T])),
                 expression(italic(D[T])*" vs "*italic(D[V])) )

# - Create main graph
(g1 <- ggplot(datGraph3, aes(x=SubSampleSize, y=Value, group=Set)) + theme_minimal() + 
  labs(x=bquote("Subsample size "*italic(n[s])*" = |"*italic(D[S])*"|"), y=bquote("Error measure value "*italic(E)*'['*epsilon(italic(n[s]))*"] (%)")) + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  # annotate elbow/stationary point beyond which error measure decreases markedly slower
  geom_point(data=datAnnotate_elbow, aes(x=SubSampleSize, y=Value, colour=Set), shape=1, size=5, show.legend=F) +
  geom_segment(data=datAnnotate_elbow, aes(x=SubSampleSize, xend=SubSampleSize, y=0, yend=Value, colour=Set),
               show.legend=F, linewidth=0.3, linetype="dashed") + 
  geom_label(data=datAnnotate_elbow, aes(x=SubSampleSize, label=Label, y=Value, colour=Set), show.legend = F, 
             nudge_y=0.0002, nudge_x=1050000, size=2) + 
  # main line graph with overlaid points
  geom_ribbon(aes(x=SubSampleSize, ymin=Value_Lower, ymax=Value_Upper, fill=Set), alpha=0.5) + 
  geom_line(aes(colour=Set, linetype=Set), linewidth=0.5) + 
  geom_point(aes(x=SubSampleSize, y=Value, colour=Set, shape=Set), size=1.3) + 
  # annotate data table
  annotate(geom="table", x=4000000, y=0.011, family=chosenFont, size=2.9,
             label=datAnnotate, parse=T) +
  # facets & scale options
  facet_grid(Facet_label ~ ., labeller=label_parsed) + 
  scale_colour_manual(name="Mean MAE", values=col.v, label=label.v) + 
  scale_shape_discrete(name="Mean MAE", label=label.v) + 
  scale_linetype_manual(name="Mean MAE", values=linetype.v, label=label.v) + 
  scale_fill_manual(name="95% CI for mean", values=fill.v, label=label.v2) + 
  scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
  scale_x_continuous(breaks=pretty_breaks(), label=label_comma(scale=0.000001, suffix="m"))
)

# - Save graph
ggsave(g1, file=paste0(genFigPath, "DefaultRates_SubSampleRates_Experiment.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datCredit, datGraph2, datGraph2_lower, datGraph2_upper, datGraph2_margins, datResults, datGraph3, datAnnotate, datAnnotate_elbow, g1)
gc()
