# ================================ BETA REGRESSION MODEL: Data Fusion 3 ================================
# Creating features/variables that are specific to the BR modelling technique
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

# -- Inputs:
#   - datCredit_train | Training set, created from subsampled set
#   - datCredit_valid | Validation set, created from subsampled set
#
# -- Outputs:
#   - datCredit_train | Training set, created from subsampled set, and amended for this technique
#   - datCredit_valid | Validation set, created from subsampled set, and amended for this technique
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)


# --- Technique-specific feature engineering
### NOTE: Transition rates are the outcomes from the outcome variable for our beta regression (BR) models.
# We create these outcome variables in this technique-specific script by aggregating them from a given dataset
# to the portfolio-level. If these rates are created instead in the subsampled set, then they will be identical 
# between the subsequent training and validation set, thereby defeating the purpose of validation on unseen data.
# We therefore calculate the transition rates separately within both the training and validation sets to ensure 
# at least some variation in the outcome variable between these two sets.

# -- Training set
# - Aggregate to cohort-level and create transition rates: From Performing state
datAggr_P_train <- datCredit_train[MarkovStatus=="Perf",list(Y_PerfToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                                   by=list(Date)]

# - Aggregate to cohort-level and create transition rates: From default state
datAggr_D_train <- datCredit_train[MarkovStatus=="Def",list(Y_DefToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                                   by=list(Date)]

# -- Validation set
# - Aggregate to cohort-level and create transition rates: From Performing state
datAggr_P_valid <- datCredit_valid[MarkovStatus=="Perf",list(Y_PerfToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                                             Y_PerfToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                                   by=list(Date)]

# - Aggregate to cohort-level and create transition rates: From default state
datAggr_D_valid <- datCredit_valid[MarkovStatus=="Def",list(Y_DefToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                                            Y_DefToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                                   by=list(Date)]


# -- Merge sets together and create lagged versions of each transition rate
datAggr_train <- merge(datAggr_P_train, datAggr_D_train, by="Date")
datAggr_valid <- merge(datAggr_P_valid, datAggr_D_valid, by="Date")


# -- Create lagged versions of the transition rate, which are used as input variables.
# - Training set
datAggr_train[, Prev_PD := shift(Y_PerfToDef,n=1,type="lag",fill=0)]
datAggr_train[, Prev_PP := shift(Y_PerfToPerf,n=1,type="lag",fill=0)]
datAggr_train[, Prev_PS := shift(Y_PerfToSet,n=1,type="lag",fill=0)]
datAggr_train[, Prev_DP := shift(Y_DefToPerf,n=1,type="lag",fill=0)]
datAggr_train[, Prev_DW := shift(Y_DefToWO,n=1,type="lag",fill=0)]
datAggr_train[, Prev_DD := shift(Y_DefToDef,n=1,type="lag",fill=0)]
datAggr_train[, Prev_DS := shift(Y_DefToSet,n=1,type="lag",fill=0)]
# - Validation set
datAggr_valid[, Prev_PD := shift(Y_PerfToDef,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_PP := shift(Y_PerfToPerf,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_PS := shift(Y_PerfToSet,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_DP := shift(Y_DefToPerf,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_DW := shift(Y_DefToWO,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_DD := shift(Y_DefToDef,n=1,type="lag",fill=0)]
datAggr_valid[, Prev_DS := shift(Y_DefToSet,n=1,type="lag",fill=0)]

# -- Retain relevant features
# Training set
datAggr_train_features <- datCredit_train[!duplicated(Date),] %>% 
  select(contains("Date")|(contains("M_",ignore.case = FALSE)|contains("Aggr")|contains("Ave")))
# validation set
datAggr_valid_features <- datCredit_valid[!duplicated(Date),] %>% 
  select(contains("Date")|(contains("M_",ignore.case = FALSE)|contains("Aggr")|contains("Ave")))
# Remove stratifier since it will not be used; an expediency
datAggr_train_features[,Date_Origination:=NULL]
datAggr_valid_features[,Date_Origination:=NULL]

# - Merge the portfolio level features
datAggr_train <- merge(datAggr_train, datAggr_train_features, by="Date", all.x=T); gc()
datAggr_valid <- merge(datAggr_valid, datAggr_valid_features, by="Date", all.x=T); gc()

# --- Save datasets for later consumption
pack.ffdf(paste0(genPath, "creditdata_train_BR"), datAggr_train); gc()
pack.ffdf(paste0(genPath, "creditdata_valid_BR"), datAggr_valid); gc()

# - Cleanup
rm(datAggr_D_train, datAggr_D_valid, datAggr_P_valid, datAggr_P_train,
   datAggr_train, datAggr_valid)
