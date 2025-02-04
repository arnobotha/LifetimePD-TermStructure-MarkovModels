# ================================ BETA REGRESSION MODEL: Data Fusion 3 ================================
# We estimate a Markov chain across various datasets and compare results
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
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)



# --- 3. Transition Probability Matrix (TPM) Analysis

# --- Feasibility checks
# - Check Performing to Write-Off transition for a sufficient number of transitions
cat("Nr of performing to write-off transitions in the full dataset = ", 
    datCredit_smp[MarkovStatus=="Perf" & MarkovStatus_Future=="W_Off", .N],"\n",sep="")
cat("Nr of performing to write-off transitions in the full dataset = ", 
    datCredit_train[MarkovStatus=="Perf" & MarkovStatus_Future=="W_Off", .N],"\n",sep="")
cat("Nr of performing to write-off transitions in the full dataset = ", 
    datCredit_valid[MarkovStatus=="Perf" & MarkovStatus_Future=="W_Off", .N],"\n",sep="")
### RESULTS: The number of scarce transitions > 50 in the training set, which is deemed appropriate for modelling

# --- Estimating time-homogeneous stationary transition matrix of a Markov chain
datResults1 <- Markov_TPM(DataSetMC=datCredit_smp,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))
datResults2 <- Markov_TPM(DataSetMC=datCredit_train,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))
datResults3 <- Markov_TPM(DataSetMC=datCredit_valid,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath, "matTransition_sample"), datResults1); gc()
pack.ffdf(paste0(genObjPath, "matTransition_train"), datResults2); gc()
pack.ffdf(paste0(genObjPath, "matTransition_valid"), datResults3); gc()

# - Cleanup
rm(datCredit_smp, datCredit_train, datCredit_valid); gc()

