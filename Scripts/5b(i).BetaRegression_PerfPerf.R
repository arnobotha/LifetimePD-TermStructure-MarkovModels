# =================================== BETA REGRESSION MODEL: PERF-PERF =================================
# Fitting various beta regression models towards finalizing the input space of the final beta regression
# model in modelling the transition rate: Performing to Performing
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
#   - 5a.BetaRegression_DataFusion3.R
#
# -- Inputs:
#   - datCredit_train | Training set, created from subsampled set
#   - datCredit_valid | Validation set, created from subsampled set
#
# -- Outputs:
#   - <Analytics> | Input space
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train_BR"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid_BR"), tempPath)




# ------ 2. Modelling themes | Mu

# --- Portfolio-level

# --- Macroeconomic-level

# - Unemployment: lags (which are best?)
# asdfasdfasdf

# ---  Fusion step
# Combine insights mined from previous themes

# --- Link function


# ------ 3. Finalised input space of the model | Mu

# call RolandFunction() only here on validation; not prior.





# ------ 4. Modelling themes | Phi

# --- Portfolio-level

# --- Macroeconomic-level

# - Unemployment: lags (which are best?)
# asdfasdfasdf

# ---  Fusion step
# Combine insights mined from previous themes

# --- Link function



# ------ 5. Finalised input space of the model | Phi
