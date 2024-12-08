# ========================== SUBSAMPLED RESAMPLING & DATA FUSION FOR PD MODELS =========================
# Subsampling and resampling data prior to fusing the input space towards PD-modelling
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Marcel Muller (MM), Roland Breedt (RB)

# DESCRIPTION:
# This script performs the following high-level tasks:
#   1) Subsample main dataset into a more manageable but still representative dataset
#   2) Fuse subsampled set with input space
#   3) Screen input space against missingness and apply appropriate treatments
#   4) Perform feature engineering (transforms, ratio-type)
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
#   - creditdata_input1 | Imported input space from script 2a
#
# -- Outputs:
#   - datCredit_smp | Subsampled set, fused with input space, duly prepared
#   - datCredit_train | Training set, created from subsampled set
#   - datCredit_valid | Validation set, created from subsampled set
# ------------------------------------------------------------------------------------------------------


### AB [20241-12-07]: Busy aligning with LifetimePD-TermStructure_RecurrentEvents codebase, but not yet run/tested


# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)
if (!exists('datExclusions')) unpack.ffdf(paste0(genObjPath,"Exclusions-TruEnd-Enriched"), tempPath)

# - Resampling, stratification, and other general parameters
smp_frac<-0.7
smp_size <- 135000
# Implied sampling fraction for the downsampling step
smp_perc <- smp_size/datCredit_real[!duplicated(LoanID),.N] 
cat("Implied sampling fraction = ", round(smp_perc*100,3),"% of loan accounts","\n",sep="")
### RESULTS: Implied sampling fraction = 20.746%


# - Create indicator variables that do not overlap | Markov modelling preparation
datCredit_real[,Mark_Perf_Ind := ifelse(MarkovStatus_Future=="Perf",1,0)]
datCredit_real[,Mark_Def_Ind := ifelse(MarkovStatus_Future=="Def",1,0)]
datCredit_real[,Mark_Set_Ind := ifelse(MarkovStatus_Future=="Set",1,0)]
datCredit_real[,Mark_WO_Ind := ifelse(MarkovStatus_Future=="W_Off",1,0)]

# - Set Reference Category for multinomial model | Beta regression preparation
# (Performing for "From Performing" transitions and Default for "From Default" transitions)
datCredit_real[,Target_FromP := relevel(factor(MarkovStatus_Future),ref="Perf")]
datCredit_real[,Target_FromD := relevel(factor(MarkovStatus_Future),ref="Def")]





# ------ 2. Clustered subsampling scheme with 1-way stratified random sampling (Date_Origination)
# - Set seed for sampling
set.seed(6,kind="Mersenne-Twister")

# - Training Key population
# Get unique subject IDs or keys from the full dataset
datKeys <- datCredit_real %>% subset(Counter==1, c("Date", "LoanID", "Date_Origination"))
# Use stratified random sampling to select at random some keys from which the training set will be populated 
datKeys_sampled <- datKeys %>% group_by(Date_Origination) %>% slice_sample(prop=smp_perc)

# - Obtain the associated loan records in creating the subsampled dataset
datCredit_smp <- copy(datCredit_real %>% subset(LoanID %in% datKeys_sampled$LoanID))
cat("Nr of observations in Subset = ",nrow(datCredit_smp),"\n",sep="")
### RESULTS: Nr of observations in Subset = 9 588 659

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final_smp1a"), datCredit_smp)
pack.ffdf(paste0(genPath,"creditdata_final_sampledKeys"), datKeys_sampled)

# - Cleanup
rm(datKeys, datKeys_sampled, datCredit_real); gc()





# ------ 3. Fuse the input space with the subsampled prepared dataset
# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final_smp1a"), tempPath)
if (!exists('datInput.raw')) unpack.ffdf(paste0(genPath,"creditdata_input1"), tempPath)

# - Find intersection between fields in input space and those perhaps already in the main credit dataset
overlap_flds <- intersect(colnames(datCredit_smp), colnames(datInput.raw))
check.input1 <- length(overlap_flds) == 0 # FALSE; duplicate columns exists.
cat(check.input1 %?% 'SAFE: No overlapping fields in the input space and the main credit dataset' %:%
      'WARNING: Overlapping field(s) detected in the input space and the main credit dataset.')
# Conditional reporting
if (check.input1 == 0) {cat('NOTE: The following fields overlap: ', overlap_flds,"\n",sep="\t")}

# - Remove any additional variables that are not going to be used
suppressWarnings( datInput.raw[, `:=`(slc_status_final_pred7 = NULL, slc_status_final = NULL, 
                                      slc_curing_ind = NULL, datex = NULL)])

# - Format the date in the correct format for merging
datInput.raw[, date := as.Date(date, format="%Y-%m-%d")]

# - Rename the datasets for merging
colnames(datInput.raw)[colnames(datInput.raw) %in% c("date", "acct_no")] <- c("Date", "LoanID")

# - [SANITY CHECK] Check the data grain
check_input2a <- datInput.raw[, list(Freq = .N), by=list(LoanID, Date)][Freq>1, .N]
cat( (check_input2a == 0) %?% 'SAFE: Grain of {datInput.rawdatInput.raw} confirmed.\n' %:% 
       paste0('WARNING: Grain broken in {datInput.raw} for ', check_input2a, " cases.\n") )
# Data grain broken for some missing LoanIDs, though we aren't interested in these cases in anyways

# - Merge on LoanID and Date by performing a left-join
datCredit_smp <- merge(datCredit_smp, datInput.raw, by=c("Date", "LoanID"), all.x=T); gc()

# [SANITY CHECK] Confirm dataset's grain after fusion
check_input2b <- datCredit_smp[,list(Freqs = .N), by=list(LoanID, Date)][Freqs > 1,.N]
cat( (check_input2b == 0) %?% 'SAFE: Grain of {datCredit_smp} confirmed after fusion.\n' %:% 
       paste0('ERROR: Grain broken in {datCredit_smp} after fusion for ', check_input2b, " cases.\n") )

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4c"), datCredit_smp); gc()

# - Clean-up
rm(datInput.raw, check_input2a, check_input2b, overlap_flds); gc()





# ------- 4. Feature engineering for modelling purposes

# - Load in main dataset (subsampled)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

# --- 1. Missing value indicators for the input space variables
# NOTE: There are a lot of missing values for these variables because of system changes etc.
datCredit_smp[, value_ind_slc_pmnt_method := ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "", 1, 0)]
datCredit_smp[, value_ind_slc_days_excess := ifelse(is.na(slc_days_excess) | slc_days_excess == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_pre_lim_perc := ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_roll_ever_24 := ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_arr_dir_3 := ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_prepaid_perc_dir_12 := ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 1, 0)]
datCredit_smp[, value_ind_slc_past_due_amt := ifelse(is.na(slc_past_due_amt) | slc_past_due_amt == "", 1, 0)]

# - Check the missingness of the variables
# If they are more than 50% missing - remove
table(datCredit_smp$value_ind_slc_pmnt_method) %>% prop.table()              # missingness: 8.44% - keep the variable (categorical)
table(datCredit_smp$value_ind_slc_days_excess) %>% prop.table()              # missingness: 72.5% - discard the variable
table(datCredit_smp$value_ind_slc_acct_pre_lim_perc) %>% prop.table()        # missingness: 8.44% - keep the variable (numeric) 
table(datCredit_smp$value_ind_slc_acct_roll_ever_24) %>% prop.table()        # missingness: 8.45% - keep the variable (numeric + delinquency theme)     
table(datCredit_smp$value_ind_slc_acct_arr_dir_3) %>% prop.table()           # missingness: 8.44% - keep the variable (categorical + delinquency theme)        
table(datCredit_smp$value_ind_slc_acct_prepaid_perc_dir_12) %>% prop.table() # missingness: 8.44% - keep the variable (numeric)

# - Remove the variables that have missingness > 50%
suppressWarnings( datCredit_smp[, `:=`(value_ind_slc_days_excess = NULL, slc_days_excess = NULL)]); gc()



# --- 2. Basic statistics of each variable
# - Categorical variables
varList_Cat <- c('slc_pmnt_method','slc_acct_arr_dir_3',
                 'g0_Delinq','DefaultStatus1', 'DefaultStatus1_lead_12_max',
                 'PerfSpellResol_Type_Hist','HasLeftTruncPerfSpell',
                 'DefSpellResol_Type_Hist','HasLeftTruncDefSpell',
                 'Event_Type')
var_Info_Cat <- describe(subset(datCredit_smp, select = varList_Cat))

# - Numeric variables
varList_Num <- c('slc_past_due_amt','slc_acct_pre_lim_perc','slc_acct_prepaid_perc_dir_12','slc_acct_roll_ever_24',
                 'Age_Adj', 'PerfSpell_Num','Term','InterestRate_Nom','InterestRate_Margin','Principal','Instalment','Receipt_Inf','Arrears',
                 'Balance','TimeInPerfSpell','PerfSpell_Age', 'PerfSpell_Counter',
                 'TimeInDefSpell', 'DefSpell_Age', 'DefSpell_Counter',
                 'Event_Time', 'g0_Delinq_Num', 'g0_Delinq_SD', 'NewLoans_Aggr_Prop')
var_Info_Num <- suppressWarnings(describe(subset(datCredit_smp, select = varList_Num)))



# --- 3. Missing value treatment (categorical variables)
# Treatment: create "missing"-bin for all N/A values

# - Payment method
var_Info_Cat$slc_pmnt_method
# Merge with existing "Unknown" bin or empty values
datCredit_smp[, slc_pmnt_method := 
                ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "" | slc_pmnt_method == "Unknown",
                       "MISSING_DATA", slc_pmnt_method)]
# [SANITY CHECK] Confirm treatment success
cat( (sum(datCredit_smp$slc_pmnt_method == "" | is.na(datCredit_smp$slc_pmnt_method) | 
            datCredit_smp$slc_pmnt_method == "Unknown") == 0) %?% 
       'SAFE: Treatment successful for [slc_pmnt_method].\n' %:% 'ERROR: Treatment failed for [slc_pmnt_method] \n' )
(var_Info_Cat$slc_pmnt_method <- describe(datCredit_smp$slc_pmnt_method))
### RESULTS: Treatment for missingness was successful

# - Account-level arrears direction vs three months ago
var_Info_Cat$slc_acct_arr_dir_3
# Merge with existing "N/A" bin or empty values
datCredit_smp[, slc_acct_arr_dir_3 := 
                ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "" | slc_acct_arr_dir_3 == "N/A", 
                       "MISSING_DATA", slc_acct_arr_dir_3)]
# [SANITY CHECK] Confirm treatment success
cat( ( sum(datCredit_smp$slc_acct_arr_dir_3 == "" | is.na(datCredit_smp$slc_acct_arr_dir_3) |
             datCredit_smp$slc_acct_arr_dir_3 == "N/A") == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_arr_dir_3].\n' %:% 'ERROR: Treatment failed for [slc_acct_arr_dir_3] \n' )
(var_Info_Cat$slc_acct_arr_dir_3 <- describe(datCredit_smp$slc_acct_arr_dir_3))
### RESULTS: Treatment for missingness was successful



# --- 4. Missing value treatment (numeric variables)
# Analyse whether to use mean or median value imputation

# - Prepaid/available funds to limit
var_Info_Num$slc_acct_pre_lim_perc; hist(datCredit_smp$slc_acct_pre_lim_perc, breaks='FD')
datCredit_smp[is.na(slc_acct_pre_lim_perc), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution, with mean of ~0.09 vs median of 0,
# bounded by [0, 0.79] for 5%-95% percentiles; no outliers
# Use median imputation, given 8.44% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_pre_lim_perc_imputed_med := 
                ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 
                       median(slc_acct_pre_lim_perc, na.rm=TRUE), slc_acct_pre_lim_perc)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_pre_lim_perc_imputed_med), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_pre_lim_perc_imputed_med].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_pre_lim_perc_imputed_med] \n' )
(var_Info_Num$slc_acct_pre_lim_perc_imputed_med <- describe(datCredit_smp$slc_acct_pre_lim_perc_imputed_med)); hist(datCredit_smp$slc_acct_pre_lim_perc_imputed_med, breaks='FD')
### RESULTS: Imputation successful, with mean of 0.085 vs median of 0,
# bounded by [0, 0.73] for 5%-95% percentiles; no outliers

# - Number of times an account was in arrears over last 24 months
var_Info_Num$slc_acct_roll_ever_24; hist(datCredit_smp$slc_acct_roll_ever_24, breaks='FD')
datCredit_smp[is.na(slc_acct_roll_ever_24), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution with mean of 0.4892, though discrete values with 80% of data having 0-value.
# Use mean imputation, given 8.45% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_roll_ever_24_imputed_mean := 
                ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 
                       mean(slc_acct_roll_ever_24, na.rm=TRUE), slc_acct_roll_ever_24)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_roll_ever_24_imputed_mean), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_roll_ever_24_imputed_mean].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_roll_ever_24_imputed_mean] \n' )
(var_Info_Num$slc_acct_roll_ever_24_imputed_mean <- describe(datCredit_smp$slc_acct_roll_ever_24_imputed_mean)); hist(datCredit_smp$slc_acct_roll_ever_24_imputed_mean, breaks='FD')
### RESULTS: Imputation successful, categorical variable now has 6 distinct classes, with majority having 0-value, while
# the imputed cases (value of 0.48) being the second most prevalent.

# - Categorical version of slc_acct_roll_ever
datCredit_smp[,slc_acct_roll_ever_24_cat:=factor(ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "","Missing",slc_acct_roll_ever_24))]
cat((datCredit_smp[is.na(slc_acct_roll_ever_24_cat), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_roll_ever_24_cat].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_roll_ever_24_cat] \n')
(var_Info_Num$slc_acct_roll_ever_24_cat <- describe(datCredit_smp$slc_acct_roll_ever_24_cat))


# - Percentage-valued direction of prepaid/available funds - current compared to 12 months ago
var_Info_Num$slc_acct_prepaid_perc_dir_12; hist(datCredit_smp[slc_acct_prepaid_perc_dir_12<=5, slc_acct_prepaid_perc_dir_12])
datCredit_smp[is.na(slc_acct_prepaid_perc_dir_12), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution, with mean of ~2.26m vs median of 0, 
# bounded by [0, 3.34] for 5%-95% percentiles; some very large outliers
### AB: Scope for extreme value treatment if those outliers are correct; or use winsorized mean (Std-PrinciplesForDataPrep)
# Use median imputation, given 8.44% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_prepaid_perc_dir_12_imputed_med := 
                ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 
                       median(slc_acct_prepaid_perc_dir_12, na.rm=TRUE), slc_acct_prepaid_perc_dir_12)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_prepaid_perc_dir_12_imputed_med), .N] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_prepaid_perc_dir_12_imputed_med].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_prepaid_perc_dir_12_imputed_med] \n' )
describe(datCredit_smp$slc_acct_prepaid_perc_dir_12_imputed_med); hist(datCredit_smp[slc_acct_prepaid_perc_dir_12_imputed_med<=5, slc_acct_prepaid_perc_dir_12_imputed_med])
### RESULTS: Imputation successful, with mean of ~2m vs median of 0,
# bounded by [0, 2.99] for 5%-95% percentiles; extreme outliers

# - Amount by which the account is overdue at the associated reporting date
var_Info_Num$slc_past_due_amt; hist(datCredit_smp$slc_past_due_amt, breaks='FD')
datCredit_smp[is.na(slc_past_due_amt), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution, with mean of 2721 vs median of 0, 
# bounded by [0, 5714] for 5%-95% percentiles; some very large outliers
### MM: Scope for extreme value treatment if those outliers are correct; or use winsorized mean (Std-PrinciplesForDataPrep)
# Use median imputation, given 8.44% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_past_due_amt_imputed_med := 
                ifelse(is.na(slc_past_due_amt) | slc_past_due_amt == "", 
                       median(slc_past_due_amt, na.rm=TRUE), slc_past_due_amt)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_past_due_amt_imputed_med), .N] == 0) %?% 
       'SAFE: Treatment successful for [slc_past_due_amt_imputed_med].\n' %:% 
       'ERROR: Treatment failed for [slc_past_due_amt_imputed_med] \n' )
(var_Info_Num$slc_past_due_amt_imputed_med <- describe(datCredit_smp$slc_past_due_amt_imputed_med)); hist(datCredit_smp$slc_past_due_amt_imputed_med, breaks='FD')
### RESULTS: Imputation successful, with mean of 2491 vs median of 0,
# bounded by [0, 4638] for 5%-95% percentiles; extreme outliers

# - InterestRate_Margin (incorporating risk-based pricing info)
var_Info_Num$InterestRate_Margin; hist(datCredit_smp$InterestRate_Margin, breaks="FD")
datCredit_smp[is.na(InterestRate_Margin), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution (as expected), with mean of -0.007 vs median of -0.008, 
# bounded by [-0.02, 0.01] for 5%-95% percentiles; some negative outliers distort shape of distribution
# Use median imputation for any possible missingness
datCredit_smp[, InterestRate_Margin_imputed_mean := 
                ifelse(is.na(InterestRate_Margin) | InterestRate_Margin == "", 
                       median(InterestRate_Margin, na.rm=TRUE), InterestRate_Margin)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(InterestRate_Margin_imputed_mean), .N] == 0) %?% 
       'SAFE: Treatment successful for [InterestRate_Margin_imputed_mean].\n' %:% 
       'ERROR: Treatment failed for [InterestRate_Margin_imputed_mean] \n' )
(var_Info_Num$InterestRate_Margin_imputed_mean <- describe(datCredit_smp$InterestRate_Margin_imputed_mean)); hist(datCredit_smp$InterestRate_Margin_imputed_mean, breaks="FD")
### RESULTS: Imputation successful, with mean of -0.007 vs median of -0.008,
# bounded by [-0.02, 0.01] for 5%-95% percentiles; some negative outliers distort shape of distribution



# --- 5. Feature Engineering: ratio-type variables (Period-level)

# - Loan age to loan term
datCredit_smp[, AgeToTerm := Age_Adj/Term] # where the loan is in its lifetime
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(AgeToTerm), .N] == 0) %?% 
       'SAFE: New feature [AgeToTerm] has logical values.\n' %:% 
       'WARNING: New feature [AgeToTerm] has illogical values \n' )
(var_Info_Num$AgeToTerm <- describe(datCredit_smp$AgeToTerm)); hist(datCredit_smp[AgeToTerm<2, AgeToTerm], breaks='FD')
### RESULTS: Highly right-skewed distribution as expected, with mean of 0.37 vs median of 0.29,
# bounded by [0.03, 0.9] for 5%-95% percentiles; some large outliers (max: 224)

# - Balance to loan term | how much is still outstanding compared to Principal/Limit
datCredit_smp[, BalanceToPrincipal := Balance/Principal]
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(BalanceToPrincipal), .N] == 0) %?% 
       'SAFE: New feature [BalanceToPrincipal] has logical values.\n' %:% 
       'WARNING: New feature [BalanceToPrincipal] has illogical values \n' )
# distributional analysis
(var_Info_Num$BalanceToTerm <- describe(datCredit_smp$BalanceToPrincipal)); hist(datCredit_smp$BalanceToPrincipal, breaks='FD')
### RESULTS: Highly left-skewed distribution, with mean of 0.7 vs median of 0.85,
# bounded by [~0, 1] for 5%-95% percentiles; no outliers



# --- 6. Feature Engineering: Binning and factorisation
# - Condense the payment group
datCredit_smp[, pmnt_method_grp := 
                case_when(slc_pmnt_method == "Debit Order FNB account" | slc_pmnt_method == "Debit Order other bank" ~ "Debit Order",
                          slc_pmnt_method == "Salary" | slc_pmnt_method == "Suspense" ~ "Salary/Suspense",
                          TRUE ~ slc_pmnt_method)]
# [SANITY CHECK] Check new feature for illogical values
cat((datCredit_smp[is.na(pmnt_method_grp), .N] == 0) %?% 
      'SAFE: New feature [pmnt_method_grp] has logical values.\n' %:% 
      'WARNING: New feature [pmnt_method_grp] has illogical values \n' )
(var_Info_Cat$pmnt_method_grp <- describe(datCredit_smp$pmnt_method_grp))
### RESULTS: Bins grouped logically such that each bin now has sufficient observations, with proportions:
# Debit Order: 67.9%; MISSING_DATA: 9.7%; Salary/Suspense: 7.7%; Statement: 14.8%

# - Factorised [g0_Delinq] variable
datCredit_smp[,g0_Delinq_fac := as.factor(g0_Delinq)]
(var_Info_Cat$g0_Delinq_fac <- describe(datCredit_smp$g0_Delinq_fac))
### RESULTS: Proportion of observations in each g0 bucket:
# 0: 89.8%; 1: 5.5%; 2: 0.8%; 3: 3.9%
# [SANITY CHECK] Check new feature for illogical values
cat((anyNA(datCredit_smp$g0_Delinq_fac)) %?% 'WARNING: New feature [g0_Delinq_fac] has missing values. \n' %:%
      'SAFE: New feature [g0_Delinq_fac] has no missing values. \n')
### RESULTS: [g0_Delinq_fac] created without any missingness

# - Bin [InterestRate_Margin_imputed] | Binning the variable into three equally sized bins
datCredit_smp[, InterestRate_Margin_imputed_bin := factor(ntile(InterestRate_Margin_imputed_mean, n=3))]
(var_Info_Cat$InterestRate_Margin_Imputed_Bin <- describe(datCredit_smp$InterestRate_Margin_imputed_bin))
### RESULTS: Binning was successful into three equal sized bins each having 499 940 observations
# [SANITY CHECK] Check new feature for illogical values
cat((anyNA(datCredit_smp$InterestRate_Margin_imputed_bin)) %?% 'WARNING: New feature [InterestRate_Margin_imputed_bin] has missing values. \n' %:%
      'SAFE: New feature [InterestRate_Margin_imputed_bin] has no missing values. \n')
### RESULTS: [InterestRate_Margin_imputed_bin] created without any missingness



# --- 7. Feature Engineering: Inflating time-sensitive monetary variables to the latest date
#- Load macroeconmic information to facilitate inflation
if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)

# - Getting a range of inflation factors for each date in the sampling window
date_range <- ceiling_date(unique(datCredit_smp$Date), unit="month")-days(1)
datInflation <- data.table(Date=date_range)
datInflation[,Inf_Factor:=adjInflation_MV(datMacro=datMV, time="Date", Inflation_Growth="M_Inflation_Growth", g_start=Date, g_stop = date_range[length(date_range)]), by=Date]
datCredit_smp <- merge(datCredit_smp, datInflation, all.x=T, by="Date")

# - [SANITY CHECK]
cat((anyNA(datCredit_smp$Inf_Factor)) %?% paste0('WARNING: Inflation factor(s) is(are) missing for ', unique(datCredit_smp[is.na(Inf_Factor),Date]), '. \n') %:%
      'SAFE: Inflation factors created successfully. \n')
### RESULTS: [Inf_Factor] variables created successfully without any missingness

# - Deflate the relevant variables
datCredit_smp[, Principal_Real := Principal*Inf_Factor]
datCredit_smp[, Balance_Real := Balance*Inf_Factor]
datCredit_smp[, Instalment_Real := Instalment*Inf_Factor]

# - [SANITY CHECK]
cat( (all(anyNA(datCredit_smp$Principal_Real), anyNA(datCredit_smp$Balance_Real), anyNA(datCredit_smp$Instalment_Real)))
    %?% paste0('WARNING: Some values of [Principal_Real], [Balance_Real], and/or [Instalment_Real] not created successfully. \n') %:%
      'SAFE: Variables inflated successfully. \n')
(var_Info_Num$Principal_Real <- describe(datCredit_smp$Principal_Real))
(var_Info_Num$Balance_Real <- describe(datCredit_smp$Balance_Real))
(var_Info_Num$Instalment_Real <- describe(datCredit_smp$Instalment_Real))
### RESULTS: Variables created successfully without any missingness

# - Clean up
rm(datMV, date_range, datInflation)



# --- 8. Featuring Engineering: Portfolio-level information

# - Pre default delinquency rate
#Note: Creating an aggregated dataset with which to fuse to the full dataset
dat_g0_Delinq_Aggr <- data.table(datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq>0, na.rm=T)/.N), by=list(Date)])
colnames(dat_g0_Delinq_Aggr) <- c("Date", "g0_Delinq_Any_Aggr_Prop")
# Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
ColNames <- colnames(dat_g0_Delinq_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_g0_Delinq_Aggr[, (paste0(ColNames[j],"_Lag_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))] # Impute NA's with the non lagged value
  }
}
# [Sanity Check] Check for any missing values before merging the dat_g0_Delinq_Aggr dataset to datCredit_smp
cat((anyNA(dat_g0_Delinq_Aggr)) %?% 'WARNING: One of the new [g0_Delinq_Any_Aggr_Prop] features has missing values. \n' %:%
      'SAFE: New [g0_Delinq_Any_Aggr_Prop] features created sucessfully without any missing values. \n')
### RESULTS: [g0_Delinq_Any_Aggr_Prop] variables created successfully without any missingness

# Fusing the aggregated variable with its various lags to the full dataset
datCredit_smp <- merge(datCredit_smp, dat_g0_Delinq_Aggr, by="Date", all.x=T)
# [SANITY CHECK] Check new feature for illogical values
cat( ( sum(datCredit_smp[DefaultStatus1==0, sum(g0_Delinq_Any_Aggr_Prop + sum(g0_Delinq==0)/.N, na.rm=T), by=Date][,2])==sum(datCredit_smp[DefaultStatus1==0,.N,by=Date][,2]) & (sum(is.na(datCredit_smp$g0_Delinq_Any_Aggr_Prop))==0)) %?% 
       'SAFE: New feature [g0_Delinq_Any_Aggr_Prop] has logical values.\n' %:% 
       'WARNING: New feature [g0_Delinq_Any_Aggr_Prop] has illogical values \n' )
(var_Info_Num$g0_Delinq_Any_Aggr_Prop <- describe(datCredit_smp$g0_Delinq_Any_Aggr_Prop)); plot(unique(datCredit_smp$g0_Delinq_Any_Aggr_Prop), type="b")
### RESULTS: Variable has a logical trend, with mean of 0.059 vs median of 0.049, 
# bounded by [0.038, 0.12] for 5%-95% percentiles; no large outliers

# [SANITY CHECK] Check new feature for missingness after fusion
cat((anyNA(datCredit_smp$g0_Delinq_Any_Aggr_Prop)) %?% 'WARNING: New feature [g0_Delinq_Any_Aggr_Prop] has missing values. \n' %:%
      'SAFE: New feature [g0_Delinq_Any_Aggr_Prop] has no missing values. \n')
### RESULTS: [g0_Delinq_Any_Aggr_Prop] created without any missingness


# - Average pre-default delinquency level
datCredit_smp[,g0_Delinq_Ave:=mean(ifelse(DefaultStatus1==0,g0_Delinq,0), na.rm=T), by=Date]
# [SANITY CHECK] Check new feature for illogical values
cat( (sum(datCredit_smp[, sum(is.na(g0_Delinq_Ave)), by=Date][,2])==0) %?% 
       'SAFE: New feature [g0_Delinq_Ave] has logical values.\n' %:% 
       'WARNING: New feature [g0_Delinq_Ave] has illogical values \n' )
(var_Info_Num$g0_Delinq_Ave <- describe(datCredit_smp$g0_Delinq_Ave)); hist(datCredit_smp$g0_Delinq_Ave, breaks="FD")
### RESULTS: Follows a logical trend, with mean of 0.062 vs median of 0.051,
# bounded by [0.04, 0.126] for 5%-95% percentiles; no outliers

# - Create a lagged version of the aggregated default rate created in script 2f
dat_DefaultRate<-datCredit_smp[!duplicated(Date),list(Date,DefaultStatus1_Aggr_Prop)]
# Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
ColNames <- colnames(dat_DefaultRate)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_DefaultRate[, (paste0(ColNames[j],"_Lag_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))] # Impute NA's with the non lagged value
  }
}
# [Sanity Check] Check for any missing values before merging the dat_DefaultRate dataset to datCredit_smp
cat((anyNA(dat_DefaultRate)) %?% 'WARNING: One of the new DefaultRate features has missing values. \n' %:%
      'SAFE: New DefaultRate features created sucessfully without any missing values. \n')
### RESULTS: No missingness, continue with merge

# Fusing the various lagged versions of the DefaultRate to the full dataset
datCredit_smp <- merge(datCredit_smp, dat_DefaultRate[,-"DefaultStatus1_Aggr_Prop"], by="Date", all.x=T)
# [Sanity Check] Check if merge was successful by checking for missingness in the 12-month lagged version of the default rate
cat((anyNA(datCredit_smp$DefaultStatus1_Aggr_Prop_Lag_12)) %?% 'WARNING: Merge unsuccessful, NA values present. \n' %:%
      'SAFE: Merge successful, no NA values present. \n')
(var_Info_Num$DefaultRate_12 <- describe(datCredit_smp$DefaultStatus1_Aggr_Prop_Lag_12))
### RESULTS: No missingness after merge, merge successful. Original DefaultRate_12 has mean of 0.05 and median of 0.046; 
# bounded by [0.028, 0.084] for 5%-95% percentiles; no outliers


# - Ratio type variables (portfolio-level) during performance spells
# (Total) Arrears to (Total) Balance; (Total) Instalments to (Total) Balance
# NOTE: These portfolio-level aggregated variables are engineered to capture/ aggregate information only for accounts that are in a performance spell
# The resulting aggregated dataset can be fused to the full dataset
dat_Aggr <- data.table(datCredit_smp[DefaultStatus1==0, list(sum(Arrears, na.rm=T)/sum(Balance, na.rm=T)), by=list(Date)], # [ArrearsToBalance_Aggr]
                       datCredit_smp[DefaultStatus1==0, list(sum(Instalment, na.rm=T)/sum(Balance)), by=list(Date)][,2]) # [InstalmentToBalance_Aggr]
colnames(dat_Aggr) <- c("Date", "ArrearsToBalance_Aggr_Prop", "InstalmentToBalance_Aggr_Prop")
# Fusing the aggregated dataset to the full dataset
datCredit_smp <- merge(datCredit_smp, dat_Aggr, by="Date", all.x=T)
# [SANITY CHECK] Check new feature for illogical values
cat( (sum(datCredit_smp[, sum(is.na(ArrearsToBalance_Aggr_Prop)), by=Date][,2])==0) %?% 
       'SAFE: New feature [ArrearsToBalance_Aggr_Prop] has logical values.\n' %:% 
       'WARNING: New feature [ArrearsToBalance_Aggr_Prop] has illogical values \n' )
cat( (sum(datCredit_smp[, sum(is.na(InstalmentToBalance_Aggr_Prop)), by=Date][,2])==0) %?% 
       'SAFE: New feature [InstalmentToBalance_Aggr_Prop] has logical values.\n' %:% 
       'WARNING: New feature [InstalmentToBalance_Aggr_Prop] has illogical values \n' )
(var_Info_Num$InstalmentToBalance_Aggr_Prop <- describe(datCredit_smp$InstalmentToBalance_Aggr_Prop)); plot(unique(datCredit_smp$Date),unique(datCredit_smp$InstalmentToBalance_Aggr_Prop), type="b")
(var_Info_Num$ArrearsToBalance_Aggr_Prop <- describe(datCredit_smp$ArrearsToBalance_Aggr_Prop)); plot(unique(datCredit_smp$ArrearsToBalance_Aggr_Prop), type="b")
### RESULTS [InstalmentToBalance_Aggr_Prop]: Variable has high volatility around 2010 as seen through the graphical plot. Mean of 0.012 vs median of 0.012,
#            bounded by [0.011, 0.014] for 5%-95% percentiles; no outliers
#                   [ArrearsToBalance_Aggr_Prop]: Variable has  mean of 0.0006 vs median of 0.0005,
#                    bounded by [0.0004, 0.0015] for 5%-95% percentiles


# - Proportion of curing loans across performing/default spell type
datCredit_smp[, CuringEvents_Aggr_Prop := sum(PerfSpell_Counter==1 & PerfSpell_Num>=2, na.rm=T)/.N, by=list(Date)]
cat( (sum(datCredit_smp[, sum(is.na(CuringEvents_Aggr_Prop)), by=Date][,2])==0) %?% 
       'SAFE: New feature [CuringEvents_Aggr_Prop] has logical values.\n' %:% 
       'WARNING: New feature [CuringEvents_Aggr_Prop] has illogical values \n' )
(var_Info_Num$CuringEvents_Aggr_Prop <- describe(datCredit_smp$CuringEvents_Aggr_Prop)); plot(unique(datCredit_smp$CuringEvents_Aggr_Prop), type="b")
### RESULTS: Variable has mean of 0.0013 vs median of 0.0013,
# bounded by [0.0002, 0.0025] for 5%-95% percentiles; no outliers

# Clean up
rm(lags, ColNames, dat_DefaultRate, dat_g0_Delinq_Aggr, dat_Aggr)


# - Aggregated age-to-term of portfolio over time, i.e., percentage-based maturity
datCredit_smp[, AgeToTerm_Aggr_Mean := mean(Age_Adj/Term, na.rm=T), by=Date]
cat( (sum(datCredit_smp[, sum(is.na(AgeToTerm_Aggr_Mean)), by=Date][,2])==0) %?% 
       'SAFE: New feature [AgeToTerm_Aggr_Mean] has logical values.\n' %:% 
       'WARNING: New feature [AgeToTerm_Aggr_Mean] has illogical values \n' )
(var_Info_Num$AgeToTerm_Aggr_Mean <- describe(datCredit_smp$AgeToTerm_Aggr_Mean)); plot(unique(datCredit_smp$AgeToTerm_Aggr_Mean), type="b")
### RESULTS: Variable behaves as expected, i.e., increases as the loan portfolio matures. Has mean 0.37 and median 0.39
# bounded by [0.26, 0.41] for 5%-95% percentiles; no outliers


# - Aggregate maturity of performance spell ages over time
datCredit_smp[, PerfSpell_Maturity_Aggr_Mean := mean(PerfSpell_Age, na.rm=T), by=Date]
cat( (sum(datCredit_smp[, sum(is.na(PerfSpell_Maturity_Aggr_Mean)), by=Date][,2])==0) %?% 
       'SAFE: New feature [PerfSpell_Maturity_Aggr_Mean] has logical values.\n' %:% 
       'WARNING: New feature [Perf_SpellMaturity_Aggr_Mean] has illogical values \n' )
(var_Info_Num$PerfSpell_Maturity_Aggr_Mean <- describe(datCredit_smp$PerfSpell_Maturity_Aggr_Mean)); plot(unique(datCredit_smp$PerfSpell_Maturity_Aggr_Mean), type="b")
### RESULTS: Mean performance spell age seem to decrease over time. Has mean 138.7 and median 143.1;
# bounded by [107, 153] for 5%-95% percentiles; no outliers


# - Median-aggregated interest rate margin
# NOTE: The median is preferred over the mean since it resulted in a superior model, as investigated in the experimental script 3c(v)
# Creating an aggregated dataset
dat_IRM_Aggr <- datCredit_smp[, list(InterestRate_Margin_Aggr_Med = median(InterestRate_Margin_imputed_mean, na.rm=T)), by=list(Date)]
# Checking the time series of this variable
plot(dat_IRM_Aggr$InterestRate_Margin_Aggr_Med, type="b")
# Applying various lags
lags <- c(1,2,3,9) # Lags as found to be significant within the experimental script
dat_IRM_Aggr_Check1 <- data.table(Variable = NULL, # Dataset for conducting sanity checks
                                  Check = NULL)
ColNames <- colnames(dat_IRM_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_IRM_Aggr[, (paste0(ColNames[j],"_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))] # Impute NA's with non-lagged version of variable
  }
}
# [SANITY CHECK] Check whether the lags were created correctly
cat((anyNA(dat_IRM_Aggr[,InterestRate_Margin_Aggr_Med_1]) | anyNA(dat_IRM_Aggr[,InterestRate_Margin_Aggr_Med_2]) | anyNA(dat_IRM_Aggr[,InterestRate_Margin_Aggr_Med_3]) | anyNA(dat_IRM_Aggr[,InterestRate_Margin_Aggr_Med_9])) %?%
      "WARNING: Missingness detected, [InterestRate_Margin_Aggr_Med_1], [InterestRate_Margin_Aggr_Med_2] and/or [InterestRate_Margin_Aggr_Med_3] compromised.\n" %:%
      "SAFE: No missingness, [InterestRate_Margin_Aggr_Med_1], [InterestRate_Margin_Aggr_Med_2] and [InterestRate_Margin_Aggr_Med_3] created successfully.\n")
### RESULTS: Safe, no missingness, hence continue with merge

# Merging the credit dataset with the aggregated dataset
datCredit_smp <- merge(datCredit_smp, dat_IRM_Aggr, by="Date", all.x=T)
# Validate merging success )by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_IRM_Aggr))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_smp$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with aggregated data is successful.\n" %:%
       "WARNING: Missingness in certain aggregated fields detected, fusion compromised.\n")
(var_Info_Num$InterestRate_Margin_Aggr_Med <- describe(datCredit_smp$InterestRate_Margin_Aggr_Med)); plot(datCredit_smp[!duplicated(Date),InterestRate_Margin_Aggr_Med], type="b") # Only saving the base variable's descriptive statistics
### RESULTS: Variable follows a logical trend over time. Has mean -0.008 and median -0.009;
# bounded by [-0.012, -0.0035] for 5%-95% percentiles; no outliers

# Clean up
rm(dat_IRM_Aggr, dat_IRM_Aggr_Check1, list_merge_variables, results_missingness, output, lags, ColNames)



# --- 9. Macroeconomic feature engineering

# - Loading in the raw dataset
if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)

# - Lags of all MVs
# Specifying the lags (monthly) that should be applied
lags <- c(1,2,3,6,9,12)
# Creating a dataset with which to check if the lags are applied correctly to the macroeconomic variables
datMV_Check1 <- data.table(Variable = NULL, Check = NULL)
# Getting the column names with which to apply the lags
ColNames <- colnames(datMV)[-1]
# Looping over the specified lags and applying each to each of the specified columns
for (i in seq_along(lags)){
  for (j in seq_along(ColNames)){
    datMV[, (paste0(ColNames[j],"_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"), get(ColNames[j]))]
  }
}
# [SANITY CHECK] Check datMV for any missingness
cat( anyNA(datMV) %?% "WARNING: Missingness detected in the lagged macroeconomic variables.\n" %:%
       "SAFE: Lags applied successfully to the macroeconomic variables.\n")
### Results: Lagged variables created successfully, no missingness present


# - Rolling volatility/ standard deviations
# Specifying the rolling standard deviation windows (monthly) that should be applied
SD_windows <- c(4,5,6,9,12)
# Creating a dataset with which to check if the standard deviations are applied correctly to the macroeconomic variables
datMV_Check2 <- data.table(Variable = NULL,
                           Check = NULL)
# Getting the column names with which to apply the rolling standard deviations
ColNames <- colnames(datMV)[2:7]
# Looping over the specified lags and applying each to each of the specified columns
for (i in seq_along(SD_windows)){
  for (j in seq_along(ColNames)){
    datMV[, (paste0(ColNames[j],"_SD_",SD_windows[i])) := frollapply(get(ColNames[j]), n=SD_windows[i], FUN=sd, align="right")]
  }
}
# [SANITY CHECK] Check whether the lags were created correctly
cat( anyNA(datMV[Date>="2007-01-31"]) %?% "WARNING: Excessive missingness detected in the calculated SD macroeconomic variables.\n" %:%
       "SAFE: SD macroeconomic variables calculated and created successfully.\n")
### RESULTS: Variable created successfully. Note that there is missingness for some of the SD variables during 1980 because this is the start
# of the macroeconomic dataset and we can't reasonably calculate the SD for these dates since we don't have data from 1979 in the set.
# However, since we are only interested in the data from 2007 onwards, we do not have to worry about this issue and don't impute, since
# these dates will be discarded in any case, implying we would waste computing power if we did decide to impute.


# - Merging the macroeconomic information to the subsampled dataset
datCredit_smp <- merge(datCredit_smp, subset(datMV, select=colnames(datMV)[!(colnames(datMV) %in% ColNames)]), by = "Date", all.x = T)
# - Validate merging success )by checking for missingness (should be zero)
list_merge_variables <- list(colnames(datMV))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_smp$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with macroeconomic data is successful.\n" %:%
       "WARNING: Missingness in certain macroecnomic fields detected, fusion compromised.\n")
### RESULTS: No missingness observed, continue with packing away the data

# - Save a Macroeconomic Set that will be used in the Beta regression modelling
max_Dte<-max(datCredit_smp$Date)
min_Dte<-min(datCredit_smp$Date)
Macros_Set<-datMV[(Date>=min_Dte)&(Date<max_Dte),]

# - Pack away the Macros Set
pack.ffdf(paste0(genPath, "creditdata_Macros_Set"), Macros_Set)

# - Cleanup
rm(datMV); gc()


# --- Save "description" objects to disk
pack.ffdf(paste0(genObjPath, "var_Info_Cat"), var_Info_Cat)
pack.ffdf(paste0(genObjPath, "var_Info_Num"), var_Info_Num)

# --- Save fused- and enriched subsampled dataset for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_smp"), datCredit_smp); gc()




# --- 10. Apply basic cross-validation clustered resampling scheme with 1-way stratified sampling

# - Loading in the raw dataset
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)

# - Set seed and training set proportion
set.seed(1,kind="Mersenne-Twister")


# - Obtain first observations of all LoanIDs
dat_temp2 <- datCredit_smp %>% subset(Counter==1, c("Date", "LoanID", "Date_Origination"))

# - Use stratified sampling by the origination date using the LoanID's
dat_sub_keys2 <- dat_temp2 %>% group_by(Date_Origination) %>% slice_sample(prop=smp_frac)

# - Create the subset dataset
datCredit_train <- datCredit_smp %>% subset(LoanID %in% dat_sub_keys2$LoanID)
datCredit_valid <- datCredit_smp %>% subset(!(LoanID %in% dat_sub_keys2$LoanID))

# - [SANITY CHECK] Ensuring that the resampling scheme reconstitutes the full (subsampled) dataset
cat( (datCredit_smp[,.N] == datCredit_train[,.N] + datCredit_valid[,.N]) %?% "SAFE: Resampling scheme implemented successfully\n" %:%
       "WARNING: Resampling scheme not implemented successfully.\n")

datCredit_train<-datCredit_train[MarkovStatus_Future!="NA",]
datCredit_valid<-datCredit_valid[MarkovStatus_Future!="NA",]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_train"), datCredit_train); gc()
pack.ffdf(paste0(genPath, "creditdata_valid"), datCredit_valid); gc()


# --- Clean up
rm(varList_Cat, varList_Num, var_Info_Cat, var_Info_Num, datExcl, datExclusions,
   stratifiers, smp_perc, smp_size, targetVar, timeVar, smp_frac,
   datMV_Check1, datMV_Check2, list_merge_variables, results_missingness,
   ColNames, lags, datCredit_smp)





# ----- 5. Beta regression data preperation
# --- 1. Beta Regression Training Set
# --- Create Target Variables, i.e., transition rates
# - From performing
Targets_P<-datCredit_train[MarkovStatus=="Perf",list(Y_PerfToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                   Y_PerfToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                   Y_PerfToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                   Y_PerfToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                    by=list(Date)]
# - From default
Targets_D<-datCredit_train[MarkovStatus=="Def",list(Y_DefToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                   Y_DefToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                   Y_DefToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                   Y_DefToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                    by=list(Date)]

# - Keep relevant features
Features_Train<-datCredit_train[!duplicated(Date),]%>%select(contains("Date")|(contains("M_",ignore.case = FALSE)|contains("Aggr")))
Features_Train[,Date_Origination:=NULL]

# - Engineer more portfolio level features
Feature_Eng<-datCredit_train[,list(OutBal_Prop=sum(Balance,na.rm=TRUE)/sum(Principal,na.rm=TRUE),
                             Ave_Margin=mean(InterestRate_Margin,na.rm=TRUE),
                             g0_1prop=sum(g0_Delinq==1,na.rm=TRUE)/.N,
                             g0_2prop=sum(g0_Delinq==2,na.rm=TRUE)/.N,
                             g0_3prop=sum(g0_Delinq==3,na.rm=TRUE)/.N),by=list(Date)]

# - Merge into one set that will be used to train all the BR-models
BR_Train<-merge(Features_Train, Feature_Eng,by="Date",all.x=TRUE)
BR_Train<-merge(BR_Train, Targets_P,by="Date",all.x=TRUE)
BR_Train<-merge(BR_Train, Targets_D,by="Date",all.x=TRUE)

# - Create Previous transition rate Covariates
Previous<-BR_Train[,list(Date,Prev_PD=shift(Y_PerfToDef,n=1,type="lag",fill=0),
                              Prev_PP=shift(Y_PerfToPerf,n=1,type="lag",fill=0),
                              Prev_PS=shift(Y_PerfToSet,n=1,type="lag",fill=0),
                              Prev_DP=shift(Y_DefToPerf,n=1,type="lag",fill=0),
                              Prev_DW=shift(Y_DefToWO,n=1,type="lag",fill=0),
                              Prev_DD=shift(Y_DefToDef,n=1,type="lag",fill=0),
                              Prev_DS=shift(Y_DefToSet,n=1,type="lag",fill=0)
)]

# - Merge the previous months transition rates
BR_Train<-merge(BR_Train, Previous,by="Date",all.x=TRUE)



# --- 2. Beta Regression Validation Set
# --- Create Target Variables, i.e., transition rates
# - From performing
Targets_P<-datCredit_valid[MarkovStatus=="Perf",list(Y_PerfToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                               Y_PerfToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                               Y_PerfToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                               Y_PerfToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                           by=list(Date)]
# - From default
Targets_D<-datCredit_valid[MarkovStatus=="Def",list(Y_DefToDef=sum(Mark_Def_Ind, na.rm=TRUE)/.N,
                                              Y_DefToSet=sum(Mark_Set_Ind, na.rm=TRUE)/.N,
                                              Y_DefToPerf=sum(Mark_Perf_Ind, na.rm=TRUE)/.N,
                                              Y_DefToWO=sum(Mark_WO_Ind, na.rm=TRUE)/.N),
                           by=list(Date)]

# - Keep relevant features
Features_Valid<-datCredit_valid[!duplicated(Date),]%>%select(contains("Date")|(contains("M_",ignore.case = FALSE)|contains("Aggr")))
Features_Valid[,Date_Origination:=NULL]

# - Engineer more portfolio level features
Feature_Eng<-datCredit_valid[,list(OutBal_Prop=sum(Balance,na.rm=TRUE)/sum(Principal,na.rm=TRUE),
                                   Ave_Margin=mean(InterestRate_Margin,na.rm=TRUE),
                                   g0_1prop=sum(g0_Delinq==1,na.rm=TRUE)/.N,
                                   g0_2prop=sum(g0_Delinq==2,na.rm=TRUE)/.N,
                                   g0_3prop=sum(g0_Delinq==3,na.rm=TRUE)/.N),by=list(Date)]

# - Merge into one set that will be used to train all the BR-models
BR_Valid<-merge(Features_Train, Feature_Eng,by="Date",all.x=TRUE)
BR_Valid<-merge(BR_Valid, Targets_P,by="Date",all.x=TRUE)
BR_Valid<-merge(BR_Valid, Targets_D,by="Date",all.x=TRUE)

# - Create Previous transition rate Covariates
Previous<-BR_Valid[,list(Date,Prev_PD=shift(Y_PerfToDef,n=1,type="lag",fill=0),
                         Prev_PP=shift(Y_PerfToPerf,n=1,type="lag",fill=0),
                         Prev_PS=shift(Y_PerfToSet,n=1,type="lag",fill=0),
                         Prev_DP=shift(Y_DefToPerf,n=1,type="lag",fill=0),
                         Prev_DW=shift(Y_DefToWO,n=1,type="lag",fill=0),
                         Prev_DD=shift(Y_DefToDef,n=1,type="lag",fill=0),
                         Prev_DS=shift(Y_DefToSet,n=1,type="lag",fill=0)
)]

# - Merge the previous months transition rates
BR_Valid<-merge(BR_Valid, Previous,by="Date",all.x=TRUE)





# - Pack away the BR training & validation set
pack.ffdf(paste0(genPath, "BR_Training"), BR_Train)
pack.ffdf(paste0(genPath, "BR_Validation"), BR_Valid); gc()

proc.time() - ptm # IGNORE: elapsed runtime
