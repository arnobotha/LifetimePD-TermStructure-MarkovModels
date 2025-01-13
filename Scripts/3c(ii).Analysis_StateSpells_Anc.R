# ================================= PERFORMANCE SPELL ANALYSES =========================================
# Analysis performed on the sample size of different performance spells. The 
# analysis aids in the decision making about a suitable cut-off point beyond 
# which all performance spells are grouped together.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Dr Arno Botha
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2e.Data_Prepare_Macro.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - Bar chart of LoanIDs by performance spells
#   - Bar chart of risk events by performance spells
#   - Histogram of failure times f(t)
# ------------------------------------------------------------------------------------------------------





# ----------------- 1. Frequency distribution of time spent in state before transiting to specific states

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# lookup
lookup <- subset(datCredit_real, LoanID == datCredit_real[PerfSpell_Num == 8 & PerfSpell_Counter == 1, LoanID][1])

# - Aggregation to state-level and discard empty future states
# NOTE: Subset first to the last record per state spell
datAggr <- datCredit_real[TimeInStateSpell==StateSpell_Age, list(LoanID, StateSpell_Num_Total, StateSpell_Num, MarkovStatus, 
                                                    MarkovStatus_Future, StateSpell_Age, Event_Type)] %>%
  subset(!is.na(MarkovStatus_Future))
# datCredit_real[MarkovStatus=="Def" & MarkovStatus_Future == "Perf", .N]

# - Create state-FutureState key for graphing purposes
datAggr[, StateDest_Key := paste0(MarkovStatus,"-",MarkovStatus_Future)]

# - High-level Distributional analyses: exit states
describe(datAggr[MarkovStatus=="Perf", StateDest_Key]);
vPerfExitProbs <- table(datAggr[MarkovStatus=="Perf", StateDest_Key]) %>% prop.table()
### RESULTS: Overwhelming number of spell exits are towards settled/repaid (71%), followed by defaulting (28.5%)
describe(datAggr[MarkovStatus=="Def", StateDest_Key])
vDefExitProbs <- table(datAggr[MarkovStatus=="Def", StateDest_Key]) %>% prop.table()
### RESULTS: Majority of spells transit/cure to Performing (49.2%), followed by Settlement (27.9%) and then write-off (22.9%)

# - High-level Distributional analyses: Time spent in state i before exiting to specific state j (sojourn times)
describe(datAggr[StateDest_Key=="Perf-Set", StateSpell_Age]); hist(datAggr[StateDest_Key=="Perf-Set", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 61.99 and median 52; min of 1; max of 191
describe(datAggr[StateDest_Key=="Perf-Def", StateSpell_Age]); hist(datAggr[StateDest_Key=="Perf-Def", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 41.12 and median 24; min of 1; max of 191
describe(datAggr[StateDest_Key=="Perf-W_Off", StateSpell_Age]); hist(datAggr[StateDest_Key=="Perf-W_Off", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 56.24 and median 46; min of 1; max of 191
describe(datAggr[StateDest_Key=="Def-Perf", StateSpell_Age]); hist(datAggr[StateDest_Key=="Def-Perf", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 15.65 and median 11; min of 7; min of 7; max of 171
describe(datAggr[StateDest_Key=="Def-Set", StateSpell_Age]); hist(datAggr[StateDest_Key=="Def-Set", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 13.09 and median 8; min of 1; max of 171
describe(datAggr[StateDest_Key=="Def-W_Off", StateSpell_Age]); hist(datAggr[StateDest_Key=="Def-W_Off", StateSpell_Age], breaks="FD")
### RESULTS: Highly right-skewed distribution with mean 24.74 and median 19; min of 1, max of 176

# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(10, "Paired")[c(10,4,6)]
vCol2 <- brewer.pal(10, "Paired")[c(10,3,5)]
vCol_def <- brewer.pal(10, "Paired")[c(2,1,8)]
vCol_def2 <- brewer.pal(10, "Paired")[c(1,2,8)]
vLabels <- c(paste0("Perf-Def"="Performing -> Default (", round(vPerfExitProbs[1]*100, digits=1), "%)"), # Need to round to the first decimal place to ensure that the prior add up to one
             paste0("Perf-Set"="Performing -> Settlement (", round(vPerfExitProbs[2]*100, digits=1), "%)"),
             paste0("Perf-W_Off"="Performing -> Write-off (", round(vPerfExitProbs[3]*100, digits=1), "%)"))
vLabels2 <- c(paste0("Def-Perf"="Default -> Performing (", round(vDefExitProbs[1]*100, digits=1), "%)"), # Need to round to the first decimal place to ensure that the prior add up to one
             paste0("Def-Set"="Default -> Settlement (", round(vDefExitProbs[2]*100, digits=1), "%)"),
             paste0("Perf-W_Off"="Default -> Write-off (", round(vDefExitProbs[3]*100, digits=1), "%)"))


# - Create graph : Performing
(g1_histPerf <- ggplot(datAggr[MarkovStatus=="Perf",], aes(x=StateSpell_Age, group=StateDest_Key)) + theme_minimal() + 
    labs(y=bquote(plain('Sojourn time histogram & density ')~italic(f[kl](t))), 
         x=bquote("Sojourn times (months)"*~italic(T[kl])*" for the performing state P; "*italic(k)==1)) + 
    theme(text=element_text(family=chosenFont),legend.position=c(0.7,0.5), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Graphs
    geom_histogram(aes(y=after_stat(density), colour=StateDest_Key, fill=StateDest_Key), position="identity",
                   alpha=0.5, size=0.2, bins=50) + 
    geom_density(aes(colour=StateDest_Key, linetype=StateDest_Key), linewidth=0.8) + 
    # facets & scale options
    scale_colour_manual(name=bquote("Transition Type"), values=vCol2, labels=vLabels) + 
    scale_fill_manual(name=bquote("Transition Type"), values=vCol, labels=vLabels) + 
    scale_linetype_manual(name=bquote("Transition Type"), values=c("solid","dashed", "dotted"), labels=vLabels) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Save plot
dpi <- 250
ggsave(g1_histPerf, file=paste0(genFigPath,"SojournTime-Densities-Perf.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# - Create graph : default
(g1_histDef <- ggplot(datAggr[MarkovStatus=="Def",], aes(x=StateSpell_Age, group=StateDest_Key)) + theme_minimal() + 
    labs(y=bquote(plain('Sojourn time histogram & density ')~italic(f[kl](t))), 
         x=bquote("Sojourn times (months)"*~italic(T[kl])*" for the default state D; "*italic(k)==2)) + 
    theme(text=element_text(family=chosenFont),legend.position=c(0.7,0.5), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Graphs
    geom_histogram(aes(y=after_stat(density), colour=StateDest_Key, fill=StateDest_Key), position="identity",
                   alpha=0.5, size=0.2, bins=50) + 
    geom_density(aes(colour=StateDest_Key, linetype=StateDest_Key), linewidth=0.8) + 
    # facets & scale options
    scale_colour_manual(name=bquote("Transition Type"), values=vCol_def2, labels=vLabels2) + 
    scale_fill_manual(name=bquote("Transition Type"), values=vCol_def, labels=vLabels2) + 
    scale_linetype_manual(name=bquote("Transition Type"), values=c("solid","dashed", "dotted"), labels=vLabels2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Save plot
dpi <- 250
ggsave(g1_histDef, file=paste0(genFigPath,"SojournTime-Densities-Def.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# - Cleanup
rm(datCredit_real, datAggr, lookup, g1_histDef, g1_histPerf); gc()