# LifetimePD-TermStructure-MarkovModels
<Abstract goes here>

## Structure
This R-codebase can be run sequentially using the file numbering itself as a structure. Delinquency measures are algorithmically defined in **DelinqM.R** as data-driven functions, which may be valuable to the practitioner outside of the study's current scope. Similarly, the TruEnd-procedure from [Botha2024](https://github.com/arnobotha/TruEnd-Procedure) and its set of functions are impliemented in the **TruEnd.R** script.

## Data
This R-codebase assumes that monthly loan performance data is available. Naturally, the data itself can't be made publically available given its sensitive nature, as well as various data privacy laws, particularly the _Protection of Personal Information (POPI)_ Act of 2013 in South Africa. However, the structure and type of data that is required for reproducing this study, is sufficiently described in the commentary within the scripts. This should enable the practitioner to extract and prepare data accordingly. Moreover, this codebase assumes South African macroeconomic data is available, as sourced and collated by internal staff of the bank in question.

## Copyright
All code and scripts are hereby released under an [MIT](https://opensource.org/licenses/MIT) license. Similarly, all graphs produced by relevant scripts as well as those published here, are hereby released under a Creative Commons Attribution ([CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/)) licence.
