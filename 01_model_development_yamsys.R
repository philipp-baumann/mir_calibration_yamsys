################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
################################################################################

# Install all required packages (uncomment the lines below if you run this
# script the first time)
# List of packages to be installed
# list_packages <- c("ggplot2", "plyr", "data.table", "reshape2",
#   "mvoutlier", "hexView", "Rcpp", "hyperSpec", "prospectr",
#   "dplyr", "caret")
# Install packages from CRAN
# install.packages(list_packages, dependencies = TRUE)
# install.packages("devtools")
# Install the simplerspec package from the github repository
# (https://github.com/philipp-baumann/simplerspec)
# devtools::install_github("philipp-baumann/simplerspec")

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development helper functions
require(simplerspec)
# Load dplyr package for efficient data manipulation functions and the 
# pipe operator %>% ("then") provided from the magrittr package 
require(dplyr)

################################################################################
## Part 1: Read and pre-process spectra, Read chemical data, and join
## spectral and chemical data sets
################################################################################

# Processing of spectral data ==================================================
# spectral data preparation:

# Read spectra in text format (Alpha spectrometer) -----------------------------
soilspec_in <- read_spectra(
  path = "data/spectra/alpha_txt/"
)

# Calculate mean of spectra for each sample ------------------------------------
# Compute the standard deviation (SD) of the three measurements
# identify samples in which the spectrum has SD higher than 1.5
# and need to be re-scanned
soilspec_mean <- average_spectra(soilspec_in)
soilspec_mean$MIR_sd
soilspec_mean$cvar > 0.4 # the coefficient of variation

# don't remove outliers; keep all spectra --------------------------------------
soilspec_all <- remove_outliers(soilspec_mean,
  remove = FALSE
)

# Remove outliers; keep all spectra --------------------------------------------
# Detect outliers based on robust PCA
soilspec_rm <- remove_outliers(soilspec_mean,
  remove = TRUE
)

# get the list of outliers
outliers_list <- which(!(soilspec_all$data_meta[, 1] %in% soilspec_rm$data_meta[, 1]))
soilspec_all$data_meta[outliers_list, ]

# Average, remove outlier, resample, then pre-process spectra ------------------
soilspec <- soilspec_in %>%
  average_spectra() %>%
  # do not remove outliers
  remove_outliers(remove = FALSE) %>%
  resample_spectra(wn_lower = 510, wn_upper = 3988, wn_interval = 2) %>%
  do_pretreatment(select = "MIR1_w21")

# Check if resampling worked, extract wavenumbers that are stored in column
# names of spectral matrix
colnames(soilspec$MIR0)

# Read chemical data from csv (comma-separated values) files -------------------
soilchem <- read.csv(
  file = "data/soilchem/soilchem_yamsys.csv")
str(soilchem)
soilchem$sample_ID

# Join chemical and spectra data -----------------------------------------------

spec_chem <- join_chem_spec(dat_chem = soilchem, dat_spec = soilspec)

# Check number of rows (samples) in spectral matrix
nrow(spec_chem$MIR)
# Check lenght of ID column in spec_chem data frame -> should be identical to 
# output from line above
length(spec_chem$ID)


################################################################################
## Part 2: Run PLS regression models for different soil variables
## Use 2/3 of samples for calibration and 1/3 of samples for validation
## (argument ratio_val = 1/3); always exclude missing values for samples 
## that have missing values in the target soil property variable
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

## Total element concentration (XRF) ===========================================

# Total Fe
pls_Fe_total <- pls_ken_stone(
  # remove rows with the NA in the data
  spec_chem = spec_chem[!is.na(spec_chem$Fe_tot), ],
  ratio_val = 1/3,
  variable = Fe_tot,
  pc = 0.99,
  validation = TRUE
)

# Total Si
pls_Si_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Si_tot), ],
  ratio_val = 1/3,
  variable = Si_tot,
  validation = TRUE
)

# Total Al
pls_Al_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Al_tot), ],
  ratio_val = 1/3,
  variable = Al_tot,
  validation = TRUE
)

# Total K
pls_K_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$K_tot), ],
  ratio_val = 1/3,
  variable = K_tot,
  validation = TRUE
)

# Total Zn
pls_Zn_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_tot), ],
  ratio_val = 1/3,
  variable = Zn_tot,
  validation = TRUE
)

# Total Cu
pls_Cu_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_tot), ],
  ratio_val = 1/3,
  variable = Cu_tot,
  validation = TRUE
)

# Total Mn
pls_Mn_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_tot), ],
  ratio_val = 1/3,
  variable = Mn_tot,
  validation = TRUE
)

# Total Ca
pls_Ca_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Ca_tot), ],
  ratio_val = 1/3,
  variable = Ca_tot,
  validation = TRUE
)


## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

# pH
pls_pH <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$pH), ],
  ratio_val = 1/3,
  variable = pH,
  validation = TRUE
)

# Exchangable K
pls_exch_K <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_K), ],
  ratio_val = 1/3,
  variable = ex_K,
  validation = TRUE
)

# Exchangable Ca
pls_exch_Ca <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Ca), ],
  ratio_val = 1/3,
  variable = ex_Ca,
  validation = TRUE
)

# Exchangable Mg
pls_exch_Mg <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Mg), ],
  ratio_val = 1/3,
  variable = ex_Mg,
  validation = TRUE
)

# Exchangable Al
pls_exch_Al <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Al), ],
  ratio_val = 1/3,
  variable = ex_Al,
  validation = TRUE
)

# CEC_eff
pls_CEC <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$CEC_eff), ],
  ratio_val = 1/3,
  variable = CEC_eff,
  validation = TRUE,
  pc = 5,
  invert = FALSE,
  max_ncomp_pls = 5
)

# BS_eff
pls_BS <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$BS_eff), ],
  ratio_val = 1/3,
  variable = BS_eff,
  validation = TRUE,
  print = TRUE
)


## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

# Total C
pls_C <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$C), ],
  ratio_val = 1/3,
  variable = C,
  validation = TRUE,
  invert = FALSE,
  pc = 6,
  max_ncomp_pls = 6
)

# Total N
pls_N <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$N), ],
  ratio_val = 1/3,
  variable = N,
  pc = 6,
  max_ncomp_pls = 6
)

# Total S
pls_S <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$S), ],
  ratio_val = 1/3,
  variable = S
)

# Total P
pls_P <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$P_tot), ],
  ratio_val = 1/3,
  variable = P_tot
)

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

# Resin-extractable P
# Log-transform gives better prediction
pls_resin_P_log <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$P_meas), ],
  ratio_val = 1/3,
  variable = log(P_meas),
  validation = TRUE
)

## Models for DTPA-extractable micronutrients ==================================

# DTPA-extractable Fe
pls_Fe_DTPA_log <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Fe_DTPA), ],
  ratio_val = 1/3,
  variable = log(Fe_DTPA),
  validation = TRUE
)

# DTPA-extractable Zn
pls_Zn_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_DTPA), ],
  ratio_val = 1/3,
  variable = Zn_DTPA,
  validation = TRUE
)

# DTPA-extractable Cu
pls_Cu_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_DTPA), ],
  ratio_val = 1/3,
  variable = Cu_DTPA,
  validation = TRUE
)

# DTPA-extractable Mn
pls_Mn_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_DTPA), ],
  ratio_val = 1/3,
  variable = Mn_DTPA,
  validation = TRUE
)



################################################################################
## Write all pls models (R objects) into separate files
## using the saveRDS function
## .Rds files are R data files that contain a single R object, in our case
## it is a list from the output of the pls_ken_stone() function
## These files can be directly loaded into a new R session where e.g. calibrated
## properties are predicted based on new sample spectra
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

saveRDS(pls_Fe_total, "models/pls_Fe_total.Rds")
saveRDS(pls_Si_total, "models/pls_Si_total.Rds")
saveRDS(pls_Al_total, "models/pls_Al_total.Rds")
saveRDS(pls_K_total, "models/pls_K_total.Rds")
saveRDS(pls_Ca_total, "models/pls_Ca_total.Rds")
saveRDS(pls_Zn_total, "models/pls_Zn_total.Rds")
saveRDS(pls_Cu_total, "models/pls_Cu_total.Rds")
saveRDS(pls_Mn_total, "models/pls_Mn_total.Rds")

## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

saveRDS(pls_pH, "models/pls_pH.Rds")
saveRDS(pls_exch_K, "models/pls_exch_K.Rds")
saveRDS(pls_exch_Ca, "models/pls_exch_Ca.Rds")
saveRDS(pls_exch_Mg, "models/pls_exch_Mg.Rds")
saveRDS(pls_exch_Al, "models/pls_exch_Al.Rds")
saveRDS(pls_CEC, "models/pls_CEC.Rds")
saveRDS(pls_BS, "models/pls_BS.Rds")

## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

saveRDS(pls_N, "models/pls_N.Rds")
saveRDS(pls_C, "models/pls_C.Rds")
saveRDS(pls_S, "models/pls_S.Rds")
saveRDS(pls_P, "models/pls_P.Rds")

## ==============================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## ==============================================================

saveRDS(pls_resin_P_log, "models/pls_resin_P_log.Rds")
saveRDS(pls_Fe_DTPA_log, "models/pls_Fe_DTPA_log.Rds")
saveRDS(pls_Zn_DTPA, "models/pls_Zn_DTPA.Rds")
saveRDS(pls_Cu_DTPA, "models/pls_Cu_DTPA.Rds")
saveRDS(pls_Mn_DTPA, "models/pls_Mn_DTPA.Rds")

# Check if models have been written
list.files("rds")

################################################################################
## Test function to select reference samples to be measured by reference
## analysis methods
################################################################################

# Get list of reference and prediction samples from Kennard-Stones sampling
ref_pred_samples <- select_ref_samples(
  # Preprocessed spectra and metadata in list after preprocessing step
  list_spectra = soilspec,
  # Use 2 principal components for the computation of Mahalanobsis distance
  pc = 2,
  # Use 15% of samples for reference chemical analysis and model development
  ratio_ref = 0.15
)

# Get sample ID from selection list for determining which samples to analyze
ref_pred_samples$ref_samples$metadata

# Print plot of selected reference and predict samples in PCA space
ref_pred_samples$p_pca

################################################################################
## Some extra code for inspecting model objects, and
## predicting calibrated soil properties using the above calibration models
# on new spectra. Compare predicted values
################################################################################

# Check calibration and validation data set
ls(pls_C$data)

# Calibration set is a data frame within list pls_C$data
str(pls_C$data$calibration)

# Plot calibration and validation samples using first two PC
# Element p_pc is a ggplot2 object
pls_C$p_pc

# Plot predicted vs. observed values for both calibration and validation sets
# Graph includes commonly used model evaluation indices:
# R^2, RMSE (Root Mean Square Error; average error of prediction), and
# RPD (Ratio of Performance to Deviation) for both calibration and validation
# Element p_model is a ggplot2 object
pls_C$p_model
pls_C$p_model$data

# Get summary statistics as data frame object
pls_C$stats

################################################################################

## Perform robust PCA with spectral data =======================================

pca <- prcomp(soilspec$MIR_pre, center = TRUE, scale = FALSE)
