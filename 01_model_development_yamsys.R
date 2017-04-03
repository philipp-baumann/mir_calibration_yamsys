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
#   "dplyr", "caret", "tidyverse")
# Install packages from CRAN
# install.packages(list_packages, dependencies = TRUE)
# install.packages("devtools")
# Install the simplerspec package from the github repository
# (https://github.com/philipp-baumann/simplerspec)
# devtools::install_github("philipp-baumann/simplerspec")

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development wrapper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

################################################################################
## Part 1: Read and pre-process spectra, Read chemical data, and join
## spectral and chemical data sets
################################################################################

## Read spectra in list ========================================================

# List of OPUS files from Alpha at ETH Zürich
lf_eth <- list.files("data/spectra/soilspec_eth_bin/", full.names = T)

# Read files: ETH
spc_list_eth <- read_opus(
  fnames = lf_eth,
  in_format = c("binary"),
  out_format = "list"
)

## Spectral data processing pipe ===============================================

# ETH Alpha files
soilspec_tbl_eth <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join spectra tibble and chemical reference analysis tibble
spec_chem <- join_spc_chem(
  spc_tbl = soilspec_tbl_eth , chem_tbl = soilchem_tbl, by = "sample_id")


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
  pc = 4,
  validation = TRUE,
  pls_ncomp_max = 4
)

# Total Si
pls_Si_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Si_tot), ],
  ratio_val = 1/3,
  variable = Si_tot,
  pc = 5,
  validation = TRUE,
  pls_ncomp_max = 4
)

# Total Al
pls_Al_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Al_tot), ],
  ratio_val = 1/3,
  variable = Al_tot,
  validation = TRUE,
  pls_ncomp_max = 4
)

# Total K
pls_K_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$K_tot), ],
  ratio_val = 1/3,
  variable = K_tot,
  validation = TRUE,
  pls_ncomp_max = 5
)

# Total Zn
# remark: default model has only 1 PLS component;
# with increasing PLS component RMSE of cross-validation increases!
pls_Zn_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_tot), ],
  ratio_val = 1/3,
  variable = Zn_tot,
  validation = TRUE
)

# Total Cu
# Default number of PLS components (caret selection) is 1,
# but cross-validation indicates that 3 components has a slightly lower
# RMSE
pls_Cu_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_tot), ],
  ratio_val = 1/3,
  variable = Cu_tot,
  pc = 2,
  validation = TRUE,
  pls_ncomp_max = 3
)

# Total Mn
pls_Mn_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_tot), ],
  ratio_val = 1/3,
  variable = Mn_tot,
  pc = 2,
  validation = TRUE,
  pls_ncomp_max = 4
)

# Total Ca
pls_Ca_total <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Ca_tot), ],
  ratio_val = 1/3,
  variable = Ca_tot,
  validation = TRUE,
  pls_ncomp_max = 6
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
  validation = TRUE,
  pc = 6,
  pls_ncomp_max = 6
)

# Exchangable K
# final number of components is 1 (pls_exch_K$finalModel$ncomp)!
pls_exch_K <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_K), ],
  ratio_val = 1/3,
  variable = ex_K,
  validation = TRUE
)

# Exchangable Ca
# pc = 5 lead to much worse predictions
pls_exch_Ca <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Ca), ],
  ratio_val = 1/3,
  variable = ex_Ca,
  pc = 2,
  validation = TRUE,
  pls_ncomp_max = 5
)

# Exchangable Mg
pls_exch_Mg <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Mg), ],
  ratio_val = 1/3,
  variable = ex_Mg,
  validation = TRUE,
  pls_ncomp_max = 2
)

# Exchangable Al
pls_exch_Al <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Al), ],
  ratio_val = 1/3,
  variable = ex_Al,
  validation = TRUE,
  pls_ncomp_max = 5
)

# CEC_eff
pls_CEC <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$CEC_eff), ],
  ratio_val = 1/3,
  variable = CEC_eff,
  validation = TRUE,
  pc = 5,
  invert = FALSE,
  pls_ncomp_max = 5
)

# BS_eff
pls_BS <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$BS_eff), ],
  ratio_val = 1/3,
  variable = BS_eff,
  validation = TRUE,
  print = TRUE,
  pls_ncomp_max = 5
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
  pls_ncomp_max = 6
)

# Total N
pls_N <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$N), ],
  ratio_val = 1/3,
  variable = N,
  pc = 6,
  pls_ncomp_max = 6
)

# Total S
pls_S <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$S), ],
  ratio_val = 1/3,
  variable = S,
  pls_ncomp_max = 2
)

# Total P
pls_P <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$P_tot), ],
  ratio_val = 1/3,
  variable = P_tot,
  pls_ncomp_max = 5
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
  validation = TRUE,
  pc = 4,
  pls_ncomp_max = 4
)

## Models for DTPA-extractable micronutrients ==================================

# DTPA-extractable Fe
pls_Fe_DTPA_log <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Fe_DTPA), ],
  ratio_val = 1/3,
  variable = log(Fe_DTPA),
  validation = TRUE,
  pc = 2,
  pls_ncomp_max = 5
)

# DTPA-extractable Zn
# Default optimal number of components is 3 -> might be too low
pls_Zn_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_DTPA), ],
  ratio_val = 1/3,
  variable = Zn_DTPA,
  validation = TRUE
)

# DTPA-extractable Cu
# Be careful with selection of pc = 8 -> predictions much worse!!!
pls_Cu_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_DTPA), ],
  ratio_val = 1/3,
  variable = Cu_DTPA,
  pc = 2,
  validation = TRUE,
  pls_ncomp_max = 8
)

# DTPA-extractable Mn
# Selects per default 2 principal components -> Check how to specify more
# Components
pls_Mn_DTPA <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_DTPA), ],
  ratio_val = 1/3,
  variable = Mn_DTPA,
  validation = TRUE
)


## =============================================================================
## 5: Texture models: data are measured by IITA in Cameroon
## =============================================================================

# Model for sand
pls_sand <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$sand), ],
  validation = TRUE,
  ratio_val = 1/3,
  variable = sand,
  pls_ncomp_max = 7
)
# Cross-validated model for sand
pls_sand_cv <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$sand), ],
  validation = FALSE,
  variable = sand,
  pls_ncomp_max = 7,
  cv = "LOOCV"
)

# Model for clay
pls_clay <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$clay), ],
  validation = TRUE,
  ratio_val = 1/3,
  variable = clay,
  pls_ncomp_max = 7
)
# Cross-validated model for clay
pls_clay_cv <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$clay), ],
  variable = clay,
  validation = FALSE,
  pls_ncomp_max = 7,
  cv = "LOOCV"
)

# Model for silt
pls_silt <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$silt), ],
  validation = TRUE,
  ratio_val = 1/3,
  variable = silt,
  pls_ncomp_max = 7
)

# Variable selection for sand --------------------------------------------------

# Quartz bands: 1790 cm–1, 1870cm–1; resample spectra by only taking into
# account quartz bands
soilspec_texture <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 1750, wn_upper = 2000, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")
# Plot spectra
plot_spc(spc_tbl = soilspec_texture, y = "spc_pre", by = "sample_id",
  graph_id_1 = "texture")
# Join spectra tibble and chemical reference analysis tibble
spec_chem_texture <- join_spc_chem(
  spc_tbl = soilspec_texture , chem_tbl = soilchem_tbl, by = "sample_id")
# Adapted model for sand
pls_sand_vsel <- pls_ken_stone(
  spec_chem = spec_chem_texture[!is.na(spec_chem_texture$sand), ],
  validation = FALSE,
  cv = "LOOCV",
  variable = sand,
  pls_ncomp_max = 7
)
# remove smple below 60% sand
spec_chem_sand60 <- spec_chem_texture %>% filter(sand >= 60)
# Refit model
pls_sand_60 <- pls_ken_stone(
  spec_chem = spec_chem_sand60[!is.na(spec_chem_sand60$sand), ],
  validation = FALSE,
  cv = "LOOCV",
  variable = sand,
  pls_ncomp_max = 7
)
# Adapted model for silt
pls_silt_vsel <- pls_ken_stone(
  spec_chem = spec_chem_texture[!is.na(spec_chem_texture$silt), ],
  validation = FALSE,
  ratio_val = 1/3,
  cv = "LOOCV",
  variable = silt,
  pls_ncomp_max = 7
)

# Experiment with alternative preprocessing: use combination of 
# Savitzky-Golay (SG) and multiplicative scatter correction (MSC) --------------

# Process spectra
soilspec_msc <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  # new option: use Savitzky-Golay with polynomial of 3rd degree, a window 
  # size of 21 and a fist derivative followed by MSC 
  # (Column means of preprocessed spectra for the reference spectrum)
  preprocess_spc(select = "sg_1_w21_msc")
# Use second derivative Savitzky-Golay and MSC
soilspec_msc2 <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  # new option: use Savitzky-Golay with polynomial of 3rd degree, a window 
  # size of 21 and a fist derivative followed by MSC 
  # (Column means of preprocessed spectra for the reference spectrum)
  preprocess_spc(select = "sg_2_w21_msc")
# Plot spectral tibble as test
plot_spc(spc_tbl = soilspec_msc, y = "spc_pre", by = "sample_id")
# Combine with reference analysis data set
spec_chem_msc <- join_spc_chem(
  spc_tbl = soilspec_msc , chem_tbl = soilchem_tbl, by = "sample_id")
spec_chem_msc2 <- join_spc_chem(
  spc_tbl = soilspec_msc2 , chem_tbl = soilchem_tbl, by = "sample_id")
# Build an alternative sand content model using SG and MSC 
pls_sand_msc <- pls_ken_stone(
  spec_chem = spec_chem_msc[!is.na(spec_chem_msc$sand), ],
  validation = TRUE,
  ratio_val = 1/3,
  variable = sand,
  pls_ncomp_max = 7
)
# Build a cross-validated model
pls_sand_msc_cv <- pls_ken_stone(
  spec_chem = spec_chem_msc[!is.na(spec_chem_msc$sand), ],
  validation = FALSE,
  variable = sand,
  pls_ncomp_max = 7,
  split_method = "resampling",
  cv = "LOOCV"
)
# Build a cross-validated model using second derivative Savitzky-Golay
pls_sand_msc2_cv <- pls_ken_stone(
  spec_chem = spec_chem_msc2[!is.na(spec_chem_msc2$sand), ],
  validation = FALSE,
  variable = sand,
  pls_ncomp_max = 7,
  split_method = "resampling",
  cv = "LOOCV"
)
# Plot spectra as chross-check!
plot_spc(spec_chem_msc2, y = "spc_pre", by = "sample_id")

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

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

saveRDS(pls_resin_P_log, "models/pls_resin_P_log.Rds")
saveRDS(pls_Fe_DTPA_log, "models/pls_Fe_DTPA_log.Rds")
saveRDS(pls_Zn_DTPA, "models/pls_Zn_DTPA.Rds")
saveRDS(pls_Cu_DTPA, "models/pls_Cu_DTPA.Rds")
saveRDS(pls_Mn_DTPA, "models/pls_Mn_DTPA.Rds")

## =============================================================================
## 5: Texture: Sand, clay, and silt percentage
## =============================================================================

saveRDS(pls_sand, "models/pls_sand.Rds")
saveRDS(pls_sand_cv, "models/pls_sand_cv.Rds")
saveRDS(pls_sand_msc, "models/pls_sand_msc.Rds")
saveRDS(pls_sand_msc_cv, "models/pls_sand_msc_cv.Rds")
saveRDS(pls_silt, "models/pls_silt.Rds")
saveRDS(pls_clay, "models/pls_clay.Rds")
saveRDS(pls_clay_cv, "models/pls_clay_cv.Rds")

# Check if models have been written
list.files("models")