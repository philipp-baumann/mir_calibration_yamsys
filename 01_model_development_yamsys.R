################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
################################################################################

# Install all required packages (uncomment the lines below if you run this)

# Install the simplerspec package from the github repository;
# Installs all the required packages
# (https://github.com/philipp-baumann/simplerspec)
# devtools::install_github("philipp-baumann/simplerspec")

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development wrapper functions
library(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
library(tidyverse)

################################################################################
## Part 1: Read and pre-process spectra, Read chemical data, and join
## spectral and chemical data sets
################################################################################

## Read spectra in list ========================================================

# List of OPUS files from Alpha at ETH Zürich
lf_eth <- list.files("data/spectra/soilspec_eth_bin", full.names = TRUE)

# Read files: ETH
spc_list_eth <- read_opus_univ(fnames = lf_eth, extract = "spc")

## Spectral data processing pipe ===============================================

# ETH Alpha files
soilspec_tbl <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join spectra tibble and chemical reference analysis tibble
spec_chem <- join_spc_chem(
  spc_tbl = soilspec_tbl, chem_tbl = soilchem_tbl, by = "sample_id")


################################################################################
## Part 2: Run PLS regression models for different soil variables
## Use 2/3 of samples for calibration and 1/3 of samples for validation
## (argument ratio_val = 1/3); always exclude missing values for samples 
## that have missing values in the target soil property variable
################################################################################

## Register parallel backend for using multiple cores ==========================

# Allows to tune the models using parallel processing (e.g. use all available
# cores of a CPU); caret package automatically detects the registered backend
library(doParallel)
# Make a cluster with all possible threads (more than physical cores)
cl <- makeCluster(detectCores())
# Register backend
registerDoParallel(cl)
# Return number of parallel workers
getDoParWorkers() # 8 threads on MacBook Pro (Retina, 15-inch, Mid 2015);
# Quadcore processor

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

## Total element concentration (XRF) ===========================================

# Total Fe
pls_Fe_total <- fit_pls(
  # remove rows with the NA in the data
  spec_chem = spec_chem[!is.na(spec_chem$Fe_tot), ],
  response = Fe_tot,
  evaluation_method = "test_set",
  ratio_val = 1/3,
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 4,
  pls_ncomp_max = 4
)

# Total Si
pls_Si_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Si_tot), ],
  response = Si_tot,
  evaluation_method = "test_set",
  ratio_val = 1/3,
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 5,
  pls_ncomp_max = 4
)

# Total Al
pls_Al_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Al_tot), ],
  response = Al_tot,
  evaluation_method = "test_set",
  ratio_val = 1/3,
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 4
)

# Total K
pls_K_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$K_tot), ],
  response = K_tot,
  evaluation_method = "test_set",
  ratio_val = 1/3,
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)

# Total Zn
# remark: default model has only 1 PLS component;
# with increasing PLS component RMSE of cross-validation increases!
# model is not better if removing sample with n tot > 60
pls_Zn_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_tot), ],
  response = Zn_tot,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)

# Total Cu
# Default number of PLS components (caret selection) is 1,
# but cross-validation indicates that 3 components has a slightly lower
# RMSE
pls_Cu_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_tot), ],
  ratio_val = 1/3,
  response = Cu_tot,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 3
)

# Total Mn
pls_Mn_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_tot), ],
  ratio_val = 1/3,
  response = Mn_tot,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 4
)

# Total Ca
pls_Ca_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Ca_tot), ],
  ratio_val = 1/3,
  response = Ca_tot,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 6
)


## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

# pH
pls_pH <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$pH), ],
  ratio_val = 1/3,
  response = pH,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 6,
  pls_ncomp_max = 6
)

# Exchangable K
# final number of components is 1 (pls_exch_K$finalModel$ncomp)!
pls_exch_K <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_K), ],
  ratio_val = 1/3,
  response = ex_K,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)

# Exchangable Ca
# pc = 5 lead to much worse predictions
pls_exch_Ca <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Ca), ],
  ratio_val = 1/3,
  response = ex_Ca,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)

# Exchangable Mg
pls_exch_Mg <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Mg), ],
  ratio_val = 1/3,
  response = ex_Mg,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 2
)

# Exchangable Al
pls_exch_Al <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Al), ],
  response = ex_Al,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)

# CEC_eff
pls_CEC <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$CEC_eff), ],
  response = CEC_eff,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 5,
  pls_ncomp_max = 5
)

# BS_eff
pls_BS <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$BS_eff), ],
  response = BS_eff,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)


## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

# Total C
pls_C <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$C), ],
  response = C,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 6,
  pls_ncomp_max = 6
)

# Total N
pls_N <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$N), ],
  response = N,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 6,
  pls_ncomp_max = 6
)

# Total S
pls_S <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$S), ],
  response = S,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 2
)

# Total P
pls_P <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$P_tot), ],
  response = P_tot,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

# Resin-extractable P
# Log-transform gives better prediction
pls_resin_P_log <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$P_meas), ],
  ratio_val = 1/3,
  response = log(P_meas),
  evaluation_method = "test_set",
  split_method = "ken_sto",
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 4,
  pls_ncomp_max = 4
)

## Models for DTPA-extractable micronutrients ==================================

# DTPA-extractable Fe
pls_Fe_DTPA_log <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Fe_DTPA), ],
  response = log(Fe_DTPA),
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 5
)

# DTPA-extractable Zn
# Default optimal number of components is 3 -> might be too low
pls_Zn_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_DTPA), ],
  response = Zn_DTPA,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)

# DTPA-extractable Cu
# Be careful with selection of pc = 8 -> predictions much worse!!!
pls_Cu_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_DTPA), ],
  response = Cu_DTPA,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 8
)

# DTPA-extractable Mn
# Selects per default 2 principal components -> Check how to specify more
# Components
pls_Mn_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_DTPA), ],
  response = Mn_DTPA,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)


## =============================================================================
## 5: Texture models: data are measured by IITA in Cameroon
## =============================================================================

# Model for sand
pls_sand <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Cross-validated model for sand
pls_sand_cv <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)

# Model for clay
pls_clay <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$clay), ],
  response = clay,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Cross-validated model for clay
pls_clay_cv <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$clay), ],
  response = clay,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)

# Model for silt
pls_silt <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$silt), ],
  response = silt,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
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
pls_sand_vsel <- fit_pls(
  spec_chem = spec_chem_texture[!is.na(spec_chem_texture$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# remove smple below 60% sand
spec_chem_sand60 <- spec_chem_texture %>% filter(sand >= 60)
# Refit model
pls_sand_60 <- fit_pls(
  spec_chem = spec_chem_sand60[!is.na(spec_chem_sand60$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Adapted model for silt
pls_silt_vsel <- fit_pls(
  spec_chem = spec_chem_texture[!is.na(spec_chem_texture$silt), ],
  response = silt,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
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
pls_sand_msc <- fit_pls(
  spec_chem = spec_chem_msc[!is.na(spec_chem_msc$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "kfold_cv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Build a cross-validated model
pls_sand_msc_cv <- fit_pls(
  spec_chem = spec_chem_msc[!is.na(spec_chem_msc$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Build a cross-validated model using second derivative Savitzky-Golay
pls_sand_msc2_cv <- fit_pls(
  spec_chem = spec_chem_msc2[!is.na(spec_chem_msc2$sand), ],
  response = sand,
  evaluation_method = "test_set",
  split_method = "ken_sto",
  ratio_val = 1/3,
  tuning_method = "resampling",
  resampling_method = "loocv",
  ken_sto_pc = 2,
  pls_ncomp_max = 7
)
# Plot spectra as chross-check!
plot_spc(spec_chem_msc2, y = "spc_pre", by = "sample_id")

################################################################################
## Write all pls models (R objects) into separate files
## using the saveRDS function
## .Rds files are R data files that contain a single R object, in our case
## it is a list from the output of the fit_pls() function
## These files can be directly loaded into a new R session where e.g. calibrated
## properties are predicted based on new sample spectra
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

saveRDS(pls_Fe_total, "models/test_ken_sto/pls_Fe_total.Rds")
saveRDS(pls_Si_total, "models/test_ken_sto/pls_Si_total.Rds")
saveRDS(pls_Al_total, "models/test_ken_sto/pls_Al_total.Rds")
saveRDS(pls_K_total, "models/test_ken_sto/pls_K_total.Rds")
saveRDS(pls_Ca_total, "models/test_ken_sto/pls_Ca_total.Rds")
saveRDS(pls_Zn_total, "models/test_ken_sto/pls_Zn_total.Rds")
saveRDS(pls_Cu_total, "models/test_ken_sto/pls_Cu_total.Rds")
saveRDS(pls_Mn_total, "models/test_ken_sto/pls_Mn_total.Rds")

## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

saveRDS(pls_pH, "models/test_ken_sto/pls_pH.Rds")
saveRDS(pls_exch_K, "models/test_ken_sto/pls_exch_K.Rds")
saveRDS(pls_exch_Ca, "models/test_ken_sto/pls_exch_Ca.Rds")
saveRDS(pls_exch_Mg, "models/test_ken_sto/pls_exch_Mg.Rds")
saveRDS(pls_exch_Al, "models/test_ken_sto/pls_exch_Al.Rds")
saveRDS(pls_CEC, "models/test_ken_sto/pls_CEC.Rds")
saveRDS(pls_BS, "models/test_ken_sto/pls_BS.Rds")

## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

saveRDS(pls_N, "models/test_ken_sto/pls_N.Rds")
saveRDS(pls_C, "models/test_ken_sto/pls_C.Rds")
saveRDS(pls_S, "models/test_ken_sto/pls_S.Rds")
saveRDS(pls_P, "models/test_ken_sto/pls_P.Rds")

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

saveRDS(pls_resin_P_log, "models/test_ken_sto/pls_resin_P_log.Rds")
saveRDS(pls_Fe_DTPA_log, "models/test_ken_sto/pls_Fe_DTPA_log.Rds")
saveRDS(pls_Zn_DTPA, "models/test_ken_sto/pls_Zn_DTPA.Rds")
saveRDS(pls_Cu_DTPA, "models/test_ken_sto/pls_Cu_DTPA.Rds")
saveRDS(pls_Mn_DTPA, "models/test_ken_sto/pls_Mn_DTPA.Rds")

## =============================================================================
## 5: Texture: Sand, clay, and silt percentage
## =============================================================================

saveRDS(pls_sand, "models/test_ken_sto/pls_sand.Rds")
saveRDS(pls_sand_cv, "models/test_ken_sto/pls_sand_cv.Rds")
saveRDS(pls_sand_msc, "models/test_ken_sto/pls_sand_msc.Rds")
saveRDS(pls_sand_msc_cv, "models/test_ken_sto/pls_sand_msc_cv.Rds")
saveRDS(pls_silt, "models/test_ken_sto/pls_silt.Rds")
saveRDS(pls_clay, "models/test_ken_sto/pls_clay.Rds")
saveRDS(pls_clay_cv, "models/test_ken_sto/pls_clay_cv.Rds")

# Check if models have been written
list.files("models")