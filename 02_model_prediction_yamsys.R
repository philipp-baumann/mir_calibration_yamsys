################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
################################################################################

# Remove all R objects from memory
rm(list = ls())

################################################################################
## Read all spectral models (created and saved within <models> folder;
## see script <01_model_development_yamsys.R>)
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

pls_Fe_total <- readRDS("models/pls_Fe_total.Rds")
pls_Si_total <- readRDS("models/pls_Si_total.Rds")
pls_Al_total <- readRDS("models/pls_Al_total.Rds")
pls_K_total <- readRDS("models/pls_K_total.Rds")
pls_Ca_total <- readRDS("models/pls_Ca_total.Rds")
pls_Zn_total <- readRDS("models/pls_Zn_total.Rds")
pls_Cu_total <- readRDS("models/pls_Cu_total.Rds")
pls_Mn_total <- readRDS("models/pls_Mn_total.Rds")

## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

pls_pH <- readRDS("models/pls_pH.Rds")
pls_exch_K <- readRDS("models/pls_exch_K.Rds")
pls_exch_Ca <- readRDS("models/pls_exch_Ca.Rds")
pls_exch_Mg <- readRDS("models/pls_exch_Mg.Rds")
pls_exch_Al <- readRDS("models/pls_exch_Al.Rds")
pls_CEC <- readRDS("models/pls_CEC.Rds")
pls_BS <- readRDS("models/pls_BS.Rds")


## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

pls_C <- readRDS("models/pls_C.Rds")
pls_N <- readRDS("models/pls_N.Rds")
pls_S <- readRDS("models/pls_S.Rds")
pls_P<- readRDS("models/pls_P.Rds")

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

pls_resin_P_log <- readRDS("models/pls_resin_P_log.Rds")
pls_Fe_DTPA_log <- readRDS("models/pls_Fe_DTPA_log.Rds")
pls_Zn_DTPA <- readRDS("models/pls_Zn_DTPA.Rds")
pls_Cu_DTPA <- readRDS("models/pls_Cu_DTPA.Rds")
pls_Mn_DTPA <- readRDS("models/pls_Mn_DTPA.Rds")

################################################################################
## Predict new samples
################################################################################

# Load caret package for model predictions
library(caret)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)
# Load simplerspec package
library(simplerspec)

# Read spectra in list ========================================================

# List of OPUS files from Alpha at ESAL in Côte d'Ivoire
lf_esal <- list.files("data/spectra/soilspec_esal_bin/", full.names = T)

# Read files: ESAL
spc_list_esal <- read_opus(
  fnames = lf_esal,
  in_format = c("binary"),
  out_format = "list"
)

## Spectral data processing pipe ===============================================

# ESAL Alpha files
soilspec_tbl_esal <- spc_list_esal %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

# Create list of model output for all calibrated soil properties
models_all <- list(
  pls_Fe_total = pls_Fe_total,
  pls_Si_total = pls_Si_total,
  pls_Al_total = pls_Al_total,
  pls_K_total = pls_K_total,
  pls_Ca_total = pls_Ca_total,
  pls_Zn_total = pls_Zn_total,
  pls_Cu_total = pls_Cu_total,
  pls_Mn_total = pls_Mn_total,
  pls_pH = pls_pH,
  pls_exch_K = pls_exch_K,
  pls_exch_Ca = pls_exch_Ca,
  pls_exch_Mg = pls_exch_Mg,
  pls_exch_Al = pls_exch_Al,
  pls_CEC = pls_CEC,
  pls_BS = pls_BS,
  pls_C = pls_C,
  pls_N = pls_N,
  pls_S = pls_S,
  pls_P = pls_P,
  pls_resin_P_log = pls_resin_P_log,
  pls_Fe_DTPA_log = pls_Fe_DTPA_log,
  pls_Zn_DTPA = pls_Zn_DTPA,
  pls_Cu_DTPA = pls_Cu_DTPA,
  pls_Mn_DTPA = pls_Mn_DTPA
)

# Prediction of total C: use samples of validation
# for an example. In future, new soils will be predicted.
# Note that the spectra need to be averaged and preprocessed
# prior to prediction. In the argument unkX the spectral
# data has to be provided in matrix class.
# The resulting R object (predictions) is a data.frame
# that contains predicted values in column <pred>;
# The data are in long form, and each of the individual soil properties
# is given in the column <object>

# Function that uses pre-processed spectra, additional metadata of new
# samples, and caret model output for the different soil property models
# to create predicted values.

# Test final prediction function
predictions <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = models_all,
  spc_tbl = soilspec_tbl_esal)