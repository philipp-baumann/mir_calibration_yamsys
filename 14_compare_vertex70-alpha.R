################################################################################
################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description:  Compare Vertex 70 and ALPHA spectrometer models for 
## YAMSYS spectroscopy reference samples from pilot sites
################################################################################

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development wrapper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

## Read spectra in list ========================================================

# List of OPUS files from Alpha at ETH Zürich
lf_eth_ver70 <- list.files("data/spectra/soilspec_eth_vertex70", 
  full.names = TRUE)

# Read files: ETH Vertex 70
spc_list_ver70 <- read_opus_univ(fnames = lf_eth_ver70)
# Save as Rds and read Rds
# spc_list_ver70 <- saveRDS(spc_list_ver70, file = "out/files/spc_list_ver70.Rds")
spc_list_ver70 <- readRDS(file = "out/files/spc_list_ver70.Rds")

## Spectral data processing pipe ===============================================
names(spc_list_ver70[60])
spc_list_ver70 <- spc_list_ver70[-60]

# ETH Alpha files
soilspec_tbl_eth <- spc_list_ver70 %>%
  gather_spc() %>%
  resample_spc(wn_lower = 620, wn_upper = 3900, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")
# spc_list_ver70[60] # not working! 
spc_list_ver70[[60]]$spc # is null
spc_list_ver70[[60]]

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

pls_C <- fit_pls(
  spec_chem = spec_chem, 
  response = C, 
  split_method = "ken_sto", 
  evaluation_method = "test_set",
  tuning_method = "resampling")