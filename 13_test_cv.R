################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: May 28, 2017
## Project: YAMSYS
## Description: Test different cross-validation methods for model tuning
################################################################################

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

# Write preprocessed spectra in file
# saveRDS(soilspec_tbl_eth, file = "out/files/soilspec_tbl_eth.Rds")
soilspec_tbl_eth <- readRDS(file = "out/files/soilspec_tbl_eth.Rds")

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join spectra tibble and chemical reference analysis tibble
spec_chem <- join_spc_chem(
  spc_tbl = soilspec_tbl_eth , chem_tbl = soilchem_tbl, by = "sample_id")


################################################################################
## Part 2: Run PLS regression models for different soil variables
## use cross-validation
################################################################################

# total C model with leave-one-out cross-validation
pls_C_repcv <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$C), ],
  response = C,
  evaluation_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 1
)

pdf(file = "out/figs/pls_C_repcv.pdf")
pls_C_repcv$p_model
dev.off()