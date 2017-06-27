################################################################################
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
## Task: Test speed-up of parallel processing with caret compared to serial
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

## Register parallel backend ===================================================

library(doParallel)
# Check how many cores your computer has; for my macbook pro I have 4 physical
# cores (MacBook Pro (Retina, 15-inch, Mid 2015)
cl <- makeCluster(4)
registerDoParallel(cl)

# Return computation time when caret uses 4 cores for parallel processing ------
system.time(
# total C model with leave-one-out cross-validation
  pls_C_repcv <- fit_pls(
    spec_chem = spec_chem[!is.na(spec_chem$C), ],
    response = C,
    evaluation_method = "resampling",
    resampling_method = "rep_kfold_cv",
    pls_ncomp_max = 1
  )
)
# -> computation time was for about 124s

# Shut down the workers --------------------------------------------------------
stopCluster(cl)
# Insert serial backend, otherwise error in repetetive tasks 
# (Error in summary.connection(connection) : invalid connection)
registerDoSEQ()

# Return computation time when no parallel backend is registered ---------------
system.time(
# total C model with leave-one-out cross-validation
  pls_C_repcv <- fit_pls(
    spec_chem = spec_chem[!is.na(spec_chem$C), ],
    response = C,
    evaluation_method = "resampling",
    resampling_method = "rep_kfold_cv",
    pls_ncomp_max = 1
  )
)
# -> computation time was for about 38s; speedup more than 3 times compared to 
# serial processing!

# Detect logical not physical cores --------------------------------------------
detectCores()
# Make a cluster with all possible threads (not cores)
cl <- makeCluster(detectCores())
# Register backend
registerDoParallel(cl)
# Return number of parallel workers
getDoParWorkers() # 8 threads on MacBook Pro (Retina, 15-inch, Mid 2015)

# Return computation time with 8 threads on 4 physical cores -------------------
system.time(
# total C model with leave-one-out cross-validation
  pls_C_repcv <- fit_pls(
    spec_chem = spec_chem[!is.na(spec_chem$C), ],
    response = C,
    evaluation_method = "resampling",
    resampling_method = "rep_kfold_cv",
    pls_ncomp_max = 1
  )
)
# -> not significantly faster: 35s

# Shut down the workers --------------------------------------------------------
stopCluster(cl)
