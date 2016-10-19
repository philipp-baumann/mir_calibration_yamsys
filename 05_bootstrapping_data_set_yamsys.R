# Load simplerspec package for spectral model development helper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

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

# Don't average replicate spectra by sample_id, use resampled spectra (spc_rs)
# instead of averaged spectra (spc_mean) for preprocessing
soilspec_rep<- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  preprocess_spc(select = "sg_1_w21", column_in = "spc_rs")

# Resample preprocessed data, randomly select one row within the sample_id 
# group; use functions from dplyr package
soilspec_1rep <- soilspec_rep %>% group_by(sample_id) %>%
  sample_n(1)

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join spectra tibble and chemical reference analysis tibble
spec_chem_1rep <- join_spc_chem(
  spc_tbl = soilspec_1rep , chem_tbl = soilchem_tbl, by = "sample_id")

## PLS regression modeling ====================================================

# Total soil carbon (C) model
pls_C_1rep <- pls_ken_stone(
  spec_chem = spec_chem_1rep[!is.na(spec_chem_1rep$C), ],
  ratio_val = 1/3,
  variable = C,
  validation = TRUE,
  invert = FALSE,
  pc = 6,
  pls_ncomp_max = 6
)

