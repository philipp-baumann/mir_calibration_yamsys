################################################################################
## Test deep learning for spectroscopy (Artificial neural networks)
################################################################################

# Load libraries
library(h2o)
library(dplyr)
library(simplerspec)

## Load spectra ################################################################

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

# Convert spectra to matrix using data.table::rbindlist
spc_pre <- matrix(data.table:::rbindlist(soilspec_tbl_eth$spc_pre))


