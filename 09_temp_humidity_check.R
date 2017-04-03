################################################################################
## Test temperature and humidity reading feature in OPUS file reader
################################################################################

# Load simplerspec package for spectral model development helper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

## YAMSYS soil reference rescans from ALPHA spectrometer in Ivory Coast ========

# Read spectra from OPUS files -------------------------------------------------

# List of OPUS files from Alpha at ESAL in Yamoussoukro
lf_esal <- list.files("data/spectra/soilspec_esal_bin/", full.names = T)

# List of OPUS files from Alpha at ETH Zurich
lf_eth <- list.files("data/spectra/soilspec_eth_bin/", full.names = T)

# Read files: ESAL
spc_list_esal <- read_opus(
  fnames = lf_esal,
  in_format = c("binary"),
  out_format = "list"
)

# Spectra data processing pipe -------------------------------------------------

soilspec_tbl_esal <- spc_list_esal %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

# Gather temperature values for all OPUS spectra files -------------------------

# Print list of metadata data frames
soilspec_tbl_esal$metadata

# Combine list of metadata data frames (in tibble list-column) into 
# one data frame
metadata_esal <- do.call(rbind, soilspec_tbl_esal$metadata)

# Filter and plot data
ggplot2::qplot(x = date_time, y = temp_scanner_sample, data = metadata_esal)
ggplot2::qplot(x = date_time, y = hum_abs_bg, data = metadata_esal)
ggplot2::qplot(x = date_time, y = hum_rel_sample, data = metadata_esal)

soilspec_tbl_eth %>% 
  filter(scan_id == "BF_lo_01_soil_cal.0") %>% 
  select(wavenumbers) %>% 
  .[["wavenumbers"]]

source("R/plot-spc.R")

soilspec_tbl_eth %>% 
  filter(scan_id == "BF_lo_01_soil_cal.0") %>% 
  plot_spc(y = "spc", by = "sample_id")
