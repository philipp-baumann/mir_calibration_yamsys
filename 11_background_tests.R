################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: March 21, 2017s
## Project: YAMSYS
## Description: 
################################################################################

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development wrapper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

## Read spectra in list ========================================================

# List of OPUS files containing spectra obtained with gold background reference
lf_gold <- list.files("data/spectra/soilspec_background/yamsys_bg_gold", 
  full.names = TRUE)
# List of OPUS files containing spectra obtained with KBR (Potassium Bromide)
# reference
lf_kbr <- list.files("data/spectra/soilspec_background/yamsys_bg_kbr", 
  full.names = TRUE)

# Read files: Gold reference
spc_gold <- read_opus(
  fnames = lf_gold,
  in_format = c("binary"),
  out_format = "list"
)
# -> obtained error: "Error in hexView::readRaw(file.name, width = NULL, 
# offset = offs_spc_sample,  :  Invalid number of bytes" 
# => Deselecting atmospheric correction changes structure of binary file
# Read files: KBr reference
spc_KBr <- read_opus(
  fnames = lf_kbr,
  in_format = c("binary"),
  out_format = "list"
)