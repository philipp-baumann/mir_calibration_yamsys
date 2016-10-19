################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: October 17, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
################################################################################

## Load required packages ======================================================
library(simplerspec)
library(tidyverse)

## Read spectra in list ========================================================

# List of OPUS files from Alpha at ETH Zürich
lf_eth <- list.files("data/spectra/soilspec_eth_bin/", full.names = T)

# List of OPUS files from Alpha at ESAL in Côte d'Ivoire
lf_esal <- list.files("data/spectra/soilspec_esal_bin/", full.names = T)

# Read files: ETH
spc_list_eth <- read_opus(
  fnames = lf_eth,
  in_format = c("binary"),
  out_format = "list"
)
# ESAL
spc_list_esal <- read_opus(
  fnames = lf_esal,
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

# ESAL Alpha files
soilspec_tbl_esal <- spc_list_esal %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

## Read chemical reference data and join with spectral data ====================

soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# ETH Alpha spectra
spc_chem_eth <- join_spc_chem(
  spc_tbl = soilspec_tbl_eth , chem_tbl = soilchem_tbl, by = "sample_id")

# ICRAF Alpha spectra at ESAL
spc_chem_esal <- join_spc_chem(
  spc_tbl = soilspec_tbl_esal , chem_tbl = soilchem_tbl, by = "sample_id")


################################################################################
## Part 2: Run PLS regression models for different soil variables
## Use 2/3 of samples for calibration and 1/3 of samples for validation
## (argument ratio_val = 1/3); always exclude missing values for samples 
## that have missing values in the target soil property variable
################################################################################

## Example model for carbon with preprocessed ETH Alpha spectra ================

pls_C <- pls_ken_stone(
  spec_chem = spc_chem_eth[!is.na(spc_chem_eth$C), ],
  ratio_val = 1/3,
  variable = C,
  validation = TRUE,
  invert = FALSE,
  pc = 6,
  pls_ncomp_max = 6
)

# Check missing values in spectra
# spectra_eth <- data.table::rbindlist(soilspec_tbl_eth$spc_pre)
# spectra_eth[, 1]
# spectra_eth

# Test plotting
plot_spc(soilspec_tbl_eth, y = "spc_pre", by = "sample_id")


################################################################################
## Part 3: Predict processed spectra from ICRAF ALPHA spectrometer at 
## ESAL in Côte d'Ivoire using the model developed with ETH ALPHA spectra
################################################################################

# Prediction of ESAL spectra based on ETH model
# Collect different model in a list (add more models...)
models_eth <- list(
  pls_C = pls_C
)

# Predict values ---------------------------------------------------------------

# Function appends predictions as columns (names are taken from list of 
# models) to <spc_tbl> spectra tibble; functions takes preprocessed spectra
# in <spc_pre> column and reshapes prediction output from caret package
predictions_esal <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = models_eth,
  spc_tbl = soilspec_tbl_esal)


## Combine predicted values with chemical reference data set ===================

# Rename variables
soilchem_tbl <- soilchem_tbl %>% rename(sample_id = sample_ID)
pred_chem <- dplyr::inner_join(soilchem_tbl, predictions_esal)

## Plot predicted vs. observed =================================================

pdf(file = "out/figs/spectrometer_validation_C.pdf", width = 5, height = 5)
qplot(x = C, y = pls_C, data = pred_chem, 
  xlab = expression(paste("Observed C [", g, " ", kg^-1, "]")), 
  ylab = expression(paste("Predicted C [", g, " ", kg^-1, "]",
    " (rescanned)")),
  xlim = c(0, 25), ylim = c(0,25)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 21, y = 19, label = "1:1 line",
    color="red") +
  coord_fixed() + 
  labs(title = "Spectrometer validation by predicting \n ICRAF ALPHA spectra based on ETH ALPHA model")
dev.off()

## Calculate model statistics 
summary_df(df = as.data.frame(pred_chem), x = "pls_C", y = "C")

## BF_mo_08_soil_cal has missing values
soilspec_tbl_esal %>% 
  filter(sample_id == "BF_mo_08_soil_cal") %>% 
  select(spc_pre) %>% .[[1]]

# Reaading of this file doesn't work !!!! -> wrong wavenumber
spc_list_esal[["BF_mo_08_soil_cal.2"]]
