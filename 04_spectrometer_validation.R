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
lf_eth <- list.files("data/spectra/soilspec_eth_bin", full.names = TRUE)

# List of OPUS files from Alpha at ESAL in Côte d'Ivoire
lf_esal <- list.files("data/spectra/soilspec_esal_bin", full.names = TRUE)

# Read files: ETH
spc_list_eth <- read_opus_univ(fnames = lf_eth, 
  extract = c("spc", "sc_sm", "sc_rf"))
# ESAL
spc_list_esal <- read_opus(
  fnames = lf_esal,
  in_format = c("binary"),
  out_format = "list"
)

spc_esal_sel <- read_opus_univ(fnames = lf_esal, 
  extract = c("spc", "sc_sm", "sc_rf"))

spc_esal_sel$BF_lo_01_soil_cal.0[["sc_rf"]]

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
  spc_tbl = soilspec_tbl_eth, chem_tbl = soilchem_tbl, by = "sample_id")

# ICRAF Alpha spectra at ESAL
spc_chem_esal <- join_spc_chem(
  spc_tbl = soilspec_tbl_esal, chem_tbl = soilchem_tbl, by = "sample_id")


################################################################################
## Part 2: Run PLS regression models for different soil variables
## Use 2/3 of samples for calibration and 1/3 of samples for validation
## (argument ratio_val = 1/3); always exclude missing values for samples 
## that have missing values in the target soil property variable
################################################################################

## Example model for carbon with preprocessed ETH Alpha spectra ================

# pls_C <- pls_ken_stone(
#   spec_chem = spc_chem_eth[!is.na(spc_chem_eth$C), ],
#   ratio_val = 1/3,
#   variable = C,
#   validation = TRUE,
#   invert = FALSE,
#   pc = 6,
#   pls_ncomp_max = 6
# )

# Read models from RDS files
pls_C <- readRDS(file = "models/pls_C.Rds")
pls_CEC <- readRDS(file = "models/pls_CEC.Rds")

# Check missing values in spectra
# spectra_eth <- data.table::rbindlist(soilspec_tbl_eth$spc_pre)
# spectra_eth[, 1]
# spectra_eth

# Test plotting
p_pre_eth <- plot_spc(soilspec_tbl_eth, y = "spc_pre", by = "sample_id") +
  ylab("Preproc. abs")
p_pre_esal <- plot_spc(soilspec_tbl_eth, y = "spc_pre", by = "sample_id") +
  ylab("Preproc. abs")

p_raw_eth <- plot_spc(soilspec_tbl_eth, y = "spc_mean", by = "sample_id") +
  ggtitle("YAMSYS spectro ref.: ALPHA SAE, \n KBr background, atmos. comp., all samples (n = 94)")
p_raw_esal <- plot_spc(soilspec_tbl_esal, y = "spc_mean", by = "sample_id") +
   ggtitle("YAMSYS spectro ref.: ALPHA Yamoussoukro, \n gold background, samples Burkina Faso (n = 40)")

# Arrange plots ----------------------------------------------------------------

pdf(file = "out/figs/spc_comp_eth_esal.pdf", width = 12, height = 6)
cowplot::plot_grid(p_raw_eth, p_raw_esal, p_pre_eth, p_pre_esal, 
  labels = c("", "", "", ""), ncol = 2)
dev.off()

################################################################################
## Part 3: Predict processed spectra from ICRAF ALPHA spectrometer at 
## ESAL in Côte d'Ivoire using the model developed with ETH ALPHA spectra
################################################################################

# Prediction of ESAL spectra based on ETH model
# Collect different model in a list (add more models...)
models_eth <- list(
  pls_C = pls_C,
  pls_CEC = pls_CEC
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

p_pred_esal <- qplot(x = C, y = pls_C, data = pred_chem,
  xlab = expression(paste("Observed C [", g, " ", kg^-1, "]")), 
  ylab = expression(paste("Predicted C [", g, " ", kg^-1, "]",
    " (rescanned)")),
  xlim = c(0, 25), ylim = c(0,25)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 21, y = 19, label = "1:1 line",
    color="red") +
  coord_fixed() + 
  labs(title = "Spectrometer validation by predicting \n Yamoussoukro ALPHA spectra \n based on ETH ALPHA model") +
  theme_bw()

pdf(file = "out/figs/spectrometer_validation_C.pdf", width = 5, height = 5)
p_pred_esal
dev.off()
# -> Warning message to fix: 
# "Removed 1 rows containing missing values (geom_point)"

## Calculate model statistics 
summary_df(df = as.data.frame(pred_chem), x = "C", y = "pls_C")

## BF_mo_08_soil_cal has missing values
# soilspec_tbl_esal %>% 
#   filter(sample_id == "BF_mo_08_soil_cal") %>%
#   select(spc_pre) %>% .[[1]]

# Reaading of this file doesn't work !!!! -> wrong wavenumber
# spc_list_esal[["BF_mo_08_soil_cal.2"]]


################################################################################
## Compare sets of preprocessed spectra from ETH Zürich ALPHA and 
## Côte d'Ivoire ALPHA spectrometer
################################################################################

# Côte d'Ivoire (esal)
# soilspec_esal <- soilspec_tbl_esal %>% 
#   select(sample_id, spc_pre) %>%
#   # add new column "instrument_location"
#   mutate(instrument_location = rep("esal", nrow(.)))

# ETH Zürich (eth)
# soilspec_eth <- soilspec_tbl_eth %>% 
#   select(sample_id, spc_pre) %>% 
#   # add new column "instrument_location"
#   mutate(instrument_location = rep("eth", nrow(.))) %>% 
#   # select only samples that occur were also rescanned in Côte d'Ivoire
#   .[.$sample_id %in% soilspec_esal$sample_id, ]

# Combine ETH and ESAL spectrometer data (bind rows)
# soilspec_eth_esal <- rbind(soilspec_esal, soilspec_eth)

# Define new function to calculate and plot 
# differences between preproceses spectral tibbles of the two instruments ------

# diff_spc <- function(spc_tbl, y, align_by, compare_by, slice = TRUE,
#                      format_out = "long",
#                      xlab = expression(paste("Wavenumber [", cm^-1, "]")),
#                      ylab = "Ratio of first derivatives of absorbance") {
#   
#   # (0) Group by factors and slice
#   group_cols <- c(align_by, compare_by)
#   if (slice == TRUE) {
#     spc_tbl <- purrr::slice_rows(spc_tbl, group_cols) %>% 
#       slice(1L)
#       # apply function to each slice
#       # purrr::by_slice(1L)
#   } 
#   # (1) Gather spectra into one data.table
#   if(y == "spc") {
#     # raw spectra are not yet data.tables and extraction is done alternatively
#     # via do.call(rbind, list) -> a little bit slower
#     dt <- data.table::data.table(do.call(rbind, spc_tbl[, y][[y]]))
#   } else {
#     dt <- data.table::rbindlist(spc_tbl[, y][[y]])
#   }
#   # (2) Extract ID variable and append it to the data.table
#   id <- spc_tbl[, align_by][[align_by]]
#   dt <- dt[,id:=id]
#   # Define variable to compare spectra by
#   compare <- spc_tbl[, compare_by][[compare_by]]
#   dt <- dt[, compare:=compare]
#   if(format_out == "long") {
#   # (3) Convert data.table from wide to long form
#   # Omit id
#     dt_long <- data.table::melt(
#       dt, measure=names(dt)[!names(dt) %in% c("id", "compare")]
#     )
#     # Convert variable column from factor to numeric
#     dt_long[, variable := as.numeric(as.character(variable))][]
#   } else { # if not long
#     dt[]
#   }
#   # (4) Split data table by `compare_by` column -> results in data.frame
#   df <- split(dt, compare)
#   # Define new id
#   id <- df[[1]]$id
#   # (5) Calculate preprocessed absorbance ratio between the two spectrometer
#   # locations
#   df_ratio <- purrr::map2_df(
#     df[[1]][, -c("compare", "id"), ],
#     df[[2]][, -c("compare", "id"), ], 
#     `/`)
#   # Save name of comparison method as vector
#   comparison_metric <- rep(paste(
#     unique(df[[1]]$compare[1]), 
#     "/",
#     unique(df[[2]]$compare[1])), 
#     nrow(df[[1]]))
#   # Transform into data.table and add sample_id and description of comparison
#   # measure
#   dt_ratio <- data.table::data.table(df_ratio)
#   dt_ratio[, c("id", "comparison_metric") := list(id, comparison_metric), ][]
#   # (6) Plot ratio of preprocessed spectra
#   # Convert dt_ratio into long form
#   dt_ratio_long <- data.table::melt(
#     dt_ratio, measure=names(dt_ratio)[!names(dt_ratio) %in%
#       c("id", "comparison_metric")]
#   )
#   # Convert variable column from factor to numeric
#   dt_ratio_long <- dt_ratio_long[, 
#     variable := as.numeric(as.character(variable))]
#   # Define nice breaks for x axis
#   brk_ratio  <- pretty(as.numeric(names(dt_ratio)[!names(dt_ratio) %in%
#     c("id", "comparison_metric")]), n = 10)
#   # ggplot2 plot
#   p_ratio <- ggplot(dt_ratio_long, aes(variable, value)) +  # group = group)) +
#     labs(x = xlab, y = ylab) +
#     theme_bw() +
#     scale_x_reverse(breaks = brk_ratio) +
#     geom_line(aes(colour = comparison_metric), colour = "black",
#       alpha = 1, size = 0.2) +
#     # scale_color_manual(values = "black")
#     # scale_color_manual(values = rep("black", nrow(dt_ratio))) +
#     # Remove legend
#     # guides(colour = FALSE) +
#     geom_hline(yintercept = 1, colour = "red")
#   p_ratio
# }
# 
# # Test function
# spc_difference <- diff_spc(
#   soilspec_eth_esal, 
#   y = "spc_pre",
#   # implement multiple by's
#   align_by = "sample_id",
#   compare_by = "instrument_location",
#   format_out = "wide")
# # Save ratio plot in pdf
# ggplot2::ggsave("out/figs/prepr_abs_ratio_eth_esal.pdf", spc_difference)
# 
# # Print only from -5 to 5 (adapt y axis scale)
# spc_difference_small_yscale <- spc_difference +
#   ylim(c(-5, 5)) +
#   geom_hline(yintercept = 1, colour = "red")
# # Save plot
# ggplot2::ggsave("out/figs/prepr_abs_ratio_eth_esal_small_scale.pdf",
#   spc_difference_small_yscale)
# 
# # Check data structure
# str(spc_difference)
# 
# # Work with purrr
# spc_compare <- spc_difference %>%
#   split(.$compare) %>%
#   names()
#   map2(.[, -c("compare", "id"), ], `/`)
# 
# spc_difference_eth <- spc_difference[compare == "eth"]
# spc_difference_esal <- spc_difference[compare == "esal"]
# 
# map2_df(
#   spc_difference_eth[, -c("compare", "id"), ], 
#   spc_difference_esal[, -c("compare", "id"), ], `/`)
# 
# # Divide columns 
# library(data.table)
# for(j in cols){
#   set(dt, i=NULL, j=j, value= dt[[j]]/dt[[j]][rownum])
# }
# 
# # Use purrr
# mtcars %>%
#   split(.$cyl) %>%
#   map(~ lm(mpg ~ wt, data = .)) %>%
#   map(summary) %>%
#   map_dbl("r.squared")
# 
# # Fixed and constant arguments
# Map(function(x, w) weighted.mean(x, w, na.rm = TRUE), xs, ws)
