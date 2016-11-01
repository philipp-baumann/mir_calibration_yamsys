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
soilspec_rep <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  preprocess_spc(select = "sg_1_w21", column_in = "spc_rs")

# Randomly select one row within the sample_id 
# group; use functions from dplyr package
soilspec_1rep <- soilspec_rep %>% group_by(sample_id) %>%
  sample_n(1)

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join spectra tibble and chemical reference analysis tibble -------------------

# Random selection of one scan replicate per sample_id group
spec_chem_1rep <- join_spc_chem(
  spc_tbl = soilspec_1rep , chem_tbl = soilchem_tbl, by = "sample_id")

spec_chem <- join_spc_chem(
  spc_tbl = soilspec_rep , chem_tbl = soilchem_tbl, by = "sample_id")

## PLS regression modeling ====================================================

# Total soil carbon (C) model using only one randomly selected replicate scan
# per sample_id
pls_C_1rep <- pls_ken_stone(
  spec_chem = spec_chem_1rep[!is.na(spec_chem_1rep$C), ],
  ratio_val = 1/3,
  variable = C,
  validation = TRUE,
  invert = FALSE,
  pc = 6,
  pls_ncomp_max = 6
)

# Total soil carbon (C) model using average scans by sample_id
pls_C <- pls_ken_stone(
  spec_chem = spec_chem[!is.na(spec_chem$C), ],
  ratio_val = 1/3,
  variable = C,
  validation = TRUE,
  invert = FALSE,
  pc = 6,
  pls_ncomp_max = 6
)

# Make graphs comparing model performances -------------------------------------

# C model graph using 1 random replicate scan per sample_id
plot_C_1rep <- pls_C_1rep$p_model +
  labs(title = "Randomly select one replicate spectrum per sample_id") +
  theme(plot.title = element_text(size = 11))

# C model graph using all average scans by sample_id
plot_C <- pls_C$p_model +
  labs(title = "Average spectra per sample_id") +
  theme(plot.title = element_text(size = 11))

# Draw one combined graph using cowplot
plot_C_combined <- cowplot::plot_grid(plot_C, plot_C_1rep, 
  labels = c("A", "B"), nrow = 2, align = "v")
ggsave("out/figs/model_C_all_vs_1rep.pdf", plot_C_combined, width = 5)


################################################################################
## Spectrometer validation using only one replicate scan
################################################################################

## Read spectra in list ========================================================

# List of OPUS files from Alpha at ESAL in Côte d'Ivoire
lf_esal <- list.files("data/spectra/soilspec_esal_bin/", full.names = T)

# ESAL
spc_list_esal <- read_opus(
  fnames = lf_esal,
  in_format = c("binary"),
  out_format = "list"
)

## Spectral data processing pipe ===============================================

# Don't average replicate spectra by sample_id, use resampled spectra (spc_rs)
# instead of averaged spectra (spc_mean) for preprocessing
soilspec_rep_esal <- spc_list_esal %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  preprocess_spc(select = "sg_1_w21", column_in = "spc_rs")

# Randomly select one row within the sample_id 
# group; use functions from dplyr package
soilspec_1rep_esal <- soilspec_rep_esal %>% group_by(sample_id) %>%
  sample_n(1)

# Join spectra tibble and chemical reference analysis tibble ===================

# Random selection of one scan replicate per sample_id group
spec_chem_1rep_esal <- join_spc_chem(
  spc_tbl = soilspec_1rep_esal , chem_tbl = soilchem_tbl, by = "sample_id")


## Predict values using ESAL ALPHA spectra =====================================

# Prediction of ESAL spectra based on ETH model
# Collect different model in a list (add more models...)
model_eth_1rep <- list(
  pls_C_1rep = pls_C_1rep
)

# Predict values ---------------------------------------------------------------

# Function appends predictions as columns (names are taken from list of 
# models) to <spc_tbl> spectra tibble; functions takes preprocessed spectra
# in <spc_pre> column and reshapes prediction output from caret package
predictions_esal_1rep <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = model_eth_1rep,
  spc_tbl = soilspec_1rep_esal)


## Combine predicted values with chemical reference data set ===================

# Rename variables
soilchem_tbl <- soilchem_tbl %>% rename(sample_id = sample_ID)
pred_chem_1rep <- dplyr::inner_join(predictions_esal_1rep, soilchem_tbl)


## Plot predicted vs. observed =================================================

pdf(file = "out/figs/spectrometer_validation_C_1rep.pdf", width = 5, height = 5)
qplot(x = C, y = pls_C_1rep, data = pred_chem_1rep, 
  xlab = expression(paste("Observed C [", g, " ", kg^-1, "]")), 
  ylab = expression(paste("Predicted C [", g, " ", kg^-1, "]",
    " (rescanned)")),
  xlim = c(0, 25), ylim = c(0,25)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 21, y = 19, label = "1:1 line",
    color="red") +
  coord_fixed() + 
  labs(title = "Spectrometer validation by predicting \n ICRAF ALPHA spectra based on ETH ALPHA model, \n randomly select one replicate spectrum per sample_id")
dev.off()


