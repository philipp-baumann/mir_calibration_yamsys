################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: June 22, 2017
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
spc_gold <- read_opus_univ(fnames = lf_gold, extract = c("spc"))
# Read files: KBr reference
spc_kbr <- read_opus_univ(fnames = lf_kbr, extract = c("spc"))

# Try new universal OPUS reader ------------------------------------------------

spc_gold <- spc_gold %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

spc_kbr <- spc_kbr %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

# Read chemical reference data from YAMSYS spectroscopy reference samples
soilchem_yamsys <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")

# Join preprocessed spectra with chemical reference data from YAMSYS
spc_chem_gold <- join_spc_chem(spc_tbl = spc_gold, chem_tbl = soilchem_yamsys)
spc_chem_kbr <- join_spc_chem(spc_tbl = spc_kbr, chem_tbl = soilchem_yamsys)

# Plot two raw (mean) spectra sets (gold vs. KBr background)
p_raw_gold <- plot_spc(spc_tbl = spc_gold, graph_id_1 = "gold",
  y = "spc_mean", by = "sample_id", alpha = 0.5) +
  labs(title = "Rescans of 5 YAMSYS reference samples \n SAE ALPHA, gold background")
p_raw_kbr <- plot_spc(spc_tbl = spc_kbr, graph_id_1 = "KBr", 
  y = "spc_mean", by = "sample_id", alpha = 0.5) +
  labs(title = "Rescans of 5 YAMSYS reference samples \n SAE ALPHA, KBr background, atmos. comp.")

# Plot two preprocessed spectra (gold vs. KBr background)
p_pre_gold <- plot_spc(spc_tbl = spc_gold, graph_id_1 = "gold", 
  y = "spc_pre", by = "sample_id", alpha = 0.5) +
  ylab("Preproc. abs.")
p_pre_kbr <- plot_spc(spc_tbl = spc_kbr, graph_id_1 = "KBr", 
  y = "spc_pre", by = "sample_id", alpha = 0.5) +
  ylab("Preproc. abs.")

# Read PLS model for C
pls_C <- readRDS(file = "models/pls_C.Rds")

# Predict carbon based on new spectra
# Collect models in list
models_eth <- list(
  'pls_C' = pls_C
)

predictions_gold <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = models_eth,
  spc_tbl = spc_chem_gold)

predictions_kbr <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = models_eth,
  spc_tbl = spc_chem_kbr)

# Compare predictions ----------------------------------------------------------

# gold background (tested at ETH ALPHA)
p_pred_gold <- ggplot(data = predictions_gold, aes(x = C, y = pls_C)) +
  geom_point() +
  xlim(c(0, 40)) +
  ylim(c(0, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 21, y = 19, label = "1:1 line",
    color="red") +
  coord_fixed() +
  xlab(expression(paste("Observed C [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C [", g, " ", kg^-1, "]",
    " (gold background)"))) +
  labs(title = "Predictions using rescanned samples with gold background \n SAE ALPHA") +
  theme_bw()

# KBr background (tested at ETH ALPHA)
p_pred_kbr <- ggplot(data = predictions_kbr, aes(x = C, y = pls_C)) +
  geom_point() +
  xlim(c(0, 40)) +
  ylim(c(0, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 21, y = 19, label = "1:1 line",
    color="red") +
  coord_fixed() +
  xlab(expression(paste("Observed C [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C [", g, " ", kg^-1, "]",
    " (KBr background)"))) +
   labs(title = "Predictions using rescanned samples with KBr background \n SAE ALPHA, atmos. comp.") +
  theme_bw()

# Make summary plots with cowplot ----------------------------------------------

pdf(file = "out/figs/spc_comp_kbr_gold.pdf", width = 12, height = 6)
cowplot::plot_grid(p_raw_kbr, p_raw_gold, p_pre_kbr, p_pre_gold, 
  labels = c("", "", "", ""), ncol = 2)
dev.off()

pdf(file = "out/figs/pred_comp_kbr_gold.pdf", width = 12, height = 6)
cowplot::plot_grid(p_pred_kbr, p_pred_gold, 
  labels = c("", ""), ncol = 2)
dev.off()
