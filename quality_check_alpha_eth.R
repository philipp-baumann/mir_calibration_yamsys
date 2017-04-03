################################################################################
## Spectra quality check of the ALPHA spectrometer at ETH based on
## 5 samples from the carbon model validation set that cover range
## of observed carbon values well
################################################################################

# Remove all R objects from memory
rm(list = ls())

# Load simplerspec package for spectral model development wrapper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)
require(gridExtra)
require(grid)

## Select 6 random samples from the validation set for a spectra quality 
## check =======================================================================

# Read total C model 
pls_C <- readRDS("models/pls_C.Rds")

# Select 6 random samples from the calibration set using a fixed random seed
set.seed(48)
df_split <- modelr::crossv_mc(pls_C$data$validation, n = 1, test = 1/6) 
C_validation <- df_split %>% .[1, ] %>% .$test %>% .[[1]] %>% as_tibble()
# Print carbon values
C_validation$C
# Save sample_id as file
C_validation$sample_id %>%
  write(file = "out/files/quality_check_val_carbon.txt")

## Compare new predictions with observed chemical values #######################

# Create a list of models
models <- list(
  # Total carbon model
  pls_C = pls_C
)

# Read, gather, resample, average and preprocess quality check spectra =========

# Rescans at an effective resolution of 1 cm^-1 --------------------------------
# Read files: ETH
# List of OPUS files
lf_eth_scan_1 <- list.files("data/spectra/soilspec_eth_qcheck_res_1/", full.names = T)
# Read files
spc_list_eth_scan_1 <- read_opus(
  fnames = lf_eth_scan_1,
  in_format = c("binary"),
  out_format = "list"
)
# Spectra processing workflow
soilspec_eth_scan_1 <- spc_list_eth_scan_1 %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

# Rescans at an effective resolution of 2 cm^-1 --------------------------------
# Read files: ETH
# List of OPUS files
lf_eth_scan_2 <- list.files("data/spectra/soilspec_eth_qcheck_res_2/", full.names = T)
# Read files
spc_list_eth_scan_2 <- read_opus(
  fnames = lf_eth_scan_2,
  in_format = c("binary"),
  out_format = "list"
)
# Spectra processing workflow
soilspec_eth_scan_2 <- spc_list_eth_scan_2 %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")


## Plot quality check spectra ==================================================

# Source plotting function plot_spc()
source("R/plot-spc.R")

# Plot rescan at resolution of 1 cm^-1
p_spc_eth_scan_1_raw <- plot_spc(spc_tbl = soilspec_eth_scan_1, 
  y = "spc_mean", by = "sample_id") +
  geom_line(aes(colour = id), alpha = 1) +
  ggtitle(expression(paste("Scanned at 1 ", cm^-1, " resolution")))
# Convert graph into gtable
p_spc_eth_scan_1_raw <- ggplot_gtable(ggplot_build(p_spc_eth_scan_1_raw))
p_spc_eth_scan_1_pre <- plot_spc(spc_tbl = soilspec_eth_scan_1, 
  y = "spc_pre", by = "sample_id") +
   geom_line(aes(colour = id), alpha = 1) +
  ylab("SG 1, window size = 21 points (Absorbance)")

# Plot rescan at resolution of 2 cm^-1
p_spc_eth_scan_2_raw <- plot_spc(spc_tbl = soilspec_eth_scan_2, 
  y = "spc_mean", by = "sample_id") +
  geom_line(aes(colour = id), alpha = 1) +
  ggtitle(expression(paste("Scanned at 2 ", cm^-1, " resolution")))
# Convert graph into gtable
p_spc_eth_scan_2_raw <- ggplot_gtable(ggplot_build(p_spc_eth_scan_2_raw))
p_spc_eth_scan_2_pre <- plot_spc(spc_tbl = soilspec_eth_scan_2, 
  y = "spc_pre", by = "sample_id") +
   geom_line(aes(colour = id), alpha = 1) +
  ylab("SG 1, window size = 21 points (Absorbance)")

# Combined graph
pdf(file = "out/figs/spc_qcheck.pdf", width = 16, height = 8)
p_spc_eth_qcheck <- cowplot::plot_grid(
  p_spc_eth_scan_1_raw, p_spc_eth_scan_2_raw,
  p_spc_eth_scan_1_pre, p_spc_eth_scan_2_pre,ncol = 2
)
p_spc_eth_qcheck
dev.off()

# Preprocessed spectra
plot_spc(spc_tbl = soilspec_eth_scan_1, y = "spc_pre", by = "sample_id") +
  geom_line(aes(colour = id), alpha = 1)
plot_spc(spc_tbl = soilspec_eth_scan_2, y = "spc_pre", by = "sample_id") +
  geom_line(aes(colour = id), alpha = 1)

## Predict quality check spectra based on calibration model ====================

# Predictions for rescans at resolution of 1 cm^-1 -----------------------------
predicted_C_rescan_res_1 <- predict_from_spc(
  model_list = models, spc_tbl = soilspec_eth_scan_1)

# Show predicted values
predicted_C_rescan_res_1$pls_C

# Join predicted values with chemical reference values
pred_obs_C_rescan_res_1 <- dplyr::inner_join(
  pls_C$data$validation, predicted_C_rescan_res_1, 
  by = "sample_id")

# Calculate prediction accuracy statistics 
C_stats_res_1 <- summary_df(df = as.data.frame(pred_obs_C_rescan_res_1), 
  x = "C", y = "pls_C"
)

# Make annotation for graph
C_stats_annotation_res_1 <- plyr::mutate(C_stats_res_1,
    rmse = as.character(as.expression(paste0("RMSE == ",
      round(rmsd, 2)))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ",
      round(r2, 2)))),
    rpd = as.character(as.expression(paste("RPD == ",
      round(rpd, 2)))),
    n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot observed vs. predicted (quality check rescan)
p_pred_qcheck_res_1 <- qplot(x = C, y = pls_C, data = pred_obs_C_rescan_res_1) +
  geom_abline(col = "red") +
  annotate(geom = "text", x = 20, y = 18, label = "1:1 line",
    color = "red") +
  xlim(c(0, 25)) +
  ylim(c(0, 25)) +
  xlab(expression(paste("Observed C (YAMSYS) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C rescans at 1 ", cm^-1, " [", g, " ", kg^-1, "]"))) +
  geom_text(data = C_stats_annotation_res_1, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_1, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_1, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_1, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  theme_bw() +
  coord_fixed()

# Predictions for rescans at resolution of 2 cm^-1 -----------------------------
predicted_C_rescan_res_2 <- predict_from_spc(
  model_list = models, spc_tbl = soilspec_eth_scan_2)

# Show predicted values
predicted_C_rescan_res_2$pls_C

# Join predicted values with chemical reference values
pred_obs_C_rescan_res_2 <- dplyr::inner_join(
  pls_C$data$validation, predicted_C_rescan_res_2, 
  by = "sample_id")

# Calculate prediction accuracy statistics 
C_stats_res_2 <- summary_df(df = as.data.frame(pred_obs_C_rescan_res_2), 
  x = "C", y = "pls_C"
)

# Make annotation for graph
C_stats_annotation_res_2 <- plyr::mutate(C_stats_res_2,
    rmse = as.character(as.expression(paste0("RMSE == ",
      round(rmsd, 2)))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ",
      round(r2, 2)))),
    rpd = as.character(as.expression(paste("RPD == ",
      round(rpd, 2)))),
    n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot observed vs. predicted (quality check rescan)
p_pred_qcheck_res_2 <- qplot(x = C, y = pls_C, data = pred_obs_C_rescan_res_2) +
  geom_abline(col = "red") +
  annotate(geom = "text", x = 20, y = 18, label = "1:1 line",
    color = "red") +
  xlim(c(0, 25)) +
  ylim(c(0, 25)) +
  xlab(expression(paste("Observed C (YAMSYS) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C rescans at 2 ", cm^-1, " [", g, " ", kg^-1, "]"))) +
  geom_text(data = C_stats_annotation_res_2, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_2, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_2, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
  geom_text(data = C_stats_annotation_res_2, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  theme_bw() +
  coord_fixed()

# Arrange predictions for different resolution in one graph --------------------
p_pred_qcheck <- cowplot::plot_grid(p_pred_qcheck_res_1, p_pred_qcheck_res_2)

ggsave(filename = "out/figs/C_pred_qcheck.pdf", p_pred_qcheck)

