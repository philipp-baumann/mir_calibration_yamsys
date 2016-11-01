################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: October 28, 2016
## Project: YAMSYS
## Description: Cross-comparisoin ICRAF LDSF reference analysis and predictions 
# of site Petit Bouaké (Lilyo) with predictions of YAMSYS analyses and model
# predictions.
################################################################################

# Load simplerspec package for spectral model development helper functions
require(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
require(tidyverse)

################################################################################
## Part 1: Read YAMSYS reference model including yam field samples of all
## 4 sites and rescanned LDSF reference soil samples from Petit-Bouaké (Lilyo) 
## obtained by ICRAF
################################################################################

# Total carbon PLS regression model
pls_C <- readRDS("models/pls_C.Rds")
# Total nitrogen PLS regression model
pls_N <- readRDS("models/pls_N.Rds")
# pH PLS regression model
pls_pH <- readRDS("models/pls_pH.Rds")

# Store models into a list
models_eth <- list(
  pls_C = pls_C,
  pls_N = pls_N,
  pls_pH = pls_pH
)

################################################################################
## Part 2: Read spectra of ICRAF and predict values using the YAMSYS model
################################################################################

## Read spectra in list ========================================================

# List of OPUS files LDSF in Lilyo
lf_ldsf_lilyo <- list.files("data/spectra/soilspec_ldsf_petit-bouake/",
  full.names = T)

# Read files: ETH
spc_list_ldsf_lilyo <- simplerspec::read_opus(
  fnames = lf_ldsf_lilyo,
  in_format = c("binary"),
  out_format = "list"
)

## Spectral data processing pipe ===============================================

soilspec_ldsf_lilyo <- spc_list_ldsf_lilyo %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

## Predict LDSF samples of Lilyo using spectra of ICRAF ========================

predictions_ldsf_lilyo <- predict_from_spc(
  # List of models (output from pls_ken_stone())
  model_list = models_eth,
  spc_tbl = soilspec_ldsf_lilyo)

# Create a new column SSN in the predicted data: copy of sample_id column
predictions_ldsf_lilyo$SSN <- predictions_ldsf_lilyo$sample_id

## Compare predicted vs. measured values =======================================

# Read chemical values ---------------------------------------------------------
# Read reference values of LDSF
ldsf_lilyo_ref_icraf <- read_csv(
  "data/ldsf_petit-bouake/V4C_IR_predicted_soil_properties_Lilyo_ref.csv")
ldsf_lilyo_pred_icraf <- read_csv(
  "data/ldsf_petit-bouake/V4C_IR_predicted_soil_properties_Lilyo_pred.csv")

# Convert observed values of Carbon from percent to g kg^(-1)
ldsf_lilyo_ref_icraf <- ldsf_lilyo_ref_icraf %>% 
  mutate(Total_Carbon_pred = Total_Carbon * 10) %>% 
  select(SSN, Total_Carbon_pred)

# Convert predicted values of Carbon from percent to g kg^(-1)
ldsf_lilyo_pred_icraf <- ldsf_lilyo_pred_icraf %>% 
  mutate(Total_Carbon = Total_Carbon * 10)

# Combine predicted values with chemical reference data set --------------------

# Join predicted and chemical reference analysis data sets
ldsf_lilyo_pred_obs <- dplyr::inner_join(
  ldsf_lilyo_ref_icraf, ldsf_lilyo_pred_icraf, by = "SSN"
)

## Sample "esal000593" has two predictions for Topsoil 
## -> Source is reference data sheet
ldsf_lilyo_pred_obs %>% filter(SSN == "esal000593")

# Calculate prediction accuracy statistics 
C_stats <- summary_df(df = as.data.frame(ldsf_lilyo_pred_obs), 
  x = "Total_Carbon", y = "Total_Carbon_pred"
)

C_stats_annotation <- plyr::mutate(C_stats,
    rmse = as.character(as.expression(paste0("RMSE == ",
      round(rmsd, 2)))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ",
      round(r2, 2)))),
    rpd = as.character(as.expression(paste("RPD == ",
      round(rpd, 2)))),
    n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot observed vs. predicted values (group by topsoil and subsoil)
p_obs_ldsf_pred_yamsys <- 
  ggplot(aes(x = Total_Carbon, y = Total_Carbon_pred), 
    data = ldsf_lilyo_pred_obs) +
  geom_point(aes(shape = factor(Depth))) +
  xlab(expression(paste("Observed C LDSF (ICRAF analyses) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C LDSF ( ICRAF model, ICRAF spectra) [", g, " ", kg^-1, "]"))) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 28, y = 25, label = "1:1 line",
    color="red") +
  # Model evaluation statistics: annotations
  geom_text(data = C_stats_annotation, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_annotation, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_annotation, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
  geom_text(data = C_stats_annotation, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  xlim(c(-3, 35)) +
  ylim(c(-3, 35)) +
  scale_shape_manual("", values = c(1, 19)) +
  coord_fixed() +
  theme_bw()

# Save plot as pdf
ggsave(filename = "out/figs/p_obs_ldsf_pred_yamsys.pdf",
  p_obs_ldsf_pred_yamsys)


################################################################################
## Part 3: Compare predictions of LDSF by ICRAF
################################################################################

# Read reference values of LDSF
ldsf_lilyo_ref_icraf <- read_csv(
  "data/ldsf_petit-bouake/V4C_IR_predicted_soil_properties_Lilyo_ref.csv") %>% 
  select(SSN, Total_Carbon, Depth) %>% 
  mutate(Total_Carbon = 10 * Total_Carbon)

# Print data
ldsf_lilyo_ref_icraf

# Rename predicted values for carbon and 
# convert observed values of Carbon from percent to g kg^(-1)
ldsf_lilyo_ref_icraf <- ldsf_lilyo_ref_icraf %>% 
  mutate(Total_Carbo = Total_Carbon * 10) %>%
  select(SSN, Total_Carbon, Depth)

# Join reference analysis of LDSF measured by ICRAF and prections by ICRAF
ldsf_lilyo_pred_eth_icraf <- dplyr::inner_join(
  ldsf_lilyo_ref_icraf, predictions_ldsf_lilyo
)

ldsf_lilyo_pred_eth_icraf

# Calculate model statistics
C_stats_pred_eth_icraf <- summary_df(df = as.data.frame(ldsf_lilyo_pred_eth_icraf), 
  x = "Total_Carbon", y = "pls_C"
)
C_stats_icraf_annotation <- plyr::mutate(C_stats_pred_eth_icraf,
  rmse = as.character(as.expression(paste0("RMSE == ",
    round(rmsd, 2)))),
  r2 = as.character(as.expression(paste0("italic(R)^2 == ",
    round(r2, 2)))),
  rpd = as.character(as.expression(paste("RPD == ",
    round(rpd, 2)))),
  n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot predicted vs. observed
p_pred_eth_obs_ldsf <- 
  ggplot(aes(x = Total_Carbon, y = pls_C), 
    data = ldsf_lilyo_pred_eth_icraf) +
  geom_point(aes(shape = factor(Depth))) +
  xlab(expression(paste("Observed C LDSF (ICRAF analyses) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C LDSF (YAMSYS model, ICRAF spectra) [", g, " ", kg^-1, "]"))) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 28, y = 25, label = "1:1 line",
    color="red") +
  # Model evaluation statistics: annotations
 geom_text(data = C_stats_icraf_annotation, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_icraf_annotation, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_icraf_annotation, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
    geom_text(data = C_stats_icraf_annotation, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  xlim(c(-3, 35)) +
  ylim(c(-3, 35)) +
  scale_shape_manual("", values = c(1, 19)) +
  coord_fixed() +
  theme_bw()


################################################################################
## Part 4: Compare predictions using ICRAF spectra with ETH model and ICRAF
## prediction outputs
################################################################################

ldsf_lilyo_pred_icraf

# Read ICRAF predicted values of LDSF
ldsf_lilyo_pred_icraf <- read_csv(
  "data/ldsf_petit-bouake/V4C_IR_predicted_soil_properties_Lilyo_pred.csv") %>% 
  mutate(Total_Carbon_pred_icraf = Total_Carbon * 10) %>%
select(SSN, Total_Carbon_pred_icraf, Depth)
  

# Join ICRAF predicted values of LDSF with predictions from Lucien's example
# spectra from LDSF predicted by the YAMSYS model developed at ETH
ldsf_pred_yamsys_icraf <- dplyr::inner_join(
  ldsf_lilyo_pred_icraf, predictions_ldsf_lilyo)

# Calculate model statistics
C_stats_pred_icraf_yamsys <- summary_df(
  df = as.data.frame(ldsf_pred_yamsys_icraf),
  x = "Total_Carbon_pred_icraf", y = "pls_C"
)
C_stats_pred_icraf_yamsys <- plyr::mutate(C_stats_pred_icraf_yamsys,
  rmse = as.character(as.expression(paste0("RMSE == ",
    round(rmsd, 2)))),
  r2 = as.character(as.expression(paste0("italic(R)^2 == ",
    round(r2, 2)))),
  rpd = as.character(as.expression(paste("RPD == ",
    round(rpd, 2)))),
  n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot predicted vs. observed
p_pred_icraf_yamsys <-
  ggplot(aes(x = Total_Carbon_pred_icraf, y = pls_C), 
    data = ldsf_pred_yamsys_icraf) +
  geom_point(aes(shape = factor(Depth))) +
  xlab(expression(paste("Predicted C LDSF (ICRAF model, ICRAF spectra) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Predicted C LDSF (YAMSYS model, ICRAF spectra) [", g, " ", kg^-1, "]"))) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom="text", x = 28, y = 25, label = "1:1 line",
    color="red") +
  # Model evaluation statistics: annotations
  # Model evaluation statistics: annotations
  geom_text(data = C_stats_pred_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_pred_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_pred_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
    geom_text(data = C_stats_pred_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  xlim(c(-3, 35)) +
  ylim(c(-3, 35)) +
  scale_shape_manual("", values = c(1, 19)) +
  coord_fixed() +
  theme_bw()

################################################################################
## Plot chemical reference data for 14 Topsoil calibration samples from
## the LDSF in Lilyo analyzed by ICRAF vs. analyzed at ETH Zurich
################################################################################

# Read ICRAF LDSF reference data and extract SSN
ldsf_lilyo_icraf <- read_csv(
  "data/ldsf_petit-bouake/V4C_IR_predicted_soil_properties_Lilyo_ref.csv") %>% 
  mutate(Total_Carbon = 10 * Total_Carbon) %>% 
  filter(Depth == "Topsoil")

# Extract SSN
ldsf_lilyo_icraf_ssn <- ldsf_lilyo_icraf %>% 
  select(SSN) %>% .[["SSN"]]

# Read soil wet chemical reference analyses of YAMSYS
soilchem_yamsys <- read_csv(
  "data/soilchem/soilchem_yamsys.csv")

# Select only reference samples from LDSF in Soubré
soilchem_yamsys_ldsf <- soilchem_yamsys %>% filter(site == "sb_icraf")

# Add SSN column from ICRAF LDSF reference analyses to YAMSYS data set
soilchem_yamsys_ldsf$SSN <- ldsf_lilyo_icraf_ssn

# Join datasets of ICRAF and ETH Zürich
ldsf_ref_icraf_yamsys <- inner_join(soilchem_yamsys_ldsf, ldsf_lilyo_icraf,
  by = "SSN")

# Calculate model statistics
C_stats_ref_icraf_yamsys <- summary_df(
  df = as.data.frame(ldsf_ref_icraf_yamsys),
  x = "Total_Carbon", y = "C"
)
C_stats_ref_icraf_yamsys <- plyr::mutate(C_stats_ref_icraf_yamsys,
  rmse = as.character(as.expression(paste0("RMSE == ",
    round(rmsd, 2)))),
  r2 = as.character(as.expression(paste0("italic(R)^2 == ",
    round(r2, 2)))),
  rpd = as.character(as.expression(paste("RPD == ",
    round(rpd, 2)))),
  n = as.character(as.expression(paste0("italic(n) == ", n)))
)

# Plot compariosn LDSF topsoil total carbon analyses by ICRAF and ETH
p_ref_icraf_yamsys <-
  ggplot(aes(x = Total_Carbon, y = C), 
    data = ldsf_ref_icraf_yamsys) +
  geom_point(aes(shape = factor(Depth))) +
  xlab(expression(paste("Observed C LDSF 2013 (ICRAF analyses) [", g, " ", kg^-1, "]"))) +
  ylab(expression(paste("Observed C LDSF 2015 (ETHZ analyses) [", g, " ", kg^-1, "]"))) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  annotate(geom = "text", x = 28, y = 25, label = "1:1 line",
    color="red") +
  geom_text(data = C_stats_ref_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = n), size = 3,
    hjust = -0.15, vjust = 1.5, parse = TRUE) +
  geom_text(data = C_stats_ref_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = r2), size = 3,
    hjust = -0.1, vjust = 3, parse = TRUE) +
  geom_text(data = C_stats_ref_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = rmse), size = 3,
    hjust = -0.1, vjust = 6.5, parse = TRUE) +
    geom_text(data = C_stats_ref_icraf_yamsys, 
    aes(x = -Inf, y = Inf, label = rpd), size = 3,
    hjust = -0.1, vjust = 9, parse = TRUE) +
  xlim(c(-3, 35)) +
  ylim(c(-3, 35)) +
  coord_fixed() +
  scale_shape_manual("", values = 19) +
  theme_bw()

################################################################################
## Combine different set of combinations for model, spectra, and reference
## analysis comparisons
################################################################################

p_intercomp <- cowplot::plot_grid(p_obs_ldsf_pred_yamsys, p_pred_eth_obs_ldsf,
  p_pred_icraf_yamsys, p_ref_icraf_yamsys,
  labels = c("A: ICRAF LDSF model calibration", 
    "B: YAMSYS model prediction vs. LDSF reference", 
    "C: Prediction intercomparision YAMSYS vs. ICRAF",
    "D: Chemical analyses of resampled LDSF reference point"), ncol = 2,
  hjust = -0.04)

# Legend
#c("A: LDSF model calibration", "B: YAMSYS model prediction vs. LDSF reference", 
#  "C: Prediction intercomparision YAMSYS vs. ICRAF"),

ggsave(filename = "out/figs/p_icraf_eth_intercomp.pdf",
  p_intercomp, width = 12, height = 12)


















 