################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: October 31, 2016
## Project: YAMSYS
## Description: Model evaluation by comparing model-based predictions and
## measured values for the YAMSYS soil calibration models
################################################################################

# Remove all R objects from memory
rm(list = ls())

################################################################################
## Read all spectral models (created and saved within <models> folder;
## see script <01_model_development_yamsys.R>)
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

pls_Fe_total <- readRDS("models/pls_Fe_total.Rds")
pls_Si_total <- readRDS("models/pls_Si_total.Rds")
pls_Al_total <- readRDS("models/pls_Al_total.Rds")
pls_K_total <- readRDS("models/pls_K_total.Rds")
pls_Ca_total <- readRDS("models/pls_Ca_total.Rds")
pls_Zn_total <- readRDS("models/pls_Zn_total.Rds")
pls_Cu_total <- readRDS("models/pls_Cu_total.Rds")
pls_Mn_total <- readRDS("models/pls_Mn_total.Rds")

## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

pls_pH <- readRDS("models/pls_pH.Rds")
pls_exch_K <- readRDS("models/pls_exch_K.Rds")
pls_exch_Ca <- readRDS("models/pls_exch_Ca.Rds")
pls_exch_Mg <- readRDS("models/pls_exch_Mg.Rds")
pls_exch_Al <- readRDS("models/pls_exch_Al.Rds")
pls_CEC <- readRDS("models/pls_CEC.Rds")
pls_BS <- readRDS("models/pls_BS.Rds")


## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

pls_C <- readRDS("models/pls_C.Rds")
pls_N <- readRDS("models/pls_N.Rds")
pls_S <- readRDS("models/pls_S.Rds")
pls_P <- readRDS("models/pls_P.Rds")

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

pls_resin_P_log <- readRDS("models/pls_resin_P_log.Rds")
pls_Fe_DTPA_log <- readRDS("models/pls_Fe_DTPA_log.Rds")
pls_Zn_DTPA <- readRDS("models/pls_Zn_DTPA.Rds")
pls_Cu_DTPA <- readRDS("models/pls_Cu_DTPA.Rds")
pls_Mn_DTPA <- readRDS("models/pls_Mn_DTPA.Rds")

################################################################################
## Summarize number of PLS regression coefficients 
################################################################################

# List of models
models <- list(
  pls_Fe_total = pls_Fe_total,
  pls_Si_total = pls_Si_total,
  pls_Al_total = pls_Al_total,
  pls_K_total = pls_K_total,
  pls_Ca_total = pls_Ca_total,
  pls_Zn_total = pls_Zn_total,
  pls_Cu_total = pls_Cu_total,
  pls_Mn_total = pls_Mn_total,
  pls_pH = pls_pH,
  pls_exch_K = pls_exch_K,
  pls_exch_Ca = pls_exch_Ca,
  pls_exch_Mg = pls_exch_Mg,
  pls_exch_Al = pls_exch_Al,
  pls_CEC = pls_CEC,
  pls_BS = pls_BS,
  pls_C = pls_C,
  pls_N = pls_N,
  pls_S = pls_S,
  pls_P = pls_P,
  pls_resin_P_log = pls_resin_P_log,
  pls_Fe_DTPA_log = pls_Fe_DTPA_log,
  pls_Zn_DTPA = pls_Zn_DTPA,
  pls_Cu_DTPA = pls_Cu_DTPA,
  pls_Mn_DTPA = pls_Mn_DTPA
)

# List number of final components per model
lapply(models, function(x) x$pls_model$finalModel$ncomp)

# Extract all variable names
lapply(models, function(x) x$stats$variable)

# Export model evaluation statistics -------------------------------------------

model_evaluation <- do.call(rbind, lapply(models, function(x) x$stats)) %>% 
  filter(dataType == "Validation") %>%
  select(variable, n, ncomp, r2, rmse, msd, SB_prop, NU_prop, LC_prop)

# Save model evaluation as csv (all soil models)
write_csv(model_evaluation, path = "out/tables/model_evaluation_soil_all.csv")
# Select soil models with R^2 > 0.6 in validation
model_evaluation %>% filter(r2 >= 0.6) %>% 
  write_csv(path = "out/tables/model_evaluation_soil_r2_06.csv")

## Inspect cross-validation RMSE during model tuning ===========================

# Example of exchangeable K model
pdf(file = "out/figs/model_tuning_exch_K.pdf", width = 4, height = 4)
plot(pls_exch_K$pls_model)
dev.off()

# Example of total carbon model
pdf(file = "out/figs/model_tuning_C.pdf", width = 4, height = 4)
plot(pls_C$pls_model)
dev.off()
