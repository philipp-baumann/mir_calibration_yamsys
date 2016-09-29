################################################################################
## Script author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Date: September 6, 2016
## Project: YAMSYS
## Description: Develop PLS regression calibration models for the YAMSYS
## pilot sites
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
pls_P<- readRDS("models/pls_P.Rds")

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
## Graphical summary of the model evaluations
################################################################################

## Summary for soil properties =================================================

# Function to extract ggplot graph element from pls model output list ----------
extract_pls_plot <- function(list_in) {
  list_plot <- lapply(list_in, function(x) x$p_model)
  return(list_plot)
}

# Create list of model objects -------------------------------------------------
list_pls_models <-  list(
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

# Create list of ggplot2 plot objects for all variables ------------------------
list_pls_plot <- extract_pls_plot(list_pls_models)

# Edit axis labels in pls plot list --------------------------------------------

# Soil mineralogy related
# Total Fe
xlab_Fe <- xlab(expression(paste("Total Fe", " [g ", kg^-1, "]")))
ylab_Fe <- ylab(expression(paste("Total Fe", " [g ", kg^-1, "]")))
# Total Si
xlab_Si <- xlab(expression(paste("Total Si", " [g ", kg^-1, "]")))
ylab_Si <- ylab(expression(paste("Total Si", " [g ", kg^-1, "]")))
# Total Al
xlab_Al <- xlab(expression(paste("Total Al", " [g ", kg^-1, "]")))
ylab_Al <- ylab(expression(paste("Total Al", " [g ", kg^-1, "]")))
# Total K
xlab_K <- xlab(expression(paste("Total K", " [g ", kg^-1, "]")))
ylab_K <- ylab(expression(paste("Total K", " [g ", kg^-1, "]")))
# Total Ca
xlab_Ca <- xlab(expression(paste("Total Ca", " [g ", kg^-1, "]")))
ylab_Ca <- ylab(expression(paste("Total Ca", " [g ", kg^-1, "]")))
# Total Zn
xlab_Zn <- xlab(expression(paste("Total Zn", " [mg ", kg^-1, "]")))
ylab_Zn <- ylab(expression(paste("Total Zn", " [mg ", kg^-1, "]"))) 
# Total Cu
xlab_Cu <- xlab(expression(paste("Total Cu", " [mg ", kg^-1, "]")))
ylab_Cu <- ylab(expression(paste("Total Cu", " [mg ", kg^-1, "]")))
# Total Mn
xlab_Mn <- xlab(expression(paste("Total Mn", " [mg ", kg^-1, "]")))
ylab_Mn <- ylab(expression(paste("Total Mn", " [mg ", kg^-1, "]")))

# Soil mineralogy + plant nutrition related
# pH
xlab_pH <- xlab(expression(paste("pH", "(", H[2], "O", ")")))
ylab_pH <- ylab(expression(paste("pH", "(", H[2], "O", ")")))
# Exchangeable K
xlab_K_exch <- xlab(expression(paste("K (exch.)", " [mg ", kg^-1, "]")))
ylab_K_exch <- ylab(expression(paste("K (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Ca
xlab_Ca_exch <- xlab(expression(paste("Ca (exch.)", " [mg ", kg^-1, "]")))
ylab_Ca_exch <- ylab(expression(paste("Ca (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Mg
xlab_Mg_exch <- xlab(expression(paste("Mg (exch.)", " [mg ", kg^-1, "]")))
ylab_Mg_exch <- ylab(expression(paste("Mg (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Al
xlab_Al_exch <- xlab(expression(paste("Al (exch.)", " [mg ", kg^-1, "]")))
ylab_Al_exch <- ylab(expression(paste("Al (exch.)", " [mg ", kg^-1, "]")))
# Effective Cation Exchange Capacity (CEC)
xlab_CEC <- xlab(expression(paste("Obs. ", CEC[eff], " [cmol(+) ", kg^-1, "]")))
ylab_CEC <- ylab(expression(paste("Pred. ", CEC[eff], " [cmol(+) ", kg^-1, "]")))
# Base saturation 
xlab_BS <- xlab("Base saturation [%]")
ylab_BS <- ylab("Base saturation [%]")


# Soil organic matter
# Total C
xlab_C <- xlab(expression(paste("Obs. total C", " [g ", kg^-1, "]")))
ylab_C <- ylab(expression(paste("Pred. total C", " [g ", kg^-1, "]")))
# Total nitrogen
xlab_N <- xlab(expression(paste("Total N", " [g ", kg^-1, "]")))
ylab_N <- ylab(expression(paste("Total N", " [g ", kg^-1, "]")))
# Total sulfur
xlab_S <- xlab(expression(paste("Total S", " [mg ", kg^-1, "]")))
ylab_S <- ylab(expression(paste("Total S", " [mg ", kg^-1, "]")))
# Total P
xlab_P <- xlab(expression(paste("Total P", " [mg ", kg^-1, "]")))
ylab_P <- ylab(expression(paste("Total P", " [mg ", kg^-1, "]"))) 


# Plant nutrition related soil properties
# Resin extractable P 
xlab_P_resin <- xlab(expression(paste("log(Resin P)", " [mg ", kg^-1, "]")))
ylab_P_resin <- ylab(expression(paste("log(Resin P)", " [mg ", kg^-1, "]")))
# Fe(DTPA)
xlab_Fe_DTPA <- xlab(expression(paste("log(Fe (DTPA))", " [mg ", kg^-1, "]")))
ylab_Fe_DTPA <- ylab(expression(paste("log(Fe (DTPA))", " [mg ", kg^-1, "]")))
# Zn(DTPA)
xlab_Zn_DTPA <- xlab(expression(paste("Zn (DTPA)", " [mg ", kg^-1, "]")))
ylab_Zn_DTPA <- ylab(expression(paste("Zn (DTPA)", " [mg ", kg^-1, "]")))
# Cu(DTPA)
xlab_Cu_DTPA <- xlab(expression(paste("Cu (DTPA)", " [mg ", kg^-1, "]")))
ylab_Cu_DTPA <- ylab(expression(paste("Cu (DTPA)", " [mg ", kg^-1, "]")))
# Mn(DTPA)
xlab_Mn_DTPA <- xlab(expression(paste("Mn (DTPA)", " [mg ", kg^-1, "]")))
ylab_Mn_DTPA <- ylab(expression(paste("Mn (DTPA)", " [mg ", kg^-1, "]")))


# Add new x and y axis annotations to model plots in list ----------------------

list_pls_plot$pls_Fe_total <- list_pls_plot$pls_Fe_total +
  xlab_Fe + ylab_Fe
list_pls_plot$pls_Si_total <- list_pls_plot$pls_Si_total +
  xlab_Si + ylab_Si
list_pls_plot$pls_Al_total <- list_pls_plot$pls_Al_total +
  xlab_Al + ylab_Al
list_pls_plot$pls_K_total <- list_pls_plot$pls_K_total +
  xlab_K + ylab_K
list_pls_plot$pls_Ca_total <- list_pls_plot$pls_Ca_total +
  xlab_Ca + ylab_Ca
list_pls_plot$pls_Zn_total <- list_pls_plot$pls_Zn_total +
  xlab_Zn + ylab_Zn
list_pls_plot$pls_Cu_total <- list_pls_plot$pls_Cu_total +
  xlab_Cu + ylab_Cu
list_pls_plot$pls_Mn_total <- list_pls_plot$pls_Mn_total +
  xlab_Mn + ylab_Mn

list_pls_plot$pls_pH <- list_pls_plot$pls_pH +
  xlab_pH + ylab_pH
list_pls_plot$pls_exch_K <- list_pls_plot$pls_exch_K +
  xlab_K_exch + ylab_K_exch
list_pls_plot$pls_exch_Ca <- list_pls_plot$pls_exch_Ca +
  xlab_Ca_exch + ylab_Ca_exch
list_pls_plot$pls_exch_Mg <- list_pls_plot$pls_exch_Mg +
  xlab_Mg_exch + ylab_Mg_exch
list_pls_plot$pls_exch_Al <- list_pls_plot$pls_exch_Al +
  xlab_Al_exch + ylab_Al_exch
list_pls_plot$pls_BS <- list_pls_plot$pls_BS +
  xlab_BS + ylab_BS
list_pls_plot$pls_CEC <- list_pls_plot$pls_CEC +
  xlab_CEC + ylab_CEC

list_pls_plot$pls_soil_C_all <- list_pls_plot$pls_soil_C_all +
  xlab_C + ylab_C
list_pls_plot$pls_soil_N_all <- list_pls_plot$pls_soil_N_all +
  xlab_N + ylab_N + xlim(c(0, 2.2)) + ylim(c(0, 2.2))
list_pls_plot$pls_soil_S_all <- list_pls_plot$pls_soil_S_all +
  xlab_S + ylab_S
list_pls_plot$pls_P_total <- list_pls_plot$pls_P_total +
  xlab_P + ylab_P

list_pls_plot$pls_resin_P <- list_pls_plot$pls_resin_P +
  xlab_P_resin + ylab_P_resin
list_pls_plot$pls_Fe_DTPA <- list_pls_plot$pls_Fe_DTPA +
  xlab_Fe_DTPA + ylab_Fe_DTPA
list_pls_plot$pls_Zn_DTPA <- list_pls_plot$pls_Zn_DTPA +
  xlab_Zn_DTPA + ylab_Zn_DTPA
list_pls_plot$pls_Cu_DTPA <- list_pls_plot$pls_Cu_DTPA +
  xlab_Cu_DTPA + ylab_Cu_DTPA
list_pls_plot$pls_Mn_DTPA <- list_pls_plot$pls_Mn_DTPA +
  xlab_Mn_DTPA + ylab_Mn_DTPA

