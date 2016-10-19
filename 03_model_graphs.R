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

# Load ggplot2 package
library(ggplot2)

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
xlab_N <- xlab(expression(paste("Obs. total N", " [g ", kg^-1, "]")))
ylab_N <- ylab(expression(paste("Pred. total N", " [g ", kg^-1, "]")))
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

list_pls_plot$pls_C <- list_pls_plot$pls_C +
  xlab_C + ylab_C
list_pls_plot$pls_N <- list_pls_plot$pls_N +
  xlab_N + ylab_N # + xlim(c(0, 2.2)) + ylim(c(0, 2.2))
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

# Arrange pls plots in panels, separate figures according to soil property
# categories -------------------------------------------------------------------

# Model selection for poster
models_poster <- c(
  "pls_C", "pls_N", "pls_CEC"
)
# Model selection for evaluation
models_mineralogy <- c(
  "pls_Fe_total", "pls_Si_total", "pls_Al_total", "pls_K_total", "pls_Ca_total",
  "pls_Zn_total", "pls_Cu_total", "pls_Mn_total"
)
models_mineralogy_nutrition <- c(
  "pls_pH", "pls_exch_K", "pls_exch_Ca", "pls_exch_Mg", "pls_exch_Al",
  "pls_CEC", "pls_BS"
)
models_organic <- c(
  "pls_C", "pls_N", "pls_S", "pls_P"
)
models_nutrition <- c(
  "pls_resin_P_log", "pls_Fe_DTPA_log", "pls_Zn_DTPA", "pls_Cu_DTPA",
  "pls_Mn_DTPA"
)

# Create ggplot2 grobs
grob_list_poster <- lapply(
  list_pls_plot[models_poster], ggplotGrob
)
grob_list_mineralogy <- lapply(
  list_pls_plot[models_mineralogy], ggplotGrob
)
grob_list_mineralogy_nutrition <- lapply(
  list_pls_plot[models_mineralogy_nutrition], ggplotGrob
)
grob_list_organic <- lapply(
  list_pls_plot[models_organic], ggplotGrob
)
grob_list_nutrition <- lapply(
  list_pls_plot[models_nutrition], ggplotGrob
)


# Print graphs using gtable_arrange function -----------------------------------

library(gridExtra)
library(grid)
# Source function gtable_arrange provided in the <R> folder
source("R/gtable_arrange.R")

# Produce a pdf with model evaluation graphs in panels
pdf(file = "out/figs/poster_models_soil.pdf",
  width = 7, height = 5)
gtable_arrange(ncol = 2, grobs = grob_list_poster, 
  left = "Predicted",
  bottom = "Observed")
dev.off() 

# Mineralogy
pdf(file = "out/figs/models_mineralogy.pdf",
  width = 7, height = 10)
gtable_arrange(ncol = 2, grobs = grob_list_mineralogy, 
  left = "Predicted",
  bottom = "Observed")
dev.off()

# Mineralogy and plant nutrition
pdf(file = "out/figs/models_mineralogy_nutrition.pdf",
  width = 7, height = 10)
gtable_arrange(ncol = 2, grobs = grob_list_mineralogy_nutrition, 
  left = "Predicted",
  bottom = "Observed")
dev.off()

# Organic matter
pdf(file = "out/figs/models_organic.pdf",
  width = 7, height = 5)
gtable_arrange(ncol = 2, grobs = grob_list_organic,
  left = "Predicted",
  bottom = "Observed")
dev.off()

# Nutrition
# Organic matter
pdf(file = "out/figs/models_nutrition.pdf",
  width = 7, height = 8)
gtable_arrange(ncol = 2, grobs = grob_list_nutrition, 
  left = "Predicted",
  bottom = "Observed")
dev.off()



################################################################################
## Maps of sampled fields within the four pilot sites
################################################################################

## Read sampling metadata ======================================================
# Alternative file readers
library(readr)

# Read field data from Léo
data_field_lo <- read_csv(file = "data/sampling/data_field_lo.csv" )
# Midebdo
data_field_mo <- read_csv(file = "data/sampling/data_field_mo.csv" )
# Soubré
data_field_sb <- read_csv(file = "data/sampling/data_field_sb.csv" )
# Tiéningboué
# Set column classes
cls <- c(gps_long = "numeric", gps_lat = "numeric")
data_field_tb <- read_csv(file = "data/sampling/data_field_tb.csv",
  col_types = list(
    gps_long = col_double(),
    gps_lat = col_double()
))

# Plot sampling distribution ===================================================
library(dplyr)

# Tiéningboué
pdf(file = "out/figs/poster_sampling_tb.pdf", width = 4, height = 3)
# make pretty breaks
gps_long_brk <- pretty(data_field_tb$gps_long, n = 4)
gps_lat_brk <- pretty(data_field_tb$gps_lat, n = 4)
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string <- as_labeller(c(`tb` = "Tiéningboué, Côte d'Ivoire"))
# Plot graph of sampling distribution
dplyr::filter(data_field_tb, mat_type == "soil_cal") %>% 
  ggplot(data = ., aes(x = gps_long, y = gps_lat)) +
  coord_fixed(ratio = 1) +
  geom_point(aes(colour = species, shape = species)) +
  xlab("E (UTM) [m]") +
  ylab("N (UTM) [m]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_discrete(name = "") + 
  scale_shape_discrete(name = "") +
  scale_x_continuous(breaks = gps_long_brk) +
  scale_y_continuous(breaks = gps_lat_brk) +
  facet_wrap("site", labeller = to_string) +
  theme(legend.text = element_text(face = "italic"))
dev.off()


################################################################################
## Plots of spectra
################################################################################

library(simplerspec)

# Reading, averaging, outlier and outlier removal of spectra -------------------
soilspec_plotting <- read_spectra(path = "data/spectra/alpha_txt/") %>% 
  average_spectra() %>%
  # do not remove outliers
  remove_outliers(remove = FALSE)

# Create plots of soil spectra -------------------------------------------------

# Source spectral plotting function
source(file = "R/plot-spectra.R")
pdf(file = "out/figs/poster_soilspec.pdf", width = 6, height = 1.5)
plot_spectra(spc = soilspec_plotting$MIR_mean, no_group = TRUE) +
  theme(legend.position = "none")
dev.off()
