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

## =============================================================================
## 5: Texture: 
## Sand, silt and clay
## =============================================================================
pls_sand <- readRDS("models/pls_sand.Rds")
pls_sand_cv <- readRDS("models/pls_sand_cv.Rds")
pls_sand_msc <- readRDS("models/pls_sand_msc.Rds")
pls_sand_msc_cv <- readRDS("models/pls_sand_msc_cv.Rds")
pls_silt <- readRDS("models/pls_silt.Rds")
pls_clay <- readRDS("models/pls_clay.Rds")
pls_clay_cv <- readRDS("models/pls_clay_cv.Rds")


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
  pls_Mn_DTPA = pls_Mn_DTPA,
  pls_sand = pls_sand,
  pls_sand_cv = pls_sand_cv,
  pls_sand_msc = pls_sand_msc,
  pls_sand_msc_cv = pls_sand_msc_cv,
  pls_silt = pls_silt,
  pls_clay = pls_clay,
  pls_clay_cv = pls_clay_cv
)

# Create list of ggplot2 plot objects for all variables ------------------------
list_pls_plot <- extract_pls_plot(list_pls_models)

# Edit axis labels in pls plot list --------------------------------------------

# Soil mineralogy related
# Total Fe
xlab_Fe <- xlab(expression(paste("Obs. total Fe", " [g ", kg^-1, "]")))
ylab_Fe <- ylab(expression(paste("Pred. total Fe", " [g ", kg^-1, "]")))
# Total Si
xlab_Si <- xlab(expression(paste("Obs. total Si", " [g ", kg^-1, "]")))
ylab_Si <- ylab(expression(paste("Pred. total Si", " [g ", kg^-1, "]")))
# Total Al
xlab_Al <- xlab(expression(paste("Obs. total Al", " [g ", kg^-1, "]")))
ylab_Al <- ylab(expression(paste("Pred. total Al", " [g ", kg^-1, "]")))
# Total K
xlab_K <- xlab(expression(paste("Obs. total K", " [g ", kg^-1, "]")))
ylab_K <- ylab(expression(paste("Pred. total K", " [g ", kg^-1, "]")))
# Total Ca
xlab_Ca <- xlab(expression(paste("Obs. total Ca", " [g ", kg^-1, "]")))
ylab_Ca <- ylab(expression(paste("Pred. total Ca", " [g ", kg^-1, "]")))
# Total Zn
xlab_Zn <- xlab(expression(paste("Obs. total Zn", " [mg ", kg^-1, "]")))
ylab_Zn <- ylab(expression(paste("Pred. total Zn", " [mg ", kg^-1, "]"))) 
# Total Cu
xlab_Cu <- xlab(expression(paste("Obs. total Cu", " [mg ", kg^-1, "]")))
ylab_Cu <- ylab(expression(paste("Pred. total Cu", " [mg ", kg^-1, "]")))
# Total Mn
xlab_Mn <- xlab(expression(paste("Obs. total Mn", " [mg ", kg^-1, "]")))
ylab_Mn <- ylab(expression(paste("Pred. total Mn", " [mg ", kg^-1, "]")))

# Soil mineralogy + plant nutrition related
# pH
xlab_pH <- xlab(expression(paste("Obs. pH", "(", H[2], "O", ")")))
ylab_pH <- ylab(expression(paste("Pred. pH", "(", H[2], "O", ")")))
# Exchangeable K
xlab_K_exch <- xlab(expression(paste("Obs. K (exch.)", " [mg ", kg^-1, "]")))
ylab_K_exch <- ylab(expression(paste("Pred .K (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Ca
xlab_Ca_exch <- xlab(expression(paste("Obs. Ca (exch.)", " [mg ", kg^-1, "]")))
ylab_Ca_exch <- ylab(expression(paste("Pred. Ca (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Mg
xlab_Mg_exch <- xlab(expression(paste("Obs. Mg (exch.)", " [mg ", kg^-1, "]")))
ylab_Mg_exch <- ylab(expression(paste("Pred. Mg (exch.)", " [mg ", kg^-1, "]")))
# Exchangeable Al
xlab_Al_exch <- xlab(expression(paste("Obs. Al (exch.)", " [mg ", kg^-1, "]")))
ylab_Al_exch <- ylab(expression(paste("Obs. Al (exch.)", " [mg ", kg^-1, "]")))
# Effective Cation Exchange Capacity (CEC)
xlab_CEC <- xlab(expression(paste("Obs. ", CEC[eff], " [cmol(+) ", kg^-1, "]")))
ylab_CEC <- ylab(expression(paste("Pred. ", CEC[eff], " [cmol(+) ", kg^-1, "]")))
# Base saturation 
xlab_BS <- xlab("Obs. Base saturation [%]")
ylab_BS <- ylab("Pred. Base saturation [%]")


# Soil organic matter
# Total C
xlab_C <- xlab(expression(paste("Obs. total C", " [g ", kg^-1, "]")))
ylab_C <- ylab(expression(paste("Pred. total C", " [g ", kg^-1, "]")))
# Total nitrogen
xlab_N <- xlab(expression(paste("Obs. total N", " [g ", kg^-1, "]")))
ylab_N <- ylab(expression(paste("Pred. total N", " [g ", kg^-1, "]")))
# Total sulfur
xlab_S <- xlab(expression(paste("Obs. total S", " [mg ", kg^-1, "]")))
ylab_S <- ylab(expression(paste("Pred. total S", " [mg ", kg^-1, "]")))
# Total P
xlab_P <- xlab(expression(paste("Obs. total P", " [mg ", kg^-1, "]")))
ylab_P <- ylab(expression(paste("Pred. total P", " [mg ", kg^-1, "]"))) 


# Plant nutrition related soil properties
# Resin extractable P 
xlab_P_resin <- xlab(expression(paste("Obs. log(Resin P)", " [mg ", kg^-1, "]")))
ylab_P_resin <- ylab(expression(paste("Pred. log(Resin P)", " [mg ", kg^-1, "]")))
# Fe(DTPA)
xlab_Fe_DTPA <- xlab(expression(paste("Obs. log(Fe (DTPA))", " [mg ", kg^-1, "]")))
ylab_Fe_DTPA <- ylab(expression(paste("Pred. log(Fe (DTPA))", " [mg ", kg^-1, "]")))
# Zn(DTPA)
xlab_Zn_DTPA <- xlab(expression(paste("Obs. Zn (DTPA)", " [mg ", kg^-1, "]")))
ylab_Zn_DTPA <- ylab(expression(paste("Pred. Zn (DTPA)", " [mg ", kg^-1, "]")))
# Cu(DTPA)
xlab_Cu_DTPA <- xlab(expression(paste("Obs. Cu (DTPA)", " [mg ", kg^-1, "]")))
ylab_Cu_DTPA <- ylab(expression(paste("Pred. Cu (DTPA)", " [mg ", kg^-1, "]")))
# Mn(DTPA)
xlab_Mn_DTPA <- xlab(expression(paste("Obs. Mn (DTPA)", " [mg ", kg^-1, "]")))
ylab_Mn_DTPA <- ylab(expression(paste("Pred. Mn (DTPA)", " [mg ", kg^-1, "]")))


# Texture:
# Sand
xlab_sand <- xlab(expression(paste("Obs. sand", " [%]")))
ylab_sand <- ylab(expression(paste("Pred. sand", " [%]")))
# Silt
xlab_silt <- xlab(expression(paste("Obs. silt", " [%]")))
ylab_silt <- ylab(expression(paste("Pred. silt", " [%]")))
# Clay
xlab_clay <- xlab(expression(paste("Obs. clay", " [%]")))
ylab_clay <- ylab(expression(paste("Pred. clay", " [%]")))


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
list_pls_plot$pls_S <- list_pls_plot$pls_S +
  xlab_S + ylab_S
list_pls_plot$pls_P <- list_pls_plot$pls_P +
  xlab_P + ylab_P

list_pls_plot$pls_resin_P_log <- list_pls_plot$pls_resin_P_log +
  xlab_P_resin + ylab_P_resin
list_pls_plot$pls_Fe_DTPA_log <- list_pls_plot$pls_Fe_DTPA_log +
  xlab_Fe_DTPA + ylab_Fe_DTPA
list_pls_plot$pls_Zn_DTPA <- list_pls_plot$pls_Zn_DTPA +
  xlab_Zn_DTPA + ylab_Zn_DTPA
list_pls_plot$pls_Cu_DTPA <- list_pls_plot$pls_Cu_DTPA +
  xlab_Cu_DTPA + ylab_Cu_DTPA
list_pls_plot$pls_Mn_DTPA <- list_pls_plot$pls_Mn_DTPA +
  xlab_Mn_DTPA + ylab_Mn_DTPA

list_pls_plot$pls_sand <- list_pls_plot$pls_sand +
  xlab_sand + ylab_sand
list_pls_plot$pls_sand_cv <- list_pls_plot$pls_sand_cv +
  xlab_sand + ylab_sand
list_pls_plot$pls_sand_msc <- list_pls_plot$pls_sand_msc +
  xlab_sand + ylab_sand
list_pls_plot$pls_sand_msc_cv <- list_pls_plot$pls_sand_msc_cv +
  xlab_sand + ylab_sand
list_pls_plot$pls_silt <- list_pls_plot$pls_silt +
  xlab_silt + ylab_silt
list_pls_plot$pls_clay <- list_pls_plot$pls_clay +
  xlab_clay + ylab_clay
list_pls_plot$pls_clay_cv <- list_pls_plot$pls_clay_cv +
  xlab_clay + ylab_clay


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
models_texture <- c(
  "pls_sand", "pls_silt", "pls_clay", "pls_clay_cv"
)
models_sand <- c(
  "pls_sand", "pls_sand_cv", "pls_sand_msc", "pls_sand_msc_cv"
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
grob_list_texture <- lapply(
  list_pls_plot[models_texture], ggplotGrob
)
grob_list_sand <- lapply(
  list_pls_plot[models_sand], ggplotGrob
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

# Texture: Sand, silt and clay
pdf(file = "out/figs/models_texture.pdf",
  width = 10, height = 6.5)
gtable_arrange(ncol = 2, grobs = grob_list_texture
  #left = "Predicted",
  #bottom = "Observed"
)
dev.off()

# Different sand models (with and without multiplicative scatter correction;
# independent validation and cross-validation evaluation)
pdf(file = "out/figs/models_sand.pdf",
  width = 10, height = 6.5)
gtable_arrange(ncol = 2, grobs = grob_list_sand
  #left = "Predicted",
  #bottom = "Observed"
)
dev.off()


################################################################################
## Maps of sampled fields within the four pilot sites
################################################################################

## Read sampling metadata ======================================================
# Alternative file readers
library(readr)

# Read files and set column classes --------------------------------------------

# Site data from Léo
data_field_lo <- read_csv(file = "data/sampling/data_field_lo.csv",
  col_types = list(gps_long = col_double(), gps_lat = col_double()))
# Site data from Midebdo
data_field_mo <- read_csv(file = "data/sampling/data_field_mo.csv",
  col_types = list(gps_long = col_double(), gps_lat = col_double()))
# Site data from Soubré
data_field_sb <- read_csv(file = "data/sampling/data_field_sb.csv",
  col_types = list(gps_long = col_double(),
  gps_lat = col_double()))
# Site data from Tiéningboué
data_field_tb <- read_csv(file = "data/sampling/data_field_tb.csv",
  col_types = list(gps_long = col_double(), gps_lat = col_double()))

## Plot sampling distribution ==================================================
library(dplyr)

# Tiéningboué, Côte d'Ivoire  --------------------------------------------------
pdf(file = "out/figs/poster_sampling_tb.pdf", width = 4, height = 3)
# make pretty breaks
gps_long_brk <- pretty(data_field_tb$gps_long, n = 4)
gps_lat_brk <- pretty(data_field_tb$gps_lat, n = 4)
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string <- as_labeller(c(`tb` = "Tiéningboué, Côte d'Ivoire"))
# Plot graph of sampling distribution
p_sampling_tb <- dplyr::filter(data_field_tb, mat_type == "soil_cal") %>% 
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
p_sampling_tb <- p_sampling_tb + guides(colour = "none", shape = "none")
p_sampling_tb
dev.off()

# Lilyo, Côte d'Ivoire  --------------------------------------------------------
pdf(file = "out/figs/poster_sampling_sb.pdf", width = 4, height = 3)
# make pretty breaks
gps_long_brk <- pretty(data_field_sb$gps_long, n = 4)
gps_lat_brk <- pretty(data_field_sb$gps_lat, n = 4)
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string <- as_labeller(c(`sb` = "Liliyo, Côte d'Ivoire"))
# Plot graph of sampling distribution
p_sampling_sb <- dplyr::filter(data_field_sb, mat_type == "soil_cal") %>% 
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
  guides(colour = "none", shape = "none") +
  theme(legend.text = element_text(face = "italic"))
p_sampling_sb
dev.off()

# Léo, Burkina Faso ----------------------------------------------------
pdf(file = "out/figs/poster_sampling_lo.pdf", width = 4, height = 3)
# make pretty breaks
gps_long_brk <- pretty(data_field_lo$gps_long, n = 4)
gps_lat_brk <- pretty(data_field_lo$gps_lat, n = 4)
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string <- as_labeller(c(`lo` = "Léo, Burkina Faso"))
# Plot graph of sampling distribution
p_sampling_lo <- dplyr::filter(data_field_lo, mat_type == "soil_cal") %>% 
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
  guides(colour = "none", shape = "none") +
  theme(legend.text = element_text(face = "italic"))
p_sampling_lo
dev.off()

# Midebdo, Burkina Faso --------------------------------------------------------
pdf(file = "out/figs/poster_sampling_mo.pdf", width = 4, height = 3)
# make pretty breaks
gps_long_brk <- pretty(data_field_mo$gps_long, n = 4)
gps_lat_brk <- pretty(data_field_mo$gps_lat, n = 4)
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string <- as_labeller(c(`mo` = "Midebdo, Burkina Faso"))
# Plot graph of sampling distribution
p_sampling_mo <- dplyr::filter(data_field_mo, mat_type == "soil_cal") %>%
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
  guides(colour = "none", shape = "none") +
  theme(legend.text = element_text(face = "italic"))
p_sampling_mo
dev.off()

# Arrange sampling distribution graphs in four panels --------------------------

# Graph in 4 panels
p_sampling <- cowplot::plot_grid(
  p_sampling_sb, p_sampling_tb, p_sampling_mo, p_sampling_lo)

# Save as pdf
ggsave(filename = "out/figs/sampling_distribution.pdf",
  p_sampling, width = 5.5, height = 5.5)

# Save graph of legend
ggsave(file = "out/figs/sampling_legend.pdf", p_sampling_tb)

################################################################################
## Plots of spectra
################################################################################

library(simplerspec)
library(tidyverse)

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

# ETH Alpha files
soilspec_tbl_eth <- spc_list_eth %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")

## Create plots of soil spectra ================================================

# Source spectral plotting function
source(file = "R/plot-spc.R")

# Plot mean spectra
pdf(file = "out/figs/soilspec_mean.pdf", width = 6, height = 3)
plot_spc(soilspec_tbl_eth, y = "spc_mean", by = "sample_id")
dev.off()

# Plot preprocessed spectra
pdf(file = "out/figs/soilspec_pre.pdf", width = 6, height = 3)
plot_spc(soilspec_tbl_eth, y = "spc_pre", by = "sample_id") + 
  ylab("Absorbance SG 1st deriv. (w21)")
dev.off()

pdf(file = "out/figs/poster_soilspec.pdf", width = 6, height = 1.5)
# Function depreciated -> not working! -> use plot_spc instead
plot_spectra(spc = soilspec_plotting$MIR_mean, no_group = TRUE) +
  theme(legend.position = "none")
dev.off()

# Quality check of scans by showing all replicate spectra (scan_id)
pdf(file = "out/figs/spectra_scan_id_yamsys.pdf", width = 15, height = 5)
plot_spc(soilspec_tbl_eth, y = "spc", by = "scan_id")
dev.off()

