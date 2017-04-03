## Source script with VIP() function
source("R/vip.R")

## Read calibration models =====================================================

# PLSR model for total C
pls_C <- readRDS("models/pls_C.Rds")

## Compute VIP for total C model ===============================================

vip_pls_C <- VIP(pls_C$pls_model$finalModel)

# p_VIPs_soilc <- ggplot() +
#   geom_line(aes(x = as.numeric(colnames(MIRdata_CNS_soil_all$MIR)),
#     y = VIPs_C[pls_soil_C_all$fit_pls$finalModel$bestIter[,1], ],
#     colour = "total C"))

# Wavenumbers from spectra tibble
wn <- as.numeric(colnames(spec_chem$spc_pre[[1]]))

ggplot() +
  geom_line(aes(x = wn,
    y = vip_pls_C[pls_C$pls_model$finalModel$bestIter[,1], ],
    colur = "total C")) +
  scale_x_reverse() + 
  geom_hline(yintercept = 1, colour = "red")
  



