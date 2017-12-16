################################################################################
## Perform peak picking for raw spectra
################################################################################

library(ChemometricsWithR)
library(tidyverse)

spc_tbl <- readRDS(file = "out/files/spec_chem.Rds")
spc <- do.call(rbind, spc_tbl$spc)

# Extract first spectrum in matrix as vector
spc_1 <- spc[1, ]

plot(spc_1, type = "l")
abline(v = pick.peaks(spc_1, 10), col = "blue")
# dev.off()

# Find possible peaks
peaks <- pick.peaks(spc_1, 10)

# Filter spectral noise from peaks ---------------------------------------------

len <- length(peaks)
peaks <- peaks[- (len - c(2, 13, 14, 16:19, 22, 25, 26, 34, 37:57, 61:len))]
# Check visually
plot(spc_1, type = "l")
abline(v = peaks, col = "blue")

# Return wavenumbers of peaks --------------------------------------------------

print(peaks)
wn <- spc_tbl$wavenumbers[[1]]
wn_peaks <- wn[peaks]
round(wn_peaks, 0)


d <- data.frame(
  date = as.Date(c("1971-09-01", "1991-12-01", "1994-12-01", "2000-01-01",
    "2002-08-01", "2005-08-01")), 
  event = c("birth", "entered college", "BS", "entered grad school", "MS",
    "PhD"))
ggplot() +
  scale_x_date(limits = as.Date(c("1970-1-1", "2010-12-31"))) +
  scale_y_continuous(name = "", limits = c(0, 1)) +
  geom_vline(data = d, mapping=aes(xintercept = date), color = "blue") +
  geom_text(data = d, mapping = aes(x = date, y = 0, label = event), size = 4, 
    angle = 90, vjust = -0.4, hjust = 0)


