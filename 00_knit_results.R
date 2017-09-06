# Include knitr results in bigger LaTex files
# https://tex.stackexchange.com/questions/342360/embedding-knitr-tex-files-on-a-bigger-latex-file
library(knitr)
library(xtable)
knit("00_manuscript_yamsys_soilspec_results.Rnw")
