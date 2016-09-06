# R Scripts for Mid-Infrared (MIR) spectroscopy for YAMSYS

The code in this repository is split up in two files that perform two categories
of tasks:

* `01_model_development_yamsys.R `: Develop spectral models and save them into
`.Rds` files: 
  Read spectra into R; average, remove outliers, pre-process spectra; join chemical data
  and spectra; perform PLS regression modeling
* `02_model_prediction_yamsys.R `: Read models developed in the first script and
predict soil properties from calibrated models and new spectra
