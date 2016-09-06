# R Scripts for Mid-Infrared (MIR) spectroscopy for YAMSYS

The code in this repository is split up in two scripts that perform two categories
of tasks:

* `01_model_development_yamsys.R `: Develop spectral models and save them into
`.Rds` files: 
  Read spectra into R; average, remove outliers, pre-process spectra; join chemical data
  and spectra; perform PLS regression modeling
* `02_model_prediction_yamsys.R `: Read models developed in the first script and
predict soil properties from calibrated models and new spectra

# Important notes

It is recommended to work with projects in RStudio. This means you 
don't have to specifiy the working directory (`set.wd()`) and data and R code can easily
be transferred between collaborators. See [here](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) for 
instructions how to set up projects in RStudio.

At the moment only R scripts are provided. Raw data sets 
(spectra and chemical data) can be requested by sending me an [email](mailto:philipp.baumann@usys.ethz.ch). I plan to 
first publish a soil spectroscopy calibration publication from the YAMSYS project 
that will be based on parts of the results of my master thesis. 
In parallel, the full data and R code will be available on a data repository.

You should create the following folders in the directory where your R project
is located:

* `data ` : Soil chemical and spectral data
  * `soilchem `: soil chemistry data for calibration and validation as text files
  * `spectra ` raw spectra. Exported text files for spectra can be put into a subfolder, e.g. `alpha_txt`
* `models `:  models as `.Rds` files. Models don't have to be re-built and just can be loaded into memory when predictions are needed
* `out ` output generated from R code
  * `figs ` : figures from plots etc.
  * `data ` : additional data output
* `predictions` : data sets from predictions using models and new spectra
* `R `: additional R scripts that have auxiliary R functions not yet shipped in the simplerspec package

You should make sure that you always have a clean memory when you re-open your project.
Models should always be loaded from R objects that are saved in the directory `models` (`.Rds`files)
In RStudio go to *Preferences* -> *General*, then disable option "Restore .RData into workspace at startup";
also select *Never* for "Save workspace to .RData on exit:".
This avoids that you have old R objects in your memory and makes sure you have a clear
workspace when you restart the R session. This forces you to always write your 
R commands in your scripts and not only manually entering commands in the console.