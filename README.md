# R Scripts for Mid-Infrared (MIR) spectroscopy for YAMSYS

The code in this repository is split up in two scripts that perform two categories
of tasks:

* `01_model_development_yamsys.R `: Develop spectral models and save them into
`.Rds` files: 
  Read spectra into R; average, remove outliers, pre-process spectra; join chemical data
  and spectra; perform PLS regression modeling
* `02_model_prediction_yamsys.R `: Read models developed in the first script and
predict soil properties from calibrated models and new spectra

The functions rely on the simplerspec package that is available on GitHub.
Package sources and instructions how to install the package can be found [here](https://github.com/philipp-baumann/simplerspec). 
The package is an interface that uses pre-existing for spectral data handling and modeling
packages, and various other packages such as the famous dplyr package.

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

* `data `: Soil chemical and spectral data
  * `soilchem `: soil chemistry data for calibration and validation as text files
  * `spectra `: raw spectra. Exported text files for spectra can be put into a subfolder, e.g. `alpha_txt`
* `models `:  models as `.Rds` files. Models don't have to be re-built and just can be loaded into memory when predictions are needed
* `out ` output generated from R code
  * `figs `: figures from plots etc.
  * `data `: additional data output
* `predictions`: data sets from predictions using models and new spectra
* `R `: additional R scripts that have auxiliary R functions not yet shipped in the simplerspec package

You should make sure that you always have a clean memory when you re-open your project.
Models should always be loaded from R objects that are saved in the directory `models` (`.Rds`files).
In RStudio go to *Preferences* -> *General*, then disable option "Restore .RData into workspace at startup";
also select *Never* for "Save workspace to .RData on exit:".
This avoids that you have old R objects in your memory and makes sure you have a clear
workspace when you restart the R session. This forces you to always write your 
R commands in your scripts and not only manually entering commands in the console.

# Metadata management and file naming

To maintain reproducible data analysis and modeling is crucial to keep spectra, chemical data, and sampling metadata (data on where and how samples where collected) well organized. All data usually stored within different 
files should be tidy data sets:

1. Each variable forms a column.
2. Each observation forms a row.
3. Each type of observational unit forms a table.

Please consider the [following paper](http://vita.had.co.nz/papers/tidy-data.html) of Hadley Wickham to understand the concept of *tidy data*. Detailed example R code to make data tidy using the dplyr package can be found
[here](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html).

Chemical data and sampling metadata should be saved in different tables. Both data
sets should contain the column *sample_id*. The *sample_id* names have to be unique, meaning
that only one sample row is allowed per *sample_id*. The *sample_id* allows to link
different tables (usualy data frames in R) during the data analysis and modeling.
You can use excel to store metadata and data. However, export files as `.csv` text files.
Use comma (`,`) as column separator. If you use Microsoft Excel make sure you don't use
semicolon (`;`). To check open the file in a text editor. If yes, you can also replace all 
semicolons by commas.

Sampling metadata should at least contain the following columns:

* `sample_id `: unique identifier; The sample id should be meaningful and should contain information about the field_id, the country, and site.
* `field_id `
* `country `
* `site `: name or abbreviation of the region where field are located
* `farmer_name`
* `material`: e.g. "plant" or "soil"
* `date` : Use only number format (integer), %yyyymmdd%, eg. 20150704.
* `latitude`
* `longitude`
* `altitude`
* `coordinate_system `: reference geographic coordinate system

Optionally, you can also add more columns such as:

* `species `: species names
* `variety `: variety names
* `material_type `: e.g. for plants "leaf", "vine", or "tuber"
* any additional variables noted in the field, eg. development stage or 
information on planting systems.


File names of spectra should be exactly identical to the *sample_id* and should not
use space. use underscores (`_`) instead of spaces.
analysis and modeling.