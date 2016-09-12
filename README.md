# R Scripts for Mid-Infrared (MIR) spectroscopy for YAMSYS

The code in this repository performs two categories
of modeling tasks:

* `01_model_development_yamsys.R `: Calibrate and test models and save them into
`.Rds` files: 
  Read spectra into R
  1. average
  2. remove outliers
  3. pre-process spectra
  4. join chemical data and spectra$
  5. perform PLS regression modeling
* `02_model_prediction_yamsys.R `: Read models developed in the first script and
predict unkown soil and plant properties from new spectra based on the models calibrated

The functions rely on the simplerspec package that is available on GitHub.
Package sources and instructions how to install the package can be found [here](https://github.com/philipp-baumann/simplerspec). 
The package is an interface that uses pre-existing packges for spectral data handling and modeling, and other packages such as the famous dplyr package.

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

* `data `: soil or plant chemical and spectral data
  * `metadata `: metadata from field sampling in a single text (`.csv`) file
  * `soilchem `: soil chemistry data for calibration and validation as text files
  * `plantchem `: plant chemistry data for calibration and validation as text files
  * `spectra `: raw spectra. Exported text files for spectra can be put into a subfolder, e.g. `alpha_txt`
* `models `:  models as `.Rds` files. Models don't have to be re-built and just can be loaded into memory when predictions are needed
* `out ` outputs generated from R code
  * `figs `: figures from plots, etc.
  * `data `: additional data outputs
* `predictions`: data sets from predictions using models and new spectra
* `R `: additional user-defined R scripts, e.g. for plotting
You should make sure that you always have a clean memory when you re-open your project.
Models should always be loaded from R objects that are saved in the folder `models` (`.Rds`files).
In RStudio go to *Preferences* -> *General*, then disable option "Restore .RData into workspace at startup";
also select *Never* for "Save workspace to .RData on exit:".
This avoids that you have old R objects in your memory and makes sure you have a clear
workspace when you restart the R session. This forces you to always write your 
R commands in your scripts and not only manually entering commands in the console.

## Metadata management and file naming

To maintain reproducible data analysis and modeling is crucial to keep spectra, chemical data, and sampling metadata (data on where and how samples were collected) well organized. All data usually stored within different 
files should be tidy data sets:

1. Each variable forms a column.
2. Each observation forms a row.
3. Each type of observational unit forms a table. Chemical data metadata are stored
in two different tables.

Please consider the [following paper](http://vita.had.co.nz/papers/tidy-data.html) of Hadley Wickham to understand the concept of *tidy data*. Detailed example R code to make data tidy using the [dplyr](https://github.com/hadley/dplyr) package can be found
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

* `sample_id `: Unique identifier; The sample id should be meaningful and should contain information about the field_id, the country, and site.
* `field_id ` 
* `country_name `: full country name where samples were collected
* `country_code `: abbreviation of country name
* `site_name `: name of administrative region where field are located
* `site_code `: abbreviation of the administrative region where field are located
* `institute_name `: name of the research university/institute
* `farmer_name ` : name of the farmer
* `material `: e.g. "plant" or "soil"
* `species `: scientific species name for plants
* `variety `: variety names for plants
* `material_type `: e.g. for plants "leaf", "vine", "tuber", "root", "grain", or "aboveground_biomass"
* `land_use `: land use type
* `sampling_date `: date of sampling. Use only number format (integer), %yyyymmdd%, eg. 20150704.
* `latitude `: coordinates in decimal degreee
* `longitude `: coordinates in decimal degree
* `altitude  `: altitude in m above sea level
* any additional variables noted in the field, eg. development stage or 
information on planting systems.


File names of spectra should be exactly identical to the *sample_id* and should not
use space. use underscores (`_`) instead of spaces.
analysis and modeling.

Create an additional file called `metadata_soil_<name_of_project>` or
`metadata_plant_<name_of_project>` where you specifify the units and methology details for the measured soil and plant properties. Use identical column names as in the chemical reference
data file. Add also details on your study as the research questions, experimental design, and
other remarks.