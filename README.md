#Introduction

This package has been developed to assist the PEPFAR Data Exchange Community
with validation of their data import payloads. Various checks have been 
developed to allow users to identify and validate
their import payloads prior to submission. In order to use this package,
you will need an active account on DATIM. Other pre-requisites are listed
in the DESCRIPTION file of this package.

# datim-validation
You will need at least version 4.1 of R installed. 

To get started, be sure you have installed the `devtools` package with 
```R
install.packages('devtools')
```

Once you have installed that, you can proceed to try and load the `datim-validation` package with

```R
library(devtools)
install_git('https://github.com/pepfar-datim/datim-validation')
```

If you prefer, you can download the source of the package with the vignettes and build these as well (recommended for new users).
```R
devtools::install_github('https://github.com/pepfar-datim/datim-validation',build_vignettes = TRUE)
```

This package uses `renv`, which allows for fine grained control of dependencies. 
While not required, it is reccomended that you use `renv` to ensure that you
versions of dependencies in your local enviornment are the same as those which
a particular version of `datimvalidation` has been tested against.  You can 
install `renv` with the following command. 

```R
install.packages("renv")
```

Once `renv` has been installed, you can load all of the depencies as defined
in the lock file with the following command. 
```R
renv::restore()
```

This command will restore and build a local copy of all dependencies in an 
isolated environment on your machine, and will not affect the global R
environment. 


Once the package has installed, you can load it with the following command:

```R
library(datimvalidation)
```

Consult the vignette in the package documentation on how to use the library. A very basic script is provided below as an example of how you can validate your data payload. 

```
require(datimvalidation)

#Adjust this to the location of your own credentials file. 
secrets <- "/home/littebobbytables/.secrets/datim.json"
loadSecrets(secrets)

#Adjust this to point to the location of your data payload. 
datafile <- "/home/littebobbytables/mydata.csv"
d <- d2Parser(filename = datafile ,type = "csv")

#You may need to adjust the datasets to match those which you are submitting data for. 
#If you get non-empty data frames here, then your data has issues. 

checkDataElementOrgunitValidity(data = d,datasets = c("zUoy5hk8r0q","KWRj80vEfHU"))
checkDataElementDisaggValidity(data=d,datasets = c("zUoy5hk8r0q","KWRj80vEfHU"))
checkValueTypeCompliance(d)
checkNegativeValues(d)
validateData(data = d, datasets = c("zUoy5hk8r0q","KWRj80vEfHU"),
 return_violations_only = TRUE)
```
