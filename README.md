[![Build Status](https://travis-ci.org/jason-p-pickering/datim-validation.svg?branch=master)](https://travis-ci.org/jason-p-pickering/datim-validation)
[![codecov.io](https://codecov.io/github/jason-p-pickering/datim-validation/coverage.svg?branch=master)](https://codecov.io/github/jason-p-pickering/datim-validation?branch=master)

# datim-validation
Datim validation scripts

To get started, be sure you have installed the `devtools` package with 
```R
install.packages('devtools')
```

Once you have installed that, you can proceed to try and load the `datim-validation` package with

```R
library(devtools)
install_github("jason-p-pickering/datim-validation")
```

If you prefer, you can download the source of the package with the vignettes and build these as well (reccomended for new users).
```R
devtools::install_github('jason-p-pickering/datim-validation',build_vignettes = TRUE)
```


Once the package has installed, you can load it with the following command:

```R
library(datimvalidation)
```

Consult the vignette in the package documentation on how to use the library. A very basic script is provided below as an example of how you can validate your data payload. 

```
require(datimvalidation)

#Adjust this to the location of your own credentials file. 
secrets<-"/home/littebobbytables/.secrets/datim.json"
loadSecrets(secrets)

#Adjust this to point to the location of your data payload. 
datafile<-"/home/littebobbytables/mydata.csv"
d<-d2Parser(filename = datafile ,type = "csv")

#You may need to adjust the datasets to match those which you are submitting data for. 
#If you get non-empty data frames here, then your data has issues. 

checkDataElementOrgunitValidity(data = d,datasets = c("zUoy5hk8r0q","KWRj80vEfHU"))
checkDataElementDisaggValidity(data=d,datasets = c("zUoy5hk8r0q","KWRj80vEfHU"))
checkValueTypeCompliance(d)
checkNegativeValues(d)
validateData(data = d, datasets = c("zUoy5hk8r0q","KWRj80vEfHU"),
 return_violations_only = TRUE)
```
