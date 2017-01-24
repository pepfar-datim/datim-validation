# datim-validation
Datim validation scripts

This package will help you to validate your data imports into DATIM. It consists of a number of checks, which seek to replicate the various restrictions placed on data import. 

This package does not consist of a single script, but rather a number of functions which each perform a particular type of validation, such as ensuring that mechanisms which are being submitted are valid with respect to time period which they are being submitted for. 

For further details of how to get started, please consult the vignette which is part of this package. 

To get started, be sure you have installed the `devtools` package with the following `R` command:  
```R
install.packages('devtools')
```

Once you have installed that, you can proceed to try and load the `data-validation` package with

```R
library(devtools)
install_github("jason-p-pickering/datim-validation")
```

If you prefer, you can download the source of the package with the vignettes and build these as well. 
```R
devtools::install_github('jason-p-pickering/datim-validation',build_vignettes = TRUE)
```


Once the package has installed, you can load it with 

```R
library(datimvalidation)
```

Consult the vignette in the package documentation on how to use the library.
