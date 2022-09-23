# Introduction
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

This package uses `renv`, which allows for fine grained control of dependencies.  While not required, it is reccomended that you use `renv` to ensure that you
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


A very basic script is provided below as an example of how you can validate your data payload. You should consult the `datimutils` package documentation for details regarding usage of the `loginToDATIM` function. Also consult this packages function documentation for more information on each validation function.

```
require(datimvalidation)
require(datimutils)
require(magrittr)
datim_config <- "/home/littebobbytables/.secrets/datim.json"
loginToDATIM(config)


#Adjust this to point to the location of your exchange file.
datafile <- "/home/littebobbytables/mydata.csv"
d <- d2Parser(filename = datafile, type = "csv", datastream = "RESULTS") 
#Run all reccomended validations. Note, this process may take several minutes.
d <- runValidation(d)

```

You can check any messages which may have been generated
during the parsing and validation of your data.

```R
print(d$info$messages)
```

Any messages with a level of "ERROR" generally indicate problems which must be
fixed prior to import. Messages with a level of "WARNING" may prevent your data 
from being imported, and should be carefully reviewd. Messages with a level of "INFO" are provided for your information only.


If you export your data to CSV file with the following command.

```R
write.table(d$data$import, 
file="my_export_file.csv",
quote=TRUE,sep=",", na="")
```

Note that this file will will only contain data which has been 
deemed to be valid by the parser. For instance, duplicate and blank 
rows from the original file will be removed. Also, all identifiers
will be converted to IDs.


