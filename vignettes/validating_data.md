---
title: "Validation of data payloads for DATIM"
author: "Jason P. Pickering"
date: "2018-12-17"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Validation of data payloads for DATIM}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


##Using the datimvalidation package

DATIM is based on the [DHIS2](https://dhis2.org/) software, and is therefore capable of importing different types of data, including
CSV, JSON, XML as well as ADX formats. Users wishing to use the data import capabilities of DATIM, should therefore familiarize themselves with 
the various formats which DHIS2 supports and the syntax of each of these. The DHIS2 development team has provided extensive technical
documetnation on the various formats [here](http://dhis2.github.io/dhis2-docs/master/en/developer/html/webapi_data_values.html). 

In addition to the general DHIS2 documentation, a series of DATIM specific resources is available [here](https://datim.zendesk.com/hc/en-us/sections/200413199-Data-Import-and-Exchange). These guides provide more detailed information on the data import process into DATIM, including code listings for indicators and disaggregations. 

DATIM has strict controls on data imports, including the requirement to adhere to the numerous validation rules of the system. To help data importers to prepare their files for submission to DATIM, this R package has been created to help validate the data prior to importing it into the system. This pacakge will help you to load your data payload and validate it against the business logic of DATIM, prior to  attempting to import it into the system. This approach is useful, as it will allow you to interactively correct mistakes in the payload prior to submission of the data for import. 

The `datavalidation` library provides an abstraction layer to the various validation routines which are necessary to import data into DATIM. These scripts, to a large extent, emulate the logic of the server. Basic functions are exposed to allow
you to determine if your data contains invalid data elements, incorrect disaggregations, and inactive mechanisms. Each function is documented in seperate sections of the package. 


##Getting started

In order to get started, you will need to have installed the R package "datimvalidation". The source code for this library can be found [here](https://github.com/jason-p-pickering/datim-validation). 


You will need an active internet connection and an active DATIM user name in order to utilize the `datimvalidation` package. Meta-data will be retrieved from the DATIM server, using your username and password , and stored in a local cache. Once the objects are cached, the package can be used off-line, until the cache is invalidated. By default, cached objects are stored for a week and then invalidated. 

To get started, you should create a "secrets" file, which will contain the authentication information required to access DATIM. You should keep this in a secure place on your computer, as it will require storing the username and password you access DATIM as a file on your disk. If you are unable to securely store this file, you can also enter your username and password through a dialog. A secrets file should a single JSON file which looks like this: 

```json
{
  "dhis": {
    "baseurl": "http://dev-de.datim.org",
    "username": "admin",
    "password": "district"
  }
}
```

To get started, we will load the package and provide a location of our secrets file


```r
require(datimvalidation)
secrets <- "/home/littebobbytables/.secrets/datim.json"
loadSecrets(secrets)
```


##Parsing your data

Next, we will use the `d2Parser` function, which is a general purpose function to load the different formats of data which 
DATIM accepts and to standardize this so that we can use other validation functions on the data. 

In addition to your username and password, you will need to specific the `type` of the file. This should be one of 'json','csv',or 'xml'. 
You will also need to know the identification scheme of the file. The easiest way is to open the file in a text editor, and see what id scheme
the payload is using. This could be 'code','id' or 'shortName'. Consult the specifc function documentation for more information on id schemes.


##Mechanism validation

Depending on the size of the datasets and the objects which need to be recoded, this operation could take serveral minutes. 


```r
d <- d2Parser(file=filename,type = "csv",
            dataElementIdScheme = "code",orgUnitIdScheme = "id",
            idScheme = "id" 
            ,invalidData = TRUE )
```

Now, we have loaded our data payload into the object `d`, we can proceed to validate it. The `getInvalidMechanisms` function will check that all mechanisms present in the payload are currently active in DATIM. You must supply the period for which you want to validate the mechanisms over, for instance "2016Oct", which would correspond to the period October 2016-September 2017. If any data is not contained within that period, or the mechanism is not active for that period, it will be returned as an invalid mechanism. 

When uploading data to DATIM, any data which has an invalid mechanism will be excluded from the upload.


```r
#Check for valid mechanisms and simply print those which are not valid
getInvalidMechanisms(d)
```
##Data element/period validation

Certain MER data elements are collected only in certain quarters. Quarterly data elements are collected each quarter, SAPR data elements are collected in quarters 2 and 4, and APR data elements are collected only in quarter 4. 
In order to ensure that no data values are submitted for the wrong periods, you can check the "cadence" of the data elements to be sure that only the correct data elements are submitted for a particular time period. 


```r
#Check for valid data element-period combinations and  print those which are not valid
checkDataElementCadence(d)
```


##Data element  and disaggregation validation

Depending on the type of payload you are submitting, there may be multiple data sets which are bundled together. There are strict controls in DATIM regarding the submission of data elements to given organisaion units. For instance, community data should not be submitted for facility sites, and vice versa. It can be difficult to determine for a given data payload whether certain data should be submitted for a given 

Next, we can check the indicators and disaggregations for validity against the current dataset form definitions. A data frame of indicator/disagg combinations will be returned. The 'dataset' paramater should be either the exact name of the dataset, or a grouping name like "MER Results" of 



```r
getInvalidDataElements(d)
```

##Value type validation

DATIM has strict controls on the type of data which can be imported into each data element. For instance, it is not possible to import a data value specified as "11/02/2015" as a date, because DATIM expects the date to be specified in "YYYY-MM-DD" format. In this case, the data would need to be transmitted as "2015-11-02". Using the `checkValueTypeCompliance` function, it is possible to validate a given data payload against the value types which have been specified for each data element. As an example, we can simply invoked the `checkValueTypeCompliance`, with our parsed data (`d`), along with the URL, username and password. Only data which does not 
meet the specified value type will be returned.


```r
#Check for indicator / disagg combinations and save the result as a CSV file
checkValueTypeCompliance(d)
```

##Validation rule validation

We can perform a check of the validation rules as defined in DATIM on the data payload.

The `validateData` function is capable of performing the validation operation in parallel. To activate this, you must inform R of the number of parallel
sessions which may be spawned. Usually, taking the number of cores on your system is a good starting point.You can modify the following command based on the number of system cores you have access to. Consult the `plyr` documentation for specifics on parallel operations. 

For users of the `datimvalidation` package on Linux, you may be able to decrease the amount of time it takes to perform the validation, which can be a very lengthy process, depending on the amount of data and speed of your machine. 

Use the following command to inform that you would like to spawn four parallel processes. This operation is only supported on the Linux operating system. 


```r
doMC::registerDoMC(cores=4) # or however many cores you have access to
parallel=TRUE
```


In order to run the validation rule check, simply invoke the following command. 


```r
#Run the validation rule and save the output as a CSV file
vr_violations <- validateData(data = d,
                            return_violations_only = TRUE,
                            parallel =parallel )
```

A list of validation rule violations will be returned to the `vr_violations`object.
