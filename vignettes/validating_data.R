## ----eval=FALSE---------------------------------------------------------------
#  options(scipen=999) #Turn of any scientific notation
#  require(datimutils)
#  require(datimvalidation)
#  loginToDATIM(config_path = "/home/littebobbytables/.secrets/datim.json")
#  options("maxCacheAge"="7 days")

## ----eval=FALSE---------------------------------------------------------------
#  d <- d2Parser(file=filename,type = "csv",
#              dataElementIdScheme = "code",orgUnitIdScheme = "id",
#              idScheme = "id"
#              ,invalidData = TRUE )

## ----eval=FALSE---------------------------------------------------------------
#  #Check for valid mechanisms and simply print those which are not valid
#  checkMechanismValidity(d)

## ----eval=FALSE---------------------------------------------------------------
#  checkDataElementDisaggValidity(d)

## ----eval=FALSE---------------------------------------------------------------
#  #Check for indicator / disagg combinations and save the result as a CSV file
#  checkValueTypeCompliance(d)

## ----eval=FALSE---------------------------------------------------------------
#  doMC::registerDoMC(cores=4) # or however many cores you have access to
#  parallel=TRUE

## ----eval=FALSE---------------------------------------------------------------
#  #Run the validation rule and save the output as a CSV file
#  vr_violations <- validateData(data = d,
#                              return_violations_only = TRUE,
#                              parallel =parallel )
#  

