require(datimvalidation)

loadSecrets("/home/jason/.secrets/datim-prod.json")
clearCache(force=TRUE)
d_raw<-read.csv("/home/jason/development/Namibia2019Q1_v2.csv")
d_exlcude<-d_raw$orgunit_internal_id %in% c("MJH8UxLWyAG","HojSEG9tdsa")
d_raw<-d_raw[!d_exlcude,]
write.table(d_raw,file="/home/jason/development/Namibia2019Q1_v3.csv",
            row.names = FALSE,col.names = TRUE,sep=",",quote=TRUE)
d<-d2Parser(filename = "/home/jason/development/Namibia2019Q1_v3.csv",
            type = "csv",
            dataElementIdScheme = "name",idScheme = "code")
ds<-getCurrentMERDataSets(type="RESULTS")
checkDataElementOrgunitValidity(data = d,organisationUnit = "XOivy2uDpMF",
                                datasets = ds)
checkDataElementDisaggValidity(d,datasets = ds)
checkValueTypeCompliance(d)
checkNegativeValues(d)
doMC::registerDoMC(cores=4) # or however many cores you have access to
parallel=TRUE
vr_violations<-validateData(data = d,
return_violations_only = FALSE,
parallel =parallel,
datasets = ds)
