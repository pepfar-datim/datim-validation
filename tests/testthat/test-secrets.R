#Secrets does not exist.
expect_error(loadSecrets("secrets2.json"))
httrmock::start_replaying()

context("Load secrets")
expect_silent(loadSecrets("secrets.json"))
expect_equal(getOption("organisationUnit"),"ybg3MO3hcf4")
expect_equal(getOption("baseurl"),"https://www.datim.org/")
url<-URLencode( URL = paste0(getOption("baseurl"),"api/me") )
r<-httr::GET(url ,httr::timeout(60))
expect_equal(httr::content(r)$id, "Uhsev5495GW")

context("Load datasets")
ds<-getDataSets()
#Should be a data frame
expect_is(ds,"data.frame")
#Should have these columns
expect_named(ds,c("name","id","formType"))
#Should have a length greater than one
expect_true(length(ds) > 0)

context("Load element map")
des<-getDataElementMap()
expect_is(des,"data.frame")
#Should have these columns
expect_named(des,c("code","name","id","shortName","optionSet.id"))
#Should have a length greater than one
expect_true(length(des) > 0)


context("Loading organisation units")
ous<-getOrganisationUnitMap()
expect_is(ous,"data.frame")
#Should have these columns
expect_named(ous,c("code","name","id","shortName"))
#Should have a length greater than one
expect_true(length(ous) > 0)

#Period Info
p<-getPeriodInfo("2016Q3")
expect_is(p,"data.frame")
expect_named(p,c("periodid","iso","startdate","enddate","periodtype"))
expect_true(p$startdate == "2016-07-01")
expect_true(p$iso == "2016Q3")
expect_true(p$enddate == "2016-09-30")
expect_true(p$periodtype == "Quarterly")

expect_true(p$startdate == "2016-07-01")
expect_true(p$iso == "2016Q3")
expect_true(p$enddate == "2016-09-30")
expect_true(p$periodtype == "Quarterly")

expect_true(nrow(getPeriodInfo("2016Q5")) == 0)

httrmock::stop_replaying()
