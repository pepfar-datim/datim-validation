checkValidMechanisms <-
function(data,base.url,username,passowrd,organisationUnit) {
r<-GET(URLencode(paste0(base.url,"api/categoryOptions?filter=organisationUnits.id:eq:",organisationUnit,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
       authenticate(username,password))
r<- content(r, "parsed", "application/json")
mechs.valid<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
mechs.valid<-colwise(as.character)(mechs.valid)
mechs<-unique(data$attributeOptionCombo)
mechs[!(mechs %in% mechs.valid$categoryOptionCombos.id)] }
