checkValidMechanisms <-
function(mechs,base.url,ou,username,passowrd) {
r<-GET(URLencode(paste0(base.url,"api/categoryOptions?filter=organisationUnits.id:eq:",ou,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
       authenticate(username,password))
r<- content(r, "parsed", "application/json")
mechs.valid<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
mechs.valid<-colwise(as.character)(mechs.valid)
mechs<-unique(mechs)
mechs[!(mechs %in% mechs.valid$categoryOptionCombos.id)] }
