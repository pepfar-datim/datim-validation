getInvalidDataElements<-function(data,base.url,username,password,dataset){
  #Valid data set assignments against the dataset
  r<-GET(URLencode(paste0(base.url,"api/sqlViews/bkJ3PteNu7A/data.json?paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  des<-ldply(lapply(r$rows, function(x) t(unlist(x))))
  foo<-ldply(lapply(r$header, function(x) t(unlist(x))))[1]
  names(des)<-as.character(foo$name)
  des<-colwise(as.character)(des)
  #Datasets
  
  r<-GET(URLencode(paste0(base.url,"api/dataSets?filter=name:like:",dataset,"&fields=id,name")),
                    authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  ds<-ldply(lapply(r$dataSets, function(x) t(unlist(x))))
  ds<-colwise(as.character)(ds)
  
  des.this<-des[des$dataset %in% ds$name,]
  des.this$combi<-paste0(des.this$dataelementuid,".",des.this$categoryoptioncombouid)
  des.this<-colwise(as.character)(des.this)
  
  foo<-unique(data[,c("dataElement","categoryOptionCombo")])
  foo$combi<-paste0(foo$dataElement,".",foo$categoryOptionCombo)
  foo<-foo[!(foo$combi %in% des.this$combi ),]
  foo<-foo[complete.cases(foo),]
  #Get all data element names and uids
  foo$dataElementName<-mapvalues(as.character(foo$dataElement),as.character(des.this$dataelementuid),as.character(des.this$shortname),warn_missing=FALSE)
  foo$categoryOptionComboName<-mapvalues(foo$categoryOptionCombo,des.this$categoryoptioncombouid,as.character(des.this$categoryoptioncombo),warn_missing=FALSE)
  return(foo[,c("dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo")])
}