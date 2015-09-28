getInvalidDataElements<-function(data,base.url,username,password){
  #Valid data set assignments against the dataset
  r<-GET(URLencode(paste0(base.url,"api/sqlViews/bkJ3PteNu7A/data.json?paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  des<-ldply(lapply(r$rows, function(x) t(unlist(x))))
  foo<-ldply(lapply(r$header, function(x) t(unlist(x))))[1]
  names(des)<-as.character(foo$name)
  des.this<-des[grepl(ds,des$dataset),]
  des.this$combi<-paste0(des.this$dataelementuid,".",des.this$categoryoptioncombouid)
  des.this<-colwise(as.character)(des.this)
  
  foo<-unique(data[,c("dataElement","categoryOptionCombo")])
  foo$combi<-paste0(foo$dataElement,".",foo$categoryOptionCombo)
  foo<-foo[!(foo$combi %in% des.this$combi ),]
  foo<-foo[complete.cases(foo),]
  foo$dataElementName<-mapvalues(foo$dataElement,des.this$dataelementuid,as.character(des.this$shortname),warn_missing=FALSE)
  foo$categoryOptionComboName<-mapvalues(foo$categoryOptionCombo,des.this$categoryoptioncombouid,as.character(des.this$categoryoptioncombo),warn_missing=FALSE)
  return(foo)
}