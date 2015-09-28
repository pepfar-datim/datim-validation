d2Parser<-function(filename,type,base.url,username,password,organisationUnit,dataElementIdScheme="code",orgUnitIdScheme="code",idScheme="code") {
  valid_type <- type %in% c("xml","json","csv")
if (!valid_type) { print("ERROR:Not a valid file type"); stop()} 

header<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value","storedby","lastUpdated","comment")

if ( type == "xml") {
  d<-xmlTreeParse(filename,useInternalNode=TRUE)
  data<-data.frame(t( sapply(xmlRoot(d) [ "dataValue" ], xmlAttrs) ) )
  #Get all the attributes specified in the 
  data.attrs<-xmlAttrs(xmlRoot(d))
  #Period
  if (!is.na(data.attrs["period"])) { data$period<-data.attrs["period"] }
  if (!is.na(data.attrs["orgUnit"])) { data$orgUnit<-data.attrs["orgUnit"] }
  if (!is.na(data.attrs["attributeOptionCombo"])) { data$attributeOptionCombo<-data.attrs["attributeOptionCombo"] }
  }

if ( type == "csv") { data<-read.csv(filename,) }

if (type == "json") {
  
  j<-fromJSON(file=filename)
  data<-ldply(lapply(j$dataValues, function(x) t(unlist(x))))
  if (!is.null(j[["period"]])) { data$period<-j$period }
  if (!is.null(j[["orgUnit"]])) { data$orgUnit<-j$orgUnit }
  if (!is.null(j[["attributeOptionCombo"]])) { data$attributeOptionCombo<-j$attributeOptionComboid }
}

data<-data[,header[ header %in% names(data)]]
data$value<-as.numeric(data$value)
data$orgUnit<-remapOUs(data$orgUnit,base.url,username,password,mode=orgUnitIdScheme,organisationUnit)
data$dataElement<-remapDEs(data$dataElement,base.url,username,password,mode=dataElementIdScheme)
data$attributeOptionCombo<-remapMechs(data$attributeOptionCombo,base.url=base.url,username=username,password=password,mode=idScheme,ou=organisationUnit)

return(data)
}
