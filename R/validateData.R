#' @title Function which checks the of a DATIM data payload agaist define validation rules
#' 
#' @description validateData should be supplied a d2Parser compliant data frame.
#'The data frame is checked dynamically against validation rules defined in the DATIM server.
#' @param data d2Parser data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param organisationUnit UID of the Operating Unit
#' @param return_violations_only Paramater to return only violations or all validation rule evalualtions.
#' @return Returns a data frame with validation rule results.
#' @note
#' checData(foo,"https://www.datim.org","admin")
validateData<-function(data,base.url,username,password,organisationUnit,return_violations_only=TRUE) {
#Calculate the totals  
data.totals<-aggregate(value ~ dataElement + period + orgUnit + attributeOptionCombo, data = data,FUN=sum)
  
#Check the data against the validation rules
expression.pattern<-"[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
data$combi<-paste0(data$dataElement,".",data$categoryOptionCombo)
#Get a copy of the metadata from the server
r<-GET(paste0(base.url,"api/validationRules.xml?fields=id,name,description,leftSide[expression,missingValueStrategy],rightSide[expression,missingValueStrategy],operator&paging=false"),
       authenticate(username,password))
vr.xml<- content(r, "parsed", "application/xml")
vr.names<-sapply(getNodeSet(vr.xml,"//o:validationRule","o"),xmlGetAttr,"name")
vr.op<-sapply(getNodeSet(vr.xml,"//o:operator","o"),xmlValue)
vr.ls<-sapply(getNodeSet(vr.xml,"//o:validationRule/o:leftSide/o:expression","o"),xmlValue)
vr.rs<-sapply(getNodeSet(vr.xml,"//o:validationRule/o:rightSide/o:expression","o"),xmlValue)
vr.ls.strategy<-sapply(getNodeSet(vr.xml,"//o:validationRule/o:leftSide/o:missingValueStrategy","o"),xmlValue)
vr.rs.strategy<-sapply(getNodeSet(vr.xml,"//o:validationRule/o:rightSide/o:missingValueStrategy","o"),xmlValue)
vr<-data.frame(name=vr.names,ls=vr.ls,op=vr.op,rs=vr.rs,ls.strategy=vr.ls.strategy,rs.strategy=vr.rs.strategy)

#Static predefined map of operators
op.map<-data.frame(x=c("greater_than_or_equal_to","greater_than","equal_to","not_equal_to","less_than_or_equal_to","less_than"),
                   y=c(">=",">","==","!=","<=","<"),stringsAsFactors=F)
#Strategies
strat.map<-data.frame(x=c("SKIP_IF_ANY_VALUE_MISSING","SKIP_IF_ALL_VALUES_MISSING","NEVER_SKIP"))
#Remap the operators
vr$op<-mapvalues(vr$op,op.map$x,op.map$y,warn_missing=FALSE)
#Remove decorations
vr$ls<-gsub("[#{}]","",vr$ls)
vr$rs<-gsub("[#{}]","",vr$rs)
#Count the left and right side operators
vr$rs.ops<-stringr::str_count(vr$rs,expression.pattern)
vr$ls.ops<-stringr::str_count(vr$ls,expression.pattern)
#vr$rs.ops<-ifelse(vr$rs.ops==0,1,vr$rs.ops)
#vr$ls.ops<-ifelse(vr$ls.ops==0,1,vr$ls.ops)
#Loop over unique OU/PE/ACOC combos
loop_map<-unique(data[,c("period","orgUnit","attributeOptionCombo")])

#Write the intermediate results to a temp file
vr_results_tmp_file<-tempfile()

for(i in 1:nrow(loop_map)) {
  this.data<-data[data$period==loop_map$period[i] & 
                    data$orgUnit==loop_map$orgUnit[i] & 
                    data$attributeOptionCombo == loop_map$attributeOptionCombo[i],]
  
  this.data.totals<-data.totals[data.totals$period==loop_map$period[i] & 
                                  data.totals$orgUnit==loop_map$orgUnit[i] & 
                                  data.totals$attributeOptionCombo == loop_map$attributeOptionCombo[i],]
  this.des<-unique(this.data$dataElement)
  matches <-vr[ grepl(paste(this.des,collapse="|"), vr$ls) | grepl(paste(this.des,collapse="|"), vr$rs),]
  
  #Get the matching rules
  matches$ls<-mapvalues(matches$ls,this.data$combi,this.data$value,warn_missing=FALSE)
  matches$ls<-mapvalues(matches$ls,this.data.totals$dataElement,this.data.totals$value,warn_missing=FALSE)
  matches$ls.count<-stringr::str_count(matches$ls,expression.pattern)
  
  matches$rs<-mapvalues(matches$rs,this.data$combi,this.data$value,warn_missing=FALSE)
  matches$rs<-mapvalues(matches$rs,this.data.totals$dataElement,this.data.totals$value,warn_missing=FALSE)
  matches$rs.count<-stringr::str_count(matches$rs,expression.pattern)
  #Remove rules which should not be evaluated
  foo<-!(matches$ls.strategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$ls.ops != matches$ls.count)) | 
    !(matches$rs.strategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$rs.ops != matches$rs.count)) | 
    matches$rs.strategy == "NEVER_SKIP" |
    matches$ls.strategy == "NEVER_SKIP"
  matches<-matches[foo,]
  
  matches$ls<-gsub(expression.pattern,"0",matches$ls) 
  matches$rs<-gsub(expression.pattern,"0",matches$rs)
  matches$ls<-sapply(matches$ls,function(x) {eval(parse(text=x))})
  matches$rs<-sapply(matches$rs,function(x) {eval(parse(text=x))})
  matches$formula<-paste(matches$ls,matches$op,matches$rs) 
  matches$result<-sapply(matches$formula,function(x) {eval(parse(text=x))})
  matches<-merge(matches,loop_map[i,],all=T)
  write.table(matches,file=vr_results_tmp_file,append=(i!=1),sep=",",col.names=(i==1),row.names=F) }

#Read the temp file
validation.results<-read.csv(vr_results_tmp_file)
#Remap the OUs
r<-GET(URLencode(paste0(base.url,"api/organisationUnits/",organisationUnit,".json?includeDescendants=true&filter=level:ge:3&fields=id,name&paging=false")),
       authenticate(username,password))
r<- content(r, "parsed", "application/json")
sites<-ldply(lapply(r$organisationUnits, function(x) t(unlist(x))))
names(sites)<-c("id","name")
sites<-colwise(as.character)(sites)
validation.results$orgUnit<-mapvalues(validation.results$orgUnit,sites$id,sites$name,warn_missing=FALSE)
#Remap the mechanisms
r<-GET(URLencode(paste0(base.url,"/api/categoryOptions?filter=organisationUnits.id:eq:",organisationUnit,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
       authenticate(username,password))
r<- content(r, "parsed", "application/json")
mechs<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
mechs<-colwise(as.character)(mechs)
validation.results$attributeOptionCombo<-mapvalues(validation.results$attributeOptionCombo,mechs$categoryOptionCombos.id,mechs$code)

if (return_violations_only) {
  
validation.results[!validation.results$result,] 

} else { 
validation.results 

}
}
