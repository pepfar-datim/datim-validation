remapOUs<-function(ous,base.url,username,password,mode="code") {
  is_valid_mode<-mode %in% c("code","name","shortName")
  if ( is_valid_mode == FALSE )  {break}
  r<-GET(URLencode(paste0(base.url,"api/organisationUnits/",ou,".json?includeDescendants=true&filter=level:ge:3&fields=id,code,name,shortName&paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  sites<-ldply(lapply(r$organisationUnits, function(x) t(unlist(x))))
  sites<-colwise(as.character)(sites)
  cmd<-paste0("mapvalues(ous,sites$",mode,",sites$id,warn_missing = FALSE)")
  eval(parse(text=cmd)) }