remapMechs<-function(mechs_in,base.url,username,password,mode,ou) {
  is_valid_mode<-mode %in% c("code","name")
  if ( is_valid_mode == FALSE )  {break}
  r<-GET(URLencode(paste0(base.url,"/api/categoryOptions?filter=organisationUnits.id:eq:",ou,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  mechs<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
  mechs<-colwise(as.character)(mechs)
  cmd<-paste0("mapvalues(mechs_in,mechs$",mode,",mechs$categoryOptionCombos.id,warn_missing = FALSE)")
  as.character(eval(parse(text=cmd))) }