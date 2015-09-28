remapDEs<-function(des_in,base.url,username,password,mode){
  is_valid_mode<-mode %in% c("code","name","shortName")
  if ( is_valid_mode == FALSE )  {break}
  r<-GET(URLencode(paste0(base.url,"api/dataElements?fields=id,code,shortName&paging=false")), authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  des<-ldply(lapply(r$dataElements, function(x) t(unlist(x))))
  des<-colwise(as.character)(des)
  cmd<-paste0("mapvalues(des_in,des$",mode,",des$id,warn_missing = FALSE)")
  eval(parse(text=cmd)) }
