#' @export
#' @title loadSecrets(secrets)

#' @description Load a DHIS secrets file which should be a JSON file as below
#' {
#' dhis": {
#'  "baseurl": "http://localhost:8080",
#'  "username": "admin",
#'  "password": "district"
#'  }
#' }
#'
#'
#' @param secrets Location of the secrets file
#' @return Returns a boolean value indicating that the secrets file is valid by accessing /api/me
#'
loadSecrets<-function(secrets=NA) {
  #Load from a file
  if (!is.na(secrets) ) {
  
    assertthat::assert_that(file.exists(secrets))
    s<-jsonlite::fromJSON(secrets) 
    } else {
      #TODO: Need to deal with command line interfaces
      getPass<-function(){  
        
        
        wnd<-tcltk::tktoplevel();tcltk::tclVar("")->passVar;  
        #Label  
        tcltk::tkgrid(tcltk::tklabel(wnd,text="Enter password:"));  
        #Password box  
        tcltk::tkgrid(tcltk::tkentry(wnd,textvariable=passVar,show="*")->passBox);  
        #Hitting return will also submit password  
        tcltk::tkbind(passBox,"<Return>",function() tcltk::tkdestroy(wnd));  
        #OK button  
        tcltk::tkgrid(tcltk::tkbutton(wnd,text="OK",command=function() tcltk::tkdestroy(wnd)));  
        #Wait for user to click OK  
        tcltk::tkwait.window(wnd);  
        password<-tcltk::tclvalue(passVar);  
        return(password);  
      }  
      
      
    s<-list(dhis=list())
    s$dhis$username<-readline("Username: ")
    s$dhis$password<-getPass()
    s$dhis$baseurl<-readline("Server URL (ends with /): ")
}
  
  options("baseurl"= s$dhis$baseurl )
  options("secrets"=secrets)
  url<-URLencode( URL = paste0(getOption("baseurl"),"api/me") )
  #Logging in here will give us a cookie to reuse
  r<-httr::GET(url ,
               httr::authenticate(s$dhis$username,s$dhis$password),
               httr::timeout(60))
  assertthat::assert_that(r$status == 200L)
  r<- httr::content(r, "text")
  me<-jsonlite::fromJSON(r)
  options("organisationUnit" = me$organisationUnits$id)
  }
