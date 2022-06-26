get_last_weekdate <- function() {
  
  if(lubridate::wday(Sys.Date()) ==1){
    date<-Sys.Date()-2
  }
  else if(lubridate::wday(Sys.Date()) ==6){
    date <- Sys.Date()-1
  } 
  else{
    date <- Sys.Date() 
  }
  
  return(date)
}