get_last_weekdate <- function() {
  
  if(lubridate::wday(Sys.Date()) ==1){
    date<-Sys.Date()-2
  }
  else if(lubridate::wday(Sys.Date()) ==7){
    date <- Sys.Date()-1
  } 
  else{
    date <- Sys.Date() 
  }
  
  return(date)
}
