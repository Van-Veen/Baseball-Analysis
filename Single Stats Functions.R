
# Libraries ::--------------------------------------------------------

library(data.table)
library(magrittr)
library(lubridate)


# Loading Dependencies ::-----------------------------------------------

loadSource <- function(fileName, atWork = F){
  
  fp <- "/Users/joelstewart/Desktop/Baseball Analysis/Syntax/"
  fpw <- "C:/Users/jstewart/Desktop/Baseball Analysis/Syntax/"
  
  if(atWork == T){
    source(paste(fpw, fileName, ".R", sep = ""))
  }else{
    source(paste(fp, fileName, ".R", sep = ""))
  }
  
  #return(targetSource)
  
}

loadSource("BB Main", T)


# Single Stat Functions ::-------------------------------------------------

batting_avg <- function(playerName, atWork = F, agg = NULL){
  
  avg <- batting(playerName, atWork = atWork) %>% 
    .[, round(sum(H)/sum(AB), 3), by = agg]
  
  return(avg)
}

slashLine <- function(playerName, atWork = F, agg = NULL){
  
  t1 <- batting(playerName, atWork = atWork) %>% 
    .[, c("playerID", "yearID", "teamID", "lgID", "Age", "BA", "OBP", "SLG"), with = F] %>% 
    .[, BAm := round(mean(BA), 3), by = agg] %>% 
    .[, OBPm := round(mean(OBP), 3), by = agg] %>% 
    .[, SLGm := round(mean(SLG), 3), by = agg]
  
  if(is.null(agg)){
    
    t1 <- t1[, -c("BA", "OBP", "SLG"), with = F] %>% 
      .[yearID == max(yearID)] %>% 
      .[, slashLine := paste(BAm, OBPm, SLGm, sep = "/")] %>% 
      .[, c("playerID", "slashLine"), with = F]
    
  }else{
    
    t1 <- t1[, c("playerID", agg, "BAm", "OBPm", "SLGm"), with = F] %>% 
      .[, slashLine := paste(BAm, OBPm, SLGm, sep = "/")] %>% 
      .[, -c("OBPm", "BAm", "SLGm"), with = F]
    
    if(agg == "teamID"){
      
      t1 <- unique(t1)
      
    }
    
  }
  
  return(t1)
  
}


