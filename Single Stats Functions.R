
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
  
  t1 <- batting(playerName, atWork = atWork) 
  
  if(is.null(agg)){
    
    t1 <- t1[, BA := round(sum(H)/sum(AB), 3)] %>% 
      .[, OBP := round(sum(H, BB, HBP)/sum(AB, BB, HBP, SF), 3)] %>% 
      .[, SLG := round((((sum(H) - sum(B2) - sum(B3) - sum(HR)) + (sum(B2) * 2) + (sum(B3) * 3) + (sum(HR) * 4))/sum(AB)), 3)] %>% 
      .[, c("playerID", "BA", "OBP", "SLG"), with = F] %>% 
      .[, slashLine := paste(BA, OBP, SLG, sep = "/")] %>% 
      .[, c("playerID", "slashLine"), with = F] %>% 
      unique(.)
    
  }else{
    
    t1 <- t1[, BA := round(sum(H)/sum(AB), 3), by = agg] %>% 
      .[, OBP := round(sum(H, BB, HBP)/sum(AB, BB, HBP, SF), 3), by = agg] %>% 
      .[, SLG := round((((sum(H) - sum(B2) - sum(B3) - sum(HR)) + (sum(B2) * 2) + (sum(B3) * 3) + (sum(HR) * 4))/sum(AB)), 3), by = agg] %>% 
      .[, slashLine := paste(BA, OBP, SLG, sep = "/")] %>% 
      .[, c("playerID", agg, "slashLine"), with = F] %>% 
      unique(.)
    
      
    }
  
  return(t1)
  
}

