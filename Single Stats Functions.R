
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

