

# packages ::-----------------------------------
library(data.table)
library(magrittr)
library(lubridate)


# Functions ::-----------------------------------
tableLoader <- function(tableName, atWork = T){
  
  fp <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/"
  fpw <- "C:/Users/jstewart/Desktop/Baseball Analysis/Data/"
  
  if(atWork == T){
    DF <- fread(paste(fpw, tableName, ".csv", sep = ""))
  }else{
    DF <- fread(paste(fp, tableName, ".csv", sep = ""))
  }
  
  return(DF)
}

getPlayer <- function(playerName, atWork = F){
  DF <- tableLoader("Master", atWork = atWork) %>% 
    .[, fullName := paste(nameFirst, nameLast, sep = " ")] %>% 
    .[fullName == playerName] %>% 
    .[, age_CF := ifelse(birthMonth > 6, 1, 0)]
  
  return(DF)
}

batting <- function(playerName, atWork = F){
  player <- getPlayer(playerName, atWork = atWork)$playerID
  birthYear <- getPlayer(playerName, atWork = atWork)[, birthYear + age_CF]
  
  DF <- tableLoader("Batting", atWork = atWork) %>% 
    .[playerID == player] %>% 
    setnames(., c("2B", "3B"), c("B2", "B3")) %>% 
    .[, Age := yearID - birthYear] %>% 
    .[, BA := round(H/AB, 3)] %>% 
    .[, PA := AB + BB + HBP + SF + SH, by = c("yearID", "teamID")] %>% 
    .[, OBP := round((H + BB + HBP)/(AB + BB + HBP + SF), 3)] %>% 
    .[, SLG := round((((H - B2 - B3 - HR) + (B2*2) + (B3 * 3) + (HR * 4))/AB), 3)] %>% 
    .[, OPS := OBP + SLG] %>% 
    .[, TB := (H - B2 - B3 - HR) + (B2*2) + (B3 * 3) + (HR * 4)]
  
  return(DF)
}


