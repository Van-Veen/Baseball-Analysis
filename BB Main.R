

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

bbref <- function(playerName){
  
  ID <- getPlayer(playerName)$bbrefID
  firstInitial <- substr(ID, 1, 1)
  baseURL <- "https://www.baseball-reference.com/players/"
  
  finalURL <- paste(baseURL, firstInitial, "/", ID, ".shtml", sep = "")
  
  browseURL(finalURL)
  
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

advBatting <- function(playerName, atWork = F){
  player <- getPlayer(playerName, atWork = atWork)$playerID
  birthYear <- getPlayer(playerName, atWork = atWork)[, birthYear + age_CF]
  
  DF <- tableLoader("Batting", atWork = atWork) %>% 
    .[playerID == player] %>% 
    setnames(., c("2B", "3B"), c("B2", "B3")) %>% 
    .[, Age := yearID - birthYear] %>%
    .[, PA := AB + BB + HBP + SF + SH, by = c("yearID", "teamID")] %>% 
    .[, BBp := round((BB)/PA * 100, 1), by = c("yearID", "teamID")] %>% 
    .[, Kp := round(SO/PA * 100, 1), by = c("yearID", "teamID")] %>% 
    .[, BBK := ifelse(Kp > 0, round(BB/SO, 2), 0.00)] %>% 
    .[, BA := round(H/AB, 3)] %>% 
    .[, OBP := round((H + BB + HBP)/(AB + BB + HBP + SF), 3)] %>% 
    .[, SLG := round((((H - B2 - B3 - HR) + (B2*2) + (B3 * 3) + (HR * 4))/AB), 3)] %>% 
    .[, OPS := OBP + SLG] %>% 
    .[, ISO := SLG - BA] %>% 
    .[, BABIP := round((H - HR)/(AB - SO - HR + SF), 3)] %>% 
    .[, c("playerID", "yearID", "stint", "teamID", "lgID", "Age", "BBp", "Kp", "BBK", "BA",
          "OBP", "SLG", "OPS", "ISO", "BABIP"), with = F]
  
  return(DF)
    
}

IDCW <- function(atWork = T){
  
  t1 <- t1 <- tableLoader("Master", atWork = atWork) %>% 
    .[, nameFirst := gsub("\\. ", "\\.", nameFirst)] %>% 
    .[, fullName := paste(nameFirst, nameLast, sep = " ")] %>% 
    .[, recentYear := year(as.Date(finalGame))] %>% 
    .[, c("playerID", "retroID", "bbrefID", "fullName", "recentYear"), with = F]
  
  t2 <- tableLoader("batting", atWork = atWork) %>% 
    .[, c("playerID", "yearID", "teamID", "stint"), with = F] %>% 
    .[, maxStint := max(stint), by = .(playerID, yearID)] %>% 
    .[stint == maxStint] %>% 
    .[, lastYear := max(yearID), by = playerID] %>% 
    .[lastYear == yearID]
  
  t1 <- merge(t1, t2, by = "playerID", all.x = T) %>% 
    .[, c("playerID", "retroID", "bbrefID", "fullName", "teamID", "recentYear"), with = F] 
  
  return(t1)
  
}

