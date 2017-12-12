
# packages ::-----------------------------------
library(data.table)
library(magrittr)
library(lubridate)
library(stringr)


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

bbref <- function(player, dy = NULL){
  
  # Pulling in the Lahmans Master directory
  fp <- "C:/Users/jstewart/Desktop/Baseball Analysis/Data/Master.csv"
  
  DF <- fread(fp)
  
  # Create a queryName that is all caps for standardization
  # Create an indicator for duplicate names
  # Create a debut year variable for further filtering of dupes
  DF <- DF[, queryName := toupper(paste(nameFirst, nameLast, sep = " "))] %>% 
    .[, DebutYr := year(as.Date(debut))] %>% 
    .[, c("queryName", "DebutYr", "bbrefID"), with = F] %>% 
    .[, Dupes := .N, by = queryName] %>% 
    .[, Dupes := ifelse(Dupes > 1, 1, 0)]
  
  # Formatting the function argument for standardization
  PLAYER <- toupper(player)
  
  # Specifying base bbref url for later usage
  baseURL <- "https://www.baseball-reference.com/players/"
  
  # Producing an error for erroneous query
  if(PLAYER %in% DF$queryName == F){return(warning("ERROR: Player not found in database."))}
  
  # Given the player is in the database, we parse down the master directory to players matching the argument parameter
  selectedPlayer <- DF[queryName == PLAYER]
  
  # Checking for dupes
  if(max(selectedPlayer$Dupes) == 1){
    
    # If debut year is not specified on a dupe, we return an error message and provide debut years for each dupe
    if(is.null(dy)){
      
      debutYears <- paste(selectedPlayer$DebutYr, collapse = ", ")
      
      message <- cat(paste("There are multiple players named ", str_to_title(PLAYER), " with the following debut years: ", debutYears, "\n", 
                           "Please specify which debut year as a secondary function argument to query appropriate player.", sep = "" ))
      
      return(message)
      
      # If debut year is specified, we navagate to that player's info and open up their bbref page  
    }else{
      
      selectedPlayer <- selectedPlayer[DebutYr == dy]
      
      firstLetter <- gsub(".* ", "", PLAYER) %>% substr(., 1, 1) %>% tolower(.)
      
      id <- selectedPlayer$bbrefID
      
      selectedURL <- paste(baseURL, firstLetter, "/", id, ".shtml", sep = "")
      
      browseURL(selectedURL)
      
    }
    
    # given no duplicate player names, we can simply create the bbref url and open up that player's personal page 
  }else{
    
    firstLetter <- gsub(".* ", "", PLAYER) %>% substr(., 1, 1) %>% tolower(.)
    
    id <- selectedPlayer$bbrefID
    
    selectedURL <- paste(baseURL, firstLetter, "/", id, ".shtml", sep = "")
    
    browseURL(selectedURL)
    
  }
  
  
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

OPS_scores <- function(){
  
  Category <- LETTERS[1:7]
  Classification <- c("Great", "Very Good", "Above Average", "Average", "Below Average", "Poor", "Very Poor")
  OBP_min <- c(.9000, .8333, .7667, .7000, .6334, .5667, .0001)
  OBP_max <- c(5.0000, .8999, .8332, .7666, .6999, .6333, .5666)
  
  ScoreCW <- data.table(Category, Classification, OBP_min, OBP_max)
  
}

