
# Future need for automated tests.
# 1. In the prepBatters function, we will need to writeup some automated test to validate that the player name merged
# with Lahman's is the appropriate player, not someone with the same name that last played in the 70s or someone with the
# same name but on a different team



library(data.table)
library(magrittr)
library(plyr)

# Getting batters ID, first and last names, number, and team label for a given game on a specific date, via gd2.mlb.com

batters <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_09/gid_2017_08_09_lanmlb_arimlb_1/players.xml"

# This function takes in a URL from gd2.mlb.com that contains player info and parses out specific lines, the ones
# mentioned above, building a database of identifiers for each player on both teams for a given date and match.
# The important part here is the 'playerID'. This identifier is specified in other data that we will be pulling
# and we will use this as a crosswalk to attach the players name to a given play

# it is important to mention that there are other elements that can be pulled, so revisit how this function works
# later on to see if there are other datapoints that may be useful.

# First, though, I'm going to write a quick function that generates a retroID for players, namely rookies, that won't
# have a retroID in the results of prepBatters.

IDmaker <- function(fName, lName, atWork = T){
  
  t1 <- tableLoader("Master", atWork = atWork) %>% .[, unique(retroID)]
  
  newID <- ifelse(nchar(lName) == 3, paste(lName, "-", sep = ""), lName)
  newID <- ifelse(nchar(lName) == 2, paste(lName, "--", sep = ""), lName)
  newID <- tolower(substr(newID, 1, 4))
  newID <- paste(newID, tolower(substr(fName, 1, 1)), sep = "")
  
  temp <- paste(newID, "0", sep = "")
  
  existing <- length(t1[grep(temp, t1)]) + 1
  existing <- ifelse(nchar(existing) == 1, paste("00", existing, sep = ""), existing)
  existing <- ifelse(nchar(existing) == 2, paste("0", existing, sep = ""), existing)

  newID <- paste(newID, existing, sep = "")
  
  return(newID)
  
}

prepBatters <- function(batterURL, atWork = T){
  
  batters <- suppressWarnings(readLines(batterURL)) %>% 
    trimws(.) %>% 
    .[grep("<player id=", .)] %>% 
    strsplit(., "\"") %>% 
    unlist(.) %>% 
    gsub("<|=", "", .) %>% 
    trimws(.)
  
  playerID <- batters[c(grep("player id", batters) + 1)]
  firstName <- batters[c(grep("first", batters) + 1)]
  lastName <- batters[c(grep("last", batters) + 1)]
  shirtNum <- batters[c(grep("num", batters) + 1)]
  team <- batters[c(grep("parent_team_abbrev", batters) + 1)]
  

  DF <- data.table(playerID, firstName, lastName, shirtNum, team) %>% 
    .[, fullName := paste(firstName, lastName, sep = " ")] %>% 
    unique(.)
  
  batters <- suppressWarnings(readLines(batterURL)) %>% 
    trimws(.) %>% 
    .[grep("game_position=", .)] %>% 
    strsplit(., "\"") %>% 
    unlist(.) %>% 
    gsub("<|=", "", .) %>% 
    trimws(.)
  
  playerID <- batters[c(grep("player id", batters) + 1)]
  position <- batters[c(grep("game_position", batters) + 1)]
  
  POS <- data.table(playerID, position)
  
  DF <- merge(DF, POS, by = "playerID", all = T) %>% 
    .[, FULLNAME := toupper(fullName)] %>% 
    .[!is.na(position), position :=  mapvalues(position, from = c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"), to = c(paste("P", seq(1, 9), sep = "")))]

  
  IDs <- IDCW(atWork = atWork)
  
  DF <- merge(DF, IDs, by = "fullName", all.x = T)
  
  MissingID <- DF[is.na(retroID)]
  DF <- DF[!is.na(retroID)]
  
  newID <- rep("", nrow(MissingID))
  firstNames <- MissingID$firstName
  lastNames <- MissingID$lastName
  
  for(i in 1:nrow(MissingID)){
    newID[i] <- IDmaker(firstNames[i], lastNames[i], atWork = atWork)
  }
  
  MissingID <- MissingID[, retroID := newID]
  
  DF <- rbind(DF, MissingID) %>% 
    setnames(., c("playerID.x", "playerID.y"), c("mlbID", "playerID"))
  
  return(DF)
  
  
}

batterCW <- prepBatters(batters)

# Now that we have a dataset that contains player id numbers, we can pull some of the game data, parse it out, and 
# try to structure it in a way that will allow it to be stackable from older data pulled from RetroSheets.

seqFiller <- function(x){
  
  newArray <- rep(x[1], length(x))
  newArray[1] <- x[1]
  currentItem <- x[1]
  
  for(i in 2:length(x)){
    
    if(is.na(x[i])){
      
      newArray[i] <- currentItem
      
    }else{
      
      currentItem <- x[i]
      newArray[i] <- x[i]
      
    }
    
  }
  
  return(newArray)
  
}

plays_path <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_09/gid_2017_08_09_lanmlb_arimlb_1/game_events.xml"

pullGame <- function(gameURL){
  
  gameText <- suppressWarnings(readLines(gameURL))
  
  DF <- gameText %>% 
    trimws(.) %>% 
    .[grep("atbat num=|batter=|pitcher=|pitch sv_id=", .)] %>% 
    data.table(.) %>% 
    setnames(., ".", "V1") %>% 
    .[grep("<atbat num=\"", V1), V1 := sub("<atbat num=\"", "", V1)] %>% 
    .[grep("start_tfs=", V1), V1 := paste("Idx", V1, sep = ",")] %>% 
    .[grep("start_tfs=", V1), V1 := sub("\"", "@", V1)] %>% 
    .[grep("@", V1), V1 := sub("@.*", "", V1)] %>% 
    .[grep("batter=|pitcher=", V1), V1 := gsub("\"", "", V1)] %>% 
    .[grep("batter=|pitcher=", V1), V1 := gsub("=", ",", V1)] %>% 
    .[grep("des=", V1), V1 := sub(".*des=\"", "", V1)] %>% 
    .[grep("des_es=", V1), V1 := paste("pitch", V1, sep = ",")] %>% 
    .[grep("des_es=", V1), V1 := sub("\" des_es=.*", "", V1)] %>%
    .[, V1 := gsub("In play,", "In play:", V1)] %>% 
    .[, Var := sub(",.*", "", V1)] %>% 
    .[, Val := sub(".*,", "", V1)] %>% 
    .[, -(1), with = F] %>% 
    .[Var == "Idx", Idx := Val] %>% 
    .[, Idx := seqFiller(Idx)] %>% 
    .[Var != "Idx"] %>% 
    .[Var == "batter", playerID := Val] %>% 
    .[, playerID := seqFiller(playerID)] %>% 
    .[Var != "batter"] %>% 
    .[Var == "pitcher", P1 := Val] %>% 
    .[, P1 := seqFiller(P1)] %>% 
    .[Var != "pitcher"] %>% 
    .[Val == "Hit By Pitch", Result := "H"] %>% 
    .[grep("Swinging Strike", Val), Result := "S"] %>% 
    .[Val == "Ball", Result := "B"] %>% 
    .[Val == "Called Strike", Result := "C"] %>% 
    .[Val == "Foul", Result := "F"] %>% 
    .[grep("In play:", Val), Result := "X"] %>% 
    .[Val == "Foul Bunt", Result := "L"] %>% 
    .[Val == "Foul Tip", Result := "T"] %>% 
    .[Val == "Ball In Dirt", Result := "B"]
  
  INNS <- gameText %>% 
    trimws(.) %>% 
    .[grep("inning num=|atbat num=", .)] %>% 
    data.table(.) %>% 
    setnames(., ".", "V1")  %>% 
    .[grep("<atbat num=\"", V1), V1 := sub("<atbat num=\"", "", V1)] %>% 
    .[grep("start_tfs=", V1), V1 := paste("Idx", V1, sep = ",")] %>% 
    .[grep("start_tfs=", V1), V1 := sub("\"", "@", V1)] %>% 
    .[grep("@", V1), V1 := sub("@.*", "", V1)] %>% 
    .[grep("inning num=", V1), V1 := gsub("<inning num=\"", "Inning,", V1)] %>% 
    .[, V1 := gsub("\">", "", V1)] %>% 
    .[grep("Inning", V1), Inning := gsub("Inning,", "", V1)] %>% 
    .[, Inning := seqFiller(Inning)] %>% 
    .[!grep("Inning", V1)] %>% 
    .[, V1 := gsub("Idx,", "", V1)] %>% 
    setnames(., "V1", "Idx")
  
  DF <- merge(DF, INNS, by = "Idx", all = T) %>% 
    .[, Idx := as.numeric(Idx)] %>% 
    .[order(Idx)]
  
  DATE <- gameText %>% 
    trimws(.) %>% 
    .[grep("start_tfs_zulu", .)] %>% 
    data.table(.) %>% 
    .[1] %>% 
    setnames(., ".", "Date") %>% 
    .[, Date := gsub("start_tfs_zulu=\"", "", Date)] %>% 
    .[, Date := sub("T.*", "", Date)] %>% 
    .[, Date := as.Date(Date)] %>% 
    .[, unique(Date)]
  
  
  DF <- DF[, Date := rep(DATE, nrow(DF))]
  
  return(DF)
  
}

DF <- pullGame(plays_path)



temp <- suppressWarnings(readLines(plays_path)) %>% 
  trimws(.) %>% 
  strsplit(., "<") %>% 
  unlist(.)

temp[c(grep("Defensive", temp) + 4)]

# Getting a list of starting players to merge onto 'DF' This will hopefully fill rows P1 to P9 of the very first line. We can then later
# plug in substitutes and have the defensive position change accordingly. First, though, I need to go back to the prepBatters function and either
# create the process of getting the team name or include the workflow process that gets the RetroID from RS

STARTERS <- prepBatters(batters) %>% 
  .[!is.na(position)] %>% 
  .[, Inning := rep("1", nrow(.))] %>% 
  dcast(., Inning + team ~ position, value.var = "playerID")


test <- merge(DF, STARTERS, by = c("Inning", "P1"), all = T) %>% 
  .[, c("Date", "Idx", "Var", "Inning", "playerID", "Result", "Val", "team","P1", "P2", "P3", "P4", "P5",
        "P6", "P7", "P8", "P9"), with = F]


# Descriptive Event Log

eventLog <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_09/gid_2017_08_09_lanmlb_arimlb_1/eventLog.xml"
#eventLog <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_10/gid_2017_08_10_lanmlb_arimlb_1/eventLog.xml"

EVE <- suppressWarnings(readLines(eventLog)) %>% 
  trimws(.) %>% 
  strsplit(., "\"") %>% 
  unlist(.) %>% 
  gsub("<|>|=", "", .) %>% 
  trimws(.)


eventNumber <- EVE[c(grep("event number", EVE) + 1)]
innings <- EVE[c(grep("inning", EVE) + 1)]
play <- EVE[c(grep("description", EVE) + 1)]


EVE <- data.table(eventNumber, innings, play) %>% 
  .[, play := toupper(play)] %>% 
  .[, play := gsub("  ", "", play)] %>% 
  .[, Event := rep("", nrow(.))] %>% 
  .[, EventMod := rep("", nrow(.))] %>% 
  .[, BaseMove := rep("", nrow(.))] %>% 
  .[grep("WILD PITCH", play), Event := "WP"] %>% 
  .[grep("STRIKES", play), Event := "K"]



EVE[grep("OFFENSIVE", play)] # Pinch Hitters
EVE[grep("PITCHING", play)] # Pitching Changes
EVE[grep("WILD PITCH", descriptions)] # Wild Pitch
EVE[grep("GROUNDS OUT", play)]
EVE[grep("TO 2ND", play)]
EVE[grep("LINES OUT", play)]
EVE[grep("LINE DRIVE", play)]
EVE[grep("WALKS", play)]
EVE[grep("To 3RD", play)]
EVE[grep("FLIES OUT", play)]


EVE[grep("STRIKES", play)]
EVE[grep("SINGLES", play)]
EVE[grep("DOUBLES", play)]
EVE[grep("SCORES", play)]







