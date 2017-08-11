


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

prepBatters <- function(batterURL){
  
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

  
  return(DF)
  
}

batterCW <- prepBatters(batters)

# Now that we have a dataset that contains player id numbers, we can pull some of the game data, parse it out, and 
# try to structure it in a way that will allow it to be stackable from older data pulled from RetroSheets.


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
EVE[grep("TO 2ND", descriptions)]

EVE[grep("STRIKES", play)]
EVE[grep("SINGLES", play)]
EVE[grep("DOUBLES", play)]
EVE[grep("SCORES", play)]








library(data.table)

# Getting batters ID, first and last names, number, and team label for a given game on a specific date, via gd2.mlb.com

batters <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_09/gid_2017_08_09_lanmlb_arimlb_1/players.xml"

# This function takes in a URL from gd2.mlb.com that contains player info and parses out specific lines, the ones
# mentioned above, building a database of identifiers for each player on both teams for a given date and match.
# The important part here is the 'playerID'. This identifier is specified in other data that we will be pulling
# and we will use this as a crosswalk to attach the players name to a given play

# it is important to mention that there are other elements that can be pulled, so revisit how this function works
# later on to see if there are other datapoints that may be useful.

prepBatters <- function(batterURL){
  
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
  team <- batters[c(grep("team_abbrev", batters) + 1)]
  
  DF <- data.table(playerID, firstName, lastName, shirtNum, team)
  
  return(DF)
  
}

test <- prepBatters(batters)


# Now that we have a dataset that contains player id numbers, we can pull some of the game data, parse it out, and 
# try to structure it in a way that will allow it to be stackable from older data pulled from RetroSheets.


plays_path <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_08/day_09/gid_2017_08_09_lanmlb_arimlb_1/game_events.xml"


test2 <- readLines(plays_path) %>% 
  trimws(.)



test2[grep("batter=", test2)] %>% length(.)
test2[grep("pitcher=", test2)] %>% length(.)



bat_idx <- grep("atbat num=", test2)

test2[5:28] %>% .[grep("pitch sv_id=", .)]


prepEvents <- function(eventURL){
  
  events <- suppressWarnings(readLines(eventURL)) %>% trimws(.)
  
  bat_idx <- c(grep("atbat num=", events), length(events))
  
  pitchCounts <- rep(0, length(bat_idx) - 1)
  
  for(i in 1:length(bat_idx) - 1){
    
    x <- bat_idx[i]
    y <- bat_idx[i + 1]
    
    pitchCounts[i] <- events[x:y] %>% .[grep("pitch sv_id=", .)] %>% length(.)
    
  }
  
  #T1 <- events[x:y] %>% .[grep("pitch sv_id=", .)] %>% length(.)

  
  return(pitchCounts)
  
}


prepEvents(plays_path) %>% length(.)

test <- prepEvents(plays_path)

x <- 72

test2[test[x]:test[x + 1]]


