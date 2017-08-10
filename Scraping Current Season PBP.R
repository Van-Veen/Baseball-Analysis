




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


