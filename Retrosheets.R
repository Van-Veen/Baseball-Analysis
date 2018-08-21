
library(data.table)
library(magrittr)
library(zoo)
library(lubridate)

# Helper functions
parseSegment = function(input_string){
  
  post_parse = gsub(".*^([^,]+),", "", input_string)
  
  return(post_parse)
  
}

isFoulStrike = function(pitch_data){
  
  max_fouls = pitch_data[, max(FOUL_SEQ, na.rm = T)]
  
  for(i in 1:max_fouls){
    pitch_data[FOUL_SEQ == i, STRIKE := ifelse(STRIKE_SEQ < 2, 1, STRIKE)]
    pitch_data[, STRIKE_SEQ := cumsum(STRIKE), by = BAT_ID]
  }
  
  return(pitch_data)
  
}
countShift = function(processed_pitches){
  
  pitches = processed_pitches[, c("BAT_ID", "PITCH_SEQ", "COUNT"), with = F] %>% 
    .[, PITCH := as.numeric(gsub("P", "", PITCH_SEQ))]
  
  starting_pitches = data.table("BAT_ID" = unique(pitches$BAT_ID)) %>% 
    .[, PITCH_SEQ := "P00"] %>% 
    .[, COUNT := "0-0"] %>% 
    .[, PITCH := 0]
  
  pitches = rbind(pitches, starting_pitches) %>% 
    .[order(BAT_ID, PITCH_SEQ, PITCH)] %>% 
    .[, PITCH := as.character(PITCH + 1)] %>% 
    .[, PITCH := ifelse(nchar(PITCH) == 1, paste("0", PITCH, sep = ""), PITCH)] %>% 
    .[, PITCH_SEQ := paste("P", PITCH, sep = "")] %>% 
    .[, -c("PITCH"), with = F] %>% 
    setnames(., "COUNT", "PRE_COUNT")
  
  setnames(processed_pitches, "COUNT", "POST_COUNT")
  
  count_shift = merge(processed_pitches, pitches, by = c("BAT_ID", "PITCH_SEQ"), all.x = T)
  
  return(count_shift)
  
}

# Function that loads a specific season, creates a GAME_ID, identifies event type by TAG, and 
# assigns an event identifier

loadSeason = function(season){
  
  base_fp = "/Users/joelstewart/Desktop/Baseball Analysis/Data/Retrosheets/"
  season_fp = paste(base_fp, season, "/", sep = "")
  
  event_files = list.files(season_fp)
  event_files = event_files[grepl("\\.EVA|\\.EVN", event_files)]
  
  # looping through the selected seasons event files
  for(i in 1:length(event_files)){
    
    team_events_fp = paste(season_fp, event_files[i], sep = "")
    team_events = readLines(team_events_fp)
    
    team_events = data.table("RAW" = team_events) %>% 
      .[grepl("id,", RAW), GAME_ID := RAW] %>% 
      .[, GAME_ID := gsub(".*,", "", GAME_ID)] %>% 
      .[, GAME_ID := zoo::na.locf(GAME_ID)] %>% 
      .[, TAG := gsub(",.*", "", RAW)] %>% 
      .[, EVENT_ID := paste(GAME_ID, seq(1, .N), sep = ""), by = GAME_ID]
    
    # combining all event files into one object
    if(i == 1){
      
      all_events = team_events
      
    }else{
      
      all_events = rbind(all_events, team_events)
      
    }
  }
  
  return(all_events)
}

test = loadSeason("2017eve")
test[, unique(TAG)]

# Processing Game info/metadata ::-------------------------------------------------------------------
processGameInfo = function(processed_season){
  
  processed_season = processed_season[TAG == "info"] %>% 
    .[, c("GAME_ID", "RAW"), with = F] %>% 
    .[, RAW := gsub("info,", "", RAW)] %>% 
    .[, VARIABLE := gsub(",.*", "", RAW)] %>% 
    .[, VALUE := gsub(".*,", "", RAW)] %>% 
    dcast(., GAME_ID ~ VARIABLE, value.var = "VALUE") %>% 
    .[, date := as.Date(date, format = "%Y/%m/%d")] %>% 
    setnames(., names(.), toupper(names(.)))
  
  return(processed_season)
  
}

test2 = processGameInfo(test)
head(test2)

# According to retrosheet documentation, the version tag is obsolete and can be ignored ::----------------------------------------

# For the data Tag, at this point retrosheet only uses this field to indicate the number of earned runs
# for each pitcher that participated in each game. This info will be derived from the actual play data
# and we can ignore this tag as well

# Processing starting lineups ::-------------------------------------------------------------------------
processStarters = function(processed_season){
  
  starting_lineup = processed_season[TAG == "start"] %>% 
    .[, NUM_STARTERS := .N, by = GAME_ID] %>% 
    .[, USE_DH := ifelse(NUM_STARTERS == 20, 1, 0)] %>% 
    .[, RAW := gsub("start,", "", RAW)] %>% 
    .[, PLAYER_ID := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, FULL_NAME := gsub(",.*", "", RAW)] %>% 
    .[, FULL_NAME := gsub('"', '', FULL_NAME)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, HOME := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, BAT_ORDER := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, POSITION := RAW] %>% 
    .[, -c("RAW", "TAG"), with = F]
  
  
  
  return(starting_lineup)
  
}

test3 = processStarters(test)
head(test3)

# At some point, we'll have further derived tables, looking at the batting order and player position at each
# instance of the game. This will incorporate information from the sub tag

# Processing substitutions ::---------------------------------
processSubstitutions = function(processed_season){
  
  substitutions = processed_season[TAG == "sub"] %>% 
    .[, RAW := gsub("sub,", "", RAW)] %>% 
    .[, PLAYER_ID := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, FULL_NAME := gsub(",.*", "", RAW)] %>% 
    .[, FULL_NAME := gsub('"', '', FULL_NAME)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, HOME := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, BAT_ORDER := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, POSITION := RAW] %>% 
    .[, -c("RAW", "TAG"), with = F]
  
  return(substitutions)
}

test8 = processSubstitutions(test)
head(test8)
# Isolating Plays and pitches ::----------------------------------------------------------------

cleanPlayTag = function(processed_season){
  
  plays = processed_season[TAG == "play"] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, INNING := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, HOME := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, PLAYER_ID := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, COUNT := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, PITCHES := gsub(",.*", "", RAW)] %>% 
    .[, RAW := parseSegment(RAW)] %>% 
    .[, PLAY := RAW] %>% 
    .[, -c("RAW", "TAG"), with = F]
  
  return(plays)
  
}

test4 = cleanPlayTag(test)
head(test4)

processPlays = function(clean_plays){
  
  processed_plays = clean_plays[, -c("COUNT"), with = F] %>% 
    setnames(., "PLAY", "PLAY_RAW") %>% 
    .[, PLAY := gsub("/.*", "", PLAY_RAW)] %>% 
    .[, PLAY_MOD := ""] %>%  
    .[grepl("/", PLAY_RAW), PLAY_MOD := gsub(".*^([^/]+)/", "", PLAY_RAW)] %>% 
    .[, BASE_MOVE := ""] %>% 
    .[grepl("\\.", PLAY_RAW), BASE_MOVE := gsub(".*\\.", "", PLAY_RAW)] %>% 
    .[, PLAY := gsub("\\..*", "", PLAY)] %>% 
    .[, PLAY_MOD := gsub("\\..*", "", PLAY_MOD)] %>%
    .[, BAT_ID := rleid(PLAYER_ID), by = .(GAME_ID, INNING, HOME)] %>% 
    .[, BAT_ID := paste(GAME_ID, INNING, HOME, BAT_ID, sep = "")] %>% 
    # Walks, Intentional Walks
    .[, BB := ifelse(grepl("W\\+", PLAY) | PLAY %in% c("W", "IW"), 1, 0)] %>% 
    .[, IBB := ifelse(grepl("IW", PLAY), 1, 0)] %>% 
    .[BB == 1 | IBB == 1, PLAY := gsub("W|W\\+|IW", "", PLAY)] %>% 
    # Strikeouts (still need to code K23)
    .[, SO := ifelse(grepl("K|K\\+", PLAY) & !grepl("BK", PLAY), 1, 0)] %>% 
    .[SO == 1 & !grepl("K2", PLAY), PLAY := gsub("K|K\\+", "", PLAY)] %>% 
    # Singles Base Hits
    .[, B1 := ifelse(!grepl("CS|SB", PLAY) & grepl("S", PLAY), 1, 0)] %>% 
    .[B1 == 1, FIELDED := gsub("S", "", PLAY)] %>% 
    .[B1 == 1, PLAY := ""] %>% 
    # Doubles and Ground Rule Doubles
    .[, B2 := ifelse(!grepl("DI", PLAY) & grepl("D", PLAY), 1, 0)] %>% 
    .[, DGR := ifelse(grepl("DGR", PLAY), 1, 0)] %>% 
    .[B2 == 1 | DGR == 1, FIELDED := gsub("D|DGR", "", PLAY)] %>% 
    .[B2 == 1 | DGR == 1, PLAY := ""] %>% 
    # Triples
    .[, B3 := ifelse(grepl("T", PLAY), 1, 0)] %>% 
    .[B3 == 1, FIELDED := gsub("T", "", PLAY)] %>% 
    .[B3 == 1, PLAY := ""] %>% 
    # Home Runs
    .[, HR := ifelse(grepl("HR|H", PLAY) & !grepl("CSH|SBH|HP", PLAY), 1, 0)] %>% 
    .[HR == 1, FIELDED := gsub("H|HR", "", PLAY)] %>% 
    .[HR == 1, PLAY := ""] %>% 
    # Summing all hits
    .[, H := B1 + B2 + B3 + HR]
  
  
  return(processed_plays)
} 

test5 = processPlays(test4) 

test5[PLAYER_ID == "troum001", sum(H)]
test5[, sum(SO)]
test5[SO == 1, table(PLAY)]
test5[grepl("IW", PLAY), table(PLAY)]
test5[B2 == 1 | DGR == 1, table(PLAY)]


# Processing Pitches ::--------------------------
processPitches = function(clean_plays){
  
  processed_pitches = clean_plays[, -c("COUNT", "PLAY"), with = F] %>% 
    .[, BAT_SEQ := rleid(PLAYER_ID), by = .(GAME_ID, INNING, HOME)] %>% 
    .[, REC_SEQ := seq(1, .N), by = .(GAME_ID, INNING, HOME, BAT_SEQ)] %>% 
    .[, BAT_ID := paste(GAME_ID, INNING, HOME, BAT_SEQ, sep = "")] %>% 
    .[, MAX_REC_SEQ := max(REC_SEQ), by = BAT_ID]
  
  each_pitch = processed_pitches[MAX_REC_SEQ == REC_SEQ, c("BAT_ID", "PITCHES"), with = F] %>% 
    .[, PITCHES := gsub("\\.", "", PITCHES)] %>% 
    .[, PITCHES := gsub("\\^>", ">", PITCHES)] %>% 
    .[, PITCHES := gsub("\\^", ">", PITCHES)]
  
  for(i in 1:max(nchar(each_pitch$PITCHES))){
    
    pitchVar = paste("P", i, sep = "")
    
    each_pitch = each_pitch[, newVar := substr(PITCHES, i, i)] %>% 
      setnames(., "newVar", pitchVar)
  }
  
  each_pitch = melt(each_pitch[, -c("PITCHES"), with = F], id.vars = "BAT_ID") %>% 
    .[value != ""] %>% 
    .[order(BAT_ID, variable)] %>% 
    .[, PITCH_ID := as.numeric(gsub("P", "", variable))] %>% 
    .[, PITCH_ID := ifelse(PITCH_ID < 10, paste("0", PITCH_ID, sep = ""), as.character(PITCH_ID))] %>% 
    .[, PITCH_ID := paste(BAT_ID, PITCH_ID, sep = "")] %>% 
    setnames(., c("variable", "value"), c("PITCH_SEQ", "PITCH_RESULT")) %>% 
    .[, PITCH_SEQ := as.character(PITCH_SEQ)] %>% 
    .[, PITCH_SEQ := ifelse(nchar(PITCH_SEQ) == 2, paste("P0", substr(PITCH_SEQ, 2, 2), sep = ""), PITCH_SEQ)] %>% 
    # after determining what 'D' is, add to IS_PITCH and code whatever it turns out to be
    .[, IS_PITCH := ifelse(PITCH_RESULT %in% c("B", "C", "F", "H", "L", "M", "O", "P", "Q", "R", "S", "T", "U", "V", "X", "Y"), 1, 0)] %>% 
    .[, BALL := 0] %>% 
    .[, STRIKE := 0] %>% 
    .[, FOUL := 0] %>% 
    .[, IN_PLAY := ifelse(PITCH_RESULT %in% c("X", "Y"), 1, 0)] %>% 
    .[, HBP := ifelse(PITCH_RESULT == "H", 1, 0)] %>% 
    .[PITCH_RESULT %in% c("B", "V", "I", "P"), BALL := 1] %>% 
    .[PITCH_RESULT %in% c("C", "S", "L", "M", "O", "Q", "T", "K"), STRIKE := 1] %>% 
    .[PITCH_RESULT %in% c("F"), FOUL := 1] %>% 
    .[, PO_THROW := 0] %>% 
    .[PITCH_RESULT %in% c("1", "2", "3", "+"), PO_THROW := 1] %>% 
    .[, BACK_PICK := ifelse(PITCH_RESULT == "+", 1, 0)] %>% 
    .[, BLOCK := ifelse(PITCH_RESULT == "*", 1, 0)] %>% 
    .[, UNKNOWN_RESULT := ifelse(PITCH_RESULT == "U", 1, 0)] %>% 
    .[order(BAT_ID, PITCH_SEQ)]
  
  advancing_baserunner = each_pitch[PITCH_RESULT == ">"] %>% 
    .[, c("BAT_ID", "PITCH_SEQ"), with = F] %>% 
    .[, NEW_SEQ := as.numeric(gsub("P", "", PITCH_SEQ))] %>% 
    .[, PITCH_SEQ := paste("P", NEW_SEQ + 1, sep = "")] %>% 
    .[, PITCH_SEQ := ifelse(nchar(PITCH_SEQ) == 2, paste("P0", substr(PITCH_SEQ, 2, 2), sep = ""), PITCH_SEQ)] %>% 
    .[, -c("NEW_SEQ"), with = F] %>% 
    .[, ADV_RUNNER := 1]
  
  each_pitch = merge(each_pitch, advancing_baserunner, by = c("BAT_ID", "PITCH_SEQ"), all.x = T) %>% 
    .[, ADV_RUNNER := ifelse(is.na(ADV_RUNNER), 0, ADV_RUNNER)]
  
  each_pitch = each_pitch[, STRIKE_SEQ := cumsum(STRIKE), by = BAT_ID] %>% 
    .[, BALL_SEQ := cumsum(BALL), by = BAT_ID] %>% 
    .[FOUL == 1, FOUL_SEQ := seq(1, .N), by = BAT_ID]
  
  each_pitch = isFoulStrike(each_pitch) %>% 
    .[, COUNT := paste(BALL_SEQ, STRIKE_SEQ, sep = "-")]
  
  each_pitch = countShift(each_pitch)
  
  each_pitch = each_pitch[, FIRST_PITCH := ifelse(PITCH_SEQ == "P01", 1, 0)] %>% 
    .[IS_PITCH == 1, PITCH_AB := rleid(PITCH_ID), by = BAT_ID] %>% 
    .[, FULL_COUNT := ifelse(PRE_COUNT == "3-2", 1, 0)]
  
  return(each_pitch)
  
}



test6 = processPitches(test4)

randomAB = sample(test6$BAT_ID, 1)
test7 = test6[BAT_ID %in% randomAB]
test7

# NOTES ::--------------------------------
# Note: For future work, we'll need to write a test to determine the extent of missing play information. I discovered
# this by testing the loadSeason function on the 1990 season. In a visual inspection, I saw that there were play items
# where the play was coded as "??". The three lines of code below demonstrate this. It looks like there are 177 games with
# missing play information. Of these 177 games, most of them are completely missing play info (N == 157). For the remainng
# 20 games, play info is either slightly missing or almost completely missing. We'll need to write an algorithm that 
# decides what processes to follow in regards to missing play info.

# Revisited the retrosheet docs and now see that ?? signifies some uncertainty about the play, but the play info is 
# still there. Still approach with caution

test = loadSeason("1990eve")
test[TAG == "play", sum(grepl("\\?\\?", RAW))/.N * 100, by = GAME_ID]
test[TAG == "play", sum(grepl("\\?\\?", RAW))/.N * 100, by = GAME_ID] %>% .[, table(V1 != 0)]
test[TAG == "play", sum(grepl("\\?\\?", RAW))/.N * 100, by = GAME_ID] %>% .[V1 != 0, summary(V1)]
test[TAG == "play", sum(grepl("\\?\\?", RAW))/.N * 100, by = GAME_ID] %>% .[V1 != 0, table(round(V1))]





