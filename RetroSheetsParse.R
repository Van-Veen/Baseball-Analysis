library(data.table)
library(magrittr)

# version: omit
# id: DONE!
# info:
# play::plays
# play::pitch
# start
# sub
# data
# com:
# badj:
# padj:
# ladj:

# Tables Produced ::---------------------------------------------------
# 1. RAW
# 2. IDS
# 3. INFO
# 4. STARTERS
# 5. SUBS
# 6. STARTERS + SUBS -> BATORDER
# 7. SUBS -> OFFENSIVE_SUBS
# 8. DATA
# 9. COM 
# 10. PLAYS
# 11. PITCH
# 12. RAW -> CONTENT
# 13. STARTERS + SUBS -> DEFENSE

# X. HELPER FUNCTIONS ::---------------------------------------------------------------------------

seqFiller <- function(x){
  
  newArray <- rep(NA, length(x))
  currentItem <- x[1]
  newArray[1] <- currentItem
  
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


# 0. RAW ::---------------------------------------------------------------------------------------
fpw <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/2016eve/2016LAN.EVN"
#fpw <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/2016eve/2016ARI.EVN"
#fpw <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/2016eve/2016SFN.EVN"
tempPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/RStemp2.csv"

DFm <- readLines(fpw) %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",")

maxLen <- length(DFm)

RAW <- data.table("Idx" = seq(1, length(DFm)), "Content" = readLines(fpw)) %>% 
  .[, Tag := gsub(",.*", "", Content)]

DF <- DFm[-grep("version,", DFm)] 


# 1. IDs ::-----------------------------------------------------------------------
# First we isolate game id lines, save them to their own file, then omit them from 
# the collection. We then fill in the missing Idx rows with the preceeding GameID, unless
# a change in the array occurs. This essentially gives us a table that we can merge on
# to other tables, giving each table a game identifier

IDS <- DF[grep("id,", DF)] %>% 
  writeLines(., tempPath)

IDS <- fread(tempPath) %>% 
  setnames(., names(.), c("Idx", "NULL", "GameID")) %>% 
  .[, c("Idx", "GameID"), with = F]

IDS <- data.table("Idx" = seq(min(IDS$Idx), maxLen)) %>% 
  merge(., IDS, by = "Idx", all = T) %>% 
  .[, GameID := seqFiller(GameID)]


DF <- DF[-grep("id,", DF)]

# 1.1 Modulating RAW based on IDS fields ::--------------------------------------------------------

RAW <- merge(RAW, IDS, by = "Idx", all.x = T)
RAW <- RAW[, PlayID := paste(GameID, Idx, sep = "")] %>% 
  .[Tag != "play", PlayID := NA]

# 2. INFO ::----------------------------------------------------------------------------

#infoPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/GameInfo_temp.csv"

INFO <- DF[grep("info,", DF)] %>% writeLines(., tempPath)

INFO <- fread(tempPath) %>% 
  setnames(., names(.), c("Idx", "NULL", "Category", "Value")) %>% 
  .[, c("Idx", "Category", "Value"), with = F] %>% 
  merge(., IDS, by = "Idx", all.x = T) %>%  
  dcast(., GameID ~ Category, value.var = c("Value"))

INFO <- INFO[, date := as.Date(date, format = "%Y/%m/%d")]

DF <- DF[-grep("info,", DF)]

# 3. STARTERS & SUBS ::---------------------------------------------------------------------------

starterPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/starters_temp.csv"

STARTERS <- DF[grep("start,", DF)] %>% writeLines(., tempPath)

STARTERS <- fread(tempPath) %>% 
  setnames(., names(.), c("Idx", "NULL", "RetroID", "FullName", "HomeVisit", "BatOrder", "Position")) %>% 
  .[, c("Idx", "RetroID", "FullName", "HomeVisit", "BatOrder", "Position"), with = F] %>% 
  merge(., IDS, by = "Idx", all.x = T) %>% 
  .[, Position := paste("P", Position, sep = "")]

DF <- DF[-grep("start,", DF)]

subsPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/subs_temp.csv"

SUBS <- DF[grep("sub,", DF)] %>% writeLines(., tempPath)
SUBS <- fread(tempPath) %>% 
  setnames(., names(.), c("Idx", "NULL", "RetroID", "FullName", "HomeVisit", "BatOrder", "Position")) %>% 
  .[, c("Idx", "RetroID", "FullName", "HomeVisit", "BatOrder", "Position"), with = F] %>% 
  merge(., IDS, by = "Idx", all.x = T)

DF <- DF[-grep("sub,", DF)]

BATORDER <- rbind(STARTERS, SUBS) %>% 
  .[, c("GameID", "RetroID", "HomeVisit", "BatOrder"), with = F] %>% 
  .[order(GameID, HomeVisit, BatOrder)]

OFFENSIVE_SUBS <- SUBS[Position %in% c(10:12)]
SUBS <- SUBS[Position %in% c(10:12) == F] %>% 
  .[, Position := paste("P", Position, sep = "")]

# 4. DATA ::---------------------------------------------------------------------------

dataPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/Data_temp.csv"

DATA <- DF[grep("data,", DF)] %>% writeLines(., tempPath)
DATA <- fread(tempPath) %>% 
  setnames(., names(.), c("Idx", "NULL", "Category", "RetroID", "ER")) %>% 
  .[, c("Idx", "Category", "RetroID", "ER")] %>% 
  merge(., IDS, by = "Idx", all.x = T)

DF <- DF[-grep("data,", DF)]

# NOTE: As of 11/14/2017, the items in the 'data' rows contain only the earned runs by 
# each pitcher for each game. I'm leaving it here, as-is, for now but with minimal processing
# we can bring it in and use it. However, we shouldn't need it because these values can be
# derived from the plays data.

# 5. COM ::-------------------------------------------------------------------------

comPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/com_temp.csv"

COM <- DF[grep(",com,", DF)] %>% writeLines(., tempPath)
COM <- fread(tempPath)

DF <- DF[-grep(",com,", DF)]

# NOTE: Figure out what to do with this later. This might be trickier because
# multiple com lines are associated with a single comment, either containing data
# about what happened or having an actual comment spill over on to new lines.

# 6. PLAYS ::-----------------------------------------------------

playsPath <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/plays_temp.csv"

PLAYS <- DF[grep(",play,", DF)] %>% writeLines(., tempPath)
PLAYS <- fread(tempPath) %>% 
  .[, -c(2), with = F] %>% 
  setnames(., names(.), c("Idx", "Inning", "HomeVisit", "RetroID", "CAP", "Pitches", "Play")) %>% 
  merge(., IDS, by = "Idx", all.x = T) %>% 
  .[Play != "NP"]

DF <- DF[-grep(",play,", DF)]

PITCH <- PLAYS[, c("GameID", "Idx", "Inning", "HomeVisit", "RetroID", "CAP", "Pitches"), with = F]
PLAYS <- PLAYS[, c("GameID", "Idx", "Inning", "HomeVisit", "RetroID", "Play" )]


# 7. CONTENT::-----------------------------------------

CONTENT <- dcast(RAW, GameID ~ Tag, value.var = "Content", fun.aggregate = length)


# 8. DEFENSE ::-------------------------------------------------------------------

POS_START <- dcast(STARTERS, GameID + HomeVisit ~ Position, value.var = "RetroID") %>% 
  .[, HomeVisit := ifelse(HomeVisit == 1, 0, 1)] %>% 
  .[, Start := 1]

POS_PLAYS <- PLAYS[, c("Idx", "GameID", "HomeVisit", "Inning"), with = F] %>% 
  .[, Start := 0] %>% 
  .[, MinIdx := min(Idx), by = .(GameID, HomeVisit)] %>% 
  .[MinIdx == Idx, Start := 1] %>% 
  .[, c("Idx", "GameID", "HomeVisit", "Inning", "Start"), with = F] %>% 
  merge(., POS_START, by = c("GameID", "HomeVisit", "Start"), all.x = T) %>% 
  .[order(Idx)]

POS_SUBS <- dcast(SUBS, GameID + Idx + HomeVisit ~ Position, value.var = "RetroID") %>% 
  .[, HomeVisit := ifelse(HomeVisit == 1, 0, 1)]

POS_ALL <- rbind(POS_PLAYS, POS_SUBS, fill = T) %>% .[order(Idx)]

fillerCols <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")

POS_HOME <- POS_ALL[HomeVisit == 1]  %>% .[, (fillerCols) := lapply(.SD, function(x) seqFiller(x)), .SDcols = (fillerCols)]
POS_AWAY <- POS_ALL[HomeVisit == 0]  %>% .[, (fillerCols) := lapply(.SD, function(x) seqFiller(x)), .SDcols = (fillerCols)]

colOrder <- c("Idx", "GameID", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")

DEFENSE <- rbind(POS_HOME, POS_AWAY) %>% 
  .[, c(colOrder), with = F] %>% 
  .[Idx %in% PLAYS$Idx] %>% 
  .[, PlayID := paste(GameID, Idx, sep = "")] %>% 
  .[, -c("Idx"), with = F]

# 9. Processing PLAYS::-----------------------------------------------------------------------------------

PLAYS <- PLAYS[, PlayID := paste(GameID, Idx, sep = "")]

# 10. Processing PITCH ::--------------------------------------------------------------------------------

test <- PITCH[, PlayID := paste(GameID, Idx, sep = "")] %>% 
  .[, c("PlayID", "Idx", "Pitches"), with = F] %>% 
  .[, Pitches := gsub(".*\\.", "", Pitches)]

mostPitches <- max(nchar(test$Pitches))

test <- test[, Pitches := gsub("", ",", Pitches) %>% substr(., 2, nchar(.) - 1)]
  
test <- paste(test$PlayID, test$Idx, test$Pitches, sep = ",")

writeLines(test, tempPath)

colNames <- c("PlayID", "Idx", paste("p", seq(1, mostPitches), sep = ""))

test <- read.table(tempPath, header = F, fill = T, sep = ",", col.names = colNames) %>% 
  data.table(.) %>% 
  melt(., id.var = c("PlayID", "Idx")) %>% 
  .[order(Idx)] %>% 
  .[value != ""] %>% 
  .[, variable := gsub("p", "", variable)] %>% 
  .[, PitchID := paste(PlayID, variable, sep = "")] %>% 
  .[, c("PlayID", "Idx", "PitchID", "value"), with = F] %>% 
  setnames(., "value", "PitchResult")

PITCH_CW <- fread("/Users/joelstewart/Desktop/Baseball Analysis/Data/Helper Files/PitchCW.csv")

test_p <- merge(test, PITCH, by = c("PlayID", "Idx"), all.x = T) %>% 
  merge(., DEFENSE[, c("PlayID", "P1"), with = F], by = "PlayID", all.x = T) %>% 
  .[order(Idx)] %>% 
  setnames(., c("RetroID", "P1"), c("Batter", "Pitcher")) %>% 
  merge(., PITCH_CW, by = "PitchResult", all.x = T) %>% 
  .[order(Idx, PitchID)]

PITCH_TYPES_GP <- dcast(test_p[PitchCount == 1], GameID + Pitcher ~ PitchResult )
PITCH_TYPES_P <- dcast(test_p[PitchCount == 1], Pitcher ~ PitchResult, value.var = "PitchResult",fun.aggregate = length)
  
# 11. Processing BASES ::------------------------------------------------

head(PLAYS)

BASES <- PLAYS[grep("\\.", Play)] %>% 
  .[, c("PlayID", "Play"), with = F] %>% 
  .[, AdvBase := gsub(".*\\.", "", Play)] %>% 
  .[, Play2 := gsub("\\..*", "", Play)] %>% 
  .[grep("HP", Play), HBP := 1] %>% 
  .[grep("WP", Play), WP := 1]

ON_BASE <- PLAYS[, c("PlayID", "Play"), with = F] %>% 
  .[PlayID %in% BASES$PlayID == F] %>% 
  .[, AdvBase := NA] %>% 
  .[, Play2 := gsub("\\..*", "", Play)] %>% 
  .[grep("HP", Play), HBP := 1] %>% 
  .[, WP := NA]

test <- PLAYS[, c("PlayID", "Play"), with = F] %>%
  .[grep("\\.", Play), AdvBase := gsub(".*\\.", "", Play)] %>% 
  .[, Play := gsub("\\..*", "", Play)] %>% 
  .[, slashCount := str_count(Play, "/")] %>% 
  .[, Play := gsub("L-|L\\+", "L", Play)] %>% 
  .[, Play := gsub("G-|G\\+", "G", Play)] %>% 
  .[, Play := gsub("P-", "P", Play)] %>% 
  .[, Play := gsub("F-|F\\+", "F", Play)]

test[slashCount == 0, table(Play)]

nrow(test) # 6097
test[grep("/G", Play)] %>% nrow(.) #177


# Base changing situations that need to be addressed
# 1. Balks: BK
# 2. Caught Stealing: CS%
# 3. Hit by Pitch: HP
# 4. Intentional Walks: IW
# 5. Home Runs : HR; B-H
# 6. Stolen Bases: SB
# 7. Walks : W
#

PLAYS[grep("C", Play)]
PLAYS[grep("SF", Play)]
PLAYS[grep("GTP", Play)]
PLAYS[grep("/L", Play)][, table(gsub(".*/L", "", Play))]
PLAYS[grep("K\\+", Play)]
PLAYS[grep("/DP", Play)]
PLAYS[grep("FO", Play)]
