# Shelved
# 1. Credits field needs to be cleaned, removing either "(" or ")" and processed, ideally in a function. Initially, I think putouts and assists will 
#    be contained in another table, with the key being PlayID
# 2. Need to figure out how to attribute a stolen base to the specified baserunner, paying particular attention to multi base steals per event
# 3. Need to revist contributions to AdvBase to ensure that explicit basemoves aren't present or usurped
# 4. Need to summate pick off attempts from pitches data with extant PO data from successfule, or failed, pick off attempts
# 5. Need to revisit derived metrics to see if they should contribute to Out indicator

head(PLAYS)


DF <- PLAYS[, -c("HomeVisit"), with = F] %>% 
  .[, PlayID := paste(GameID, Idx, sep = "")] %>% 
  .[grep("\\.", Play), AdvBase := gsub(".*\\.", "", Play)] %>% 
  .[, Play := gsub("\\..*", "", Play)] %>% 
  .[, Play := gsub("L-|L\\+", "L", Play)] %>% 
  .[, Play := gsub("G-|G\\+", "G", Play)] %>% 
  .[, Play := gsub("F-|F\\+", "F", Play)] %>% 
  .[, Outcome := gsub("/.*", "", Play)] %>% 
  .[Play != Outcome, Mods := gsub("^[^/]*/", "", Play)] %>% 
  .[, NumMods := str_count(Mods, "/") + 1]


DF[, table(Outcome)]

# Checking for Steal Attempts ::--------------------------------------------------------------------
# 1. Create dummy indicator for base steal attempt
# 2. Create dummy indicator for stolen base, setting them to 0 because they were caught
# 3. Take care of the combos
#   a. K+CS Events: Create dummy indicator for strikeouts, setting these to 1 then parse off "K+" text
#   b. POCS: Create dummy variable for Pick offs, setting them to 1 then parse off the "PO" text
# 4. Create credits field that contains the assists and putout codes, the stuff in the parentheses. Deal with later after collected from all play types.
# 5. Remove credits portion then update AdvBase to account for failed base movements.

DF[grep("CS", Outcome)]

DF <- DF[grep("CS", Outcome), SBattempt := 1] %>% 
  .[grep("CS", Outcome), SB := 0] %>% 
  .[grep("CS", Outcome), PO := ifelse(str_count(Outcome, "PO") != 0, 1, NA)] %>% 
  .[PO == 1, Outcome := gsub("PO", "", Outcome)] %>% 
  .[grep("K\\+CS", Outcome), SO := 1] %>% 
  .[grep("K\\+CS", Outcome), Outcome := gsub("K\\+CS", "CS", Outcome)] %>% 
  .[grep("CS", Outcome), Credits := gsub(".*\\(", "", Outcome)] %>% 
  .[grep("CS", Outcome), CS := gsub("\\(.*", "", Outcome)] %>% 
  .[!is.na(CS), CSprocess := gsub("CS", "", CS)] %>% 
  .[, CSprocess := ifelse(CSprocess == "H", "3XH", paste((as.numeric(CSprocess) - 1), "X", CSprocess, sep = ""))] %>% 
  .[!is.na(CSprocess), AdvBase := ifelse(is.na(AdvBase), CSprocess, paste(CSprocess, AdvBase, sep = ";"))] %>% 
  .[, -c("CSprocess"), with = F] %>% 
  .[grep("CS", Outcome), Outcome := NA]



# Looking at K+ and W+ Events ::-------------------------------------------------------------------
# 1. Update SO to account for K+ events, then remove K+ from the outcome. Leave the remains to be processed in bulk along with other codes
# 2. Create BB code to account for walks in W+ events, then remove the W+ and leave remains to be processed in bulk...

DF[grep("K\\+", Outcome)]
DF[grep("W\\+", Outcome)]

DF <- DF[grep("K\\+", Outcome), SO := 1] %>%  
  .[grep("W\\+", Outcome), BB := 1] %>% 
  .[, Outcome := gsub("K\\+|W\\+", "", Outcome)]



# Addressing successful stolen bases ::-------------------------------------------------------------
# 1. Update indicator for SBattempt
# 2. Update indicator for SB
# 3. Create process to update AdvBase to demonstrate base movements that occurred as a result from SB actions

DF[grep("SB", Outcome)]

DF <- DF[grep("SB", Outcome), SBattempt := 1] %>% 
  .[grep("SB", Outcome), SB := 1] %>%
  .[grep("SB", Outcome), SBprocess := Outcome ] %>% 
  .[!is.na(SBprocess), SBprocess := gsub("SB2", "1-2", SBprocess)] %>% 
  .[!is.na(SBprocess), SBprocess := gsub("SB3", "2-3", SBprocess)] %>% 
  .[!is.na(SBprocess), SBprocess := gsub("SBH", "3-H", SBprocess)] %>% 
  .[!is.na(SBprocess), SBprocess := gsub("SB1", "B-1", SBprocess)] %>% 
  .[!is.na(SBprocess), AdvBase := ifelse(!is.na(AdvBase), paste(AdvBase, SBprocess, sep = ";"), AdvBase)] %>% 
  .[!is.na(SBprocess), AdvBase := ifelse(is.na(AdvBase), SBprocess, AdvBase)] %>% 
  .[, -c("SBprocess"), with = F] %>% 
  .[grep("SB", Outcome), Outcome := NA]




# Addressing single hits ::----------------------------------------------------------------------
# 1. Create indicator for hits
# 2. Create indicator for singles
# 3. Create schema for attributing fielders that fielded the ball following the single
# 4. Update AdvBase to account for the single

DF[grep("S", Outcome)]

DF <- DF[grep("S", Outcome), H := 1] %>% 
  .[grep("S", Outcome), B1 := 1] %>% 
  .[grep("S", Outcome), Fielded := gsub("S", "", Outcome)] %>% 
  .[grep("S", Outcome), Outcome := NA] %>% 
  .[!is.na(B1), AdvBase := ifelse(is.na(AdvBase), "B-1", paste(AdvBase, "B-1", sep = ";"))]



# Addressing strike outs ::--------------------------------------------------------------
# 1. Update SO to account for strikes

DF[Outcome == "K"]

DF <- DF[Outcome == "K", SO := 1] %>% 
  .[Outcome == "K", Out := 1] %>% 
  .[Outcome == "K", Outcome := NA]


# Addressing Defensive Indifference ::--------------------------------------------------
# 1. Create indicator for defensive indifference

DF[grep("DI", Outcome)]

DF <- DF[grep("DI", Outcome), DI := 1] %>% 
  .[DI == 1, SB := 1] %>% 
  .[grep("DI", Outcome), Outcome := NA]


# Addressing ground rule doubles ::---------------------------------------------------
# 1. Create an indicator for ground rule doubles
# 2. Update AdvBase to account for the batter's movemen to second on ground rule doubles

DF[grep("DGR", Outcome)]

DF <- DF[grep("DGR", Outcome), DGR := 1] %>% 
  .[DGR == 1, AdvBase := ifelse(is.na(AdvBase), "B-2", paste(AdvBase, "B-2", sep = ";"))] %>% 
  .[DGR == 1, Outcome := NA]


# Addressing doubles and triple hits ::------------------------------------------------
# 1. Create indicators for doubles and triples
# 2. Update Fielded indicator to account for positions that fielded the double/triple
# 3. Update AdvBase to account for the batter's advancement, if necessary
# 4. Update hits indicator

DF[grep("D", Outcome)]
DF[grep("D", Outcome), table(Outcome)]

DF <- DF[grep("D", Outcome), B2 := 1] %>% 
  .[B2 == 1, Fielded := gsub("D", "", Outcome)] %>% 
  .[B2 == 1, AdvBase := ifelse(is.na(AdvBase), "B-2", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-2", sep = ";"), AdvBase))] %>% 
  .[B2 == 1, H := 1] %>% 
  .[B2 == 1, Outcome := NA]

DF[grep("T", Outcome), ]
DF[grep("T", Outcome), table(Outcome)]
DF[grep("T", Outcome), table(AdvBase)]

DF <- DF[grep("T", Outcome), B3 := 1] %>% 
  .[B3 == 1, Fielded := gsub("T", "", Outcome)] %>% 
  .[B3 == 1, AdvBase := ifelse(is.na(AdvBase), "B-3", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-3", sep = ";"), AdvBase))] %>% 
  .[B3 == 1, H := 1] %>% 
  .[B3 == 1, Outcome := NA]

# Addressing situations where the batter is hit by a pitch ::-------------------------------------------
# 1. Create indicator for Hit by Pitch
# 2. Update AdvBase for batter's progression to first

DF[grep("HP", Outcome)]
DF[Outcome == "HP"]

DF <- DF[Outcome == "HP", HBP := 1] %>% 
  .[HBP == 1, AdvBase := ifelse(is.na(AdvBase), "B-1", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-1", sep = ";"), AdvBase))] %>% 
  .[HBP == 1, Outcome := NA]


# Addressing Home runs ::-----------------------------------------------------------
# 1. Create an indicator for home runs
# 2. Update the hits indicator to account for home runs
# 3. Update the AdvBase indicator to account for the batter's progression from base to home

DF[grep("H", Outcome)]
DF[grep("H", Outcome), table(Outcome)]

DF <- DF[Outcome == "HR", HR := 1] %>% 
  .[HR == 1, H := 1] %>% 
  .[HR == 1, AdvBase := ifelse(is.na(AdvBase), "B-H", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-H", sep = ";"), AdvBase))] %>% 
  .[HR == 1, Outcome := NA]

# Addressing Wild pitches ::--------------------------------------------------
# 1. Create indicator for wild pitch thrown


DF[grep("WP", Outcome)]
DF[Outcome == "WP"]

DF <- DF[Outcome == "WP", WP := 1] %>% 
  .[WP == 1, Outcome := NA]


# Addressing base on balls ::----------------------------------------------
# 1. Update BB indicator to account for walks
# 2. Update AdvBase to account for batter's advancement to first following a walk

DF[grep("W", Outcome), table(Outcome)]

DF <- DF[Outcome == "W", BB := 1] %>% 
  .[Outcome == "W", AdvBase := ifelse(is.na(AdvBase), "B-1", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-1", sep = ";"), AdvBase))] %>% 
  .[Outcome == "W", Outcome := NA]


# Addressing intentional base on balls ::---------------------------------------------
# 1. Create indicator for intentional base on balls
# 2. Update AdvBase to account for the batters advance to first base

DF[Outcome == "IW"]
DF[Outcome == "IW", table(AdvBase)]

DF <- DF[Outcome == "IW", IBB := 1] %>% 
  .[Outcome == "IW", AdvBase := ifelse(is.na(AdvBase), "B-1", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-1", sep = ";"), AdvBase))] %>% 
  .[Outcome == "IW", Outcome := NA]


# Addressing pick off attempts ::-----------------------------------------------------------------------------------
# 1. Create temporary indicator for pick off attempts
# 2. Update Out field for successful pick off attempts
# 3. Create error code field for indication of who made error during pick off attempt. Use this later with other error situations
# 4. Update PO indicator for successful pick offs
# 5. Update credits field to account for assists and put outs for pick out attempts
# 6. Create field for base that the pick off was successful on. Use this later for creating BASES table.

DF[grep("PO", Outcome)]

DF <- DF[grep("PO", Outcome), POA := 1] %>% 
  .[POA == 1, Out := ifelse(str_count(Outcome, "E") == 0, 1, Out)] %>% 
  .[POA == 1 & str_count(Outcome, "E") > 0, ERR := Outcome] %>% 
  .[POA == 1 & !is.na(ERR), ERR := gsub(".*\\(", "", ERR)] %>% 
  .[POA == 1 & !is.na(ERR), ERR := gsub("\\)", "", ERR)] %>% 
  .[POA == 1 & is.na(ERR), PO := 1] %>% 
  .[POA == 1 & is.na(ERR), Credits := gsub(".*\\(|\\)", "", Outcome)] %>% 
  .[POA == 1 & is.na(ERR), PO_base := gsub("\\(.*", "", Outcome)] %>%
  .[, -c("POA"), with = F] %>% 
  .[grep("PO", Outcome), Outcome := NA]


# Addressing Errors on foul fly balls ::----------------------------------------------------------------------------------
# 1. Create indicator for error on foul fly balls
# 2. Update ERR so that it reflects the fielder who made the error on foul fly ball hit

DF[grep("FLE", Outcome)]

DF <- DF[grep("FLE", Outcome), FLE := 1] %>% 
  .[FLE == 1, ERR := gsub("FL", "", Outcome)] %>% 
  .[FLE == 1, Outcome := NA]


# Addressing other errors ::-------------------------------------------------------
# 1. Distinguish between fielder error codes and those that may have been initially caught but error happens later (5E4)
# 2. For fielding errors, update the ERR field
# 3. For fielding errors, update AdvBase for the batter's advancement to first


DF[grep("E", Outcome), table(Outcome)]
DF[substr(Outcome, 1, 1) == "E", table(Outcome)]

DF <- DF[substr(Outcome, 1, 1) == "E", ERR := Outcome] %>%
  .[substr(Outcome, 1, 1) == "E", AdvBase := ifelse(is.na(AdvBase), "B-1", ifelse(str_count(AdvBase, "B") == 0, paste(AdvBase, "B-1", sep = ";"), AdvBase))] %>% 
  .[substr(Outcome, 1, 1) == "E", Outcome := NA]

# Addressing situation where the pitcher commits a balk ::------------------------
# 1. Create an indicator variable for balks

DF[grep("BK", Outcome)]

DF <- DF[grep("BK", Outcome), BK := 1] %>% 
  .[BK == 1, Outcome := NA]

# Addressing situations where managers request reviews ::-------------------
# 1. Create indicator for MREV calls
# 2. Create ComIdx for merging MREV items to COM descriptions

DF[grep("MREV", Mods), table(Mods)]
DF[grep("MREV", Mods), table(Outcome)]

DF <- DF[grep("MREV", Mods), MREV := 1] %>% 
  .[MREV == 1, ComIdx := Idx + 1] %>% 
  .[, Mods := gsub("MREV|/MREV", "", Mods)]

# Addressing situations where umpires review plays ::--------------------------
# 1. Create an indicator for umpire reviews
# 2. Modify ComIdx to account for idx lines that may be in the com file

DF[grep("UREV", Mods), table(Mods)]
DF[grep("UREV", Mods), table(Outcome)]

DF <- DF[grep("UREV", Mods), UREV := 1] %>% 
  .[UREV == 1, ComIdx := Idx + 1] %>% 
  .[, Mods := gsub("UREV|/UREV", "", Mods)]

# Addressing Throwing error modifier codes ::------------------------ 
# 1. Create an indicator for throwing errors 

DF[grep("TH", Mods), table(Mods)]

DF <- DF[grep("TH", Mods), ERR_TH := 1] %>% 
  .[, Mods := gsub("TH|/TH|TH/", "", Mods)]

# Addressing passed balls ::------------------------
# 1. Create an indicator for passed balls

DF[grep("PB", Outcome), table(Outcome)]

DF <- DF[grep("PB", Outcome), PB := 1] %>% 
  .[grep("PB", Outcome), Outcome := NA]


DF[, table(is.na(Outcome))]/nrow(DF) * 100
DF[, table(Outcome)]
