library(data.table)
library(magrittr)
library(zoo)


# The File path and working environment for this test dataset
fpw <- "C:/Users/jstewart/Downloads/2016eve/2016LAN.EVN"
fpw <- "/Users/joelstewart/Downloads/2016eve/2016LAN.EVN"
# Writing an array for new column names to add to the lines we are going to read into working memory
playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )

# Reading in all the lines for the test dataset, lines that start with the now defunct 'version' tag. We also add an index number for reconstituting the dataset
DF <- readLines(fpw) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") 

# Here we read in the lines that begin with the 'play' tag. We further omit lines that start with the 'com' tag, parsing everything that remains
# into a data table by splitting everything by commas, unlisting, transforming everything into a matrix with specified columns, and adding the names
# from above
PLAYS <- readLines(fpw) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("play,", .)] %>% 
  .[-grep("com,", .)] %>% 
  strsplit(., ",") %>% 
  unlist(.) %>% 
  matrix(., ncol = 8, byrow = T) %>% 
  data.table(.) %>% 
  setnames(., c(paste("V", seq(1,ncol(.),1), sep = "")), playCols) %>% 
  .[, Idx := as.integer(Idx)]

# This function produces an identifier for when the Idx number increases but the PvB scenario is the same. This usually happens when a play not involving
# the PvB context occurs, such as a wild pitch (WP) that allows a baserunner to advance or, most often it seems, when there are player substitutions. I believe
# that this function will have other uses down the road.
batterIndex <- function(x){
  
  bdex <- rep(0, length(x))
  bdex[1] <- 1
  counter <- 1
  
  for(i in 2:length(x)){
    
    if(x[i] == x[i - 1]){
      
      counter <- counter + 1
      bdex[i] <- counter
      
    }else{
      
      counter <- 1
      bdex[i] <- counter
      
    }
    
  }
  
  return(bdex)
  
}

# After some examination, I found that lines where the result is NP usually follow a player substitution. These don't involve any PvB outcomes or advancements
# on bases. For the current examination, I'm just going to omit lines where Results == NP. After doing this, the batterIndex result maximums decreased from 
# eight to three. We will dig down to figure out how to parse out these situations by looking at a specific PvB scenario with a batterIndex of 3
PLAYS <- PLAYS[Play != "NP"]
PLAYS <- PLAYS[, batterCount := batterIndex(RetroID)]

# Here we isolate a situation where the batterIndex is 3, allowing us to figure out how to parse these data out
test <- PLAYS[Idx %in% c(7737:7739)]

# First, I'm going to isolate down to some variables so that I can create a long dataset where there is a single outcome for every pitch. One obstacle to this
# is that in a repeat PvB scenario where the Idx number increases, the results of the previous pitches is contained in the next line, making them redundant. 
# We overcome this obstacle by using the sub command in order to omit everything in the Results string that precedes the standard symbol for a previous play
# or event, the period ("."). Then we split out the codes in the Results field, separating each letter by a comma, adding on the Idx number before hand. We 
# write this out to a temporary file, calculate a max column variable so that when we read the data.frame back in we can specify how many columns there should
# be. For example, if a given Idx line has a Result of "BBCB", yet the subsequent Results field only have items where nchar is 4 or less, then reading these 
# data back in will create length issues. By setting the column length, shorter Results fields parse out extra columns with NAs, as specified in the function.
# This is useful, because we can then melt the dataset into a long format and omit rows where the pitch count field contains an NA. Just run it piecemeal if this
# is confusing.
T1 <- test[, c("Idx", "Category", "RetroID", "Results"), with = F]

T1 <- T1[, Results := sub(".*\\.", "", Results)] %>% 
  .[, New := strsplit(Results, "")] %>%
  .[, New := paste(Idx, New, sep = ",")] %>% 
  .[, New := gsub("character\\(0\\)", "NP", New)] %>% 
  .[, New := gsub("c\\(", "", New)] %>% 
  .[, New := gsub("\\)", "", New)] %>% 
  .[, New := gsub("\"", "", New)] %>% 
  .[, New := gsub(" ", "", New)]


maxCol <- T1[, max(nchar(Results))]

#write.table(T1$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
write.table(T1$New, "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
# Reading the table back in. This should result in a long dataset where there is a single pitch result for every single row

pp <- "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv"
pp <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv"

T2 <- read.csv(pp, col.names = c("Idx", paste("P", seq(1, maxCol, by = 1), sep = "")),
               fill = T, header = F) %>% 
  data.table(.) %>% 
  melt(., id.vars = "Idx") %>% 
  .[order(Idx)] %>% 
  .[value != ""]


T2 <- T2 %>% 
  setnames(., c("value", "variable"), c("Result", "PitchCount"))  


T3 <- test %>% 
  .[, c("Idx", "Category", "Inning", "HomeTeam", "RetroID"), with = F] %>% 
  merge(., T2, all.x = T, all.y = T, by = "Idx") %>% 
  .[, PlayCount := batterIndex(RetroID)]


# Based on the schema drawn up in the Parsing Schema, I should be able to do the following:

# 1. isolate test again into a different dataset, containing Idx, Results, and Play
# 2. remove periods from the Results field
# 3. create a new derived variable that is the number of characters of the Results field after periods have been removed
# 4. parse down this dataset so that you have the idx number, the nchar derived field, and the play field
# 5. You can then join this dataset to T3, joining on both Idx and PlayCount. May need to rename variables for merging purposes

P1 <- test[, c("Idx", "Results", "Play"), with = F] %>% 
  .[, Results := gsub("\\.", "", Results)] %>% 
  .[, PlayCount := nchar(Results)] %>% 
  .[, c("Idx", "Play", "PlayCount"), with = F] 


T4 <- merge(T3, P1, all.x = T, all.y = T, by = c("Idx", "PlayCount"))

# Now, I've finally figured out how to transform the PvB context to a long format, dealing with repeat PvB situations that are interrupted by a play
# or substitution, and merging back on the plays at the appropriate point in the PvB long format. There is much more to be done, but for now, I am going to
# wrap up what I have into a function and then do the following:

# 1. Test this functionality on a smaller subset of our test dataset, with multiple PvB face-offs. Overall, the aim here is to ensure that the process
# are performed correctly for each unit of grouping

# 2. Adding in some derived variables, for things like pitch count, total pitches, results

RSparse <- function(){
  
  #fpw <- "C:/Users/jstewart/Downloads/2016eve/2016LAN.EVN"
  fpw <- "/Users/joelstewart/Downloads/2016eve/2016LAN.EVN"
  
  playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )
  
  PLAYS <- readLines(fpw) %>% 
    .[-grep("version,", .)] %>% 
    paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
    .[grep("play,", .)] %>% 
    .[-grep("com,", .)] %>% 
    strsplit(., ",") %>% 
    unlist(.) %>% 
    matrix(., ncol = 8, byrow = T) %>% 
    data.table(.) %>% 
    setnames(., c(paste("V", seq(1,ncol(.),1), sep = "")), playCols) %>% 
    .[, Idx := as.integer(Idx)]
  
  PLAYS <- PLAYS[Play != "NP"]
  
  T1 <- PLAYS[, c("Idx", "Category", "RetroID", "Results"), with = F]
  
  T1 <- T1[, Results := sub(".*\\.", "", Results)] %>% 
    .[, New := strsplit(Results, "")] %>%
    .[, New := paste(Idx, New, sep = ",")] %>% 
    .[, New := gsub("character\\(0\\)", "NP", New)] %>% 
    .[, New := gsub("c\\(", "", New)] %>% 
    .[, New := gsub("\\)", "", New)] %>% 
    .[, New := gsub("\"", "", New)] %>% 
    .[, New := gsub(" ", "", New)]
  
  
  maxCol <- T1[, max(nchar(Results))]
  
  #write.table(T1$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
  write.table(T1$New, "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
  
  #pp <- "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv"
  pp <- "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv"
  
  T2 <- read.csv(pp, col.names = c("Idx", paste("P", seq(1, maxCol, by = 1), sep = "")),
                 fill = T, header = F) %>% 
    data.table(.) %>% 
    melt(., id.vars = "Idx") %>% 
    .[order(Idx)] %>% 
    .[value != ""]
  
  
  T2 <- T2 %>% 
    setnames(., c("value", "variable"), c("Result", "PitchCount"))  
  
  
  T3 <- PLAYS %>% 
    .[, c("Idx", "Category", "Inning", "HomeTeam", "RetroID"), with = F] %>% 
    merge(., T2, all.x = T, all.y = T, by = "Idx") %>% 
    .[, PlayCount := batterIndex(Idx)]
  
  P1 <- PLAYS[, c("Idx", "Results", "Play"), with = F] %>% 
    .[, Results := sub(".*\\.", "", Results)] %>% 
    .[, Results := gsub("\\.", "", Results)] %>% 
    .[, PlayCount := nchar(Results)] %>% 
    .[, c("Idx", "Play", "PlayCount"), with = F] 
  
  
  T4 <- merge(T3, P1, all.x = T, all.y = T, by = c("Idx", "PlayCount"))
  
  return(T4)
}

temp <- RSparse()

# At this point, the RSparse function does all of the processes from the first 130-ish lines. Now, here's a laundry list of things that need to 
# be folded into the function

# 1. The first item above seems to be fine for our test purposes. The RSparse function works as built on the whole test dataset.
# 2. Need to add pitch count. I think it is better to know the count prior to the pitch, that way one can look at what types of outcomes occur given a 
#    specific pitch sequence, say a full count (3-2) or in a 3-0 scenario. This will be particularly interesting later on when we join on the Pitch f/x data
# 3. A way to bring in the matchup metadata. Every row should probably have the date, the home team, and the opponent (visiting team).
# 4. I think everyline should have an indicator signifying who is actually pitching
# 5. Dummy indicators signifying which bases are occupied.
# 6. Base labels indicating the identifier of the player that is on the specified base
# 7. Dummy indicator for hits
# 8. Dummy indicator for RBIs
# 9. Dummy indicator for other outcomes, strikes, balls, strike outs, errors, runs, outs, etc...
# 10. Pitch count for the pitcher-vs-batter sequence
# 11. Total running Pitch Count for the pitcher
# 12. Maybe, variables H1 - H9, and V1 - V9, that contain the RetroID of the person playing in those positions, H being the home team and V being the visitors
# 13. Perhaps the most complicated, a verbal description decoding that which happens in the Play field

# These are just a few of the next steps for now. I'm sure that there will be a lot more and it is apparent now that this will end up being a pretty
# wide dataset. I will need to decide at some point if this should be segmented into different data tables and the desired information would involve functions
# that merge and calculate these datasets. I think that, at this point, I will focus on folding in the game metadata. We'll start with the results saved from
# the current iteration of the RSparse function. We'll need out initial dataset, DF, from the readLines function too. 

# Let's start by pulling lines with the 'info' tag

# After some exploration, it looks like there are some inconsistencies in the amount of line items for lines that contain the info tag. In info-tagged
# line items, there are 33 instances where the line describes saves that are only 3 items long. This throws off the conversion to a data.table. See
# the output of the following lines.

INFO <- DF[grep("info,", DF)] %>% 
  strsplit(., ",")

counter <- rep(0, length(INFO))

for(i in 1:length(INFO)){
  counter[i] <- length(INFO[[i]])
}

table(counter)


INFO[counter == 3]

# Lets take a closer look at items that contain the text 'save'. Checking to see if there are perhaps 4 item info lines that contain save info

DF[grep("save", DF)]

# It looks like these are situations where the RetroID is missing, or rather, there wasn't a save because of a lack of qualification. I would rather keep
# these lines instead of omitting them because it is likely that this info could be useful. I do want to be able to determine these outcomes based off the 
# play-by-play data itself, however the utility here still stands. Let's attach something to to the save lines missing an identifier so that we can parse
# these info lines into a four-column data.table

INFO <- DF[grep("info,", DF)]

# lets use the counter variable from above to index the ones that need to be changed

INFO[counter == 3] <- paste(INFO[counter == 3], "NA", sep = "")

# double checking the change

INFO[counter == 3]

# now we should be able to convert to a four column data.table

INFO <- INFO %>% 
  strsplit(., ",") %>% 
  unlist(.) %>% 
  matrix(., ncol = 4, byrow = T) %>% 
  data.table(.) %>% 
  setnames(., c("V1", "V2", "V3", "V4"), c("Idx", "Category", "Variable", "Value"))


# Now we have a useable data.table with every games metadata. At this time, I only want to bring in the following items into our play-by-play data

# 1. Date
# 2. Home Team
# 3. Visiting Team

META <- INFO[Variable %in% c("visteam", "hometeam", "date")] 

VISIT <- dcast(META[Variable == "visteam"], Idx ~ Variable, value.var = "Value") %>% 
  .[, Idx := as.numeric(Idx)] %>% 
  .[order(Idx, decreasing = F)] %>% 
  .[, matchSeq := seq(1, nrow(.), by = 1)]

HOME <- dcast(META[Variable == "hometeam"], Idx ~ Variable, value.var = "Value") %>% 
  .[, Idx := as.numeric(Idx)] %>% 
  .[order(Idx, decreasing = F)] %>% 
  .[, matchSeq := seq(1, nrow(.), by = 1)]

DATE <- dcast(META[Variable == "date"], Idx ~ Variable, value.var = "Value") %>% 
  .[, Idx := as.numeric(Idx)] %>% 
  .[order(Idx, decreasing = F)] %>% 
  .[, matchSeq := seq(1, nrow(.), by = 1)]

META <- DATE %>% 
  merge(., HOME, by = "matchSeq", all.x = T, all.y = T) %>% 
  merge(., VISIT, by = "matchSeq", all.x = T, all.y = T ) %>% 
  .[, c("Idx", "date", "hometeam", "visteam"), with = F] %>% 
  .[, date := as.Date(date)] %>% 
  setnames(., names(.), c("Idx", "Date", "homeLab", "visLab"))

# Here, I found it easier to create the desired data.table by breaking it out into smaller tables and bringing them back togetehr in the desired
# format. Initially, I wanted to cast these data to a wider format, but this was a bit difficult due to the uniqueness of the Idx variable...

# Now, with the META table, we have a dataset with the game's date, home team and visiting team. We can merge this with the PLAYS data, joining on
# Idx. Since Idx is a unique value, we will have the META rows file in iteratively to the main PLAYS data.table. I think then, using a function, we can fill
# in the remaining NAs in those joined columns, and then just pull off theses rows.

PLAYS <- RSparse() %>% 
  merge(., META, by = "Idx", all.x = T, all.y = T)

# I wrote the following function to fill in subsequent NA values with the most recent, previous, non NA value. It works, and I'm proud of it, but I noticed
# that it is kinda slow on my test data, which is only around 25K rows. After some research, I found there is a more optimal function in the zoo library
# that will work the same, and seemingly faster. I'm going to keep my function here, for pride's sake.

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


PLAYS <- RSparse() %>% 
  merge(., META, by = "Idx", all.x = T, all.y = T) %>% 
  .[, Date := na.locf(Date)] %>% 
  .[, homeLab := na.locf(homeLab)] %>% 
  .[, visLab := na.locf(visLab)] %>% 
  .[!is.na(Category)]

# Here, everything processed and joined on nicely. I will revisit later to see what other variables will be good to join on here.

# Next, we'll look at folding in information for starters and their respective positions, ensuring that any substitiution occurs at the appropriate point
# in the sequence and is reflected at the appropriate play index in the finalized data.table

START <- DF[grep("start,", DF)]
SUB <- DF[grep("sub,", DF)]

# It looks like these will be constructed into pretty clean data.tables, with both the START and SUB tables having the same number of columns. One thing
# I will want to do is cast out these data so that the RetroID fills each position number for both the home and visiting teams

POS <- DF[grep("start,|sub,", DF)] %>% 
  strsplit(., ",") %>% 
  unlist(.) %>% 
  matrix(., ncol = 7, byrow = T) %>% 
  data.table(.) %>% 
  setnames(., c(paste("V", seq(1, ncol(.), by = 1), sep = "")), c("Idx", "Category", "RetroID", "PlayerName", "Home", "BatOrder", "Position"))

STARTS <- POS[, Idx := as.numeric(Idx)] %>% 
  merge(META, ., by = "Idx", all.x = T, all.y = T) %>% 
  .[, Date := na.locf(Date)] %>% 
  .[!is.na(RetroID)] %>% 
  .[Category == "start"] %>% 
  .[, Position := paste("P", Position, sep = "")] %>% 
  dcast(., Date + Category + Home  ~ Position, value.var = "RetroID")

minDateStart <- POS[, Idx := as.numeric(Idx)] %>% 
  merge(META, ., by = "Idx", all.x = T, all.y = T) %>% 
  .[, Date := na.locf(Date)] %>% 
  .[!is.na(RetroID)] %>% 
  .[Category == "start"] %>% 
  .[, list("Idx" = min(Idx)), by = c("Date", "Home")]

STARTS <- merge(STARTS, minDateStart, by = c("Date", "Home"), all.x = T, all.y = T)

SUB <- POS[, Idx := as.numeric(Idx)] %>% 
  merge(META, ., by = "Idx", all.x = T, all.y = T) %>% 
  .[, Date := na.locf(Date)] %>% 
  .[!is.na(RetroID)] %>% 
  .[Category == "sub"] %>% 
  .[, c("Idx", "Date", "RetroID", "Position", "Home"), with = F] %>% 
  .[, Position := paste("P", Position, sep = "")] %>% 
  dcast(., Date + Idx + Home ~ Position, value.var = "RetroID") %>% 
  .[, Sub := rep(1, nrow(.))] %>% 
  .[, c("Idx", "Date", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",
        "P11", "P12", "Home", "Sub"), with = F]

STARTS <- STARTS[, P11 := rep(NA, nrow(STARTS))] %>% 
  .[, P12 := rep(NA, nrow(.))] %>% 
  .[, Sub := rep(0, nrow(.))] %>% 
  .[, c(names(SUB)), with = F] %>% 
  .[, Home := as.numeric(Home)]

STARTS <- PLAYS[, Home := ifelse(HomeTeam == 0, 1, 0)] %>% 
  merge(., STARTS[, c(-1), with = F], by = c("Date", "Home"), all.x = T) %>% 
  .[order(Idx)]

# This process layers in the players and their respective positions relative to each pitch and play result at the moment. Now we need to figure out how to
# incorporate player substitutions

newSubCols <- c("PlayCount", "Category", "Inning", "HomeTeam", "RetroID", "PitchCount", "Result", "Play", "homeLab", "visLab")

SUB <- SUB[, (newSubCols) := rep(NA, nrow(SUB))] %>% 
  .[, c(names(STARTS)), with = F]

PLAYS <- rbind(STARTS, SUB) %>% 
  .[order(Idx)]

# After thorough exploration, I've figured out a way to layer in RetroID identifiers for every defensive position in place while a specific batter is
# at the plate. This is done with the subFiller function, and what it does is create a crosswalk based on the data frame specified in the df parameter. This
# crosswalk is split into two, for home and away teams, and it has the index position for when the player either started or was substituted into the game, as
# well as the date and indicator for home or away team. We then plug this info in in a piece wise fashion, indexing a range of our target data frame by Idx and
# checking some additional parameters and altering the position's identifier as needed. The postion that is most given to substitutions is P1, the pitcher, and
# running this function when targeting the P1 column, the processing speed isn't bad. However, I will want to look into optimizing and cleaning up this function
# after getting more comfortable with the datasets.

subFiller <- function(df, px){
  
  cw <- df[Sub == 1 & !is.na(px), c("Idx", px, "Home", "Date"), with = F] %>% na.omit(.) %>% 
    data.frame(.)
  
  cw2 <- df[, list("Idx" = max(Idx), t1 = "NA"), by = c("Date", "Home")] %>% 
    setnames(., "t1", px) %>% 
    .[, names(cw), with = F]
  
  cw3 <- df[, list("Idx" =  min(Idx)), by = c("Date", "Home")] %>% 
    merge(., df[, c("Idx", px), with = F], by = "Idx", all.x = T) %>% 
    unique(.) %>% 
    .[, names(cw), with = F]
  
  crossWalk <- rbind(cw, cw2, cw3) 
  crossWalk <- crossWalk[order(Date, Idx)]
  
  crossWalk_home <- crossWalk[Home == 1] %>% data.frame(.)
  
  crossWalk_home[, 2] <- ifelse(crossWalk_home[, 2] == "NA", NA, crossWalk_home[,2])
  crossWalk_home[, 2] <- na.locf(crossWalk_home[, 2])
  
  crossWalk_away <- crossWalk[Home == 0] %>% data.frame(.) 
  
  crossWalk_away[, 2] <- ifelse(crossWalk_away[, 2] == "NA", NA, crossWalk_away[,2])
  crossWalk_away[, 2] <- na.locf(crossWalk_away[, 2])
  
  for(i in 1:nrow(crossWalk_home) - 1){
    
    df[Idx >= crossWalk_home$Idx[i] & 
         Idx <= crossWalk_home$Idx[i + 1] & 
         Home == crossWalk_home$Home[i] & 
         Date == crossWalk_home$Date[i], 
       c(px) := crossWalk_home[i, 2]]
    
  }
  
  for(i in 1:nrow(crossWalk_away) - 1){
    
    df[Idx >= crossWalk_away$Idx[i] & 
         Idx <= crossWalk_away$Idx[i + 1] & 
         Home == crossWalk_away$Home[i] & 
         Date == crossWalk_away$Date[i], 
       c(px) := crossWalk_away[i, 2]]
    
  }
  
  return(df)
  
}

PLAYS <- subFiller(PLAYS, "P1")
PLAYS <- subFiller(PLAYS, "P2")
PLAYS <- subFiller(PLAYS, "P3")
PLAYS <- subFiller(PLAYS, "P4")
PLAYS <- subFiller(PLAYS, "P5")
PLAYS <- subFiller(PLAYS, "P6")
PLAYS <- subFiller(PLAYS, "P7")
PLAYS <- subFiller(PLAYS, "P8")
PLAYS <- subFiller(PLAYS, "P9")


# Let's take a look now at pinch hitters and pinch runners. I need to first look into the rules and regulations involving these substitutions to better understand
# the significance of this kind of event and how it can be statistically meaningful to track and analyze. In regards to pinch hitters, the actual batter who has
# been substituted in is already present in the RetroID field, so my intuition is to just create a dummy indicator that signifies that batter's at-bats at that 
# point in time represent the fact that he is pinch hitting. This would have to be done prior to parsing off the rows where RetroID == NA, those rows that
# are unnecessarily left over from the rows of starters and substitutes in the subFiller function. I will first try to tackle this, before taking a closer look
# at pinch runners

PLAYS$P11[1] <- "xxxx001"
PLAYS <- PLAYS[, P11 := na.locf(P11)]

# It seems we can run na.locf from the zoo package on P11, but only after we install something in the very first element of this array. This is because the
# function determines that the first element is already an NA, but it needs to replace the whole vector, so some of the replacements at the end of the vector
# get recycled to the front most NA elements of the targeted vector. Becasue of this, I install a dummy RetroID, xxxxx001, to the very first element. This should
# presumably work because it is very unlikely that a pinch hitter would be subbed in during the first at-bat of the first game of the season. After this, we can
# index on rows where the RetroID == P11, then create a dummy variable indicating that these pitch results belong to a batter that is serving as a pinch hitter

PLAYS <- PLAYS[, PinchHit := ifelse(RetroID == P11, 1, 0)]

# At this point, I'm not sure what to do about pinch runners or how having these info might be useful. For the moment, I think I'll omit these and revisit their
# utility at a latter date

PLAYS <- PLAYS[!is.na(RetroID), -c("P11", "P12", "Sub"), with = F]

# Now lets examine some of the things that are in the Play field, to see if there are clear ways of determining events. We'll start off by looking at the 
# distribution of whatever the first character of each Play string is. I suspect that these will help reveal other issues that exist in parsing out this 
# dataset.

PLAYS[!is.na(Play), table(substr(Play, 1, 1))]
PLAYS[!is.na(Play) & substr(Play, 1, 1) == "C"]

PLAYS[!is.na(Play) & substr(Play, 1,1) == "S", table(gsub("/", "", substr(Play, 1, 3)))] 
PLAYS[!is.na(Play) & substr(Play, 1,1) == "D", table(gsub("/", "", substr(Play, 1, 3)))] 






