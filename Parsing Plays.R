



# The File path and working environment for this test dataset
fpw <- "C:/Users/jstewart/Downloads/2016eve/2016LAN.EVN"

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

write.table(T1$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)

# Reading the table back in. This should result in a long dataset where there is a single pitch result for every single row

T2 <- read.csv("C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv",
                col.names = c("Idx", paste("P", seq(1, maxCol, by = 1), sep = "")),
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














