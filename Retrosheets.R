library(data.table)
library(magrittr)


fpw <- "C:/Users/jstewart/Downloads/2016eve/2016LAN.EVN"
fp <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/2016eve/2016LAN.EVN"

playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )

DF <- readLines(fp) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") 


PLAYS <- readLines(fp) %>% 
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

PR <- PLAYS[, c("Idx", "Results", "Play"), with = F] %>% 
  .[, New := strsplit(Results, "")] %>%
  .[, New := paste(Idx, New, sep = ",")] %>% 
  .[, New := gsub("character\\(0\\)", "NP", New)] %>% 
  .[, New := gsub("c\\(", "", New)] %>% 
  .[, New := gsub("\\)", "", New)] %>% 
  .[, New := gsub("\"", "", New)] %>% 
  .[, New := gsub(" ", "", New)]

RSprep <- function(){
  
  playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )
  
  DF <- readLines(fpw) %>% 
    .[-grep("version,", .)] %>% 
    paste(seq(1, length(.), by = 1), ., sep = ",") 
  
  PLAYS <- DF[grep("play,", DF)] %>% 
    .[-grep("com,", .)] %>% 
    strsplit(., ",") %>% 
    unlist(.) %>% 
    matrix(., ncol = 8, byrow = T) %>% 
    data.table(.) %>% 
    setnames(., c(paste("V", seq(1, ncol(.), 1), sep = "")), playCols)
  
  PR <- PLAYS[, c("Idx", "Results", "Play"), with = F] %>% 
    .[, New := strsplit(Results, "")] %>%
    .[, New := paste(Idx, New, sep = ",")] %>% 
    .[, New := gsub("character\\(0\\)", "NP", New)] %>% 
    .[, New := gsub("c\\(", "", New)] %>% 
    .[, New := gsub("\\)", "", New)] %>% 
    .[, New := gsub("\"", "", New)] %>% 
    .[, New := gsub(" ", "", New)]
  
  maxCol <- PR[, max(nchar(Results))]
  
  write.table(PR$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
  
  PRP <- read.csv("C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv",
                  col.names = c("Idx", paste("P", seq(1, maxCol, by = 1), sep = "")),
                  fill = T, header = F) %>% 
    data.table(.) %>% 
    melt(., id.vars = "Idx") %>% 
    .[order(Idx)] %>% 
    .[value != ""]
  
  return(PRP)
}

write.table(PR$New, "/Users/joelstewart/Desktop/Test.csv", sep = ",", col.names = F, row.names = F, quote = F)
write.table(PR$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)


PR2 <- read.csv("C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", 
                col.names = c("Idx", paste("P", seq(1, 16, 1), sep = "")),
                fill = T, header = F) %>% 
  data.table(.) %>% 
  melt(., id.vars = "Idx") %>% 
  .[order(Idx)] %>% 
  .[value != ""]

test <- merge(PLAYS, PR2, by = "Idx", all.x = T, all.y = T)

GMID <- readLines(fpw) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("id,", .)] %>% 
  strsplit(., ",") %>% 
  unlist(.) %>% 
  matrix(., ncol = 3, byrow = T) %>% 
  data.table(.) %>% 
  setnames(., c("V1", "V2", "V3"), c("Idx", "Category", "TeamDate")) %>% 
  .[, Idx := as.integer(Idx)]

INFO <- readLines(fpw) %>% 
  .[-grep("version,", .)] %>%  
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("info,", .)]

START <- readLines(fp) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("start,", .)]

test <- merge(PLAYS, GMID, by = c("Idx", "Category"), all.x = T, all.y = T)

# Resources for Retrosheets
http://www.retrosheet.org/eventfile.htm#3
http://www.retrosheet.org/eventfile.htm#5
http://www.retrosheet.org/eventfile.htm

https://gist.github.com/bayesball/8892981



# Finalized work ::----------------------------------

RSprep <- function(){
  
  playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )
  
  DF <- readLines(fp) %>% 
    .[-grep("version,", .)] %>% 
    paste(seq(1, length(.), by = 1), ., sep = ",") 
  
  PLAYS <- DF[grep("play,", DF)] %>% 
    .[-grep("com,", .)] %>% 
    strsplit(., ",") %>% 
    unlist(.) %>% 
    matrix(., ncol = 8, byrow = T) %>% 
    data.table(.) %>% 
    setnames(., c(paste("V", seq(1, ncol(.), 1), sep = "")), playCols)
  
  PR <- PLAYS[, c("Idx", "Results", "Play"), with = F] %>% 
    .[, New := strsplit(Results, "")] %>%
    .[, New := paste(Idx, New, sep = ",")] %>% 
    .[, New := gsub("character\\(0\\)", "NP", New)] %>% 
    .[, New := gsub("c\\(", "", New)] %>% 
    .[, New := gsub("\\)", "", New)] %>% 
    .[, New := gsub("\"", "", New)] %>% 
    .[, New := gsub(" ", "", New)]
  
  maxCol <- PR[, max(nchar(Results))]
  
  #write.table(PR$New, "C:/Users/jstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
  write.table(PR$New, "/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv", sep = ",", col.names = F, row.names = F, quote = F)
  
  PRP <- read.csv("/Users/joelstewart/Desktop/Baseball Analysis/temp_files/pitches_parsed.csv",
                  col.names = c("Idx", paste("P", seq(1, maxCol, by = 1), sep = "")),
                  fill = T, header = F) %>% 
    data.table(.) %>% 
    melt(., id.vars = "Idx") %>% 
    .[order(Idx)] %>% 
    .[value != ""]
  
  return(PRP)
}


test <- RSprep()



countMaker <- function(pitchResult){
  
  balls <- rep(0, length(pitchResult))
  strikes <- rep(0, length(pitchResult))
  k_count <- 0
  
  for(i in 1:length(pitchResult)){
    if(pitchResult[i] == "B"){
      balls[i] <- 1
    }else if(pitchResult[i] == "K"){
      k_count <- k_count + 1
      strikes[i] <- 1
    }else if(pitchResult[i] == "F"){
      
      if(k_count < 2){
        k_count <- k_count + 1
        strikes[i] <- 1
      }
      
    }else if(pitchResult[i] == "C"){
      k_count <- k_count + 1
      strikes[i] <- 1
    }else if(pitchResult[i] == "I"){
      balls[i] <- 1
    }else if(pitchResult[i] == "L"){
      k_count <- k_count + 1
      strikes[i] <- 1
    }else if(pitchResult[i] == "M"){
      k_count <- k_count + 1
      strikes[i] <- 1
    }
  }
  
  balls <- cumsum(balls)
  strikes <- cumsum(strikes)
  result <- paste(balls, strikes, sep = "")
  
  return(result)
}

testResults <- c("B", "B", "K", "F", "F", "F")
countMaker(testResults)

test2 <- test[Idx %in% c(46:49)] %>% 
  .[, cnt := countMaker(value), by = Idx]

test3 <- RSprep() %>% 
  .[, cnt := countMaker(value), by = Idx] %>% 
  .[, variable := gsub("P", "", variable)] %>% 
  .[, variable := as.integer(variable)] #%>% 
  #.[, mv :=  max(variable), by = Idx] %>% 
  #.[variable == mv]



#NOTE: In attempting to figure out how to parse out pitcher vs. batter outcomes, I found that there are often two (and potentially more) rows
# that may be present in a batter vs. pitcher scenario. One situation I've found is when the pitcher throws a wild pitch (WP). Wild pitches are only
# considered as such when any player advances a plate during the errant pitch. In the database, I've found that there will be more than one row, and 
# the pitch array looks normal until the wild pitch is thrown. Then, in the next row, the pitch array is continued, maintaining all that exists in the row before
# it, and containing all that happened after the wild pitch. Need to figure out how to address this scenario....


WPs <- PLAYS[grep("WP", Play)]$Idx
PLAYS[Idx %in% c(WPs, WPs + 1)] %>% .[order(Idx)] %>%  head(., 51)

NPs <- PLAYS[grep("\\.", Results)]$Idx
PLAYS[Idx %in% c(WPs, WPs + 1) == F,] %>% .[Idx %in% c(NPs, NPs - 1)] %>% .[order(Idx)] %>% head(.)

test4 <- PLAYS[Idx %in% c(46:55)]

iterationCount <- function(x){
  
  previousName <- x[1]
  iterative <- c(1, rep(0, length(x) - 1))
  counter <- 1
  
  for(i in 2:length(x)){
    if(x[i] == previousName){
      counter <- counter + 1
      iterative[i] <- counter
      previousName <- x[i]
    }else{
      iterative[i] <- 1
      previousName <- x[i]
      counter <- 1
    }
  }
  
  return(iterative)
}


PLAYS <- PLAYS[, batterRep := iterationCount(RetroID)]

# List of items that constitue what I'm calling a New Line Play, where a play occurs
# WP: Wild Pitch



