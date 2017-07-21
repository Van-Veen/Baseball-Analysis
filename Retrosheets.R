
library(data.table)
library(magrittr)


fp <- "C:/Users/jstewart/Downloads/2016eve/2016LAN.EVN"

playCols <- c("Idx", "Category", "Inning", "HomeTeam", "RetroID", "Count", "Results", "Play" )

DF <- readLines(fp) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("play,", .)] %>% 
  .[-grep("com,", .)]


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


GMID <- readLines(fp) %>% 
  .[-grep("version,", .)] %>% 
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("id,", .)] %>% 
  strsplit(., ",") %>% 
  unlist(.) %>% 
  matrix(., ncol = 3, byrow = T) %>% 
  data.table(.) %>% 
  setnames(., c("V1", "V2", "V3"), c("Idx", "Category", "TeamDate")) %>% 
  .[, Idx := as.integer(Idx)]

INFO <- readLines(fp) %>% 
  .[-grep("version,", .)] %>%  
  paste(seq(1, length(.), by = 1), ., sep = ",") %>% 
  .[grep("info,", .)]



test <- merge(PLAYS, GMID, by = c("Idx", "Category"), all.x = T, all.y = T)

# Resources for Retrosheets
http://www.retrosheet.org/eventfile.htm#3
http://www.retrosheet.org/eventfile.htm#5
http://www.retrosheet.org/eventfile.htm

https://gist.github.com/bayesball/8892981