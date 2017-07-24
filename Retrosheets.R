
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

write.table(PR$New, "/Users/joelstewart/Desktop/Test.csv", sep = ",", col.names = F, row.names = F, quote = F)

PR2 <- read.csv("/Users/joelstewart/Desktop/Test.csv", 
                 col.names = c("Idx", paste("P", seq(1, 16, 1), sep = "")),
                 fill = T, header = F) %>% 
  data.table(.) %>% 
  melt(., id.vars = "Idx") %>% 
  .[order(Idx)] %>% 
  .[value != ""]

test <- merge(PLAYS, PR2, by = "Idx", all.x = T, all.y = T)

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
