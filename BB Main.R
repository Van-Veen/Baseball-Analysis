

# packages ::-----------------------------------
library(data.table)
library(magrittr)
library(lubridate)

# Root File ::------------------------------------
fp <- "/Users/joelstewart/Desktop/Baseball Analysis/Data/"

# Functions ::-----------------------------------
getPlayer <- function(playerName){
  DF <- fread("/Users/joelstewart/Desktop/Baseball Analysis/Data/Master.csv") %>% 
    .[, fullName := paste(nameFirst, nameLast, sep = " ")] %>% 
    .[fullName == playerName]
  
  return(DF)
}

batting <- function(playerName){
  x <- getPlayer(playerName)$playerID
  DF <- fread("/Users/joelstewart/Desktop/Baseball Analysis/Data/Batting.csv") %>% 
    .[playerID == x]
  
  return(DF)
}


# Scratch ::--------------------------------------

DF <- fread(paste(fp, "Master.csv", sep = "")) %>% 
  .[, fullName := paste(nameFirst, nameLast, sep = " ")] %>% 
  .[, birthDate := as.Date(paste(birthYear, birthMonth, birthDay, sep = "-"))] %>% 
  .[is.na(deathMonth) == F, deathMonth := ifelse(nchar(deathMonth) == 1, paste("0", deathMonth, sep = ""), deathMonth)] %>% 
  .[is.na(deathDay) == F, deathDay := ifelse(nchar(deathDay) == 1, paste("0", deathDay, sep = ""), deathDay)] %>% 
  .[is.na(deathYear) == F, deathDate := as.Date(paste(deathYear, deathMonth, deathYear, sep = "-"))]


DF[fullName == "Hank Aaron", (today() - birthDate)/365]


# Things to do ::-----------------------
# 1. Master table: paste together birth year, month, and date to make a date string
# 2. Add new variable for age