library(devtools)
library(textclean)
library(dplyr)
library(tm)
library(tokenizers)
library(katadasaR)
library(corpus)
library(readr)
library(readxl)
library(writexl)
devtools::install_github("nurandi/katadasaR")

#read .csv
socoreview <- read.csv("avomrt_soco.csv", header=TRUE, sep =';')

#casefolding
socoreview=tolower(socoreview)

#Tokenizing
#remove all punctuations:
socoreview = gsub("[[:punct:]]", " ", socoreview)
socoreview
#remove numbers, kita hanya butuh teks untuk analytics
socoreview = gsub("[[:digit:]]", " ", socoreview)
socoreview

#mengganti kata negasi
socoreview = gsub("\\stidak\\s"," tidak ",socoreview)
socoreview = gsub("\\stak\\s"," tidak ",socoreview)
socoreview = gsub("\\stdk\\s"," tidak ",socoreview)
socoreview = gsub("\\sga\\s"," tidak ",socoreview)
socoreview = gsub("\\sgk\\s"," tidak ",socoreview)

#Filtering 
socoreview <- strip(socoreview)
socoreview <- socoreview[!duplicated(socoreview)]

#Meengubah kata baku
spell.lex <- read.csv("KamusBaku.csv", sep = ';')
socoreview <- replace_internet_slang(socoreview, slang = paste0
                                     ("\\b", spell.lex$slang, "\\b"), 
                                     replacement = spell.lex$formal, ignore.case =  TRUE)


#Stopword
stopword_bindo <- read.csv("stp.csv", header = FALSE)
stopword_bindo <- as.character(stopword_bindo$V1) 
stopword_bindo <- c(stopword_bindo, stopwords()) 
socoreview <- removeWords(socoreview, stopword_bindo)
socoreview <- stripWhitespace(socoreview)


#Stemming
stem <- read.csv("data_avomrt_stopword.csv")
stem <- as.character(stem$x)
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}
stem <- lapply(tokenize_words(stem[]), stemming)
stem <- data.frame(text=unlist(sapply(stem,'[')))