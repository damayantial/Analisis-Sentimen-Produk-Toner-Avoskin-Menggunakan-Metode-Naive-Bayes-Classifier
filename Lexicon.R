library(devtools)
library(tm)
library(stringr)
library(katadasaR)
library(NLP)
library(ggplot2)
library(reshape2)
library(corpus)
library(dplyr)

#lexicon (summary sentimen)
words.positive = scan("s-pos.txt", what='character', comment.char=';') 
words.negative = scan("s-neg.txt", what='character', comment.char=';')
summary.sentiment = function(sentences, words.positive, words.negative,  progress='none'){
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, words.positive,  words.negative) {
    require(plyr)
    require(stringr)
    require(katadasaR)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    words = sapply(words, katadasaR)
    pos.matches = match(words, words.positive)
    neg.matches = match(words, words.negative)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    summary = sum(pos.matches) - sum(neg.matches)
    sentimen = 
      list1=c(summary, pp, nn)
    return (list1)
  }, words.positive, words.negative)
  summary_new=lapply(list, `[[`, 1)
  pp1=summary=lapply(list, `[[`, 2)
  nn1=summary=lapply(list, `[[`, 3)
  summarys.df = data.frame(summary=summary_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  list_df=list(summarys.df, positive.df, negative.df)
  return(list_df) }
avomrt <- readLines('data_avomrt_stemming.csv')
avomrtResult = summary.sentiment(avomrt, words.positive, words.negative, progress='none')
View(avomrtResult)

##create table
#Creating a copy of result data frame
test1=avomrtResult[[1]]
test2=avomrtResult[[2]]
test3=avomrtResult[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL

#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
library("reshape2")
qq1=melt(q1, ,var='Summary')
qq2=melt(q2, ,var='Positif')
qq3=melt(q3, ,var='Negatif')
qq1['Summary'] = NULL
qq2['Positif'] = NULL
qq3['Negatif'] = NULL

#Creating data frame
table1 = data.frame(Text=avomrtResult[[1]]$text, Summary=qq1)
table2 = data.frame(Text=avomrtResult[[2]]$text, Summary=qq2)
table3 = data.frame(Text=avomrtResult[[3]]$text, Summary=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text,Positif=table2$value, 
                       Negatif=table3$value, Summary=table1$value)
table_final$Sentimen <- ifelse(table_final$Summary < 0, "Negatif","Positif")
positif = sum(table_final$Summary > 0)
negatif = sum(table_final$Summary < 0)

#positive Percentage
PosPc = table_final$Positif
negPC = table_final$Negatif
table_final$PosPercent = PosPc/ (PosPc+negPC)
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage
table_final$NegPercent = negPC/ (PosPc+negPC)
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn
View(table_final)

#Merging three data frames into one
write.csv(table_final, file = 'data_avomrt_lexicon.csv', row.names = F)

