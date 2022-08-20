#Dataset
avomrtposorneg<-read.csv('data_avomrt_lexicon.csv', sep = ';')
avomrtposorneg$Sentimen <- as.factor(avomrtposorneg$Sentimen)
avomrtposorneg$Text <- as.factor(avomrtposorneg$Text)
str(avomrtposorneg)
View(avomrtposorneg)
which(!complete.cases(avomrtposorneg))
table(avomrtposorneg$Sentimen)
prop.table(table(avomrtposorneg$Sentimen))
avomrtposorneg$Text <- as.character(avomrtposorneg$Text)
avomrtposorneg$Textlength <-nchar(avomrtposorneg$Text)
library("tm")
avomrt_corpus <-Corpus(VectorSource(avomrtposorneg$Text))
print(avomrt_corpus)
inspect(avomrt_corpus)

#Membuat DTM
avomrt_dtm <-DocumentTermMatrix(avomrt_corpus)
inspect(avomrt_dtm[1:5, 1:5])

#inspect(avomrt_dtm)
#Algoritma Naive Bayes
#bagi menjadi dtm training dan test
#data latih semua data
avomrt_train <- avomrtposorneg[1:2572,]
avomrt_train

#data latih sentimen
avomrtposorneg_train_labels <- avomrtposorneg[1:2572,]$Sentimen
avomrtposorneg_train_labels
table(avomrtposorneg_train_labels)
prop.table(table(avomrtposorneg_train_labels))

avomrt_dtm_train <- avomrt_train[1:2572,]
avomrt_dtm_train

#Memisahkan data training menjadi positif atau negatif
negatif <-subset(avomrtposorneg_train_labels, avomrtposorneg$Sentimen =="Negatif")
positif <-subset(avomrtposorneg_train_labels, avomrtposorneg$Sentimen =="Positif")

#data test semua data
avomrt_test<-avomrtposorneg[2573:2858,]
avomrt_test

#data test sentiment
avomrtposorneg_test_labels <- avomrtposorneg[2573:2858,]$Sentimen
avomrtposorneg_test_labels
table(avomrtposorneg_test_labels)
prop.table(table(avomrtposorneg_test_labels))

avomrt_dtm_test <-avomrt_dtm[2573:2858,]

#Memisahkan data test menjadi positif atau negatif
negatif <-subset(avomrtposorneg_test_labels, avomrtposorneg$Sentimen =="Negatif")
positif <-subset(avomrtposorneg_test_labels, avomrtposorneg$Sentimen =="Positif")

#Membuat DTM training dan test
#dtm
avomrt_dtm_train <-avomrt_dtm[1:2572,]
avomrt_dtm_train
avomrt_dtm_test <-avomrt_dtm[2573:2858,]
avomrt_dtm_test

#Menentukan kalimat yang sering muncul
frequent_words <- findFreqTerms(avomrt_dtm_train,5)
frequent_words
length(frequent_words)
#Lalu lihat beeberapa kata
frequent_words[1:8]

#Membuat DTM menggunakan frequent words
avomrt_freq_word_train <- avomrt_dtm_train[,frequent_words]
avomrt_freq_word_train
avomrt_freq_word_test <- avomrt_dtm_test[,frequent_words]
avomrt_freq_word_test

#Membuat sebuah fungsi yes or no menggunakan algoritma Naive Bayes
avomrtyesorno <-function(x){
  y<-ifelse(x>0,1,0)
  y<-factor(y, levels=c(0,1), labels=c("No","Yes"))
  y}

#Gunakan fungsi pada avomrt_train dan avomrt_test DTM untuk mengetahui the presence of word
avomrt_train <- apply(avomrt_freq_word_train,2,avomrtyesorno)
data.frame(avomrt_train)
avomrt_test <- apply(avomrt_freq_word_test,2,avomrtyesorno)
data.frame(avomrt_test)

library("e1071")
#Melatih data training dengan Naive Bayes Classifier dengan laplace=1
avomrt_classifier <- naiveBayes(avomrt_train, avomrtposorneg_train_labels, laplace=1)
system.time(avomrt_classifier <- naiveBayes(avomrt_train, avomrtposorneg_train_labels, laplace=1))
avomrt_classifier
class(avomrt_classifier)

#Menguji data test
avomrt_test_pred <-predict(avomrt_classifier, newdata=avomrt_test)
system.time(avomrt_test_pred <- predict(avomrt_classifier, newdata=avomrt_test))
avomrt_test_pred
table(avomrt_test_pred)
avomrt_test_predict <- predict(avomrt_classifier, newdata=avomrt_test)
avomrt_test_predict
system.time(pred <-predict(avomrt_classifier, newdata=avomrt_test))
table(avomrt_test_pred) 
table(avomrtposorneg_test_labels)
prop.table(table(avomrt_test_pred, avomrtposorneg_test_labels))

#Lihat hasilnya dalam bentuk tabel
library("gmodels")
CrossTable(avomrt_test_pred, avomrtposorneg_test_labels, prop.chisq = F, prop.t = 
             F,dnn=c("Actual","Predict"))
table("Sebenarnya"=avomrtposorneg_test_labels,"Prediksi"=avomrt_test_pred)

#Menghitung akurasi perhitungan menggunakan confusion matrix
library("lattice")
library("ggplot2")
library("rlang")
library("glue")
library("caret")
conf.mat <-confusionMatrix(avomrt_test_pred, avomrtposorneg_test_labels)
conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']

write.csv(avomrt_test_pred, file='data_avomrt_pengujiansistem.csv', row.names = F)
write.csv(avomrtposorneg_test_labels, file='data_avomrt_testlabel.csv', row.names = F)
avomrt_test_pred
table(avomrt_test_pred)
avomrtposorneg_test_labels
table(avomrtposorneg_test_labels)
View(avomrtposorneg_test_labels)
View(avomrt_test_pred)
write.csv(avomrt_test_pred, file='data_avomrt_testpred.csv', row.names = F)

#Histogram
library("lattice")
histogramavomrt_negatif <- which(avomrtposorneg$Sentimen =="Negatif")
histogramavomrt_positif <- which(avomrtposorneg$Sentimen =="Positif")
hist1 <- hist(avomrtposorneg$Textlength ,main="Histogram Berdasarkan Panjang Karakter",col = "green")
hist2 <- hist(histogramavomrt_negatif, main="Histogram Data Sentimen Negatif", col="red")
hist3 <- hist(histogramavomrt_positif, main="Histogram Data Sentimen Positif", col="yellow")


#Diagram Pie
library(plotrix)
mytable <-table(avomrtposorneg$Sentimen)
lbls <- paste(names(mytable),"\n", mytable, sep="")
pie3D(mytable, labels=lbls, radius=0.7,explode=0.3,shade=0.8, theta=0.4, main="Diagram Pie Data 
Sentimen\n (Data Positif dan Negatif)")

#Membuat Wordcloud
class(negatifavomrt_cloud)
class(positifavomrt_cloud)
negatif_cloud <- which(avomrtposorneg$Sentimen =="Negatif")
positif_cloud <- which(avomrtposorneg$Sentimen =="Positif")
avomrt_corpus <- Corpus(VectorSource(avomrtposorneg$Text))
print(avomrt_corpus)
inspect(avomrt_corpus)
library("wordcloud")
library("RColorBrewer")