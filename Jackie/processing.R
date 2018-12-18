library(tm)
library(dplyr)
library(ggplot2)
library(data.table)
library(wordcloud)
install.packages("topicmodels")

library(topicmodels)

WineDataset <- function(){
  df <- fread("Data Science Programming/wine-reviews/winemag-data-130k-v2.csv")
  return(df)
}
  
wine <- WineDataset()

create_corpus<-function(text_vector){
  text_corpus<-Corpus(VectorSource(text_vector),readerControl=list(language="english"))%>%
    tm_map(stripWhitespace)%>%
    tm_map(tolower)%>%
    tm_map(removeWords,c(stopwords("english"),"wine"))%>%
    tm_map(stemDocument)
  return(text_corpus)
}

corpus <- create_corpus(wine$description)

dtm <- TermDocumentMatrix(corpus, control = list(minWordLength = 1))
m = as.matrix(dtm)


MC_tokenizer(corpus$content)

corpus$content

Model <- function(corpus){
  dtm<-DocumentTermMatrix(corpus)
  sparse<-removeSparseTerms(dtm, sparse = 0.6)
  term_freq<-findFreqTerms(dtm,1000)
  dtm1<-as.matrix(sparse)
  
  dtm<-TermDocumentMatrix(corpus,control=
                          list(dictionary=term_freq,removeNumbers=TRUE,
                               stopwords=TRUE,
                               weighting=weightTfIdf))
  dtm_matrix<-as.matrix(dtm)
  
  winecharacter<-t(dtm_matrix)
  colnames(winecharacter) <- names(dtm_matrix[,1])
  
  winecharacter<-data.frame(winecharacter,country=wine$country,
                            designation=wine$designation,
                            points=wine$points,
                            price=wine$price)
  
  winecharacter1<-data.frame(winecharacter[,1:200],points=winecharacter$points)
  model<-lm(points~.,winecharacter1)
  return(model)
}

Model(corpus)

create_table <- function(corpus){
  model <- Model1(corpus)
  goodword<-names(model$coefficients[model$coefficients>0])
  badword<-names(model$coefficients[model$coefficients<0])
  
  dff <- 
  
}

goodword<-names(model$coefficients[model$coefficients>0])
badword<-names(model$coefficients[model$coefficients<0])

goodword
