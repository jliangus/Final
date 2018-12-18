#linear model

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

model <- Model(corpus)



create_table <- function(corpus){
  model <- Model(corpus)
  goodword<-names(model$coefficients[model$coefficients>0])
  badword<-names(model$coefficients[model$coefficients<0])
  
  df <- data.frame(goodword, badword)
  
}


goodword<-names(model$coefficients[model$coefficients>0])
badword<-names(model$coefficients[model$coefficients<0])
df <- data.frame(goodword, badword)

goodword
