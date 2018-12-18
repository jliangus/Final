#function that creates word clouds 
library(wordcloud)


create_wordcloud <- function(){
  wine <- WineDataset()
  wineCorpus <- Corpus(VectorSource(wine$description))
  wineCorpus <- tm_map(wineCorpus, PlainTextDocument)
  wineCorpus <- tm_map(wineCorpus, removePunctuation)
  wineCorpus <- tm_map(wineCorpus, removeWords, stopwords('english'))
  wineCorpus <- tm_map(wineCorpus, stemDocument)
  wineCorpus <- Corpus(VectorSource(wineCorpus))
  wineCorpus <- tm_map(wineCorpus, removeWords, c('the', 'this', 'wine', stopwords('english')))
  myDTM = TermDocumentMatrix(wineCorpus, control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  v = sort(rowSums(m), decreasing = TRUE)
  cloud <- wordcloud(names(v), v, main = "All Wines", max.words = 100, colors=brewer.pal(8, "Dark2")) 
  return(cloud)
}

create_wordcloud()


wordcloud_by_variety <- function(df, variety){
  toprated <- subset(df, variety == variety)
  wineCorpus1 <- Corpus(VectorSource(toprated$description))
  wineCorpus1 <- tm_map(wineCorpus1, PlainTextDocument)
  wineCorpus1 <- tm_map(wineCorpus1, removePunctuation)
  wineCorpus1 <- tm_map(wineCorpus1, removeWords, stopwords('english'))
  wineCorpus1 <- tm_map(wineCorpus1, stemDocument)
  wineCorpus1 <- Corpus(VectorSource(wineCorpus1))
  wineCorpus1 <- tm_map(wineCorpus1, removeWords, c('the', 'this', 'wine', stopwords('english')))
  myDTM1 = TermDocumentMatrix(wineCorpus1, control = list(minWordLength = 1))
  m1 = as.matrix(myDTM1)
  v1 = sort(rowSums(m1), decreasing = TRUE)
  cloud <- wordcloud(names(v1), v1, max.words = 100, colors=brewer.pal(8, "Dark2"), main = "Wine Description for Varieties")
  return(cloud)
}

wordcloud_by_variety(wine, "Rosso")

##maybe keep? 
wordcloud_by_variety_score <- function(df, variety, score){
  toprated <- subset(df, variety == variety)
  toprated <- subset(toprated, points > as.numeric(score))
  wineCorpus1 <- Corpus(VectorSource(toprated$description))
  head(wineCorpus1)
  wineCorpus1 <- tm_map(wineCorpus1, PlainTextDocument)
  wineCorpus1 <- tm_map(wineCorpus1, removePunctuation)
  wineCorpus1 <- tm_map(wineCorpus1, removeWords, stopwords('english'))
  wineCorpus1 <- tm_map(wineCorpus1, stemDocument)
  wineCorpus1 <- Corpus(VectorSource(wineCorpus1))
  wineCorpus1 <- tm_map(wineCorpus1, removeWords, c('the', 'this', 'wine', stopwords('english')))
  myDTM1 = TermDocumentMatrix(wineCorpus1, control = list(minWordLength = 1))
  m1 = as.matrix(myDTM1)
  v1 = sort(rowSums(m1), decreasing = TRUE)
  cloud <- wordcloud(names(v1), v1, max.words = 100, colors=brewer.pal(8, "Dark2"), main = "Wine Description for Top Varieties")
  return(cloud)
}

wordcloud_by_variety_score(wine, "Rosso", "90")
