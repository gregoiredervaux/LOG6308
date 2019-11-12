library(NLP)
library(tm)
library(ggplot2)

get_data <- function(src) {
  corpus <- SimpleCorpus(DirSource(src))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

corpus_poly <- get_data('data/Poly')
corpus_udm <- get_data('data/UdM')
corpus_hec <- get_data('data/HEC')
corpus_uqam <- get_data('data/UQAM')

to_matrix <- function(corpus) {
  corpus <- tm_map(corpus,removeWords, stopwords('french'))
  corpus <- tm_map(corpus, removeWords, c('titrecours', 'descriptioncours'))
  return(TermDocumentMatrix(corpus))
}

td_poly <- to_matrix(corpus_poly)
td_udm <- to_matrix(corpus_udm)
td_hec <- to_matrix(corpus_hec)
td_uqam <- to_matrix(corpus_uqam)

