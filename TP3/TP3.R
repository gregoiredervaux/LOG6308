library(NLP)
library(tm)
library(ggplot2)
library(Matrix)

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

corpus_tot <- get_data(c('data/Poly', 'data/UdM', 'data/HEC', 'data/UQAM'))

to_matrix <- function(corpus) {
  corpus <- tm_map(corpus,removeWords, stopwords('french'))
  corpus <- tm_map(corpus, removeWords, c('titrecours', 'descriptioncours'))
  return(TermDocumentMatrix(corpus))
}

td_poly <- to_matrix(corpus_poly)
td_udm <- to_matrix(corpus_udm)
td_hec <- to_matrix(corpus_hec)
td_uqam <- to_matrix(corpus_uqam)

td_tot <- to_matrix(corpus_tot)

get_Tf_IDF <- function(td) {
  tf <- as.matrix(td)
  tf_copy <- tf
  tf_copy[tf_copy != 0] <- 1
  n_i <- rowSums(tf_copy)
  idf <- log(nDocs(td_tot)/n_i)
  tf_idf = tf * idf
  return(tf_idf)
}

tf_idf <- get_Tf_IDF(td_tot)

