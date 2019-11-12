library(NLP)
library(tm)

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

                      