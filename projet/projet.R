artists <- read.delim('./data/artists.dat', header=TRUE, sep="\t")
tags <- read.delim('./data/tags.dat', header=TRUE, sep="\t")
user_artists <- read.delim('./data/user_artists.dat', header=TRUE, sep="\t")
user_friends <- read.delim('./data/user_friends.dat', header=TRUE, sep="\t")
user_tagged_artists_timestamps <- read.delim('./data/user_taggedartists-timestamps.dat', header=TRUE, sep="\t")
user_tagged_artists <- read.delim('./data/user_taggedartists.dat', header=TRUE, sep="\t")

# test de page rank

PageRank <- function(m,d){
  A = as.matrix(m)
  n = nrow(m)
  R = matrix(1.0,1,n)
  s = rowSums(m) + 1
  oldR = matrix(0.0,1,n)
  i = 0
  while (norm(R-oldR) > 0.0001){
    oldR = R
    R = (1-d)/n + d*(R/s)%*%A
    i = i + 1
  }
  print("Nombre d'itérations : ")
  print(i)
  return(R)
}