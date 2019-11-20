library(Matrix)
library(igraph)

artists <- read.delim('./data/artists.dat', header=TRUE, sep="\t")
tags <- read.delim('./data/tags.dat', header=TRUE, sep="\t")
user_artists <- read.delim('./data/user_artists.dat', header=TRUE, sep="\t")
user_friends <- read.delim('./data/user_friends.dat', header=TRUE, sep="\t")
user_tagged_artists_timestamps <- read.delim('./data/user_taggedartists-timestamps.dat', header=TRUE, sep="\t")
user_tagged_artists <- read.delim('./data/user_taggedartists.dat', header=TRUE, sep="\t")

n_users <- 1892
n_artists <- 17632
indexes_users <- unique(user_friends[,1])
indexes_artists <- unique(user_artists[,2])


ToMatrix <- function(data,m,values){
  for (i in 1:dim(data)[1]){
    if (values){
      m[toString(data[i,1]),toString(data[i,2])] <- data[i,3]
    }else{
      m[toString(data[i,1]),toString(data[i,2])] <- 1
    }
  }
  return(as(m, "sparseMatrix"))
}

#friend_graph <- graph_from_edgelist(as.matrix(user_friends), directed=FALSE)
#m_friend.sparse <- get.adjacency(friend_graph)
m_friend.sparse <- matrix(0,n_users,n_users)
rownames(m_friend.sparse) <- indexes_users
colnames(m_friend.sparse) <- indexes_users
m_friend.sparse <- ToMatrix(user_friends,m_friend.sparse,FALSE)

m_user_artist.sparse <- matrix(0,n_users,n_artists)
rownames(m_user_artist.sparse) <- indexes_users
colnames(m_user_artist.sparse) <- indexes_artists
m_user_artist.sparse <- ToMatrix(user_artists,m_user_artist.sparse,TRUE)


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

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

cosinus.vm <- function(v,m) { n <- sqrt(rowSums(m^2)); (m %*% v)/(n * sqrt(sum(v^2))) }

dist_eucl.vm <- function(v,m) { sqrt(rowSums((m - v)^2)) }

cor.vm <- function(v, m) {
  # ici, le parametre 2 indique que nous effectuons la corrélation
  # sur les colones et non sur les lignes
  apply(m, 1, cor, y=v)
}



#############################################################################################

Closest_friends <- function(user_index,k,alpha){
  user <- m_friend.sparse[user_index,]
  friends_id <- colnames(m_friend.sparse)[user!=0]
  n_rest_friends <- k - sum(user)
  n_friends <- min(k,sum(user))
  if (n_rest_friends <= 0){
    sim <- cosinus.vm(user,m_friend.sparse[friends_id,])
    close_friends_id <- friends_id[max.nindex(sim,k)]
  }else{
    others_id <- colnames(m_friend.sparse)[user==0]
    sim <- cosinus.vm(user,m_friend.sparse[others_id,])
    close_users <- others_id[max.nindex(sim,n_rest_friends+1)]
    close_friends_id <- c(friends_id,close_users[1:n_rest_friends+1])
  }
  close_friends <- m_friend.sparse[close_friends_id,]
  weights <- cosinus.vm(user,close_friends)
  weights[1:n_friends] <- weights[1:n_friends] + alpha
  return(t(weights))
}

Friends <- function(user_index){
  user <- m_friend.sparse[user_index,]
  friends_id <- colnames(m_friend.sparse)[user!=0]
  friends <- m_friend.sparse[friends_id,]
  weights <- cosinus.vm(user,friends)
  return(t(weights))
}

KNN <- function(user_index,n,k,alpha){
  weights <- Closest_friends(user_index,k,alpha)
  #weights <- Friends(user_index)
  neighbors_id <- colnames(weights)
  neighbors <- m_user_artist.sparse[neighbors_id,]
  neighbors[neighbors == 0] <- NA
  mean_neighbors <- rowMeans(neighbors, na.rm = TRUE)
  values_neighbors <- weights %*% (m_user_artist.sparse[neighbors_id,]/mean_neighbors)
  return(colnames(m_user_artist.sparse)[max.nindex(values_neighbors,n)])
}


evaluate1 <- function(user_index,k,alpha){
  user <- m_user_artist.sparse[user_index,]
  tot_value <- sum(user)
  n <- length(user[user>0])
  music <- colnames(m_user_artist.sparse)[user>0]
  recommandations <- KNN(user_index, n, k, alpha)
  recom_value <- sum(user[recommandations])
  return(c(recom_value/tot_value,n))
}


evaluate <- function(user_index,k,alpha,percentage){
  user <- m_user_artist.sparse[user_index,]
  n <- ceiling(length(user[user>0]) * percentage)
  music <- colnames(m_user_artist.sparse)[user>0]
  recommandations <- KNN(user_index, n, k, alpha)
  precision <- length(intersect(music,recommandations))
  return(c(precision,n))
}

evaluateAll1 <- function(k,alpha,min_friend){
  tot_precision <- 0
  tot_pred <- 0
  n_notEvaluated <- 0
  for (user in indexes_users){
    if(sum(m_friend.sparse[toString(user),]) < min_friend){
      n_notEvaluated <- n_notEvaluated + 1
    }else{
      result <- evaluate1(toString(user),k,alpha)
      tot_precision <- tot_precision + result[1] * result[2]
      tot_pred <- tot_pred + result[2]
    }
  }
  precision <- tot_precision / tot_pred
  print('Numbers of users tested :')
  print(n_users - n_notEvaluated)
  print('Numbers of predictions :')
  print(tot_pred)
  print('Precision :')
  print(precision)
  return(precision)
}


evaluateAll <- function(k,alpha,min_friend,percentage){
  tot_true <- 0
  tot_precision <- 0 
  tot_pred <- 0
  n_notEvaluated <- 0
  for (user in indexes_users){
    if(sum(m_friend.sparse[toString(user),]) < min_friend){
      n_notEvaluated <- n_notEvaluated + 1
    }else{
      result <- evaluate(toString(user),k,alpha,percentage)
      tot_true <- tot_true + result[1]
      tot_pred <- tot_pred + result[2]
    }
  }
  precision <- tot_true / tot_pred
  print('Numbers of users tested :')
  print(n_users - n_notEvaluated)
  print('Numbers of predictions :')
  print(tot_pred)
  print('Precision :')
  print(precision)
  return(precision)
}

randomEvaluateAll <- function(){
  tot_true <- 0
  tot_precision <- 0
  tot_pred <- 0
  n_notEvaluated <- 0
  for (user in indexes_users){
    user <- m_user_artist.sparse[toString(user),]
    n <- length(user[user>0])
    music <- colnames(m_user_artist.sparse)[user>0]
    random_pred <- colnames(m_user_artist.sparse)[sample(1:n_artists,n)]
    tot_true <- tot_true + length(intersect(music,random_pred))
    tot_precision <- tot_precision + sum(user[random_pred])/sum(user) * n
    tot_pred <- tot_pred + n
  }
  precision_number_music <- tot_true / tot_pred
  precision_time <- tot_precision / tot_pred
  print('Numbers of predictions :')
  print(tot_pred)
  print('Precision in number of musics:')
  print(precision_number_music)
  print('Precision in percentage of time listened:')
  print(precision_time)
}

evaluateAll(40,0.5,5,1.0)

evaluateAll1(40,0.5,5)

randomEvaluateAll()

prec <- matrix(0,1,6)
k <- c(2,5,10,15,20,30)
for (i in 1:6){
  prec[i] <- evaluateAll(k[i],0.3)
}

prec2 <- matrix(0,1,6)
alpha <- c(0.3,0.4,0.5,0.6,0.7,0.8)
for (i in 1:6){
  prec2[i] <- evaluateAll(30,alpha[i])
}


prec3 <- matrix(0,1,6)
min_friend <- c(0,2,5,8,10,13)
for (i in 1:6){
  prec3[i] <- evaluateAll(30,0.5,min_friend[i])
}
not_evaluated <- c(0,203,761,1037,1170,1301)
# le mieux semble 5


prec4 <- matrix(0,1,6)
k <- c(10,15,20,30,40,50)
for (i in 1:6){
  prec4[i] <- evaluateAll(k[i],0.5,5)
}
#50 est le mieux mais 40 vaut peut-être mieux la peine

prec5 <- matrix(0,1,6)
alpha <- c(0.3,0.4,0.5,0.6,0.7,0.8)
for (i in 1:6){
  prec5[i] <- evaluateAll(40,alpha[i],5)
}
#0.5 tjrs le mieux

#Idées :  1 -> merger les 2 types de précision
#         2 -> tester les meilleurs paramètres de k, alpha et min_friend
#         3 -> faire varier les valeurs de précision 




#mieux : k = 30 alpha = 0.5
#en moyenne 49 musiques par utilisateurs -> au hasard on recommande 49 musiques sur 17632 -> 0.0028 de précision au hasard