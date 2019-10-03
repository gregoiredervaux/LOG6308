### Si données locales
setwd("~/Documents/LOG6308/TP/TP1")
u.user <- read.csv(file='./u.user.csv', sep='|', header=T)
u.item <- read.csv(file='./u.item.csv', sep='|', header=T)
u.data <- read.csv(file='./u.data.csv', sep='|', header=T)


library(Matrix)
m.sparse <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
show.m.sparse <- as.matrix(m.sparse)
rownames(m.sparse) <- paste(1:nrow(m.sparse), sep='')
colnames(m.sparse) <- u.item$movie.title

## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

# Quelle est la moyenne des votes par profession ("job") et par âge?

user_data <- merge(u.data, u.user, by.x = 'user.id', by.y = 'id')
# moyenne par profession
aggregate(user_data$rating, by=list(user_data$job), FUN=mean)

# moyenne par age
aggregate(user_data$rating, by=list(user_data$age), FUN=mean)

################## question 2 ###################

# Quels sont les 10 films les plus similaires à "Star Trek V: The Final Frontier (1989)" selon respectivement la mesure du cosinus et de la corrélation avec la matrice de votes.

start_trek_vect <- m.sparse[,'Star Trek V: The Final Frontier (1989)']
mat_cos <- cosinus.vm(start_trek_vect, m.sparse)
top_10_index <- max.nindex(mat_cos, n = 11)
top_10 <- u.item$movie.title[top_10_index]
top_10

## corrélation de Pearson entre un vecteur v et chaque colonne dela matrice m

cor.vm <- function(v, m) {
  # ici, le parametre 2 indique que nous effectuons la corrélation
  # sur les colones et non sur les lignes
  apply(m, 2, cor, y=v)
}
mat_cor = cor.vm(start_trek_vect, m.sparse)
top_10_index_cor <- max.nindex(mat_cor, n = 11)
top_10_cor <- u.item$movie.title[top_10_index_cor]
show(top_10_cor)

################## question 3 ###################

# on determine les 20 voisins les plus proches
distance.450 <- sqrt(colSums((m.sparse[,450] - m.sparse)^2)) # valeurs manquantes à 0
voisins_star_trek <- min.nindex(distance.450, 21)[2:21]
u.item$movie.title[voisins_star_trek]
# on determine les poids pour ces voisins
poids_voisins <- cosinus.vm(start_trek_vect, m.sparse[, voisins_star_trek])
show(poids_voisins)
# on selectionne maintenant les users dont on doit prédire les notes
user_index_non_vote_start_treck <- which(start_trek_vect == 0)
user_to_process <- intersect(which(rowSums(m.sparse[, voisins_star_trek]) !=0), user_index_non_vote_start_treck)
# on retire les 0 dans la matrice des notes

m_users_to_process <- m.sparse[user_to_process,voisins_star_trek]
m_users_to_process[m_users_to_process == 0] <- NA

# on applique la formule

# on retire les valeurs 0 pour calculer les moyennes
start_trek_vect[start_trek_vect == 0] <- NA
m_sparse_voisin <- m.sparse[, voisins_star_trek]
m_sparse_voisin[m_sparse_voisin == 0] <- NA

mean_note_voisins <- apply(m_sparse_voisin, 2, mean, na.rm = TRUE)
mean_note_star_treck <- mean(start_trek_vect, na.rm = TRUE)

# on créer une nouvelle m.sparse avec les moyennes de chaques notes des films voisins a la place des NA
m.sparse.to_normalisee <- m.sparse[]

normalization_by_mean <- function(v) {
  mean <- mean(v[v!=0], na.rm = TRUE)
  v[v==0] <- mean
  return(v)
}
m.sparse.normalisee <- apply(m.sparse.to_normalisee, 2, normalization_by_mean)
notes_prediction_raw <- (m.sparse.normalisee[user_to_process, voisins_star_trek] - mean_note_voisins) %*% t(poids_voisins_zero)
show(notes_prediction_raw)

notes_prediction_normalize <- mean_note_star_treck + notes_prediction_raw / sum(poids_voisins[poids_voisins_zero != 0])
show(notes_prediction_normalize)

m.sparse.binaire <- m.sparse[user_to_process, voisins_star_trek]
m.sparse.binaire[m.sparse.binaire != 0] <- 1
show(m.sparse.binaire)

poids_voisins.pondere <- m.sparse.binaire %*% t(poids_voisins)
show(poids_voisins.pondere)

notes_prediction <- mean_note_star_treck + ((m.sparse.normalisee[user_to_process, voisins_star_trek] - mean_note_voisins) %*% t(poids_voisins)) / poids_voisins.pondere
show(notes_prediction)

################## question 4 ###################

# on selectionne les utilisateurs qui n'ont pas voté pour star trek
user_index_start_treck <- which(start_trek_vect != 0)
# on croise avec les utilisateurs qui ont voté pour au moins 1 des films parmis les 20 les plus proches
user_to_process <- intersect(which(rowSums(m.sparse[, voisins_star_trek]) !=0), user_index_start_treck)
show(user_to_process)

# on re calcul la matrice des votes binaires
m.sparse.binaire <- m.sparse[user_to_process, voisins_star_trek]
m.sparse.binaire[m.sparse.binaire != 0] <- 1

# On calcul les poids pondérés par utilisateurs
poids_voisins.pondere <- m.sparse.binaire %*% t(poids_voisins)

# on prédit leurs notes
notes_prediction <- mean_note_star_treck + ((m.sparse.normalisee[user_to_process, voisins_star_trek] - mean_note_voisins) %*% t(poids_voisins)) / poids_voisins.pondere

erreur_quadra <- sqrt(sum((notes_prediction - m.sparse[user_to_process, 'Star Trek V: The Final Frontier (1989)'])**2))
show(erreur_quadra)
show(as.matrix(notes_prediction))
show(as.matrix(m.sparse[user_to_process, 'Star Trek V: The Final Frontier (1989)']))

################## question 5 ###################
library(stats)

# on normalise avec les moyennes par utilisateurs
m.sparse.for_kmean_by_user <- t(apply(m.sparse.to_normalisee, 1, normalization_by_mean))
# on normalise avec les moyennes par films
m.sparse.for_kmean_by_film <- apply(m.sparse.to_normalisee, 2, normalization_by_mean)

# on fait kmean
k_mean_obj <- kmeans(m.sparse.for_kmean_by_film, 5)

centers <- as.matrix(k_mean_obj$centers)
clusters <- as.matrix(k_mean_obj$cluster)

get_prediction <- function(clusters, centers) {
  
  clusters <- as.data.frame(clusters)
  clusters$user_name = rownames(clusters)
  
  centers <- as.data.frame(centers)
  centers$cluster_num <- rownames(centers)
  
  # on fusionne les tables clusters et centers pour obtenir les prédictions pour chaque utilisateur en fonction de son cluster d'appartenance
  prediction <- merge.data.frame(clusters, centers, by.x = "V1", by.y = "cluster_num")
  
  # on retrie les prédictions par utilisateur pour pouvoir selectionner les utilisateurs par index
  rownames(prediction) <- prediction$user_name
  return(prediction[order(as.numeric(prediction$user_name)),])
}

prediction <- get_prediction(clusters, centers)
head(prediction)
# on selectionne seulement les notes des utilisateurs ayant voté star treck
user_star_treck_prediction <- prediction[user_index_start_treck,]

head(user_star_treck_prediction["Star Trek V: The Final Frontier (1989)"])
head(m.sparse[user_index_start_treck, 'Star Trek V: The Final Frontier (1989)'])

# on calcule l'erreur quadratique entre les vrais notes des utilisateurs et la note de leurs clusters restpectif
sum_err <- sum(abs(user_star_treck_prediction["Star Trek V: The Final Frontier (1989)"] - m.sparse[user_index_start_treck, 'Star Trek V: The Final Frontier (1989)']))
err_abs_moy <- sum_err / length(user_star_treck_prediction[,"Star Trek V: The Final Frontier (1989)"])
sum_sqrt_err <- sum((user_star_treck_prediction["Star Trek V: The Final Frontier (1989)"] - m.sparse[user_index_start_treck, 'Star Trek V: The Final Frontier (1989)'])**2)
erreur_quadra <- sqrt(sum_sqrt_err / length(user_star_treck_prediction[,"Star Trek V: The Final Frontier (1989)"]))

print("erreur quadra/moyenne pour cluster 5")
show(erreur_quadra)
show(err_abs_moy)