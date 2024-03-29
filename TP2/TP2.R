m = as.matrix(read.table("./citeseer.rtable"))

i <- grep('422908', as.character(rownames(m)), ignore.case=T)
## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }
## Cosinus des colonnes d'une matrice
cosinus.mm <- function(m) { n <- sqrt(colSums(m^2)); crossprod(m)/(n %o% n) }
# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

sim.cos <- as.vector(cosinus.vm(m[,i], m))
i.sim.cos <- max.nindex(sim.cos)
sim.cos.sorted <- data.frame(Cos=sim.cos[i.sim.cos], Article=as.character(rownames(m)[i.sim.cos]), Index=i.sim.cos)

# cross validation leave-one-out
m.sim.cos = cosinus.mm(m)

# on va faire un leave-one-out
# on remplace chaque vecteur par son vecteur le plus proche

index.m.sim.cos.noColNa <- which(colSums(m.sim.cos, na.rm = TRUE) > 1)

m.clean <- m[,index.m.sim.cos.noColNa]
m.predicted <- matrix(, nrow=nrow(m.clean), ncol=ncol(m.clean))
rownames(m.predicted) <- rownames(m.clean)
colnames(m.predicted) <- colnames(m.clean)
m.clean.sim.cos <- m.sim.cos[index.m.sim.cos.noColNa,index.m.sim.cos.noColNa]

index = 1
index.closest <- max.nindex(m.clean.sim.cos[-index,index], 1)
m.predicted[,index] <- m.clean[,index.closest]
test.i2 <- data.frame(m.clean[,index], m.predicted[,index])

replace_by_closest <- function(index, m.predicted) {
  # on selectionne l'index du plus proche vecteur sans prendre en compte lui même
  index.closest <- max.nindex(m.clean.sim.cos[-index,index], 1)
  m.predicted[,index] <<- m.clean[,index.closest]
}

buffer <-lapply(1:ncol(m.clean), replace_by_closest, m.predicted)
sqrt(mean((m.clean - m.predicted)^2, na.rm=T))

# on sélection pour l'index 1 ses k plus proches voisins
index = 20
nombre_de_knn = 5
index.closest <- max.nindex(m.clean.sim.cos[-index,index], nombre_de_knn)

# on fait le produit vectoriel entre les deux vecteurs pour savoir combien d'article sont en commun
test <- m.clean[,index.closest]
sum(m.clean[, index] %*% m.clean[,index.closest])
sum(m.clean[, index])

get_accuracy_on_vect <- function(index) {
  index.closest <- max.nindex(m.clean.sim.cos[-index,index], nombre_de_knn)
  return(sum(m.clean[, index] %*% m.clean[,index.closest])/sum(m.clean[, index]))
}

acc_vect <-lapply(1:ncol(m.clean), get_accuracy_on_vect)
acc_on_all_knn <- mean(unlist(acc_vect, use.names=FALSE), rm.na=TRUE)
acc_by_knn <- acc_on_all_knn / nombre_de_knn
print(acc_tot)

acc_by_knn_vect <- lapply(1:50, function(nombre_de_knn){
  acc_vect <-lapply(1:ncol(m.clean), function(index) {
    index.closest <- max.nindex(m.clean.sim.cos[-index,index], nombre_de_knn)
    return(sum(m.clean[, index] %*% m.clean[,index.closest])/sum(m.clean[, index]))
  })
  acc_on_all_knn <- mean(unlist(acc_vect, use.names=FALSE), rm.na=TRUE)
  return(acc_on_all_knn / nombre_de_knn)
})
acc_on_all_knn_vect <- lapply(1:50, function(nombre_de_knn){
  acc_vect <-lapply(1:ncol(m.clean), function(index) {
    index.closest <- max.nindex(m.clean.sim.cos[-index,index], nombre_de_knn)
    return(sum(m.clean[, index] %*% m.clean[,index.closest])/sum(m.clean[, index]))
  })
  return(mean(unlist(acc_vect, use.names=FALSE), rm.na=TRUE))
})
plot(1:50, acc_by_knn_vect)
plot(1:50, acc_on_all_knn_vect)
