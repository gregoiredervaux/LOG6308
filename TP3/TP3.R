library(Matrix)
library(NLP)
library(tm)
library(irlba)
library(stats)
library(pROC)

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

# tf_idf <- get_Tf_IDF(td_tot)
tf_idf <- Matrix(get_Tf_IDF(td_tot), "dgCMatrix")
head(tf_idf[, 0:5])

dim_redu = 50

m.svd <- irlba(t(tf_idf), 50)
reduce_tf_idf <- m.svd$u[, 1:dim_redu] %*% diag(m.svd$d[1:dim_redu])
rownames(reduce_tf_idf) <- colnames(tf_idf)
head(reduce_tf_idf[, 1:5])

# Question 1

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

i <- grep('LOG2420', as.character(rownames(reduce_tf_idf)))
reduce_tf_idf.cos <- cosinus.mm(t(reduce_tf_idf))
i.sim.cos <- max.nindex(reduce_tf_idf.cos[,i], 11)
data.frame(Cos=reduce_tf_idf.cos[i.sim.cos, i], Index=i.sim.cos)

## Question 2
reduce_tf_idf.class <- subset(reduce_tf_idf, grepl("PSY|PHY", rownames(reduce_tf_idf)))

reduce_tf_idf.PSY <- subset(reduce_tf_idf, grepl("PSY", rownames(reduce_tf_idf)))

reduce_tf_idf.PHY <- subset(reduce_tf_idf, grepl("PHY", rownames(reduce_tf_idf)))


nfolds = 10

split <- rep(1:nfolds, length.out=nrow(reduce_tf_idf.PSY))[sample(nrow(reduce_tf_idf.PSY))]
reduce_tf_idf.PSY.test.i <- (split==nfolds)
reduce_tf_idf.PSY.train.i <- !reduce_tf_idf.PSY.test.i

split <- rep(1:nfolds, length.out=nrow(reduce_tf_idf.PHY))[sample(nrow(reduce_tf_idf.PHY))]
reduce_tf_idf.PHY.test.i <- (split==nfolds)
reduce_tf_idf.PHY.train.i <- !reduce_tf_idf.PHY.test.i

reduce_tf_idf.PSY.train <- reduce_tf_idf.PSY[reduce_tf_idf.PSY.train.i,]
reduce_tf_idf.PHY.train <- reduce_tf_idf.PHY[reduce_tf_idf.PHY.train.i,]

centroide.PSY <- colMeans(reduce_tf_idf.PSY[reduce_tf_idf.PSY.train.i,])
centroide.PHY <- colMeans(reduce_tf_idf.PHY[reduce_tf_idf.PHY.train.i,])


reduce_tf_idf.PSY.test <- reduce_tf_idf.PSY[reduce_tf_idf.PSY.test.i,]
reduce_tf_idf.PHY.test <- reduce_tf_idf.PHY[reduce_tf_idf.PHY.test.i,]

cours <- rownames(reduce_tf_idf.PSY.test)
sim.PSY <- cosinus.vm(centroide.PSY, t(reduce_tf_idf.PSY.test))[1,]
sim.PHY <- cosinus.vm(centroide.PHY, t(reduce_tf_idf.PSY.test))[1,]

prediction.PSY <- data.frame(cours = rownames(reduce_tf_idf.PSY.test),
                             sim.PSY = as.character(cosinus.vm(centroide.PSY, t(reduce_tf_idf.PSY.test))),
                             sim.PHY = as.character(cosinus.vm(centroide.PHY, t(reduce_tf_idf.PSY.test))))

prediction.PSY$pred.PSY <- apply(prediction.PSY, 1, function (v) {v['sim.PSY'] > v['sim.PHY']})
prediction.PSY$pred.PHY <- apply(prediction.PSY, 1, function (v) {v['sim.PHY'] > v['sim.PSY']})
prediction.PSY$is.PSY <- TRUE
prediction.PSY$is.PHY <- FALSE


prediction.PHY <- data.frame(cours = rownames(reduce_tf_idf.PHY.test),
                             sim.PSY = as.character(cosinus.vm(centroide.PSY, t(reduce_tf_idf.PHY.test))),
                             sim.PHY = as.character(cosinus.vm(centroide.PHY, t(reduce_tf_idf.PHY.test))))

prediction.PHY$pred.PHY <- apply(prediction.PHY, 1, function (v) {v['sim.PHY'] > v['sim.PSY']})
prediction.PHY$pred.PSY <- apply(prediction.PHY, 1, function (v) {v['sim.PSY'] > v['sim.PHY']})
prediction.PHY$is.PHY <- TRUE
prediction.PHY$is.PSY <- FALSE

head(prediction.PHY)

prediction.class <- rbind(prediction.PHY, prediction.PSY)
responce.PSY <- roc(prediction.class$is.PSY, prediction.class$pred.PSY, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)
responce.PHY <- roc(prediction.class$is.PHY, prediction.class$pred.PHY, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)

prediction.PHY <- data.frame()
prediction/PSY <- data.frame()
results <- sapply(1:nfolds , function(fold) {
  
  # construction des ensemble d'apprentissage et de test
  reduce_tf_idf.PSY.test.i <- (split==fold)
  reduce_tf_idf.PSY.train.i <- !reduce_tf_idf.PSY.test.i
  
  reduce_tf_idf.PSY.train <- reduce_tf_idf.PSY[reduce_tf_idf.PSY.train.i,]
  reduce_tf_idf.PHY.train <- reduce_tf_idf.PHY[reduce_tf_idf.PHY.train.i,]
  
  reduce_tf_idf.PSY.test <- reduce_tf_idf.PSY[reduce_tf_idf.PSY.test.i,]
  reduce_tf_idf.PHY.test <- reduce_tf_idf.PHY[reduce_tf_idf.PHY.test.i,]
  
  # on determine les centroides
  centroide.PSY <- colMeans(reduce_tf_idf.PSY[reduce_tf_idf.PSY.train.i,])
  centroide.PHY <- colMeans(reduce_tf_idf.PHY[reduce_tf_idf.PHY.train.i,])
  
  # on construit un nouveau data frame pour PSY
  prediction.PSY <- data.frame(cours = rownames(reduce_tf_idf.PSY.test),
                               sim.PSY = as.double(cosinus.vm(centroide.PSY, t(reduce_tf_idf.PSY.test))),
                               sim.PHY = as.double(cosinus.vm(centroide.PHY, t(reduce_tf_idf.PSY.test))))
  
  prediction.PSY$pred.PSY <- apply(prediction.PSY, 1, function (v) {as.numeric(v['sim.PSY']) / (as.numeric(v['sim.PHY']) + as.numeric(v['sim.PSY']))})
  prediction.PSY$pred.PHY <- apply(prediction.PSY, 1, function (v) {as.numeric(v['sim.PHY']) / (as.numeric(v['sim.PHY']) + as.numeric(v['sim.PSY']))})
  prediction.PSY$is.PSY <- 1
  prediction.PSY$is.PHY <- 0
  
  # on construit un nouveau data frame pour PHY
  prediction.PHY <- data.frame(cours = rownames(reduce_tf_idf.PHY.test),
                               sim.PSY = as.double(cosinus.vm(centroide.PSY, t(reduce_tf_idf.PHY.test))),
                               sim.PHY = as.double(cosinus.vm(centroide.PHY, t(reduce_tf_idf.PHY.test))))
  
  prediction.PHY$pred.PHY <- apply(prediction.PHY, 1, function (v) {as.numeric(v['sim.PHY']) / (as.numeric(v['sim.PHY']) + as.numeric(v['sim.PSY']))})
  prediction.PHY$pred.PSY <- apply(prediction.PHY, 1, function (v) {as.numeric(v['sim.PSY']) / (as.numeric(v['sim.PHY']) + as.numeric(v['sim.PSY']))})
  prediction.PHY$is.PHY <- 1
  prediction.PHY$is.PSY <- 0
  
  # on recupère la valeur de AUC
  prediction.class <- rbind(prediction.PHY, prediction.PSY)
  responce.PSY <- roc(prediction.class$is.PSY, prediction.class$pred.PSY)
  
  return(responce.PSY$auc)
})

print(mean(results))

## Question 3
reduce_tf_idf.class[is.na(reduce_tf_idf.PSY)]

apply_k_mean <- function(reduce_tf_idf.class) {
  
  k_mean_res <- kmeans(reduce_tf_idf.class, 2)
  
  prediction.class.kmean <- data.frame(names = rownames(reduce_tf_idf.class), cluster = as.numeric(k_mean_res$cluster))
  
  index_PHY = grepl("PHY", prediction.class.kmean$names)
  index_PSY = grepl("PSY", prediction.class.kmean$names)
  
  prediction.class.kmean$is.PHY <- index_PHY
  prediction.class.kmean$is.PSY <- index_PSY
  
  mean_cluster_PHY <- mean(prediction.class.kmean$cluster[index_PHY])
  mean_cluster_PSY <- mean(prediction.class.kmean$cluster[index_PSY])
  
  num_cluster_PHY <- if(mean_cluster_PHY < mean_cluster_PSY) 1 else 2
  num_cluster_PSY <- if(mean_cluster_PSY < mean_cluster_PHY) 1 else 2
  
  prediction.class.kmean$pred.PHY <- apply(prediction.class.kmean, 1, function(v) {v['cluster'] == num_cluster_PHY})
  prediction.class.kmean$pred.PSY <- apply(prediction.class.kmean, 1, function(v) {v['cluster'] == num_cluster_PSY})
  
  prediction.class.kmean$pred.right <- apply(prediction.class.kmean, 1, function(v) {v['is.PHY'] == v['pred.PHY']})
  
  acc_on_PHY <- sum(prediction.class.kmean$pred.right[index_PHY]) / sum(index_PHY)
  acc_on_PSY <- sum(prediction.class.kmean$pred.right[index_PSY]) / sum(index_PSY)
  acc <- sum(prediction.class.kmean$pred.right) / nrow(prediction.class.kmean)
  return(c(acc_on_PHY, acc_on_PSY, acc))
}
k_mean_acc <- apply_k_mean(reduce_tf_idf.class)
print("Accuracy on PHY: ")
print(k_mean_acc[1])
print("Accuracy on PSY")
print(k_mean_acc[2])
print("Accuracy total")
print(k_mean_acc[3])


nb_max_cours = min(sum(grepl("PHY", prediction.class.kmean$names)), sum(grepl("PSY", prediction.class.kmean$names)))

reduce_tf_idf.class.equi <- rbind(reduce_tf_idf.PSY[1:nb_max_cours,], reduce_tf_idf.PHY[1:nb_max_cours,])

k_mean_acc.equi <- apply_k_mean(reduce_tf_idf.class.equi)
print("Accuracy on PHY: ")
print(k_mean_acc.equi[1])
print("Accuracy on PSY")
print(k_mean_acc.equi[2])
print("Accuracy total")
print(k_mean_acc.equi[3])