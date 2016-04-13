# get knn
library(FNN)

info.nn <- get.knnx(train, validation, k=200)
info.ind <- info.nn$nn.index

# ======================tune k=========================
cv.pred <- list()

tic <- Sys.time()
for (i in 1:8) {
  temp.nn <- 25*2^(i-1)
  temp.ratio <- dim(train)[1]/temp.nn
  temp.C <- C.opt/temp.ratio
  temp.gamma <- gamma.opt*temp.ratio
  temp.preds <- c()
  
  for (j in 1:dim(validation)[1]) {
    temp.ind <- info.ind[j, 1:temp.nn]
    temp.train <- train[temp.ind, ]
    temp.labels <- labels.train[temp.ind]
    
    if (length(unique(temp.labels)) == 1) {
      temp.pred <- as.character(temp.labels[1])
    } else {
      temp.model <- svm(temp.train, temp.labels, scale=FALSE,
                        gamma=temp.gamma, cost=temp.C)
      temp.pred <- as.character(predict(temp.model, validation[i, ]))
    }
    
    temp.preds[j] <- temp.pred
  }
  
  cv.pred[[i]] <- factor(temp.preds, levels=levels(labels.validation))
}
Sys.time() - tic

lsvm.acc <- foreach(i=1:6, .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                          ==cv.pred[[i]]])/length(labels.validation)}

k.lsvm <- data.frame(k = 25*2^c(0:5), accuracy=lsvm.acc)
write.csv(k.lsvm, file='loose_k_search.csv', row.names = FALSE)


# ================grid search of C, gamma====================
nn0 <- 5
nn1 <- 25
nn2 <- 50
nn3 <- 100

C.tune <- 2^(c(-2,-1,0,1,2,3))
gamma.tune <- 2^(c(-8,-7,-6,-5,-4,-3))

par.tune <- vector('list', length(C.tune)*length(gamma.tune))

ind <- 1
for (i in 1:length(C.tune)) {
  for (j in 1:length(gamma.tune)) {
    par.tune[[ind]] <- c(C.tune[i], gamma.tune[j])
    ind <- ind + 1
  }
}

cl <- makeCluster(36)
registerDoSNOW(cl)

# ============k = 5===============
tic <- Sys.time()
svm5.pred <- foreach(i=1:length(par.tune), .packages = 
                        'e1071') %dopar% {lsvm(nn0, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.tune[[i]][2],
                                               par.tune[[i]][1], 40)}
Sys.time() - tic

svm5.acc <- foreach(i=1:length(par.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm5.pred[[i]]])/length(labels.validation)}

# ============k = 25===============
tic <- Sys.time()
svm25.pred <- foreach(i=1:length(par.tune), .packages = 
                        'e1071') %dopar% {lsvm(nn1, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.tune[[i]][2],
                                               par.tune[[i]][1], 200)}
Sys.time() - tic

svm25.acc <- foreach(i=1:length(par.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm25.pred[[i]]])/length(labels.validation)}

# ============k = 50===============
tic <- Sys.time()
svm50.pred <- foreach(i=1:length(par.tune), .packages = 
                        'e1071') %dopar% {lsvm(nn2, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.tune[[i]][2],
                                               par.tune[[i]][1], 200)}
Sys.time() - tic

svm50.acc <- foreach(i=1:length(par.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm50.pred[[i]]])/length(labels.validation)}

# ============k = 100===============
tic <- Sys.time()
svm100.pred <- foreach(i=1:length(par.tune), .packages = 
                        'e1071') %dopar% {lsvm(nn3, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.tune[[i]][2],
                                               par.tune[[i]][1], 400)}
Sys.time() - tic

svm100.acc <- foreach(i=1:length(par.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm100.pred[[i]]])/length(labels.validation)}

stopCluster(cl)

lsvm <- function(nn, info.ind, train, labels.train, test, gamma, cost, cachesize) {
  # perform local svm
  temp.preds <- c()
  
  for (j in 1:dim(test)[1]) {
    temp.ind <- info.ind[j, 1:nn]
    temp.train <- train[temp.ind, ]
    temp.labels <- labels.train[temp.ind]
    
    if (length(unique(temp.labels)) == 1) {
      temp.pred <- as.character(temp.labels[1])
    } else {
      temp.model <- svm(temp.train, temp.labels,
                        gamma=gamma, cost=cost, cachesize=cachesize)
      temp.pred <- as.character(predict(temp.model, test[i, ]))
    }
    
    temp.preds[j] <- temp.pred
  }
  
  temp.preds <- factor(temp.preds, levels=levels(labels.train))
  return(temp.preds)
}

# ===================data export=======================
temp.C <- c()
temp.gamma <- c()

for (i in 1:length(par.tune)) {
  temp.C[i] <- par.tune[[i]][1]
  temp.gamma[i] <- par.tune[[i]][2]
}

grid_search.k5 <- cbind(temp.C, temp.gamma, svm5.acc) 
colnames(grid_search.k5) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k5, file='k5_grid_search.csv', row.names = FALSE)

grid_search.k25 <- cbind(temp.C, temp.gamma, svm25.acc) 
colnames(grid_search.k25) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k25, file='k25_grid_search.csv', row.names = FALSE)

grid_search.k50 <- cbind(temp.C, temp.gamma, svm50.acc) 
colnames(grid_search.k50) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k50, file='k50_grid_search.csv', row.names = FALSE)

grid_search.k100 <- cbind(temp.C, temp.gamma, svm100.acc) 
colnames(grid_search.k100) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k100, file='k100_grid_search.csv', row.names = FALSE)


# ==================finely tune C, gamma========================
# k = 25
C.fine <- 2^(c(1.25, 1.5, 2, 2.25, 2.5, 2.75))
gamma.fine <- 2^(c(-8.75, -8.5, -8.25, -8, -7.75, -7.5))

par.fine <- vector('list', length(C.fine)*length(gamma.fine))

ind <- 1
for (i in 1:length(C.fine)) {
  for (j in 1:length(gamma.fine)) {
    par.fine[[ind]] <- c(C.fine[i], gamma.fine[j])
    ind <- ind + 1
  }
}

# ====================k = 5=====================
tic <- Sys.time()
svm5.pred <- foreach(i=1:length(par.fine), .packages = 
                        'e1071') %dopar% {lsvm(nn0, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.fine[[i]][2],
                                               par.fine[[i]][1], 200)}
Sys.time() - tic

svm5.acc <- foreach(i=1:length(par.fine), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm5.pred[[i]]])/length(labels.validation)}


# ====================k = 25=====================
tic <- Sys.time()
svm25.pred <- foreach(i=1:length(par.fine), .packages = 
                        'e1071') %dopar% {lsvm(nn1, info.ind,
                                               train, labels.train,
                                               validation, 
                                               par.fine[[i]][2],
                                               par.fine[[i]][1], 200)}
Sys.time() - tic

svm25.acc <- foreach(i=1:length(par.fine), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==svm25.pred[[i]]])/length(labels.validation)}


# =====================data export======================
temp.C <- c()
temp.gamma <- c()

for (i in 1:length(par.fine)) {
  temp.C[i] <- par.fine[[i]][1]
  temp.gamma[i] <- par.fine[[i]][2]
}

grid_search.k5 <- cbind(temp.C, temp.gamma, svm5.acc) 
colnames(grid_search.k5) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k5, file='k5_fine_search.csv', row.names = FALSE)

grid_search.k25 <- cbind(temp.C, temp.gamma, svm25.acc) 
colnames(grid_search.k25) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.k25, file='k25_fine_search.csv', row.names = FALSE)



# ===============local svm with optimal C, gamma====================
C.opt <- 2^1.5
gamma.opt <- 2^(-8.25)

info.nn <- get.knnx(features.svm[fold != 9, ], test, k=100)
info.ind <- info.nn$nn.index

lsvm.pred <- lsvm(25, info.ind,
                 features.svm[fold != 9, ], labels[fold != 9],
                 test, gamma.opt, C.opt, 500)

lsvm.acc <- length(lsvm.pred[lsvm.pred==labels.test])/length(lsvm.pred)

write.csv(lsvm.pred, file = 'lsvm_pred.csv', row.names = FALSE)





# =====================testing===========================
tic <- Sys.time()
temp.pred <- lsvm(nn1, info.ind,train, labels.train, validation, par.tune[[1]][2], par.tune[[1]][1], 200)
Sys.time() - tic

temp.acc <- length(temp.pred[temp.pred==labels.validation])/length(temp.pred)
temp.acc

tic <- Sys.time()
temp.pred <- lsvm(nn2, info.ind,train, labels.train, validation, par.tune[[1]][2], par.tune[[1]][1], 400)
Sys.time() - tic

temp.acc <- length(temp.pred[temp.pred==labels.validation])/length(temp.pred)
temp.acc








