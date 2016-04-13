library(class)

tune.k <- read.csv('tune_k.csv')
k.opt <- tail(tune.k[order(tune.k[,2]), ][,1], 1)

knn.pred <- knn(features.svm[fold != 9, ], test, labels[fold != 9], k=k.opt)

temp.matched <- length(knn.pred[knn.pred == labels.test])
knn.acc <- temp.matched/length(knn.pred)

write.csv(knn.pred, file = 'knn_pred.csv', row.names = FALSE)

# ======================tune k========================
cl <- makeCluster(36)
registerDoSNOW(cl)

k.tune <- c(0:35)*4 + 1

tic <- Sys.time()
knn.preds <- foreach(i=1:36, .packages = 
                        'class') %dopar% {knn(train, validation, labels.train, k=k.tune[i])}
Sys.time() - tic

stopCluster(cl)

knn.accs <- foreach(i=1:length(k.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                          ==knn.preds[[i]]])/length(labels.validation)}

tune.k <- cbind(k=k.tune, accuracy=knn.accs)
write.csv(tune.k, file = 'tune_k.csv', row.names = FALSE)
