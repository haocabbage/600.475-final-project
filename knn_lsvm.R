knn_lsvm <- function(nn, info.ind, p, train, labels.train, 
                     test, gamma, cost, cachesize) {
  # if in the local set there's no a dominant class, perform lsvm
  # dominance is determined by the threshold parameter p
  temp.preds <- c()

  for (j in 1:dim(test)[1]) {
    temp.ind <- info.ind[j, 1:nn]
    temp.train <- train[temp.ind, ]
    temp.labels <- labels.train[temp.ind]
    temp.levels <- levels(temp.labels)
    temp.table <- table(temp.labels)
    temp.freq <- as.vector(temp.table)/nn
    
    if (sum(temp.freq >= p) > 0) {
      # pick the dominant one
      temp.pred <- as.character(temp.levels[which.max(temp.table)])
    } else {
      # perform local svm
      temp.model <- svm(temp.train, temp.labels,
                        gamma=gamma, cost=cost, cachesize=cachesize)
      temp.pred <- as.character(predict(temp.model, test[i, ]))
    }
    
    temp.preds[j] <- temp.pred
  }
  
  temp.preds <- factor(temp.preds, levels=levels(labels.train))
  return(temp.preds)
}


# ===========================tune p===============================
p.tune <- c(1:36)/36
C.opt <- 8
gamma.opt <- 2^(-8)

cl <- makeCluster(36)
registerDoSNOW(cl)

knn_lsvm.preds <- foreach(i=1:36, .packages = 'e1071') %dopar% {knn_lsvm(25, info.ind, p.tune[i],
                                                                       train, labels.train,
                                                                       validation, 
                                                                       C.opt, gamma.opt, 
                                                                       400)}

stopCluster(cl)

knn_lsvm.accs <- foreach(i=1:length(p.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                                         ==knn_lsvm.preds[[i]]])/length(labels.validation)}

tune.p <- data.frame(p = p.tune, accuracy=knn_lsvm.accs)
write.csv(tune.p, file='p_search.csv', row.names = FALSE)


