library(quadprog)
library(MASS)
library(wSVM)

# ===========================multiclass wSVM==================================
classes <- levels(labels)

pair.letter <- list()
ind <- 1
for (i in 1:25) {
  for (j in (i+1):26) {
    temp.c <- c(classes[i], classes[j])
    pair.letter[[ind]] <- temp.c
    ind <- ind + 1
  }
}

multi_wsvm <- function (pair.letter, train, labels.train, 
                        weights, gamma, cost) {
  pairs <- pair.letter
  classes <- levels(labels.train)  
  models <- list()
  
  for (i in 1:length(pairs)) {
    # generate k(k-2)/2 classifiers
    # prepare sub data 
    bin <- pairs[[i]]
    sub <- as.matrix(train[labels.train == bin[1] | labels.train == bin[2], ])
    labels.sub <- labels.train[labels.train == bin[1] | labels.train == bin[2]]
    labels.sub <- as.vector(unclass(labels.sub))
    labels.sub <- ifelse(labels.sub==which(classes == bin[1]), 1, -1)
    
    subweights <- weights[labels.train == bin[1] | labels.train == bin[2]]
    subweights <- subweights/sum(subweights)
      
    # build binary classifier
    submodel <- wsvm(sub, labels.sub, subweights, 
                     kernel = list(type = 'linear', par=NULL), C = cost)
    models[[i]] <- submodel
    print(i)
  }
  
  return(models)
}



multi_wsvm.predict <- function (pair.letter, train, labels.train, 
                                test, labels.test, models) {
  
  matrix.pred <- matrix(0, dim(test)[1], length(pair.letter))
  new.X <- as.matrix(test)
  
  for (i in 1:length(pair.letter)) {
    bin <- pair.letter[[i]]
    sub <- as.matrix(train[labels.train == bin[1] | labels.train == bin[2], ])
    labels.sub <- labels.train[labels.train == bin[1] | labels.train == bin[2]]
    labels.sub <- as.vector(unclass(labels.sub))
    labels.sub <- ifelse(labels.sub==which(classes == bin[1]), 1, -1)
    
    sub.pred <- wsvm.predict(sub, labels.sub,
                               new.X, labels.test, models[[i]])
    bi.pred <- sub.pred$predicted.Y 
    bi.pred<- ifelse(bi.pred==1, bin[1], bin[2])
    print(table(bi.pred))
    matrix.pred[,i] <- bi.pred
  }
  
  pred <- c()
  
  #for (i in 1:dim(test)[1]) {
  #  pred[i] <- names(sort(table(matrix.pred[i,]), decreasing = TRUE)[1])
  #}
  
  #pred <- factor(pred, levels = levels(labels.train))
  #return(pred)
  return(matrix.pred)
}


ada_svm <- function (pair.letter, train, labels.train, 
                     test, labels.test, gamma, cost, t) {
  
  weights <- rep(1/dim(train)[1], dim(train)[1])
  error <- c()
  alpha <- c()
  acc <- c()
  models <- list()
  for (i in 1:t) {
    # fit svm
    model <- multi_wsvm(pair.letter, train, labels.train, 
                         weights, gamma, cost)
    models[[i]] <- model
    
    # update error rate and alpha
    train.pred <- multi_wsvm.predict(pair.letter, train, labels.train,
                                     train, labels.train, model)
    error[i] <- sum(weights[train.pred!=labels.train])
    alpha[i] <- log((1-error[i])/error[i])/2
    
    # update accuracy on test set
    test.pred <- matrix(0, dim(test)[1], t)
    for (j in 1:t) {
      test.pred[,j] <- multi_wsvm.predict(pair.letter, train, labels.train,
                                      test, labels.test, models[[j]])  
    }
    
    final.pred <- c()
    for (k in 1:dim(test)[1]) {
      temp <- test.pred[k,]  
      classes <- unique(temp)
    }
    
    acc[i] <- length(test.pred[test.pred==labels.test])/length(test.pred)
    
    # update weights
    sign <- ifelse(train.pred==labels.train, -1, 1)
    weights <- weights*exp(sign*alpha)
    weights <- weights/sum(weights)
  }
  
  ada.info <- list(weights, error, alpha, acc)
  return(ada.info)
}




totrain <- features.svm[fold == 1|fold == 2,]
tolabel <- labels[fold==1|fold==2]

tic <- Sys.time()
temp <- multi_wsvm(pair.letter, totrain, tolabel, 
                   rep(1/length(tolabel), length(tolabel)), 
                   0.5, 10)
Sys.time() - tic 

tic <- Sys.time()
pred <- multi_wsvm.predict(pair.letter, totrain, tolabel,
                           validation, labels.validation, temp)
Sys.time() - tic


yo <- wsvm(as.matrix(totrain[tolabel=='a'|tolabel=='b',]), 
           ifelse(tolabel[tolabel=='a'|tolabel=='b'] == 'a', 1, -1), 
           rep(1/length(tolabel[tolabel=='a'|tolabel=='b']), length(tolabel[tolabel=='a'|tolabel=='b'])),
           kernel = list(type = 'rbf', par = 0.004), C=10000)

he <- wsvm.predict(as.matrix(totrain[tolabel=='a'|tolabel=='b',]), 
                   ifelse(tolabel[tolabel=='a'|tolabel=='b'] == 'a', 1, -1), 
                   as.matrix(test[labels.test=='a'|labels.test=='b',]),
                   ifelse(labels.test[labels.test=='a'|labels.test=='b'] == 'a', 1, -1), 
                   yo,
                   TRUE)






