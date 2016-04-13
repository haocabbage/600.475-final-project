# read data
setwd("~/Dropbox/OCR_Project/code")

letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)
labels <- factor(letter_data[,2])

size <- dim(letter_data)[1]
fold <- factor(letter_data[,6])

features.svm <- read.csv("svm_features.csv")[,2:201]

train <- features.svm[fold != 9 & fold != 8, ]
validation <- features.svm[fold == 8, ]
test <- features.svm[fold == 9, ]

labels.train <- labels[fold != 9 & fold != 8]
labels.validation <- labels[fold == 8]
labels.test <- labels[fold == 9]

# import libraries
library(e1071)
library(foreach)
library(iterators)
library(snow)
library(doSNOW)

# ==========loose grid search for C, gamma==================
C.tune <- 2^(c(-3,-1,1,3,5,7))
gamma.tune <- 2^(c(-19,-17,-15,-13,-11,-9))

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

tic <- Sys.time()
svm.models <- foreach(i=1:length(par.tune), .packages = 
                      'e1071') %dopar% {svm(train, labels.train, scale=FALSE,
                                       gamma=par.tune[[i]][2],
                                       cost=par.tune[[i]][1])}
Sys.time() - tic

stopCluster(cl)

svm.pred <- list()
tic <- Sys.time()
for (i in 1:length(par.tune)) {
  svm.pred[[i]] <- predict(svm.models[[i]], validation)   
}
Sys.time() - tic

svm.acc <- foreach(i=1:length(par.tune), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                                      ==svm.pred[[i]]])/length(labels.validation)}


#===============data export==================
temp.C <- c()
temp.gamma <- c()

for (i in 1:length(par.tune)) {
  temp.C[i] <- par.tune[[i]][1]
  temp.gamma[i] <- par.tune[[i]][2]
}

grid_search.loose <- cbind(temp.C, temp.gamma, svm.acc) 
colnames(grid_search.loose) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.loose, file='loose_grid_search.csv', row.names = FALSE)

library(ggplot2)

ggplot(as.data.frame(grid_search.loose), aes(x=C, y=gamma)) + 
  stat_density2d(aes(alpha=..level.., fill=..level.., weight=accuracy), 
                 size=2, bins=10, geom="polygon") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  geom_density2d(colour="black", bins=10, aes(weight=accuracy)) +
  geom_point(data = as.data.frame(grid_search.loose)) +
  guides(alpha=FALSE) 



# ================fine grid search for C, gamma==============
C.fine <- 2^(c(2.75, 3, 3.25, 3.5, 3.75, 4))
gamma.fine <- 2^(c(-8.95, -8.85, -8.75, -8.65, -8.55, -8.45))

par.fine <- vector('list', length(C.fine)*length(gamma.fine))

ind <- 1
for (i in 1:length(C.fine)) {
  for (j in 1:length(gamma.fine)) {
    par.fine[[ind]] <- c(C.fine[i], gamma.fine[j])
    ind <- ind + 1
  }
}

cl <- makeCluster(36)
registerDoSNOW(cl)

tic <- Sys.time()
svm.models <- foreach(i=1:length(par.fine), .packages = 
                        'e1071') %dopar% {svm(train, labels.train, scale=FALSE,
                                              gamma=par.fine[[i]][2],
                                              cost=par.fine[[i]][1],
                                              cachesize=500)}
Sys.time() - tic

stopCluster(cl)

svm.pred <- list()

tic <- Sys.time()
for (i in 1:length(par.fine)) {
  svm.pred[[i]] <- predict(svm.models[[i]], validation)   
}
Sys.time() - tic

svm.acc <- foreach(i=1:length(par.fine), .combine = 'c') %do% {length(labels.validation[labels.validation
                                                               ==svm.pred[[i]]])/length(labels.validation)}


#===============data export==================
temp.C <- c()
temp.gamma <- c()

for (i in 1:length(par.fine)) {
  temp.C[i] <- par.fine[[i]][1]
  temp.gamma[i] <- par.fine[[i]][2]
}

grid_search.fine <- cbind(temp.C, temp.gamma, svm.acc) 
colnames(grid_search.fine) <- c('C', 'gamma', 'accuracy')
write.csv(grid_search.fine, file='fine_grid_search.csv', row.names = FALSE)

v <- ggplot(as.data.frame(grid_search.fine), aes(x=C, y=gamma)) + 
  stat_density2d(aes(alpha=..level.., fill=..level.., weight=accuracy), 
                 size=2, bins=10, geom="polygon") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  geom_density2d(colour="black", bins=10, aes(weight=accuracy)) +
  geom_point(data = as.data.frame(grid_search.loose)) +
  guides(alpha=FALSE) 

v


# ===============svm with optimal C, gamma====================
grid_search.fine <- read.csv('fine_grid_search.csv')
pairs <- grid_search.fine[order(grid_search.fine[,3]), ]

C.opt <- tail(pairs[,1], 1)
gamma.opt <- tail(pairs[,2], 1)

svm.fit <- svm(features.svm[fold != 9, ], labels[fold != 9], scale=FALSE,
               gamma=gamma.opt, cost=C.opt, cachesize=500)

svm.pred <- predict(svm.fit, test)
svm.acc <- length(svm.pred[svm.pred==labels.test])/length(svm.pred)

write.csv(svm.pred, file = 'svm_pred.csv', row.names = FALSE)
