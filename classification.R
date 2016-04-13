# read data
setwd("~/Dropbox/OCR_Project/code") 

letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)
size <- dim(letter_data)[1]

labels <- factor(letter_data[,2])
fold <- factor(letter_data[,6])
per.class <- as.matrix(table(labels))
per.class <- per.class/sum(per.class)

# =============svm==============
# feature scaling such that each feature has mean 0 and sd 1
library(e1071)

temp.train <- features.scale[fold != "9", ] 
temp.test <- features.scale[fold == "9", ] 

temp.labels.train <- labels[fold != "9"]
temp.labels.test <- labels[fold == "9"]

# radial kernel
runtime <- Sys.time()
temp.model <- svm(temp.train, temp.labels.train, scale = FALSE, cost = 10)
Sys.time() - runtime

runtime <- Sys.time()
temp.pred <- predict(temp.model, temp.test)
Sys.time() - runtime

temp.matched <- length(temp.pred[temp.pred == temp.labels.test])
print(temp.matched/length(temp.pred))

# parallel
library(parallelSVM)

runtime <- Sys.time()
temp.model <- parallelSVM(temp.train, temp.labels.train, scale = FALSE, cost = 10)
Sys.time() - runtime

runtime <- Sys.time()
temp.pred <- predict(temp.model, temp.test)
Sys.time() - runtime

temp.matched <- length(temp.pred[temp.pred == temp.labels.test])
print(temp.matched/length(temp.pred))


# ==========testing============
# wSVM
library(quadprog)
library(MASS)
library(wSVM)

temp.train <- as.matrix(temp.train)
temp.test <- as.matrix(temp.test)
temp.labels.train <- as.vector(unclass(temp.labels.train))
temp.labels.test <- as.vector(unclass(temp.labels.test))
temp.bin.train <- ifelse(temp.labels.train==1, 1, -1)
temp.bin.test <- ifelse(temp.labels.test==1, 1, -1)

runtime <- Sys.time()
temp.model <- wsvm(temp.train, temp.bin.train, c.n=rep(1/47010, 47010), 
                   kernel = list(type='rbf', par=1/size), C=10, eps=0.1)
Sys.time() - runtime

runtime <- Sys.time()
temp.pred <- wsvm.predict(temp.train, temp.bin.train, temp.test, 
                          temp.bin.test, comp.error.rate = TRUE)
Sys.time() - runtime

print(temp.pred$error.rate)

runtime <- Sys.time()
temp.kernel <- wsvm.kernel(temp.train, temp.train, kernel=list(type ='rbf', par = 1/47010))
Sys.time() - runtime

# get knn
library(FNN)

tic <- Sys.time() 
info.nn <- get.knnx(temp.train, temp.test, k=10)
toc <- Sys.time()
toc - tic

tic <- Sys.time() 
info.nn <- get.knnx(temp.train, temp.test, k=100)
toc <- Sys.time()
toc - tic

tic <- Sys.time() 
info.nn <- get.knnx(temp.train, temp.test, k=1000)
toc <- Sys.time()
toc - tic

# =============knn==============
library(class)

runtime <- Sys.time()

temp.train <- features[fold != "9", ] 
temp.test <- features[fold == "9", ] 
temp.pred <- knn1(temp.train, temp.test, temp.labels.train)

temp.matched <- length(temp.pred[temp.pred == temp.labels.test])
acc.knn <- temp.matched/length(temp.pred)

print(acc.knn)
Sys.time() - runtime

# ==========knn + svm===========
library(gputools)

# distance matrix
runtime <- Sys.time()
matrix.dist <- as.matrix(dist(features))
Sys.time() - runtime 

