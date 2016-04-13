# read data
setwd("/Users/haocabbage/Desktop/Letter_Recognition")

letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)

labels <- factor(letter_data[,2])
per.class <- as.matrix(table(labels))
per.class <- per.class/sum(per.class)
fold <- factor(letter_data[,6])
features <- read.csv("sample_gaussian.csv")[2:201]
features <- sqrt(features)

pixels <- read.csv("pixels.csv")[2:129]
pixels.norm <- read.csv("pixels_norm.csv")[2:626]
subimages <- read.csv("subimages.csv")[2:5001]

# =============knn==============
library(class)

runtime <- proc.time()

temp.labels.train <- labels[fold != "9"]
temp.labels.test <- labels[fold == "9"]

temp.train <- features[fold != "9", ] 
temp.test <- features[fold == "9", ] 
temp.pred <- knn1(temp.train, temp.test, temp.labels.train)

temp.matched <- length(temp.pred[temp.pred == temp.labels.test])
acc.knn <- temp.matched/length(temp.pred)

print(acc.knn)
proc.time() - runtime


# =============svm==============
# feature scaling such that each feature has mean 0 and sd 1
features.scale <- features

for (i in 1:200) {
  temp.mean <- mean(features[,i])
  temp.sd <- sd(features[,i])
  features.scale[,i] <- (features.scale[,i] - temp.mean)/temp.sd
}

write.csv(features.scale, file="svm_features.csv")

library(e1071)

# ==========knn + svm===========
# distance matrix
runtime <- proc.time()
matrix.dist <- as.matrix(dist(features))
proc.time() - runtime

# ==========testing============
temp.train <- features.scale[fold != "9", ] 
temp.test <- features.scale[fold == "9", ] 

# radial kernel
runtime <- proc.time()
temp.model <- svm(temp.train, temp.labels.train, scale = FALSE, cost = 10)
proc.time() - runtime

runtime <- proc.time()
temp.pred <- predict(temp.model, temp.test)
proc.time() - runtime

temp.matched <- length(temp.pred[temp.pred == temp.labels.test])
print(temp.matched/length(temp.pred))




