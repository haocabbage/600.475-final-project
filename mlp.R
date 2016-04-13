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

# ======================MLP with one hidden layer=========================
library(h2o)
localH2O = h2o.init()

training <- as.data.frame(cbind(labels.train, train))
training_h2o <- as.h2o(training, "training")
validation_h2o <- as.h2o(as.data.frame(validation), "validation")

mlp <- function (node, epoch, key, test, labels.test) {
  tic <- Sys.time()
  model <- 
    h2o.deeplearning(x = 2:201,  # column numbers for predictors
                     y = 1,   # column number for label
                     training_frame = key, # data in H2O format
                     activation = "RectifierWithDropout", # or 'Tanh'
                     input_dropout_ratio = 0.2, # % of inputs dropout
                     hidden_dropout_ratios = c(0.5), # % for nodes dropout
                     balance_classes = FALSE, 
                     hidden = c(node), # one layer
                     epochs = epoch) # max. no. of epochs
  print(Sys.time() - tic)
  
  temp.pred <- h2o.predict(model, test)
  temp.pred <- factor(as.data.frame(temp.pred)[,1], levels = levels(labels.test))
  
  print(length(temp.pred[temp.pred==labels.test])/length(temp.pred))
  return(temp.pred)
}


for (i in 1:8) {
  mlp(i*100, 2000, "training", validation_h2o, labels.validation)
}

mlp(600, 1000, "training", validation_h2o, labels.validation)

total <- cbind(labels[fold != 9], features.svm[fold != 9, ])
total_h2o <- as.h2o(as.data.frame(total), "total")
test_h2o <- as.h2o(as.data.frame(test), "test")

mlp.pred <- mlp(600, 1000, "total", test_h2o, labels.test)

write.csv(mlp.pred, file = "mlp_pred.csv", row.names = FALSE)
