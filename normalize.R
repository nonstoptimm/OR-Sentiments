# Install Packages
install.packages("class")
install.packages("gmodels")

# Library
library(class)
library(gmodels)

# Create Normalize Function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

normalized_score <- normalize(prep_headphone_brand$scoreNN)
normalized_star <- normalize(prep_headphone_brand$overall)

# Normalize the data
winequality_n <- as.data.frame(lapply(winequality[1:11], normalize))
validation_n <- as.data.frame(lapply(validation[1:11], normalize))

# Train the model with different K
prc_test_pred <- knn(train = winequality_n, test = validation_n,cl = winequality[,12], k=1)
prc_test_pred <- knn(train = winequality_n, test = validation_n,cl = winequality[,12], k=10)
prc_test_pred <- knn(train = winequality_n, test = validation_n,cl = winequality[,12], k=40)

# Create Crosstable
CrossTable(x = validation[,12], y = prc_test_pred, prop.chisq = FALSE)
