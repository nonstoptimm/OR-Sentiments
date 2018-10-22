# XGBOOST CLASSIFIER
# Determine Feature Importance of words
# predictScore.R
# Load required packages
library(Ckmeans.1d.dp) # required for the xgbVar-ggplot
library(dplyr)
library(pdp)
library(text2vec)
library(tidytext)
library(tidyverse)
library(xgboost)

# SET SEED
# For reproducible sampling
set.seed(101) 

# TAKE DATA SAMPLE ABOUT 30%, CHOOSE RANDOM LINES
sampleData <- function(input, x){ # x for amount of data
  sample <- sample.int(n = nrow(input), size = floor(x*nrow(input)), replace = F)
  return(sample)
}
# Apply sampleData-function
sampleCellphone <- sampleData(prep_cellphone_brand, .3) # 30% for Cellphones
sampleHeadphone <- sampleData(prep_headphone_brand, .1) # 10% for Headphones

# PULL SAMPLE OF DATASET
mediateCellphone <- as_tibble(prep_cellphone_brand[sampleCellphone, ])
mediateHeadphone <- as_tibble(prep_headphone_brand[sampleHeadphone, ])

# MEDIATE DATA
removeXGsw <- function(input) {
  input %>% 
    unnest_tokens(word, review) %>% # unnest the reviews to single words
    anti_join(stop_words) %>% # anti-join to predefined stopword list
    nest(word) %>% # nest the words again to list items
    mutate(review = map(data, unlist), # create review column and glue the terms together again
           review = map_chr(review, paste, collapse = " ")) # separate by a blank
}
# Apply removeXGsw-function
mediateCellphone <- removeXGsw(mediateCellphone)
mediateHeadphone <- removeXGsw(mediateHeadphone)

# CREATE TRAIN DATA
trainCellphone <- mediateCellphone[1:12000,] # training-data
trainHeadphone <- mediateHeadphone[1:15000,] # training-data

# CREATE TEST DATA
testCellphone <- mediateCellphone[12001:16198,] # test-data
testHeadphone <- mediateHeadphone[15001:20715,] # test-data

# CREATE THIRD DATA CHUNK
verifyCellphone <- prep_cellphone_brand[10000:14000,] # verification-data
verifyHeadphone <- prep_headphone_brand[15000:20000,] # verification-data

# CREATE VOCABULARY
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
# Mediate Data as input
createVoc <- function(input){
  vocab <- create_vocabulary(itoken(input$review, tokenizer = word_tokenizer))
  return(vocab)
}
# Apply createVoc-function
vocabCellphone <- createVoc(mediateCellphone)
vocabHeadphone <- createVoc(mediateHeadphone)

# BUILD DTM
# Build a document-term matrix using the tokenized review text. 
# This returns a dgCMatrix object
createDTM <- function(input, vocab){
  dtm <- create_dtm(itoken(input$review, tokenizer = word_tokenizer), vocab_vectorizer(vocab))
  return(dtm)
}
# Apply createDTM-function
# Train-Data
dtm_trainCellphone <- createDTM(trainCellphone, vocabCellphone)
dtm_trainHeadphone <- createDTM(trainHeadphone, vocabHeadphone)
# Test-Data
dtm_testCellphone <- createDTM(testCellphone, vocabCellphone)
dtm_testHeadphone <- createDTM(testHeadphone, vocabHeadphone)
# Verify-Data
dtm_verifyCellphone <- createDTM(verifyCellphone, vocabCellphone)
dtm_verifyHeadphone <- createDTM(verifyHeadphone, vocabHeadphone)

# TRAIN AND TEST SCORES
trainScoreCellphone <- trainCellphone$scoreNN
testScoreCellphone <- testCellphone$scoreNN

trainScoreHeadphone <- trainHeadphone$scoreNN
testScoreHeadphone <- testHeadphone$scoreNN

# CREATE XGB-MATRIX
# Turn the DTM into an XGB matrix using the sentiment scores that are to be learned
xgbMTrainCellphone <- xgb.DMatrix(dtm_trainCellphone, label = trainScoreCellphone)
xgbMTestCellphone <- xgb.DMatrix(dtm_testCellphone, label = testScoreCellphone)

xgbMTrainHeadphone <- xgb.DMatrix(dtm_trainHeadphone, label = trainScoreHeadphone)
xgbMTestHeadphone <- xgb.DMatrix(dtm_testHeadphone, label = testScoreHeadphone)

# CREATE WATCHLIST FOR PERFORMANCE MONITORING 
watchlistCellphone <- list(validation = xgbMTestCellphone, train=xgbMTrainCellphone)
watchlistHeadphone <- list(validation = xgbMTestHeadphone, train=xgbMTrainHeadphone)

# Build XGBOOST Model
xgb_params = list(
  objective = "reg:linear", # linear regression, as a continuous variable has to be predicted
  eta = 0.01,
  max.depth = 1000) # maximum depth 

# Train the model
trainXGB <- function(train_matrix, xgb_params, watchlist){
  xgb_fit <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 10000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE) # with verify process
  return(xgb_fit)
}
# Apply trainXGB-function
xgbCellphone <- trainXGB(xgbMTrainCellphone, xgb_params, watchlistCellphone)
xgbHeadphone <- trainXGB(xgbMTrainHeadphone, xgb_params, watchlistHeadphone)

# Create Feature Importance Overview
xgbImpVar <- function(input, cols){
  importance_vars <- xgb.importance(model = input, feature_names = colnames(cols))
  return(importance_vars)
}
# Apply xgbImpVar-function
importanceCellphone <- xgbImpVar(xgbCellphone, xgbMTrainCellphone)
importanceHeadphone <- xgbImpVar(xgbHeadphone, xgbMTrainHeadphone)

# CLEAN FORMAT
xgbImpVarClean <- function(input){
  importance_clean <- input[,`:=`(Cover=NULL, Frequency=NULL)]
  return(importance_clean)
}
# Apply xgbImpVarClean-function
importanceCellphone <- xgbImpVarClean(importanceCellphone)
importanceHeadphone <- xgbImpVarClean(importanceHeadphone)

# PLOT IMPORTANCE
xgbPlot <- function(input, var){
  xgb.ggplot.importance(importance_matrix = input, top_n = 20) +
  # ggtitle(paste("Feature Importance for", var, sep=" ")) +
    theme(text = element_text(size = 12, family = "LM Roman 10"))
}
# Apply xgbPlot-function
xgbPlot(importanceCellphone, "Cellphone")
xgbPlot(importanceHeadphone, "Headphone")

# PRINT MOST IMPORTANT FEATURES
head(importanceCellphone, 100)
head(importanceHeadphone, 100)
