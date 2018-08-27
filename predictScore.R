# predictScore.R
# XGBOOST CLASSIFIER
# Load required packages
library(text2vec)
library(xgboost)
library(pdp)
library(dplyr)
library(tidytext)
library(tidyverse)

# SET SEED
# For reproducible sampling
set.seed(101) 

# TAKE DATA SAMPLE ABOUT 30%
sampleData <- function(input){
  sample <- sample.int(n = nrow(input), size = floor(.3*nrow(input)), replace = F)
  return(sample)
}
# Apply sampleData Function
sampleHeadphones <- sampleData(prep_headphone_brand)
sampleCellphone <- sampleData(prep_cellphone_brand)
sampleCoffee <- sampleData(prep_coffee_brand)
sampleToaster <- sampleData(prep_toaster_brand)

# PULL SAMPLE OF DATASET
mediateHeadphone <- as_tibble(prep_headphone_brand[sampleHeadphone, ])
mediateCellphone <- as_tibble(prep_cellphone_brand[sampleCellphone, ])
mediateCoffee <- as_tibble(prep_coffee_brand[sampleCoffee, ])
mediateToaster <- as_tibble(prep_toaster_brand[sampleToaster, ])

# INDIVIDUAL STOPWORD LIST
stopword_list <- c("a",	"about",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"between",	"both",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"isn't",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"nor",	"only",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves")
#stopword_list <- as_vector()

# MEDIATE DATA
removeXGsw <- function(input){
  input %>% 
    unnest_tokens(word, review) %>% # unnest the reviews to single words
    anti_join(stopword_list, by = c("word" = "value")) %>% # anti-join to predefined stopword list
    nest(word) %>% # nest the words again to list items
    mutate(review = map(data, unlist), # create review column and glue the terms together again
           review = map_chr(review, paste, collapse = " ")) # separate by a blank
}
# Apply Function
mediateHeadphone <- removeXGsw(mediateHeadphone)
mediateCellphone <- removeXGsw(mediateCellphone)
mediateCoffee <- removeXGsw(mediateCoffee)
mediateToaster <- removeXGsw(mediateToaster)

# CREATE TRAIN DATA
trainHeadphone <- mediateHeadphone[1:50000,] # training-data
trainCellphone<- mediateCellphone[1:50000,] # training-data
trainToaster <- mediateToaster[1:50000,] # training-data
trainCoffee <- mediateCoffee[1:50000,] # training-data

# CREATE TEST DATA
testHeadphone <- mediateHeadphone[50001:62187,] # test-dataset
testCellphone <- mediateCellphone[50001:62187,] # test-dataset
testToaster <- mediateToaster[50001:62187,] # test-dataset
testCoffee <- mediateCoffee[50001:62187,] # test-dataset

# CREATE THIRD DATA CHUNK
verifyHeadphone <- prep_headphone_brand[40001:50000]
verifyCellphone <- prep_cellphone_brand[40001:50000]
verifyToaster <- prep_toaster_brand[40001:50000]
verifyCoffee <- prep_coffee_brand[40001:50000]

# CREATE VOCABULARY
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
# Mediate Data as input
createVoc <- function(input){
  vocab <- create_vocabulary(itoken(input$review,
                           tokenizer = word_tokenizer))
  return(vocab)
}
# Apply createVoc Function
vocabHeadphone <- createVoc(mediateHeadphone)
vocabCellphone <- createVoc(mediateCellphone)
vocabCoffee <- createVoc(mediateCoffee)
vocabToaster <- createVoc(mediateToaster)

# BUILD DTM
# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
createDTM <- function(input, vocab){
  dtm <- create_dtm(itoken(input$review, tokenizer = word_tokenizer), vocab_vectorizer(vocab))
  return(dtm)
}
# Apply createDTM Function
# Train
dtm_trainHeadphone <- createDTM(trainHeadphone, vocabHeadphone)
dtm_trainCellphone <- createDTM(trainCellphone, vocabCellphone)
dtm_trainCoffee <- createDTM(trainCoffee, vocabCoffee)
dtm_trainToaster <- createDTM(trainToaster, vocabToaster)
# Test
dtm_testHeadphone <- createDTM(testHeadphone, vocabHeadphone)
dtm_testCellphone <- createDTM(testCellphone, vocabCellphone)
dtm_testCoffee <- createDTM(testCoffee, vocabCoffee)
dtm_testToaster <- createDTM(testToaster, vocabToaster)
# Verify
dtm_verifyHeadphone <- createDTM(verifyHeadphone, vocabHeadphone)
dtm_verifyCellphone <- createDTM(verifyCellphone, vocabCellphone)
dtm_verifyCoffee <- createDTM(verifyCoffee, vocabCoffee)
dtm_verifyToaster <- createDTM(verifyToaster, vocabToaster)

# TRAIN AND TEST SCORES
trainScoreHeadphone <- trainHeadphone$scoreNN
testScoreHeadphone <- testHeadphone$scoreNN

trainScoreCellphone <- trainCellphone$scoreNN
testScoreCellphone <- testCoffee$scoreNN
  
trainScoreCoffee <- trainCoffee$scoreNN
testScoreCoffee <- testCoffee$scoreNN

trainScoreToaster <- trainToaster$scoreNN
testScoreToaster <- testToaster$scoreNN

# CREATE XGB MATRIX
# Turn the DTM into an XGB matrix using the sentiment labels that are to be learned
xgbMTrainHeadphone <- xgb.DMatrix(dtm_trainHeadphone, label = trainScoreHeadphone)
xgbMTestHeadphone <- xgb.DMatrix(dtm_testHeadphone, label = testScoreHeadphone)

xgbMTrainCellphone <- xgb.DMatrix(dtm_trainCellphone, label = trainScoreCellphone)
xgbMTestCellphone <- xgb.DMatrix(dtm_testCellphone, label = testScoreCellphone)

xgbMTrainCoffee <- xgb.DMatrix(dtm_trainCoffee, label = trainScoreCoffee)
xgbMTestCoffee <- xgb.DMatrix(dtm_testCoffee, label = testScoreCoffee)

xgbMTrainToaster <- xgb.DMatrix(dtm_trainToaster, label = trainScoreToaster)
xgbMTestToaster <- xgb.DMatrix(dtm_testToaster, label = testScoreToaster)

# Create a watchlist
watchlistHeadphone <- list(validation = xgbMTestHeadphone, train=xgbMTrainHeadphone)
watchlistCellphone <- list(validation = xgbMTestCellphone, train=xgbMTrainCellphone)
watchlistCoffee <- list(validation = xgbMTestCoffee, train=xgbMTrainCoffee)
watchlistToaster <- list(validation = xgbMTestToaster, train=xgbMTrainToaster)

# Build XGBOOST Model
xgb_params = list(
  objective = "reg:linear", # linear regression as a continuous variable has to be predicted
  eta = 0.01,
  max.depth = 1000)

# Train the model
trainXGB <- function(train_matrix, xgb_params, watchlist){
  #xgb_fit <- xgboost(params = xgb_params, data = train_matrix, nrounds = 10000, early_stopping_rounds = 20, maximize = FALSE) # without verify process
  xgb_fit <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 10000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE) # with verify process
  return(xgb_fit)
}
# Apply trainXGB Function
xgbHeadphone <- trainXGB(xgbMTrainHeadphone, xgb_params, watchlistHeadphone)
xgbCellphone <- trainXGB(xgbMTrainCellphone, xgb_params, watchlistCellphone)
xgbCoffee <- trainXGB(xgbMTrainCoffee, xgb_params, watchlistCoffee)
xgbToaster <- trainXGB(xgbMTrainToaster, xgb_params, watchlistToaster)

# PREDICT THIRD DATASET
prediction <- predict(xgb_fit, dtm_test2)

# Create Feature Importance Overview
xgbImpVar <- function(input, cols){
  importance_vars <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))
  return(importance_vars)
}
# Apply xgbImpVar Function
importanceHeadphone <- xgbImpVar(xgbHeadphone, xgbMTrainHeadphone)
importanceCellphone <- xgbImpVar(xgbCellphone, xgbMTrainCellphone)
importanceCoffee <- xgbImpVar(xgbCoffee, xgbMTrainCoffee)
importanceToaster <- xgbImpVar(xgbToaster, xgbMTrainToaster)

# CLEAN FORMAT
importanceClean <- importance_vars[,`:=`(Cover=NULL, Frequency=NULL)]
xgb.plot.importance(importance_matrix = importance_vars, top_n = 20)

# PRINT MOST IMPORTANT FEATURES
head(importanceHeadphone, 100)
head(importanceCellphone, 100)
head(importanceCoffee, 100)
head(importanceToaster, 100)


