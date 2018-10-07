# XGBOOST CLASSIFIER
# predictScore.R
# Load required packages
library(text2vec)
library(xgboost)
library(pdp)
library(dplyr)
library(tidytext)
library(tidyverse)
library(Ckmeans.1d.dp) # required for the xgbVar-ggplot

# SET SEED
# For reproducible sampling
set.seed(101) 

# TAKE DATA SAMPLE ABOUT 30%
sampleData <- function(input, x){ # x for amount of data
  sample <- sample.int(n = nrow(input), size = floor(x*nrow(input)), replace = F)
  return(sample)
}
# Apply sampleData-function
sampleCellphone <- sampleData(prep_cellphone_brand, .3)
sampleHeadphone <- sampleData(prep_headphone_brand, .3)
sampleToaster <- sampleData(prep_toaster_brand, .5)
sampleCoffee <- sampleData(prep_coffee_brand, .3)

# PULL SAMPLE OF DATASET
mediateCellphone <- as_tibble(prep_cellphone_brand[sampleCellphone, ])
mediateHeadphone <- as_tibble(prep_headphone_brand[sampleHeadphone, ])
mediateToaster <- as_tibble(prep_toaster_brand[sampleToaster, ])
mediateCoffee <- as_tibble(prep_coffee_brand[sampleCoffee, ])

# INDIVIDUAL STOPWORD LIST
# stopword_list <- c("a",	"about",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"between",	"both",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"it", "isn't",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"nor",	"only",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through", "to",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves")

# MEDIATE DATA
removeXGsw <- function(input){
  input %>% 
    unnest_tokens(word, review) %>% # unnest the reviews to single words
    # anti_join(stopword_list, by = c("word" = "value")) %>% # anti-join to predefined stopword list
    anti_join(stop_words) %>% # anti-join to predefined stopword list
    nest(word) %>% # nest the words again to list items
    mutate(review = map(data, unlist), # create review column and glue the terms together again
           review = map_chr(review, paste, collapse = " ")) # separate by a blank
}
# Apply removeXGsw-function
mediateCellphone <- removeXGsw(mediateCellphone)
mediateHeadphone <- removeXGsw(mediateHeadphone)
mediateToaster <- removeXGsw(mediateToaster)
mediateCoffee <- removeXGsw(mediateCoffee)

# CREATE TRAIN DATA
trainCellphone <- mediateCellphone[1:12000,] # training-data
trainHeadphone <- mediateHeadphone[1:50000,] # training-data
trainToaster <- mediateToaster[1:6000,] # training-data
trainCoffee <- mediateCoffee[1:19000,] # training-data

# CREATE TEST DATA
testCellphone <- mediateCellphone[12001:16198,] # test-data
testHeadphone <- mediateHeadphone[50001:62184,] # test-data
testToaster <- mediateToaster[6001:8002,] # test-data
testCoffee <- mediateCoffee[19001:25067,] # test-data

# CREATE THIRD DATA CHUNK
verifyCellphone <- prep_cellphone_brand[10000:14000,]
verifyHeadphone <- prep_headphone_brand[40001:50000,]
verifyToaster <- prep_toaster_brand[3000:4000,]
verifyCoffee <- prep_coffee_brand[12500:17500,]

# CREATE VOCABULARY
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
# Mediate Data as input
createVoc <- function(input){
  vocab <- create_vocabulary(itoken(input$review,
                           tokenizer = word_tokenizer))
  return(vocab)
}
# Apply createVoc-function
vocabCellphone <- createVoc(mediateCellphone)
vocabHeadphone <- createVoc(mediateHeadphone)
vocabToaster <- createVoc(mediateToaster)
vocabCoffee <- createVoc(mediateCoffee)

# BUILD DTM
# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
createDTM <- function(input, vocab){
  dtm <- create_dtm(itoken(input$review, tokenizer = word_tokenizer), vocab_vectorizer(vocab))
  return(dtm)
}
# Apply createDTM-function
# Train
dtm_trainCellphone <- createDTM(trainCellphone, vocabCellphone)
dtm_trainHeadphone <- createDTM(trainHeadphone, vocabHeadphone)
dtm_trainToaster <- createDTM(trainToaster, vocabToaster)
dtm_trainCoffee <- createDTM(trainCoffee, vocabCoffee)
# Test
dtm_testCellphone <- createDTM(testCellphone, vocabCellphone)
dtm_testHeadphone <- createDTM(testHeadphone, vocabHeadphone)
dtm_testToaster <- createDTM(testToaster, vocabToaster)
dtm_testCoffee <- createDTM(testCoffee, vocabCoffee)
# Verify
dtm_verifyCellphone <- createDTM(verifyCellphone, vocabCellphone)
dtm_verifyHeadphone <- createDTM(verifyHeadphone, vocabHeadphone)
dtm_verifyToaster <- createDTM(verifyToaster, vocabToaster)
dtm_verifyCoffee <- createDTM(verifyCoffee, vocabCoffee)

# TRAIN AND TEST SCORES
trainScoreCellphone <- trainCellphone$scoreNN
testScoreCellphone <- testCellphone$scoreNN

trainScoreHeadphone <- trainHeadphone$scoreNN
testScoreHeadphone <- testHeadphone$scoreNN

trainScoreToaster <- trainToaster$scoreNN
testScoreToaster <- testToaster$scoreNN  

trainScoreCoffee <- trainCoffee$scoreNN
testScoreCoffee <- testCoffee$scoreNN

# CREATE XGB-MATRIX
# Turn the DTM into an XGB matrix using the sentiment scores that are to be learned
xgbMTrainCellphone <- xgb.DMatrix(dtm_trainCellphone, label = trainScoreCellphone)
xgbMTestCellphone <- xgb.DMatrix(dtm_testCellphone, label = testScoreCellphone)

xgbMTrainHeadphone <- xgb.DMatrix(dtm_trainHeadphone, label = trainScoreHeadphone)
xgbMTestHeadphone <- xgb.DMatrix(dtm_testHeadphone, label = testScoreHeadphone)

xgbMTrainToaster <- xgb.DMatrix(dtm_trainToaster, label = trainScoreToaster)
xgbMTestToaster <- xgb.DMatrix(dtm_testToaster, label = testScoreToaster)

xgbMTrainCoffee <- xgb.DMatrix(dtm_trainCoffee, label = trainScoreCoffee)
xgbMTestCoffee <- xgb.DMatrix(dtm_testCoffee, label = testScoreCoffee)

# CREATE WATCHLIST FOR PERFORMANCE MONITORING 
watchlistCellphone <- list(validation = xgbMTestCellphone, train=xgbMTrainCellphone)
watchlistHeadphone <- list(validation = xgbMTestHeadphone, train=xgbMTrainHeadphone)
watchlistToaster <- list(validation = xgbMTestToaster, train=xgbMTrainToaster)
watchlistCoffee <- list(validation = xgbMTestCoffee, train=xgbMTrainCoffee)

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
# Apply trainXGB-function
xgbCellphone <- trainXGB(xgbMTrainCellphone, xgb_params, watchlistCellphone)
saveXG(xgbCellphone, "XG_Cellphones")
xgbHeadphone <- trainXGB(xgbMTrainHeadphone, xgb_params, watchlistHeadphone)
xgbCoffee <- trainXGB(xgbMTrainCoffee, xgb_params, watchlistCoffee)
saveXG(xgbCoffee, "XG_Coffee")
xgbToaster <- trainXGB(xgbMTrainToaster, xgb_params, watchlistToaster)
saveXG(xgbToaster, "XG_Toaster")

# PREDICT THIRD DATASET
prediction <- predict(xgb_fit, dtm_test2)

# Create Feature Importance Overview
xgbImpVar <- function(input, cols){
  importance_vars <- xgb.importance(model=input, feature_names = colnames(cols))
  return(importance_vars)
}
# Apply xgbImpVar-function
importanceCellphone <- xgbImpVar(xgbCellphone, xgbMTrainCellphone)
importanceHeadphone <- xgbImpVar(xgbHeadphone, xgbMTrainHeadphone)
importanceToaster <- xgbImpVar(xgbToaster, xgbMTrainToaster)
importanceCoffee <- xgbImpVar(xgbCoffee, xgbMTrainCoffee)

# CLEAN FORMAT
xgbImpVarClean <- function(input){
  importance_clean <- input[,`:=`(Cover=NULL, Frequency=NULL)]
  return(importance_clean)
}
# Apply xgbImpVarClean-function
importanceCellphone <- xgbImpVarClean(importanceCellphone)
importanceHeadphone <- xgbImpVarClean(importanceHeadphone)
importanceToaster <- xgbImpVarClean(importanceToaster)
importanceCoffee <- xgbImpVarClean(importanceCoffee)

# PLOT IMPORTANCE
xgbPlot <- function(input, var){
  xgb.ggplot.importance(importance_matrix = input, top_n = 20) +
  ggtitle(paste("Variable Importance for the XGBOOST model of", var, sep=" ")) 
  #+ geom_bar(stat = "identity", width = 10)
}
# Apply xgbPlot-function
xgbPlot(importanceCellphone, "Cellphone")
xgbPlot(importanceHeadphone, "Headphone")
xgbPlot(importanceToaster, "Toaster")
xgbPlot(importanceCoffee, "Coffee")

# PRINT MOST IMPORTANT FEATURES
head(importanceCellphone, 100)
head(importanceHeadphone, 100)
head(importanceToaster, 100)
head(importanceCoffee, 100)
