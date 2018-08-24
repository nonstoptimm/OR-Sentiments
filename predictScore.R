# XGBOOST CLASSIFIER #
library(text2vec)
library(xgboost)
library(pdp)
library(dplyr)
library(tidytext)
library(tidyverse)

# Create the document term matrix (bag of words) using the movie_review data frame provided
# in the text2vec package (sentiment analysis problem)
#data("movie_review")
train <- prep_headphone_brand[1:10000,]
test <- prep_headphone_brand[10001:12000,]
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
vocab <- create_vocabulary(itoken(train$review,
                                  preprocessor = tolower,
                                  tokenizer = word_tokenizer))

# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
dtm_train <- create_dtm(itoken(train$review,
                               preprocessor = tolower,
                               tokenizer = word_tokenizer),
                                vocab_vectorizer(vocab))

dtm_test <- create_dtm(itoken(test$review,
                              preprocessor = tolower,
                              tokenizer = word_tokenizer),
                              vocab_vectorizer(vocab))

# Turn the DTM into an XGB matrix using the sentiment labels that are to be learned
train_matrix <- xgb.DMatrix(dtm_train, label = train.y)
test_matrix <- xgb.DMatrix(dtm_test, label = validation.y)

watchlist <- list(validation = test_matrix, train=train_matrix)

# Validation
bound <- floor(nrow(prep_headphone_brand) * 0.9)
train <- train[sample(nrow(train)), ]
df.train <- train[1:bound, ]
df.validation <- train[(bound+1):nrow(train),]

train.y <- train$scoreNN
validation.y <- test$scoreNN

# xgboost model building
xgb_params = list(
  objective = "reg:linear",
  eta = 0.01,
  max.depth = 1000)

# xgb_fit <- xgboost(data = train_matrix, 
#         objective = "reg:linear",
#         eval_metric = "rmse",
#         max.depth =15, 
#         eta = 0.1, 
#         nround = 15, 
#         subsample = 0.5, 
#         colsample_bytree = 0.5, 
#         num_class = 12,
#         nthread = 3
# )

# Model
# xgb.fit <- xgboost(data = train_matrix,
#                    booster = "gbtree", objective = "reg:linear",
#                    colsample_bytree = 0.2, gamma = 0.0,
#                    learning_rate = 0.05, max_depth = 6,
#                    min_child_weight = 1.5, n_estimators = 7300,
#                    reg_alpha = 0.9, reg_lambda = 0.5,
#                    subsample = 0.2, seed = 42,
#                    silent = 1, nrounds = 25)

xgb_fit <- xgboost(params = xgb_params, data = train_matrix, nrounds = 1000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE)
xgb_fit <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 1000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE)



# Check the feature importance
importance_vars <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))
head(importance_vars, 100)

# Try to plot a partial dependency plot of one of the features
partial(xgb_fit, train = movie_review, pred.var = "bad")

xgb.plot.importance(importance_vars, top_n = 20)

######

#create train and test

train <- merged_topic_headphone[1:100000,] 
test <- merged_topic_headphone[100001:219372,]

makeFeatures <- function(train) {
  
  labeledTerms = makeDTM(train)
  
  ## Preparing the features for the XGBoost Model
  
  features <- colnames(labeledTerms)
  
  for (f in features) {
    if ((class(labeledTerms[[f]])=="factor") || (class(labeledTerms[[f]])=="character")) {
      levels <- unique(labeledTerms[[f]])
      labeledTerms[[f]] <- as.numeric(factor(labeledTerms[[f]], levels=levels))
    }
  }
  
  return(labeledTerms)
}

labeledTerms = makeFeatures(train)

labeledTermsTest = makeFeatures(test)

colnamesSame = intersect(colnames(labeledTerms),colnames(labeledTermsTest))

labeledTerms = labeledTerms[ , (colnames(labeledTerms) %in% colnamesSame)]
labeledTermsTest = labeledTermsTest[ , (colnames(labeledTermsTest) %in% colnamesSame)]

#################

# Set seed for reproducible sampling
set.seed(101) 
# Take a small sample of the data
sample <- sample.int(n = nrow(prep_headphone_brand), size = floor(.3*nrow(prep_headphone_brand)), replace = F)
# Pull sample from dataset
mediate <- prep_headphone_brand[sample, ]
stopword_list <- c("a",	"about",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"between",	"both",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"isn't",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"nor",	"only",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves")
stopword_list <- as.tibble(stopword_list)
mediate <- as.tibble(mediate)
mediate <- mediate %>% unnest_tokens(word, review) %>% anti_join(stopword_list, by = c("word" = "value")) %>%
  nest(word) %>%
  mutate(review = map(data, unlist), 
         review = map_chr(review, paste, collapse = " ")) 

# Create train and test dataset
train <- mediate[1:50000,]
test <- mediate[50001:62187,]

# Create vocabulary
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
vocab <- create_vocabulary(itoken(mediate$review,
                                  tokenizer = word_tokenizer))

# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
dtm_train <- create_dtm(itoken(train$review, tokenizer = word_tokenizer),
                        vocab_vectorizer(vocab))

dtm_test <- create_dtm(itoken(test$review,
                              preprocessor = tolower,
                              tokenizer = word_tokenizer),
                       vocab_vectorizer(vocab))

# Train and Test Scores
train.y <- train$scoreNN
validation.y <- test$scoreNN

# Turn the DTM into an XGB matrix using the sentiment labels that are to be learned
train_matrix <- xgb.DMatrix(dtm_train, label = train.y)
test_matrix <- xgb.DMatrix(dtm_test, label = validation.y)

# Create a watchlist
watchlist <- list(validation = test_matrix, train=train_matrix)

# xgboost model building
xgb_params = list(
  objective = "reg:linear",
  eta = 0.01,
  max.depth = 1000)

# Train the model
xgb_fit <- xgboost(params = xgb_params, data = train_matrix, nrounds = 1000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE)
xgb_fit <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 10000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE)

# Check the feature importance
importance_vars <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))
head(importance_vars, 100)
