# XGBOOST CLASSIFIER #

library(text2vec)
library(xgboost)
library(pdp)

# Create the document term matrix (bag of words) using the movie_review data frame provided
# in the text2vec package (sentiment analysis problem)
data("movie_review")

# Tokenize the movie reviews and create a vocabulary of tokens including document counts
vocab <- create_vocabulary(itoken(movie_review$review,
                                  preprocessor = tolower,
                                  tokenizer = word_tokenizer))

# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
dtm_train <- create_dtm(itoken(movie_review$review,
                               preprocessor = tolower,
                               tokenizer = word_tokenizer),
                        vocab_vectorizer(vocab))

# Turn the DTM into an XGB matrix using the sentiment labels that are to be learned
train_matrix <- xgb.DMatrix(dtm_train, label = movie_review$sentiment)

# xgboost model building
xgb_params = list(
  objective = "binary:logistic",
  eta = 0.01,
  max.depth = 5,
  eval_metric = "auc")

xgb_fit <- xgboost(data = train_matrix, params = xgb_params, nrounds = 20)

# Check the feature importance
importance_vars <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))
head(importance_vars, 20)

# Try to plot a partial dependency plot of one of the features
partial(xgb_fit, train = movie_review, pred.var = "bad")