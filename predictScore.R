# XGBOOST CLASSIFIER #
library(text2vec)
library(xgboost)
library(pdp)
library(dplyr)
library(tidytext)
library(tidyverse)

#################

# Set seed for reproducible sampling
set.seed(101) 
# Take a small sample of the data
sample <- sample.int(n = nrow(prep_headphone_brand), size = floor(.3*nrow(prep_headphone_brand)), replace = F)
# Pull sample from dataset
mediate <- prep_headphone_brand[sample, ]
# Create individual stopword-list
stopword_list <- c("a",	"about",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"between",	"both",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"isn't",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"nor",	"only",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves")

# Create tibbles, so that it can be proceeded by dplyr and the unnest/nest/anti-join-functions
stopword_list <- as.tibble(stopword_list)
mediate <- as.tibble(mediate)
mediate <- mediate %>% 
            unnest_tokens(word, review) %>% # unnest the reviews to single words
            anti_join(stopword_list, by = c("word" = "value")) %>% # anti-join to predefined stopword list
            nest(word) %>% # nest the words again to list items
            mutate(review = map(data, unlist), # create review column and glue the terms together again
                   review = map_chr(review, paste, collapse = " ")) # separate by a blank

# Create train and test dataset
train <- mediate[1:50000,] # training-data
test <- mediate[50001:62187,] # test-dataset
test2 <- prep_headphone_brand[40001:50000]
#test2 <- mediate[62188:84148,]

# Create vocabulary
# Tokenize the movie reviews and create a vocabulary of tokens including document counts
vocab <- create_vocabulary(itoken(mediate$review,
                                  tokenizer = word_tokenizer))

# Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object
dtm_train <- create_dtm(itoken(train$review,
                               tokenizer = word_tokenizer), vocab_vectorizer(vocab))

dtm_test <- create_dtm(itoken(test$review,
                              tokenizer = word_tokenizer), vocab_vectorizer(vocab))

dtm_test2 <- create_dtm(itoken(test2$review,
                               tokenizer = word_tokenizer), vocab_vectorizer(vocab))

#dtm_test2 <- create_dtm(itoken(test2$review,
#                               tokenizer = word_tokenizer), vocab_vectorizer(vocab))

# Train and Test Scores
train.y <- train$scoreNN
validation.y <- test$scoreNN
#validation2.y <- test2$scoreNN

# Turn the DTM into an XGB matrix using the sentiment labels that are to be learned
train_matrix <- xgb.DMatrix(dtm_train, label = train.y)
test_matrix <- xgb.DMatrix(dtm_test, label = validation.y)

#test2_matrix <- xgb.DMatrix(dtm_test2, label = train.y)

# Create a watchlist
watchlist <- list(validation = test_matrix, train=train_matrix)

# Build XGBOOST Model
xgb_params = list(
  objective = "reg:linear", # linear regression as a continuous variable has to be predicted
  eta = 0.01,
  max.depth = 1000)

# Train the model
xgb_fit <- xgboost(params = xgb_params, data = train_matrix, nrounds = 10000, early_stopping_rounds = 20, maximize = FALSE) # without verify process
#xgb_fit <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 10000, watchlist = watchlist, early_stopping_rounds = 20, maximize = FALSE) # with verify process

prediction <- predict(xgb_fit, dtm_test2)

# Create Feature Importance Overview
importance_vars <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))
importance_vars2 <- xgb.importance(model=xgb_fit, feature_names = colnames(train_matrix))

importanceClean <- importance_vars[,`:=`(Cover=NULL, Frequency=NULL)]
xgb.plot.importance(importance_matrix = importance_vars, top_n = 20)

# Print most important Features
head(importance_vars2, 100)


