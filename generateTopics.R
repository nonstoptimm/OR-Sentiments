# TOPIC MODELING
# Create LDA Topic Model
# generateTopics.R
# Load required packages
library(dplyr)
library(tidytext)
library(tm) # sparseterms
library(topicmodels)

# CREATE A BRAND LIST TO USE IT FOR ANTI-JOIN
# Brand names should be avoided within the topic model
brandList <- function(input){
  list <- input %>%
    select(brand) %>%
    arrange(brand) %>%
    distinct()
  names(list) <- "word"
  list <- as_tibble(list)
  list$word <- as.character(list$word)
  return(list)
}
# Apply brandList-function
brandListCellphone <- brandList(prep_cellphone_brand)
brandListHeadphone <- brandList(prep_headphone_brand)
brandListToaster <- brandList(prep_toaster_brand)
brandListCoffee <- brandList(prep_coffee_brand)

# TOKENIZE DATA
# Create tokenized dataset
tokenizeReview <- function(input) {
  # Remove numbers
  input$review <- gsub("[0-9]+", "", input$review)
  input %>%
    unnest_tokens(word, review)
}
# Apply dtm_wordByReview-function
tokenizedCellphone <- tokenizeReview(prep_cellphone_brand)
tokenizedHeadphone <- tokenizeReview(prep_headphone_brand)
tokenizedToaster <- tokenizeReview(prep_toaster_brand)
tokenizedCoffee <- tokenizeReview(prep_coffee_brand)

# COUNT BY REVIEW
dtmWordCounts <- function(input, brandList) {
  input %>%
    anti_join(stop_words) %>%
    anti_join(brandList) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
}
# Apply dtmWordCounts-function
wordCountsCellphone <- dtmWordCounts(tokenizedCellphone, brandListCellphone)
wordCountsHeadphone <- dtmWordCounts(tokenizedHeadphone, brandListHeadphone)
wordCountsToaster <- dtmWordCounts(tokenizedToaster, brandListToaster)
wordCountsCoffee <- dtmWordCounts(tokenizedCoffee, brandListCoffee)

# CREATE DOCUMENT-TERM-MATRIX
dtmCreator <- function(input) {
  input %>%
    cast_dtm(document, word, n)
}
# Apply dtmCreator-function
dtmCellphone <- dtmCreator(wordCountsCellphone)
dtmHeadphone <- dtmCreator(wordCountsHeadphone)
dtmToaster <- dtmCreator(wordCountsToaster)
dtmCoffee <- dtmCreator(wordCountsCoffee)

# REMOVE SPARSE TERMS
# If desired/necessary
# Highly recommended, makes it way more crispy clean
sparseDTM <- function(input, sparse){
  input <- removeSparseTerms(input, sparse)
  # Sum words in each review
  rowSum <- apply(input, 1, sum)
  # Delete all empty rows
  input <- input[rowSum > 0, ]
  return(input)
}
# Apply sparseDTM-function
dtmCellphone <- sparseDTM(dtmCellphone, 0.995)
dtmHeadphone <- sparseDTM(dtmHeadphone, 0.995)
dtmToaster <- sparseDTM(dtmToaster, 0.995) 
dtmCoffee <- sparseDTM(dtmCoffee, 0.995)

# DETECT DOCUMENT ID ACTUALLY CONSIDERED
# Tidy back the sparsed dtm and extract docID, otherwise there would be issues 
# Input must be (sparsed) dtm, which is going to be used for model training
detectDocument <- function(input){
  tidied <- tidy(input)
  tidied %>% 
    group_by(document) %>% 
    select(document) %>% 
    distinct() %>% 
    arrange(document)
}
# Apply detectDocument-function
docIDCellphone <- detectDocument(dtmCellphone)
docIDHeadphone <- detectDocument(dtmHeadphone)
docIDToaster <- detectDocument(dtmToaster)
docIDCoffee <- detectDocument(dtmCoffee)

# TRAIN MODEL WITH GIBBS
createLDA <- function(input, ntopic) {
  # Sum words in each review
  rowSum <- apply(input, 1, sum)
  # Delete all empty rows
  input <- input[rowSum > 0, ]
  # LDA Gibbs Parameter
  rwalk <- 4000 # random walk
  niter <- 2000 # number of iterations
  thin <- 500 # every 500 for continuous use
  seed <- list(2003,5,63,100001,765) # five seeds as 5 starting points
  spoint <- 5 # starting points for random starts, reduces correlations
  bestround <- TRUE # return results with highest probability
  # Amount of topics to be generated
  k <- ntopic
  # Create LDA model using Gibbs sampling
  model <- LDA(input, k, method = "Gibbs", control = list(nstart = spoint, seed = seed, best = bestround, burnin = rwalk, iter = niter, thin = thin))
  return(model)
}
# TRAIN MODEL WITH VMA
createLDAsimple <- function(input, ntopics){
  model <- LDA(input, k = ntopics, control = list(seed = 1234))
  return(model)
}
# Apply createLDA-function // train the model
LDA_reviews_headphone <- createLDA(dtmHeadphone, 5)
LDA_reviews_cellphone <- createLDA(dtmCellphone, 5)
LDA_reviews_toaster <- createLDA(dtmToaster, 5)
LDA_reviews_coffee <- createLDA(dtmCoffee, 5)

# FILTER MAIN DATA FOR DOCS ACUTALLY USED
joinFiltered <- function(input, filtered) {
  input %>%
    inner_join(filtered)
}
# Apply dtm_join-function
prep_topic_cellphone <- joinFiltered(prep_cellphone_brand, docIDCellphone)
prep_topic_headphone <- joinFiltered(prep_headphone_brand, docIDHeadphone)
prep_topic_toaster <- joinFiltered(prep_toaster_brand, docIDToaster)
prep_topic_coffee <- joinFiltered(prep_coffee_brand, docIDCoffee)

# MAIN TOPIC OF EACH DOCUMENT
# Probabilities associated with each topic assignment
aggrTopics <- function(input, ntopics){
  # Extract Topic Probabilities
    probs <- as.data.frame(input@gamma)
    # Create topic names depending on the amount of topics
    topNames <- paste("Topic", 1:ntopics, sep = "")
    # Assign topic names
    names(probs) <- topNames
    # Extract Main Topics
    main <- as.data.frame(topics(input))
    main$document <- rownames(main)
    # Remove Rownames
    rownames(main) <- c()
    # Add Proper Name
    names(main) <- c("mainTopic", "document")
    # Reorder, so that document ID comes first
    main <- main[c("document", "mainTopic")]
    # Merge together
    merged <- cbind(main, probs)
  return(merged)
}
# Apply aggrTopics-function
topicsCellphone <- aggrTopics(LDA_reviews_cellphone, 5)
topicsHeadphone <- aggrTopics(LDA_reviews_headphone, 5)
topicsToaster <- aggrTopics(LDA_reviews_toaster, 5)
topicsCoffee <- aggrTopics(LDA_reviews_coffee, 5)

# ADD VALUES TO THE DATASET
addTopicValues <- function(input, values) {
  input %>% 
    inner_join(values)
}
# Apply addTopicValues-function
merged_topic_cellphone <- addTopicValues(prep_topic_cellphone, topicsCellphone)
merged_topic_headphone <- addTopicValues(prep_topic_headphone, topicsHeadphone)
merged_topic_toaster <- addTopicValues(prep_topic_toaster, topicsToaster)
merged_topic_coffee <- addTopicValues(prep_topic_coffee, topicsCoffee)

# EXTRACT TOP WORDS PER TOPIC
topicTopWords <- function(input, number){
  topWords <- as.matrix(terms(input, number))
  return(topWords)
}
# Apply topicTopWords-function
topicWordsCellphone <- topicTopWords(LDA_reviews_cellphone, 10)
topicWordsHeadphone <- topicTopWords(LDA_reviews_headphone, 10)
topicWordsToasters <- topicTopWords(LDA_reviews_toaster, 10)
topicWordsCoffee <- topicTopWords(LDA_reviews_coffee, 10)
