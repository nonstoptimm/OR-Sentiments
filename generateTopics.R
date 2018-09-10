# generateTopics.R
# TOPIC MODELING
# Load required packages
library(dplyr)
library(purrr)
library(tidyverse)
library(tidytext)
library(tm)
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
brandListCoffee <- brandList(prep_coffee_brand)
brandListToaster <- brandList(prep_toaster_brand)

# TOKENIZE DATA
# Create tokenized dataset
tokenizeReview <- function(input) {
  # Remove numbers
  input$review <- gsub("[0-9]+", "", input$review)
  input %>%
    unnest_tokens(word, review)
}
# Apply dtm_wordByReview-function
tokenizedHeadphone <- tokenizeReview(prep_headphone_brand)
tokenizedCellphone <- tokenizeReview(prep_cellphone_brand)
tokenizedCoffee <- tokenizeReview(prep_coffee_brand)
tokenizedToaster <- tokenizeReview(prep_toaster_brand)

# COUNT BY REVIEW
dtm_wordCounts <- function(input, brandList) {
  input %>%
    anti_join(stop_words) %>%
    anti_join(brandList) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
}
# Apply dtm_wordCounts-function
wordCountsHeadphone <- dtm_wordCounts(tokenizedHeadphone, brandListHeadphone)
wordCountsCellphone <- dtm_wordCounts(tokenizedCellphone, brandListCellphone)
wordCountsCoffee <- dtm_wordCounts(tokenizedCoffee, brandListToaster)
wordCountsToaster <- dtm_wordCounts(tokenizedToaster, brandListCoffee)

# CREATE DOCUMENT-TERM-MATRIX
dtmCreator <- function(input) {
  input %>%
    cast_dtm(document, word, n)
}
# Apply dtmCreator-function
dtmHeadphone <- dtmCreator(wordCountsHeadphone)
dtmCellphone <- dtmCreator(wordCountsCellphone)
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
dtmHeadphone <- sparseDTM(dtmHeadphone, 0.995) 
dtmCellphone <- sparseDTM(dtmCellphone, 0.995)
dtmCoffee <- sparseDTM(dtmCoffee, 0.995)
dtmToaster <- sparseDTM(dtmToaster, 0.995) 

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
docIDHeadphone <- detectDocument(dtmHeadphoneSparse)
docIDHeadphone <- detectDocument(dtmHeadphone)
docIDCellphone <- detectDocument(dtmCellphone)
docIDCoffee <- detectDocument(dtmCoffee)
docIDToaster <- detectDocument(dtmToaster)

# TRAIN MODEL WITH GIBBS
createLDA <- function(input, ntopic) {
  # Sum words in each review
  rowSum <- apply(input, 1, sum)
  # Delete all empty rows
  input <- input[rowSum > 0, ]
  # LDA Gibbs Parameter
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <- list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  # Amount of topics to be generated
  k <- ntopic
  # Create LDA model using Gibbs sampling
  model <- LDA(input, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  return(model)
}
# TRAIN MODEL WITH VMA
createLDAsimple <- function(input, ntopics){
  model <- LDA(input, k = ntopics, control = list(seed = 1234))
  return(model)
}
# Apply createLDA-function / Train the model
LDA_reviews_cellphone <- createLDA(dtm_cellphone, 5)
LDA_reviews_toaster <- createLDA(dtm_toaster, 4)
LDA_reviews_coffee <- createLDA(dtm_coffee, 4)
LDA_reviews_headphone4 <- createLDA(dtm_headphoneSparse, 4)

# FILTER MAIN DATA FOR DOCS ACUTALLY USED
joinFiltered <- function(input, filtered) {
  input %>%
    inner_join(filtered)
}
# Apply dtm_join Function
prep_topic_headphone <- joinFiltered(prep_headphone_brand, docIDHeadphone)
prep_topic_cellphone <- joinFiltered(prep_cellphone_brand, docIDCellphone)
prep_topic_toaster <- joinFiltered(prep_toaster_brand, docIDToaster)
prep_topic_coffee <- joinFiltered(prep_coffee_brand, docIDCoffee)

# # EXTRACT DOCUMENT ID OF REVIEWS
# # Some reviews do not exist any more after stopword removing
# # The document ID of the remaining reviews is extracted here to perform a join later on
# dtm_docDetect <- function(input, brandList) {
#   input %>%
#     anti_join(stop_words) %>%
#     anti_join(brandList) %>%
#     select(document) %>%
#     distinct()
# }
# # Apply dtm_docDetect Function
# dtm_detect_headphone <- dtm_docDetect(dtm_headphone_byReview, brandListHeadphone)
# dtm_detect_cellphone <- dtm_docDetect(dtm_cellphone_byReview, brandListCellphone)
# dtm_detect_coffee <- dtm_docDetect(dtm_coffee_byReview, brandListCoffee)
# dtm_detect_toaster <- dtm_docDetect(dtm_toaster_byReview, brandListToaster)

# MAIN TOPIC OF EACH DOCUMENT
# probabilities associated with each topic assignment
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
topicsCellphone <- aggrTopics(LDA_reviews_cellphone, 4)
topicsCellphone <- aggrTopics(topicmodel_cellphones, 5)
topicsCellphone <- aggrTopics(LDA_reviews_cellphoneSimple, 4)
topicsHeadphone <- aggrTopics(LDA_reviews_headphone, 5)
topicsHeadphone4 <- aggrTopics(LDA_reviews_headphone4, 4)
topicsCoffee <- aggrTopics(LDA_reviews_coffee, 4)
topicsToaster <- aggrTopics(LDA_reviews_toaster, 4)


# # TIDY MODEL / WORD TOPIC PROBABILITIES
# topicWTP <- function(input) {
#   tidy(input, matrix = "beta")
# }
# # Apply topic_wtp Function (LDA-Model as input)
# LDA_cellphone_wtp <- topicWTP(LDA_reviews_cellphone)
# LDA_cellphone_wtp <- as.matrix(terms(LDA_reviews_cellphone,8))
# LDA_toaster_wtp <- topicWTP(LDA_reviews_toaster)
# LDA_coffee_wtp <- topicWTP(LDA_reviews_coffee)
# LDA_headphone_wtp <- topicWTP(LDA_reviews_headphone)
# LDA_headphone_wtp <- topics(LDA_prob_headphone)

# # LDA TOP TERMS PER TOPIC
# topicTopTerms <- function(input) {
#   input %>%
#     group_by(topic) %>%
#     top_n(10, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta)
# }
# # Apply LDATopTerms Function
# LDA_cellphone_wtp_topTerms <- topicTopTerms(LDA_cellphone_wtp)
# LDA_toaster_wtp_topTerms <- topicTopTerms(LDA_toaster_wtp)
# LDA_coffee_wtp_topTerms <- topicTopTerms(LDA_coffee_wtp)
# LDA_headphone_wtp_topTerms <- topicTopTerms(LDA_headphone_wtp)

# PLOT LDA TOP TERMS PER TOPIC
# plotTopicTopTerms <- function(input, topic) {
#   input %>%
#     mutate(term = reorder(term, beta)) %>%
#     ggplot(aes(term, beta, fill = factor(topic))) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~ topic, scales = "free") +
#     coord_flip() +
#     ggtitle(paste("Topic Modeling on", topic, sep = " "))
# }
# # Apply plotLDATopTerms Function 
# plotTopicTopTerms(LDA_cellphone_wtp_topTerms, "Phones")
# plotTopicTopTerms(LDA_toaster_wtp_topTerms, "Toaster")
# plotTopicTopTerms(LDA_coffee_wtp_topTerms, "Coffee")
# plotTopicTopTerms(LDA_headphone_wtp_topTerms, "Headphone")

# ADD VALUES TO THE DATASET
addTopicValues <- function(input, values) {
  input %>% 
    inner_join(values)
}
# Apply addTopicValues Function
merged_topic_headphone <- addTopicValues(prep, topicsHeadphone)
merged_topic_cellphone <- addTopicValues(prep_cellphone_brand, topicsCellphone)
merged_topic_toaster <- addTopicValues(merged_topic_toaster, topicsToaster)
merged_topic_coffee <- addTopicValues(merged_topic_coffee, topicsCoffee)

# CALCULATE AND ASSIGN VALUES
topicReviewScore <- function(x, y) {
  # Create numerated colnames Topic Score
  new_cols = paste("TS", 1:length(values), sep = "")
  # Create dataframe as matrix with number of cols matching to x
  effect_matrix = as.data.frame(matrix(ncol = length(x)))
  # Empty the data frame
  effect_matrix = effect_matrix[FALSE, ]
  for (lines in 1:nrow(y)) {
    # Assign the values
    curr_row = input$scoreNN[i] * currTopic[i]
    effect_matrix = rbind(effect_matrix, curr_row)
  }
  # Assign names to the data frame
  names(effect_matrix) = new_cols
  output_matrix = cbind(y, effect_matrix)
  return(output_matrix)
}

# ADD AND CALCULATE TOPIC SCORES
topicReviewScore <- function(input) {
  # Create Effect Matrix
  effect_matrix = as.data.frame(matrix(ncol = 5))
  # Empty the data frame
  effect_matrix = effect_matrix[FALSE, ]
    TS1 <- input$scoreNN * input$Topic1
    TS2 <- input$scoreNN * input$Topic2
    TS3 <- input$scoreNN * input$Topic3
    TS4 <- input$scoreNN * input$Topic4
    TS5 <- input$scoreNN * input$Topic5
    #assign(paste("Topic", "1", sep=""),"bla") TC!
    effect_matrix <- data.frame(TS1, TS2, TS3, TS4, TS5)
  input <- cbind(input, effect_matrix)
  return(input)
}
# Apply topicReviewScore
merged_topic_cellphone <- topicReviewScore(merged_topic_cellphone)
merged_topic_samsung <- topicReviewScore(merged_topic_samsung)
merged_topic_apple <- topicReviewScore(merged_topic_apple)
merged_topic_toaster <- topicReviewScore(merged_topic_toaster)
merged_topic_coffee <- topicReviewScore(merged_topic_coffee)

# # FILTER FOR SPECIAL BRAND
# # For brand-specific topic-models
# dtm_filterBrand <- function(input, selectBrand) {
#  input %>%
#    filter(brand == selectBrand)
# }
# # Apply dtm_filterBrand Function
# dtm_cellphone_apple <- dtm_filterBrand(dtm_cellphone_brand, "apple")
# dtm_cellphone_samsung <- dtm_filterBrand(dtm_cellphone_brand, "samsung")
# dtm_headphone_beats <- dtm_filterBrand(dtm_headphone_brand, "beats")
