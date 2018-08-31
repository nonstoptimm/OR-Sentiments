# generateTopics.R
# TOPIC MODELING
# Load required packages
library(dplyr)
library(topicmodels)
library(purrr)
library(tidyverse)
library(tidytext)
library(tm) 

# CREATE A BRAND LIST TO USE IT FOR ANTI-JOIN
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
# Apply brandList
brandListCellphone <- brandList(prep_cellphone_brand)
brandListHeadphone <- brandList(prep_headphone_brand)
brandListCoffee <- brandList(prep_coffee_brand)
brandListToaster <- brandList(prep_toaster_brand)

# TOKENIZE DATA
# Create tokenized dataset
dtm_wordByReview <- function(input) {
  input$review <- gsub("[0-9]+", "", input$review) # Remove numbers
  input %>%
    unnest_tokens(word, review)
}
# Apply dtm_wordByReview Function
dtm_headphone_byReview <- dtm_wordByReview(prep_headphone_brand)
dtm_cellphone_byReview <- dtm_wordByReview(prep_cellphone_brand)
dtm_coffee_byReview <- dtm_wordByReview(prep_coffee_brand)
dtm_toaster_byReview <- dtm_wordByReview(prep_toaster_brand)

# JOIN FILTERED DATA
dtm_join <- function(input, filtered) {
  input %>%
    inner_join(filtered)
}
dtm_detect_headphone <- tidied_headphone %>% group_by(document) %>% select(document) %>% distinct() %>% arrange(document)
# Apply dtm_join Function
merged_topic_headphone <- dtm_join(prep_headphone_brand, dtm_detect_headphone)
merged_topic_cellphone <- dtm_join(prep_cellphone_brand, dtm_docDetect(dtm_cellphone_byReview, brandListCellphone))
merged_topic_toaster <- dtm_join(dtm_toaster_brand, dtm_docDetect(dtm_toaster_byReview))
merged_topic_coffee <- dtm_join(dtm_coffee_brand, dtm_docDetect(dtm_coffee_byReview))
# Brand-based
merged_topic_apple <- dtm_join(dtm_cellphone_apple, dtm_docDetect(dtm_apple_byReview))
merged_topic_samsung <- dtm_join(dtm_cellphone_samsung, dtm_docDetect(dtm_samsung_byReview))

# COUNT BY REVIEW
dtm_wordCounts <- function(input, brandList) {
  input %>%
    anti_join(stop_words) %>%
    anti_join(brandList) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
}
# Apply dtm_wordCounts Function
dtm_headphone_wordCounts <- dtm_wordCounts(dtm_headphone_byReview, brandListHeadphone)
dtm_cellphone_wordCounts <- dtm_wordCounts(dtm_cellphone_byReview, brandListCellphone)
dtm_toaster_wordCounts <- dtm_wordCounts(dtm_toaster_byReview, brandListToaster)
dtm_coffee_wordCounts <- dtm_wordCounts(dtm_coffee_byReview, brandListCoffee)

# CREATE DOCUMENT-TERM-MATRIX
dtmCreator <- function(input) {
  input %>%
    cast_dtm(document, word, n)
}
# Apply dtmCreator Function
dtm_cellphone <- dtmCreator(as_tibble(dtm_cellphone_wordCounts))
dtm_cellphoneNN <- dtmCreatorNN(dtm_cellphone_wordCounts)
dtm_apple <- dtmCreator(dtm_apple_wordCounts)
dtm_samsung <- dtmCreator(dtm_samsung_wordCounts)
dtm_toaster <- dtmCreator(dtm_toaster_wordCounts)
dtm_coffee <- dtmCreator(dtm_coffee_wordCounts)
dtm_headphone <- dtmCreator(dtm_headphone_wordCounts)

# REMOVE SPARSE TERMS
# If desired/necessary
dtm_headphone <- removeSparseTerms(dtm_headphone, 0.995) 
dtm_cellphone <- removeSparseTerms(dtm_cellphone, 0.995)
dtm_coffee <- removeSparseTerms(dtm_coffee, 0.995)
dtm_toaster <- removeSparseTerms(dtm_toaster, 0.995) 

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
  # Number of topics
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
# Apply createLDA Function and train the model
LDA_reviews_cellphone <- createLDA(dtm_cellphone, 5)
LDA_reviews_toaster <- createLDA(dtm_toaster, 4)
LDA_reviews_coffee <- createLDA(dtm_coffee, 4)
LDA_reviews_headphone4 <- createLDA(dtm_headphoneSparse, 4)

# Tidy it back, to fix the issue xxx
tidied_headphone <- tidy(dtm_headphoneSparse)
# Input must be (sparsed) dtm, which has been used for model training
tidyDTM <- function(input){
  tidied <- tidy(input)
  
}

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
mainTopic <- function(input){
  matrix <- as.data.frame(topics(input))
  matrix$document <- rownames(matrix)
  # Remove Rownames
  rownames(matrix) <- c()
  # Add Proper Name
  names(matrix) <- c("mainTopic", "document")
  # Reorder
  matrix <- matrix[c("document", "mainTopic")]
  return(matrix)
}
# Apply mainTopic Function
LDA_main_cellphone <- mainTopic(LDA_reviews_cellphone)
LDA_main_cellphone <- mainTopic(LDA_reviews_cellphoneSimple)
LDA_main_headphone <- mainTopic(LDA_reviews_headphone)
LDA_main_headphone4 <- mainTopic(LDA_reviews_headphone4)
LDA_main_coffee <- mainTopic(LDA_reviews_coffee)
LDA_main_toaster <- mainTopic(LDA_reviews_toaster)

# # EXTRACT GAMMA PROBABILITIES FROM DATA FRAME
LDA_prob_cellphone <- as.data.frame(LDA_reviews_cellphone@gamma)
LDA_prob_cellphone <- as.data.frame(LDA_reviews_cellphoneSimple@gamma)
LDA_prob_toaster <- as.data.frame(LDA_reviews_toaster@gamma)
LDA_prob_coffee <- as.data.frame(LDA_reviews_coffee@gamma)
LDA_prob_headphone <- as.data.frame(LDA_reviews_headphone@gamma)
LDA_prob_headphone4 <- as.data.frame(LDA_reviews_headphone4@gamma)

# CREATE PROPER COLNAMES
# Create "Topic" colnames depending on the amount of topics
scoreCols <- function(input, num) {
  names <- paste("Topic", 1:num, sep = "")
  names(input) <- names
  return(input)
}
# Apply scoreCols Function
LDA_prob_cellphone <- scoreCols(LDA_prob_cellphone, 4)
LDA_prob_cellphone <- scoreCols(LDA_prob_cellphone, 5)
LDA_prob_toaster <- scoreCols(LDA_prob_toaster, 5)
LDA_prob_coffee <- scoreCols(LDA_prob_coffee, 5)
LDA_prob_headphone4 <- scoreCols(LDA_prob_headphone4, 4)
LDA_prob_headphone <- scoreCols(LDA_prob_headphone, 5)
# Brand-based
LDA_prob_cellphoneApple <- scoreCols(LDA_prob_CellphoneApple, 5)
LDA_prob_cellphoneSamsung <- scoreCols(LDA_prob_CellphoneSamsung, 5)

# TIDY MODEL / WORD TOPIC PROBABILITIES
topicWTP <- function(input) {
  tidy(input, matrix = "beta")
}
# Apply topic_wtp Function (LDA-Model as input)
LDA_cellphone_wtp <- topicWTP(LDA_reviews_cellphone)
LDA_cellphone_wtp <- as.matrix(terms(LDA_reviews_cellphone,8))
LDA_toaster_wtp <- topicWTP(LDA_reviews_toaster)
LDA_coffee_wtp <- topicWTP(LDA_reviews_coffee)
LDA_headphone_wtp <- topicWTP(LDA_reviews_headphone)
LDA_headphone_wtp <- topics(LDA_prob_headphone)
# Brand-based
LDA_apple_wtp <- topicWTP(LDA_reviews_apple)
LDA_samsung_wtp <- topicWTP(LDA_reviews_samsung)

# LDA TOP TERMS PER TOPIC
topicTopTerms <- function(input) {
  input %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
}
# Apply LDATopTerms Function
LDA_cellphone_wtp_topTerms <- topicTopTerms(LDA_cellphone_wtp)
LDA_toaster_wtp_topTerms <- topicTopTerms(LDA_toaster_wtp)
LDA_coffee_wtp_topTerms <- topicTopTerms(LDA_coffee_wtp)
LDA_headphone_wtp_topTerms <- topicTopTerms(LDA_headphone_wtp)
# Brand-based
LDA_apple_wtp_topTerms <- LDATopTerms(lda_apple_wtp)
LDA_samsung_wtp_topTerms <- LDATopTerms(lda_samsung_wtp)

# PLOT LDA TOP TERMS PER TOPIC
plotTopicTopTerms <- function(input, topic) {
  input %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    ggtitle(paste("Topic Modeling on", topic, sep = " "))
}
# Apply plotLDATopTerms Function 
plotTopicTopTerms(LDA_cellphone_wtp_topTerms, "Phones")
plotTopicTopTerms(LDA_toaster_wtp_topTerms, "Toaster")
plotTopicTopTerms(LDA_coffee_wtp_topTerms, "Coffee")
plotTopicTopTerms(LDA_headphone_wtp_topTerms, "Headphone")
# Brand-based
plotTopicTopTerms(LDA_apple_wtp_topTerms, "Phones, Brand Apple")
plotTopicTopTerms(LDA_samsung_wtp_topTerms, "Phones, Brand Samsung")

# PROBABILITIES ASSOCIATED WITH EACH TOPIC ASSIGNMENT
topicProbs <- function(input){
  as.data.frame(input@gamma)
}
# Apply LDAtopicProbs Function
topicProbs_Cellphone <- topicProbs(LDA_reviews_cellphone)
topicProbs_Toaster <- topicProbs(LDA_reviews_toaster)
topicProbs_Coffee <- topicProbs(LDA_reviews_coffee)
topicProbs_Headphone <- topicProbs(LDA_reviews_headphone)
# Brand-based
topicProbs_CellphoneApple <- topicProbs(LDA_reviews_apple)
topicProbs_CellphoneSamsung <- topicProbs(LDA_reviews_samsung)

# MERGE MAIN TOPIC AND PROBABILITIES DATAFRAME
mergeTopics <- function(main, prob){
  merged <- cbind(main, prob)
  return(merged)
}
# Apply mergeTopics-function
topics_headphone <- mergeTopics(LDA_main_headphone, LDA_prob_headphone)

# ADD VALUES TO THE DATASET
addTopicValues <- function(input, values) {
  input <- cbind(input, values)
  return(input)
}
# Apply addTopicValues Function
merged_topic_headphone <- addTopicValues(merged_topic_headphone, topicProbs_Headphone)
merged_topic_cellphone <- addTopicValues(prep_cellphone_brand, topicProbs_Cellphone)
merged_topic_toaster <- addTopicValues(merged_topic_toaster, topicProbs_Toaster)
merged_topic_coffee <- addTopicValues(merged_topic_coffee, topicProbs_Coffee)
# Brand-based
merged_topic_apple <- addTopicValues(merged_topic_apple, topicProbs_CellphoneApple)
merged_topic_samsung <- addTopicValues(merged_topic_samsung, topicProbs_CellphoneSamsung)

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
