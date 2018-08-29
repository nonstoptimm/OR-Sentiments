# generateTopics.R
# TOPIC MODELING
# Load required packages
library(topicmodels)
library(purrr)
library(dplyr)
library(tidyverse)
library(tidytext)

# FILTER FOR SPECIAL BRAND
# For brand-specific topic-models
dtm_filterBrand <- function(input, selectBrand) {
  input %>%
    filter(brand == selectBrand)
}
# Apply dtm_filterBrand Function
dtm_cellphone_apple <- dtm_filterBrand(dtm_cellphone_brand, "apple")
dtm_cellphone_samsung <- dtm_filterBrand(dtm_cellphone_brand, "samsung")
dtm_headphone_beats <- dtm_filterBrand(dtm_headphone_brand, "beats")

# SPLIT INTO WORDS
# Create tokenized dataset
dtm_wordByReview <- function(input) {
  input %>%
    unnest_tokens(word, review)
}
# Apply dtm_wordByReview Function
dtm_headphone_byReview <- dtm_wordByReview(dtm_headphone_brand)
dtm_cellphone_byReview <- dtm_wordByReview(dtm_cellphone_brand)
dtm_coffee_byReview <- dtm_wordByReview(dtm_coffee_brand)
dtm_toaster_byReview <- dtm_wordByReview(dtm_toaster_brand)
# Brand-oriented
dtm_apple_byReview <- dtm_wordByReview(dtm_cellphone_apple)
dtm_samsung_byReview <- dtm_wordByReview(dtm_cellphone_samsung)

# EXTRACT DOCUMENT ID OF REVIEWS
# Some reviews do not exist any more after stopword removing
# The document ID of the remaining reviews is extracted here to perform a join later on
dtm_docDetect <- function(input) {
  input %>%
    anti_join(stop_words) %>%
    select(document) %>%
    distinct()
}
# Apply dtm_docDetect Function
dtm_detect_headphone <- dtm_docDetect(dtm_headphone_byReview)
dtm_detect_apple <- dtm_docDetect(dtm_apple_byReview)
dtm_detect_cellphone <- dtm_docDetect(dtm_cellphone_byReview)
dtm_detect_coffee <- dtm_docDetect(dtm_coffee_byReview)

# JOIN FILTERED DATA
dtm_join <- function(input, filtered) {
  input %>%
    inner_join(filtered)
}
# Apply dtm_join Function
merged_topic_headphone <- dtm_join(dtm_headphone_brand, dtm_docDetect(dtm_headphone_byReview))
merged_topic_cellphone <- dtm_join(dtm_cellphone_brand, dtm_docDetect(dtm_cellphone_byReview))
merged_topic_toaster <- dtm_join(dtm_toaster_brand, dtm_docDetect(dtm_toaster_byReview))
merged_topic_coffee <- dtm_join(dtm_coffee_brand, dtm_docDetect(dtm_coffee_byReview))
# Brand-based
merged_topic_apple <- dtm_join(dtm_cellphone_apple, dtm_docDetect(dtm_apple_byReview))
merged_topic_samsung <- dtm_join(dtm_cellphone_samsung, dtm_docDetect(dtm_samsung_byReview))

# COUNT BY REVIEW
dtm_wordCounts <- function(input) {
  input %>%
    anti_join(stop_words) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
}
# Apply dtm_wordCounts Function
dtm_headphone_wordCounts <- dtm_wordCounts(dtm_headphone_byReview)
dtm_cellphone_wordCounts <- dtm_wordCounts(dtm_cellphone_byReview)
dtm_toaster_wordCounts <- dtm_wordCounts(dtm_toaster_byReview)
dtm_coffee_wordCounts <- dtm_wordCounts(dtm_coffee_byReview)
# Brand-based
dtm_apple_wordCounts <- dtm_wordCounts(dtm_apple_byReview)
dtm_samsung_wordCounts <- dtm_wordCounts(dtm_samsung_byReview)

# CREATE DOCUMENT TERM MATRIX
dtmCreator <- function(input) {
  input %>%
    cast_dtm(document, word, n)
}

# Apply dtmCreator
dtm_cellphone <- dtmCreator(dtm_cellphone_wordCounts)
dtm_cellphoneNN <- dtmCreatorNN(dtm_cellphone_wordCounts)
dtm_apple <- dtmCreator(dtm_apple_wordCounts)
dtm_samsung <- dtmCreator(dtm_samsung_wordCounts)
dtm_toaster <- dtmCreator(dtm_toaster_wordCounts)
dtm_coffee <- dtmCreator(dtm_coffee_wordCounts)
dtm_headphone <- dtmCreator(dtm_headphone_wordCounts)

dtm_headphone_sparse <- removeSparseTerms(dtm_headphone, 0.97) #tm
saveRDS(dtm_headphone, "output/dtm.rds")
tidied_apple <- tidy(dtm_apple)

# Train Model
createLDA <- function(input) {
  # Find the sum of words in each Document to detect if there are empty ones
  rowTotals <- apply(input , 1, sum) 
  input <- input[rowTotals> 0, ]
  # Set parameters for LDA
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  # Number of topics
  k <- 5
  # Create LDA model using Gibbs sampling
  ldamodel <- LDA(input,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  return(ldamodel)
}
# Apply createLDA and train the model
LDA_reviews_cellphone <- createLDA(dtm_cellphone)
LDA_reviews_apple <- createLDA(dtm_apple)
LDA_reviews_samsung <- createLDA(dtm_samsung)
LDA_reviews_toaster <- createLDA(dtm_toaster)
LDA_reviews_coffee <- createLDA(dtm_coffee)
LDA_reviews_headphone <- createLDA(dtm_headphone_sparse)

LDA_reviews_cellphone <- as.matrix(topics(LDA_reviews_cellphone))
write.csv(as.matrix(topics(LDA_reviews_coffee)),file="LDAGibbs5DocsToTopics_Coffee.csv")

# EXTRACT GAMMA PROBABILITIES FROM DATA FRAME
LDAProb_Cellphone <- as.data.frame(LDA_reviews_cellphone@gamma)
LDAProb_Toaster <- as.data.frame(LDA_reviews_toaster@gamma)
LDAProb_Coffee <- as.data.frame(LDA_reviews_coffee@gamma)
LDAProb_Headphone <- as.data.frame(LDA_reviews_headphone@gamma)
# Brand-based
LDAProb_CellphoneApple <- as.data.frame(LDA_reviews_apple@gamma)
LDAProb_CellphoneSamsung <- as.data.frame(LDA_reviews_samsung@gamma)

# CREATE PROPER COLNAMES
# Create "Topic" colnames depending on the amount of topics
scoreCols <- function(input) {
  names <- paste("Topic", 1:5, sep = "")
  names(input) <- names
  return(input)
}
# Apply scoreCols Function
LDAProb_Cellphone <- scoreCols(LDAProb_Cellphone)
LDAProb_CellphoneApple <- scoreCols(LDAProb_CellphoneApple)
LDAProb_CellphoneSamsung <- scoreCols(LDAProb_CellphoneSamsung)
LDAProb_Toaster <- scoreCols(LDAProb_Toaster)
LDAProb_Coffee <- scoreCols(LDAProb_Coffee)
LDAProb_Headphone <- scoreCols(LDAProb_Headphone)

# Write as CSV
write.csv(as.data.frame(LDA_reviews_coffee@gamma),file="LDAGibbs5TopicProbabilities_Coffee.csv")

# TIDY MODEL / WORD TOPIC PROBABILITIES
topic_wtp <- function(input) {
  tidy(input, matrix = "beta")
}
# Apply topic_wtp Function (LDA-Model as input)
lda_cellphone_wtp <- topic_wtp(LDA_reviews_cellphone)
lda_toaster_wtp <- topic_wtp(LDA_reviews_toaster)
lda_coffee_wtp <- topic_wtp(LDA_reviews_coffee)
lda_headphone_wtp <- topic_wtp(LDA_reviews_headphone)
# Brand-based
lda_apple_wtp <- topic_wtp(LDA_reviews_apple)
lda_samsung_wtp <- topic_wtp(LDA_reviews_samsung)

# LDA TOP TERMS PER TOPIC
LDATopTerms <- function(input) {
  input %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
}
# Apply LDATopTerms Function
lda_cellphone_wtp_topTerms <- LDATopTerms(lda_cellphone_wtp)
lda_toaster_wtp_topTerms <- LDATopTerms(lda_toaster_wtp)
lda_coffee_wtp_topTerms <- LDATopTerms(lda_coffee_wtp)
lda_headphone_wtp_topTerms <- LDATopTerms(lda_headphone_wtp)
# Brand-based
lda_apple_wtp_topTerms <- LDATopTerms(lda_apple_wtp)
lda_samsung_wtp_topTerms <- LDATopTerms(lda_samsung_wtp)

# PLOT LDA TOP TERMS PER TOPIC
plotLDATopTerms <- function(input, topic) {
  input %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    ggtitle(paste("Topic Modeling on", topic, sep = " "))
}
# Apply plotLDATopTerms Function 
plotLDATopTerms(lda_cellphone_wtp_topTerms, "Phones")
plotLDATopTerms(lda_toaster_wtp_topTerms, "Toaster")
plotLDATopTerms(lda_coffee_wtp_topTerms, "Coffee")
plotLDATopTerms(lda_headphone_wtp_topTerms, "Headphone")
# Brand-based
plotLDATopTerms(lda_apple_wtp_topTerms, "Phones, Brand Apple")
plotLDATopTerms(lda_samsung_wtp_topTerms, "Phones, Brand Samsung")

# PROBABILITIES ASSOCIATED WITH EACH TOPIC ASSIGNMENT
LDAtopicProbs <- function(input){
  as.data.frame(input@gamma)
}
# Apply LDAtopicProbs Function
LDAtopicProbs_Cellphone <- LDAtopicProbs(LDA_reviews_cellphone)
LDAtopicProbs_Toaster <- LDAtopicProbs(LDA_reviews_toaster)
LDAtopicProbs_Coffee <- LDAtopicProbs(LDA_reviews_coffee)
LDAtopicProbs_Headphone <- LDAtopicProbs(LDA_reviews_headphone)
# Brand-based
LDAtopicProbs_CellphoneApple <- LDAtopicProbs(LDA_reviews_apple)
LDAtopicProbs_CellphoneSamsung <- LDAtopicProbs(LDA_reviews_samsung)

# ADD VALUES TO THE DATASET
addLDAvalues <- function(input, values) {
  input <- cbind(input, values)
  return(input)
}
# Apply addLDAvalues Function
merged_topic_headphone <- addLDAvalues(merged_topic_headphone, LDAProb_Headphone)
merged_topic_cellphone <- addLDAvalues(merged_topic_cellphone, LDAProb_Cellphone)
merged_topic_toaster <- addLDAvalues(merged_topic_toaster, LDAProb_Toaster)
merged_topic_coffee <- addLDAvalues(merged_topic_coffee, LDAProb_Coffee)
# Brand-based
merged_topic_apple <- addLDAvalues(merged_topic_apple, LDAProb_CellphoneApple)
merged_topic_samsung <- addLDAvalues(merged_topic_samsung, LDAProb_CellphoneSamsung)

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

# Create Boxplot for Overall Star Rating vs. Sentiment Score
boxplot(Topic1 + Topic2 + Topic3 + Topic4 + Topic5~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")
