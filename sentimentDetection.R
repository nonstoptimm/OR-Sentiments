# LEXICON-BASED SENTIMENT ANALYSIS
# sentimentDetection.R
library(dplyr)
library(tidytext)
library(ggplot2)

# CREATE UNIGRAM TOKENS
# Unnest the reviews to one word per row
tokenizeReview <- function(input) {
  input %>%
    unnest_tokens(word, review)
}
# Apply tokenizeReview-function
tokenized_headphone <- tokenizeReview(prep_headphone_brand) # Headphones
tokenized_cellphone <- tokenizeReview(prep_cellphone_brand) # Cellphones
tokenized_coffee <- tokenizeReview(prep_coffee_brand) # Coffee Makers 
tokenized_toaster <- tokenizeReview(prep_toaster_brand) # Toaster

# GET SENTIMENT LABELS FOR BING OR NRC
# Tokenized dataset as input and mention the lexicon (only nrc and bing possible)
# Does not play a crucuial role within this thesis, was just tested
getSentiment <- function(input, lexicon) {
  input %>%
    inner_join(get_sentiments(lexicon)) %>% # join to sentiment lexicon
    count(word, sentiment, sort = TRUE) %>% # count sentiment hits
    ungroup()
}
# Apply function to tokenized data set
# Bing-Lexicon
getSentiment(tokenized_coffee, "bing")
getSentiment(tokenized_toaster, "bing")
getSentiment(tokenized_headphone, "bing")
getSentiment(tokenized_cellphone, "bing")
# nrc-Lexicon
getSentiment(tokenized_coffee, "nrc")
getSentiment(tokenized_toaster, "nrc")
getSentiment(tokenized_headphone, "nrc")
getSentiment(tokenized_cellphone, "nrc")

# AFINN-SENTIMENT SCORE CALCULATION BY REVIEW  
# Calculate Sentiment Score for every review
getSentimentAFINN <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(document) %>%
    summarize(scoreLX = mean(score),
              words = n()) %>%
    ungroup()
}
# Apply sentimentReview-function
sentimentReviewHeadphone <- getSentimentAFINN(tokenized_headphone)
sentimentReviewCellphone <- getSentimentAFINN(tokenized_cellphone)
sentimentReviewToaster <- getSentimentAFINN(tokenized_toaster)
sentimentReviewCoffee <- getSentimentAFINN(tokenized_coffee)

# AFINN-SENTIMENT SCORE CALCULATION BY BRAND
# Word frequency as input
sentimentScoreAFINN <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(brand) %>%
    summarize(scoreAFINN = sum(score * n) / sum(n), numWords = n())
}
# Apply sentimentScoreAFINN Function
scoreCellphoneAFINN <- sentimentScoreAFINN(wf_cellphone_brand)
scoreHeadphoneAFINN <- sentimentScoreAFINN(wf_headphone_brand)
scoreToasterAFINN <- sentimentScoreAFINN(wf_toaster_brand)
scoreCoffeeAFINN <- sentimentScoreAFINN(wf_coffee_brand)

# PLOT SENTIMENT SCORE BASED ON BRAND
plotSentimentScoreAFINN <- function(input, num, text) {
  input %>%
    filter(numWords > num) %>%
    mutate(brand = reorder(brand, scoreAFINN)) %>%
    ggplot(aes(brand, scoreAFINN, fill = scoreAFINN > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    xlab("Brand") +
    ylab("Average sentiment score") +
    ggtitle(paste("Average Sentiment Score for frequent brands in ", text, "-category", sep = ""))
}
# Apply plotSentimentScoreAFINN Function
plotSentimentScoreAFINN(scoreCellphoneAFINN, 300, "Cellphones")
plotSentimentScoreAFINN(scoreHeadphoneAFINN, 680, "Headphones")
plotSentimentScoreAFINN(scoreToasterAFINN, 270, "Toaster")
plotSentimentScoreAFINN(scoreCoffeeAFINN, 505, "Coffee")

# SENTIMENT CONTRIBUTION
sentiContributions <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
}
# Apply sentiContributionsBrand Function
sentiContributions(tokenized_headphone)
sentiContributionsBrand(tokenized_cellphone)
sentiContributionsBrand(tokenized_toaster)
sentiContributionsBrand(tokenized_coffee)

# PLOT SENTIMENT CONTRIBUTION
sentiContributionPlot <- function(input, selectCategory) {
  input %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    ggtitle(paste("Sentiment Contribution within ", selectCategory, "-Category", sep = ""))
}
# Apply sentiContributionPlot Function
sentiContributionPlot(sentiContributions(tokenized_headphone), "Headphones")
sentiContributionPlot(sentiContributions(tokenized_cellphone), "Cellphones")
sentiContributionPlot(sentiContributions(tokenized_coffee), "Coffee")
sentiContributionPlot(sentiContributions(tokenized_toaster), "Toaster")

# CREATE BOXPLOT FOR OVERALL VS. SCORE
boxplotScore <- function(input, cat){
  input$overall <- as.factor(input$overall)
  ggplot(input, aes(x=overall, y=scoreNN)) + 
    geom_boxplot() +
    theme(text = element_text(size=18), plot.title = element_text(size = 14, face = "bold")) +
    ggtitle(paste("Sentiment-Score vs. Overall Rating for", cat, sep = " ")) +
    ylim(-5,3)
}
# Apply boxplotScore Function
boxplotScore(prep_headphone_brand, "Headphones")
boxplotScore(prep_cellphone_brand, "Cellphones")
boxplotScore(prep_toaster_brand, "Toasters")
boxplotScore(prep_coffee_brand, "Coffee Makers")
