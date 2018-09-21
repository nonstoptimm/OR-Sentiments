# LEXICON-BASED SENTIMENT ANALYSIS
# sentimentDetection.R
# Load required packages
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
# Apply getSentiment-function
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
sentimentReviewHeadphone <- getSentimentAFINN(tokenized_headphone) # Headphones
sentimentReviewCellphone <- getSentimentAFINN(tokenized_cellphone) # Cellphones
sentimentReviewToaster <- getSentimentAFINN(tokenized_toaster) # Toaster
sentimentReviewCoffee <- getSentimentAFINN(tokenized_coffee) # Coffee

# AFINN-SENTIMENT SCORE CALCULATION BY BRAND
# Word frequency as input
sentimentScoreAFINN <- function(input, brandList) {
  input <- input %>%
    filter(brand %in% brandList$brand) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(brand) %>%
    summarize(scoreAFINN = sum(score * n) / sum(n))
  input <- left_join(input, brandList, by = "brand")
}
# Apply sentimentScoreAFINN-function
scoreCellphoneAFINN <- sentimentScoreAFINN(wf_cellphone_brand, top10brands_cellphone)
scoreHeadphoneAFINN <- sentimentScoreAFINN(wf_headphone_brand, top10brands_headphone)
scoreToasterAFINN <- sentimentScoreAFINN(wf_toaster_brand, top10brands_toaster)
scoreCoffeeAFINN <- sentimentScoreAFINN(wf_coffee_brand, top10brands_coffee)

# PLOT SENTIMENT SCORE BASED ON BRAND
plotSentimentScoreAFINN <- function(input, names, text) {
  input$brand <- names
  input %>%
    mutate(brand = reorder(brand, scoreAFINN)) %>%
    ggplot(aes(brand, scoreAFINN, fill = priceGroup)) +
      geom_col(colour="black") +
      coord_flip() +
      xlab("Brand") +
      ylab("Average sentiment score") +
      ggtitle(paste("Mean Score for Brands in ", text, "-Category", sep = "")) +
      geom_hline(aes(yintercept=mean(scoreAFINN), color="mean"), size=1) +
      geom_label(aes(label = meanStar), color = "black", show.legend = FALSE, hjust = 1.2) +
      scale_fill_brewer(name = "Price Segment") +
      scale_color_manual(name = "Statistics", values = c(mean = "red"))
}
# Apply plotSentimentScoreAFINN-function
plotSentimentScoreAFINN(scoreCellphoneAFINN, nameBrandCellphone , "Cellphones")
plotSentimentScoreAFINN(scoreHeadphoneAFINN, nameBrandHeadphone,"Headphones")
plotSentimentScoreAFINN(scoreToasterAFINN, nameBrandToaster, "Toaster")
plotSentimentScoreAFINN(scoreCoffeeAFINN, nameBrandCoffee, "Coffee")

# SENTIMENT CONTRIBUTION
sentiContributions <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
}
# Apply sentiContributionsBrand-function
contributionHeadphone <- sentiContributions(tokenized_headphone)
contributionCellphone <- sentiContributions(tokenized_cellphone)
contributionToaster <- sentiContributions(tokenized_toaster)
contributionCoffee <- sentiContributions(tokenized_coffee)

# PLOT SENTIMENT CONTRIBUTION
sentiContributionPlot <- function(input, selectCategory) {
  input %>%
    top_n(11, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    ggtitle(paste("Sentiment Contribution within ", selectCategory, "-Category", sep = ""))
}
# Apply sentiContributionPlot-function
sentiContributionPlot(contributionHeadphone, "Headphones")
sentiContributionPlot(contributionCellphone, "Cellphones")
sentiContributionPlot(contributionToaster, "Toaster")
sentiContributionPlot(contributionCoffee, "Coffee")

# CREATE BOXPLOT FOR OVERALL VS. SCORELX
boxplotScoreLX <- function(input, cat){
  input$overall <- as.factor(input$overall)
  ggplot(input, aes(x=overall, y=scoreLX)) + 
    geom_boxplot() +
    theme(text = element_text(size=18), plot.title = element_text(size = 14, face = "bold")) +
    ylim(-4,5) +
    #geom_hline(aes(yintercept=mean(scoreLX), color="mean"), size=1) +
    #scale_color_manual(name = "Statistics", values = c(mean = "red")) +
    ggtitle(paste("Lexicon-Sentiment-Score vs. Overall Rating for", cat, sep = " "))
}
# Apply boxplotScore-function
boxplotScoreLX(prep_headphone_brand, "Headphones")
boxplotScoreLX(prep_cellphone_brand, "Cellphones")
boxplotScoreLX(prep_toaster_brand, "Toasters")
boxplotScoreLX(prep_coffee_brand, "Coffee Makers")

# CREATE BOXPLOT FOR OVERALL VS. SCORENN
boxplotScoreNN <- function(input, cat){
  input$overall <- as.factor(input$overall)
  ggplot(input, aes(x=overall, y=scoreNN)) + 
    geom_boxplot() +
    theme(text = element_text(size=18), plot.title = element_text(size = 14, face = "bold")) +
    ggtitle(paste("ML-Sentiment-Score vs. Overall Rating for", cat, sep = " ")) +
    ylim(-5,3)
}
# Apply boxplotScore-function
boxplotScoreNN(prep_headphone_brand, "Headphones")
boxplotScoreNN(prep_cellphone_brand, "Cellphones")
boxplotScoreNN(prep_toaster_brand, "Toasters")
boxplotScoreNN(prep_coffee_brand, "Coffee Makers")
