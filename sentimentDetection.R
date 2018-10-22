# LEXICON-BASED SENTIMENT ANALYSIS
# Sentiment detection and comparison to star levels
# sentimentDetection.R
# Load required packages
library(dplyr)
library(ggplot2)
library(tidytext)

# CREATE UNIGRAM TOKENS
# Unnest the reviews to one word per row
tokenizeReview <- function(input) {
  input %>%
    unnest_tokens(word, review)
}
# Apply tokenizeReview-function
tokenized_cellphone <- tokenizeReview(prep_cellphone_brand) # Cellphones
tokenized_headphone <- tokenizeReview(prep_headphone_brand) # Headphones
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
    group_by(document) %>% # as for every document
    summarize(scoreLX = mean(score),
              words = n()) %>%
    ungroup()
}
# Apply sentimentReview-function
sentimentReviewCellphone <- getSentimentAFINN(tokenized_cellphone) # Cellphones
sentimentReviewHeadphone <- getSentimentAFINN(tokenized_headphone) # Headphones
sentimentReviewToaster <- getSentimentAFINN(tokenized_toaster) # Toaster
sentimentReviewCoffee <- getSentimentAFINN(tokenized_coffee) # Coffee

# AFINN-SENTIMENT SCORE CALCULATION BY BRAND
# Word frequency as input
sentimentScoreAFINN <- function(input, brandList) {
  input <- input %>%
    filter(brand %in% brandList$brand) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(brand) %>%
    summarize(meanLX = sum(score * n) / sum(n))
  input <- left_join(input, brandList, by = "brand")
}
# Apply sentimentScoreAFINN-function
scoreHeadphoneAFINN <- sentimentScoreAFINN(wf_headphone_brand, top10brands_headphone)
scoreCellphoneAFINN <- sentimentScoreAFINN(wf_cellphone_brand, top10brands_cellphone)
scoreToasterAFINN <- sentimentScoreAFINN(wf_toaster_brand, top10brands_toaster)
scoreCoffeeAFINN <- sentimentScoreAFINN(wf_coffee_brand, top10brands_coffee)

# ADD SENTIMENT SCORE PER TOP BRAND
addBrandScore <- function(input, aggregated, brandList) {
  input <- input %>% 
    filter(brand %in% brandList$brand) %>%
    group_by(brand) %>%
    summarize(meanNN = mean(scoreNN)) %>% 
    arrange(brand) %>% 
    select(brand, meanNN) %>% 
    left_join(aggregated, by = "brand")
  input <- input[, c("properBrand", "brand", "reviewCount", "meanStar", "meanLX", "meanNN", "avgPrice", "priceGroup")]
}
# Apply addBrandScore-function
scoreHeadphoneBrand <- addBrandScore(prep_headphone_brand, scoreHeadphoneAFINN, top10brands_headphone)
scoreCellphoneBrand <- addBrandScore(prep_cellphone_brand, scoreCellphoneAFINN, top10brands_cellphone)
scoreToasterBrand <- addBrandScore(prep_toaster_brand, scoreToasterAFINN, top10brands_toaster)
scoreCoffeeBrand <- addBrandScore(prep_coffee_brand, scoreCoffeeAFINN, top10brands_coffee)

# PLOT SENTIMENT SCORE BASED ON BRAND
plotSentimentScore <- function(input, text, scoreChoice, ylim) {
  if(scoreChoice == "ML-Model") {
    input$score <- input$meanNN    
  } else if(scoreChoice == "Lexicon") {
    input$score <- input$meanLX
  }
  input$priceGroup <- factor(scoreHeadphoneBrand$priceGroup, levels = c("Low", "Medium", "High"))
  input %>%
    mutate(properBrand = reorder(properBrand, score)) %>%
    ggplot(aes(properBrand, score, fill = priceGroup)) +
      geom_col(color = "black") +
      coord_flip() +
      ylim(ylim) +
      xlab("Brand") +
      ylab(paste("Average Sentiment Score (", scoreChoice, ")", sep = "")) +
      # ggtitle(paste("Mean Score for Brands in ", text, "-Category (", scoreChoice, ")", sep = "")) +
      geom_hline(aes(yintercept = mean(score), color="Mean"), size=1) +
      scale_fill_manual(name = "Price Segment", values=c("#00CC99", "#FDEE00", "#007FFF")) +
      geom_label(aes(label = meanStar), color = "black", show.legend = FALSE, hjust = + 1.2, family = "LM Roman 10") +
      theme(text = element_text(size = 15, family = "LM Roman 10")) +
      scale_color_manual(name = "Statistics", values = c(Mean = "red"))
}
# Apply plotSentimentScoreAFINN-function
plotSentimentScore(scoreHeadphoneBrand,"Headphones", "Lexicon", c(0, 1.7)) 
plotSentimentScore(scoreCellphoneBrand, "Cellphones", "Lexicon", c(0, 1.7))
plotSentimentScore(scoreToasterBrand, "Toaster", "Lexicon", c(0, 1.7)) 
plotSentimentScore(scoreCoffeeBrand, "Coffee", "Lexicon", c(0, 1.7)) 
plotSentimentScore(scoreHeadphoneBrand, "Headphones", "ML-Model", c(-0.1, 0.6)) 
plotSentimentScore(scoreCellphoneBrand, "Cellphones", "ML-Model", c(-1.3, 0.3)) 
plotSentimentScore(scoreToasterBrand, "Toaster", "ML-Model", c(-0.25, 0.48))
plotSentimentScore(scoreCoffeeBrand, "Coffee", "ML-Model",  c(-0.5, 0.730)) 

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
    xlab("Sentiment Contribution") +
    ylab("Word") +
    theme(text = element_text(size = 15, family = "LM Roman 10")) +
    # ggtitle(paste("Sentiment Contribution within ", selectCategory, "-Category", sep = "")) +
    scale_fill_manual(values=c( "firebrick", "dodgerblue4"))
}
# Apply sentiContributionPlot-function
sentiContributionPlot(contributionHeadphone, "Headphones")
sentiContributionPlot(contributionCellphone, "Cellphones")
sentiContributionPlot(contributionToaster, "Toaster")
sentiContributionPlot(contributionCoffee, "Coffee")

# CREATE BOXPLOT FOR OVERALL VS. SCORELX
boxplotScoreLX <- function(input, cat){
  # As factor to create x-axis-scale
  input$overall <- as.factor(input$overall)
  # Plot data
  ggplot(input, aes(x = overall, y = scoreLX)) + 
    geom_boxplot() +
    xlab("Star Rating") +
    ylab("Sentiment Score (Lexicon)") +
    theme(text = element_text(size = 18), plot.title = element_text(size = 14, face = "bold")) +
    # ggtitle(paste("Lexicon-Sentiment-Score vs. Overall Rating for", cat, sep = " ")) +
    theme(text = element_text(size = 15, family = "LM Roman 10")) +
    ylim(-4,5)
}
# Apply boxplotScore-function
boxplotScoreLX(prep_headphone_brand, "Headphones")
boxplotScoreLX(prep_cellphone_brand, "Cellphones")
boxplotScoreLX(prep_toaster_brand, "Toasters")
boxplotScoreLX(prep_coffee_brand, "Coffee Makers")

# CREATE BOXPLOT FOR OVERALL VS. SCORENN
boxplotScoreNN <- function(input, cat){
  input$overall <- as.factor(input$overall)
  ggplot(input, aes(x = overall, y = scoreNN)) + 
    geom_boxplot() +
    xlab("Star Rating") +
    ylab("Sentiment Score (ML)") +
    theme(text = element_text(size = 18), plot.title = element_text(size = 14, face = "bold")) +
    # ggtitle(paste("ML-Sentiment-Score vs. Overall Rating for", cat, sep = " ")) +
    theme(text = element_text(size = 15, family = "LM Roman 10")) +
    ylim(-5,3)
}
# Apply boxplotScore-function
boxplotScoreNN(prep_headphone_brand, "Headphones")
boxplotScoreNN(prep_cellphone_brand, "Cellphones")
boxplotScoreNN(prep_toaster_brand, "Toasters")
boxplotScoreNN(prep_coffee_brand, "Coffee Makers")
