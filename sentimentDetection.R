### SENTIMENT ###
# sentimentDetection.R

# Function for getting Lexicon-based Sentiment
getSentiment <- function(input, lexicon) {
  input %>%
    inner_join(get_sentiments(lexicon)) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
}
# Get Sentiments for Unigrams
getSentiment(tokenized_coffee, "bing")
getSentiment(tokenized_coffee, "nrc")
getSentiment(tokenized_toaster, "bing")
getSentiment(tokenized_toaster, "nrc")
getSentiment(tokenized_headphone, "bing")
getSentiment(tokenized_headphone, "nrc")
getSentiment(tokenized_cellphone, "bing")
getSentiment(tokenized_cellphone, "nrc")

# Calculate Sentiment Score for AFINN
sentimentScoreAFINN <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(brand) %>%
    summarize(scoreAFINN = sum(score * n) / sum(n), numWords = n()) %>%
    arrange(desc(scoreAFINN))
}
# Apply it to dataset
scoreCellphoneAFINN <- sentimentScoreAFINN(wf_cellphone_brand)
scoreHeadphoneAFINN <- sentimentScoreAFINN(wf_headphone_brand)
scoreToasterAFINN <- sentimentScoreAFINN(wf_toaster_brand)
scoreCoffeeAFINN <- sentimentScoreAFINN(wf_coffee_brand)

# Plot Sentiment Score for AFINN based on Brands
plotSentimentScoreAFINN <- function(input, num, text) {
  input %>%
    filter(numWords > num) %>%
    mutate(brand = reorder(brand, scoreAFINN)) %>%
    ggplot(aes(brand, scoreAFINN, fill = scoreAFINN > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    ylab("Average sentiment score") +
    ggtitle(paste("Average Sentiment Score for Brands in Category", text, sep = " "))
}
# Apply plotSentimentScoreAFINN Function
plotSentimentScoreAFINN(scoreCellphoneAFINN, 80, "Cellphones")
plotSentimentScoreAFINN(scoreHeadphoneAFINN, 500, "Headphones")
plotSentimentScoreAFINN(scoreToasterAFINN, 80, "Toaster")
plotSentimentScoreAFINN(scoreCoffeeAFINN, 200, "Coffee")
            
# SENTIMENT CONTRIBUTION
# EHER NICHT VERWENDEN
#sentiContributions <- function(input) {
#  input %>%
#  inner_join(get_sentiments("afinn"), by = "word") %>%
#  group_by(word) %>%
#  summarize(occurences = n(),
#            contribution = sum(score))
#}
# Apply sentiContributions Function
#sentiContributions(tokenized_headphone)
#sentiContributions(tokenized_cellphone)
#sentiContributions(tokenized_coffee)
#sentiContributions(tokenized_toaster)

# SENTIMENT CONTRIBUTION FOR BRANDED PRODUCTS
# sentiContributionsBrand <- function(input, selectBrand) {
#   input %>%
#     filter(brand == selectBrand) %>%
#     inner_join(get_sentiments("afinn"), by = "word") %>%
#     group_by(word) %>%
#     summarize(occurences = n(),
#               contribution = sum(score))
# }
# Apply sentiContributionsBrand Function
# sentiContributionsBrand(tokenized_headphone, "beats")
# sentiContributionsBrand(tokenized_cellphone, "apple")
# sentiContributionsBrand(tokenized_toaster, "")
# sentiContributionsBrand(tokenized_coffee, "")

# PLOT SENTIMENT CONTRIBUTION
sentiContributionPlot <- function(input, selectCategory) {
  input %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ggtitle(paste("Sentiment Contribution within", selectCategory, sep = " "))
}
# Apply sentiContributionPlot Function
sentiContributionPlot(sentiContributions(tokenized_headphone), "Headphones")
sentiContributionPlot(sentiContributionsBrand(tokenized_headphone, "beats"), "Cellphones for the brand Beats")
sentiContributionPlot(sentiContributionsBrand(tokenized_headphone, "beyerdynamic"), "Cellphones for the brand Beyerdynamic")
sentiContributionPlot(sentiContributionsBrand(tokenized_headphone, "sennheiser"), "Cellphones for the brand Sennheiser")
sentiContributionPlot(sentiContributionsBrand(tokenized_headphone, "panasonic"), "Cellphones for the brand Panasonic")
sentiContributionPlot(sentiContributions(tokenized_cellphone), "Cellphones")
sentiContributionPlot(sentiContributionsBrand(tokenized_cellphone, "samsung"), "Cellphones for the brand Samsung")
sentiContributionPlot(sentiContributionsBrand(tokenized_cellphone, "apple"), "Cellphones for the brand Apple")
sentiContributionPlot(sentiContributions(tokenized_coffee), "Coffee")
sentiContributionPlot(sentiContributions(tokenized_toaster), "Toaster")

### SENTIMENT BY REVIEW
sentimentReview <- function(input) {
  input %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(asin, reviewerID, scoreNN, overall, title) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)
}
# Apply sentimentReview Function
sentimentReviewCellphone <- sentimentReview(tokenized_cellphone)
sentimentReviewHeadphone <- sentimentReview(tokenized_headphone)
sentimentReviewToaster <- sentimentReview(tokenized_toaster)
sentimentReviewCoffee <- sentimentReview(tokenized_coffee)
