### SENTIMENT ###
# Pipe tidy_reviews to the next line
tidy_reviews <- merged_branded %>% 
  # Transform the lyrics column to a word column
  unnest_tokens(word, reviewText)

totals <- tidy_reviews %>%
  # Count by song to find the word totals for each song
  count(asin) %>%
  # Rename the new column
  rename(total_words = n)

review_counts <- tidy_reviews %>%
  # Combine totals with tidy_lyrics using the "song" column
  left_join(totals, by = "asin")

review_sentiment <- review_counts %>%
  # Implement sentiment analysis with the "nrc" lexicon
  inner_join(get_sentiments("bing"))

review_sentiment %>%
  # Find how many sentiment words each song has
  count(asin, sentiment, sort = TRUE)

# Which reviews have the highest proportion of negative words?
review_sentiment %>%
  # Count using three arguments
  count(asin, sentiment, total_words) %>%
  ungroup() %>%
  # Make a new percent column with mutate 
  mutate(percent = n / total_words) %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Arrange by descending percent
  arrange(desc(percent))

# What reviews have the highest proportion of positive words?
review_sentiment %>%
  count(asin, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  filter(sentiment == "positive") %>%
  arrange(desc(percent))

### WORD CHOICE FOR SUB-CATEGORIES

tidy_reviews %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by word and station
  count(word, station) %>%
  # Group by station
  group_by(station) %>%
  # Take the top 10 words for each station
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, station, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = station)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ station, nrow = 2, scales = "free") +
  coord_flip()


### SENTIMENT OVER TIME

# Needed Package: Lubridate

sentiment_by_time <- tidy_tv %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(show_date, unit = "6 months")) %>%
  # Group by date
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc"))

sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment == "positive" | sentiment == "negative") %>%
  # Count by date, sentiment, and total_words
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(date, percent, fill = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)





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
#scoreHeadphoneAFINN <- sentimentScoreAFINN(wf_headphone_brand)
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
#plotSentimentScoreAFINN(scoreHeadphoneAFINN, "Headphones")
plotSentimentScoreAFINN(scoreToasterAFINN, 80, "Toaster")
plotSentimentScoreAFINN(scoreCoffeeAFINN, 200, "Coffee")
            
# SENTIMENT CONTRIBUTION
sentiContributions <- function(input) {
  input %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))
}
# Apply sentiContributions Function
sentiContributions(tokenized_cellphone)

# SENTIMENT CONTRIBUTION FOR BRANDED PRODUCTS
sentiContributionsBrand <- function(input, selectBrand) {
  input %>%
    filter(brand == selectBrand) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
}
sentiContributionsBrand(tokenized_cellphone, "apple")

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
# sentimentReviewHeadphone <- sentimentReview(tokenized_headphone)
sentimentReviewToaster <- sentimentReview(tokenized_toaster)
sentimentReviewCoffee <- sentimentReview(tokenized_coffee)
