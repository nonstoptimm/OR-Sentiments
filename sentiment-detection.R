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

