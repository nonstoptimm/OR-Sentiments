### DATA CLEANING IN CLEANING.R ###
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(textcat)

# CREATE UNIGRAM TOKENS
tokenizeReview <- function(input) {
  input %>%
  select(asin, overall, reviewTime, reviewerID, review, brand, price, title, categories.0.2, scoreNN) %>%
  unnest_tokens(word, review)
}
# Apply it to datasets
tokenized_cellphone <- tokenizeReview(prep_cellphone_brand)
tokenized_headphone <- tokenizeReview(prep_headphone_brand)
tokenized_coffee <- tokenizeReview(prep_coffee_brand)
tokenized_toaster <- tokenizeReview(prep_toaster_brand)

# Remove Stopwords
removeStopwords <- function(input){
  input %>%
  anti_join(stop_words)
} 
# Apply to tokenized dataset
tokenized_cellphone <- removeStopwords(tokenized_cellphone)
tokenized_headphone <- removeStopwords(tokenized_headphone)
tokenized_coffee <- removeStopwords(tokenized_coffee)
tokenized_toaster <- removeStopwords(tokenized_toaster)

# Remove Unwanted Words
removeUnwanted <- function(input, unwanted){
  input %>%
    anti_join(unwanted)
} 
# Apply to tokenized datasets
# nothing yet

# COUNT TOP WORDS
countWords <- function(input) { 
  input %>%
    count(word, sort = TRUE) 
}
# Apply to tokenized dataset
countWordsHeadphone <- countWords(tokenized_headphone)
countWordsCellphone <- countWords(tokenized_cellphone)
countWordsToaster <- countWords(tokenized_toaster)
countWordsCoffee <- countWords(tokenized_coffee)

# Count for DTM
countWordsDocument <- function(input) { 
  input %>%
    count(asin, reviewerID, word, sort = TRUE) %>%
    arrange(asin, reviewerID)
}
# Apply Count for DTM
#countWordsDocumentCellphone <- countWordsDocument(tokenized_cellphone)

# Count Words without Unwanted Words
countWordsUnw <- function(input, unwanted) { 
    input %>%
      filter(!word %in% unwanted) %>%
      count(word, sort = TRUE) 
}
# Count Top Words excluding unwanted 
wf_cellphone <- countWordsUnw(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_headphone <- countWordsUnw(tokenized_headphone, c("headphone"))
wf_toaster <- countWordsUnw(tokenized_toaster, c("toaster", "toast", "buy", "product"))
wf_coffee <- countWordsUnw(tokenized_coffee, c("coffee", "buy", "machine", "maker"))

# Most common words based on brand
countWordsBrand <- function(input, unwanted) {
  input %>%
  filter(!brand == "" & !brand == word) %>%
  filter(!word %in% unwanted) %>%
  count(brand, word, sort = TRUE) %>%
  ungroup()
}
# Apply countWordsBrand
wf_cellphone_brand <- countWordsBrand(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_coffee_brand <- countWordsBrand(tokenized_coffee, c("coffee", "buy", "machine", "maker"))
wf_toaster_brand <- countWordsBrand(tokenized_toaster, c("toaster", "toast", "buy", "product"))
wf_headphone_brand <- countWordsBrand(tokenized_headphone, c("headphone", "buy"))


# Most common words entire category
countWordsCategory <- function(input, unwanted) {
  input %>%
    filter(!brand == "" & !brand == word) %>%
    filter(!word %in% unwanted) %>%
    count(categories.0.2, word, sort = TRUE) %>%
    ungroup()
}
# Apply countWordsCategory
wf_cellphone_category <- countWordsCategory(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_coffee_category <- countWordsCategory(tokenized_coffee, c("coffee", "buy", "machine", "maker"))
wf_toaster_category <- countWordsCategory(tokenized_toaster, c("toaster", "toast", "buy", "product"))
wf_headphone_category <- countWordsCategory(tokenized_headphone, c("headphone", "buy"))

# TF-IDF for the whole Category
tf_idf_general <- function(input) {
  input %>%
  bind_tf_idf(asin, reviewerID, word, n) %>%
  arrange(desc(tf_idf))  
}
tf_idf_general(countWordsHeadphone)

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

# Function for plotting 
getSentimentPlot <- function(input) {
  input %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
}

# Lexical Diversity
lexDiversity <- function(input) {
  input %>%
  group_by(asin,reviewTime) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))
}

lexDiv_Cellphone <- lexDiversity(tokenized_cellphone)

# Create TF IDF for Overall Rating
popular_tfidf_words <- function(input, undesired) {
  input %>%
  unnest_tokens(word, review) %>%
  distinct() %>%
  filter(!word %in% undesired) %>%
  count(overall, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, overall, n)
}

tfidf_cellphone <- popular_tfidf_words(merged_cellphone, c("phone", "phones", "2"))

# Top Popular TF-IDF Words
top_popular_tfidf_words <- function(input) {
  input %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(overall) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(overall, tf_idf) %>%
  mutate(row = row_number())
}

tfidf_cellphone_top <- top_popular_tfidf_words(tfidf_cellphone)




top_popular_tfidf_words <- function(input) {
  input %>%
  ggplot(aes(x = row, tf_idf,
             fill = overall)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  facet_wrap(~overall, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()
}
top_popular_tfidf_words(tfidf_cellphone_top)


# Plot Amount of Word Distribution
full_word_count %>%
  ggplot() +
  geom_boxplot(aes(x = num_words)) +
  ylab("Song Count") +
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())