# tidyData.R
### DATA CLEANING IN CLEANING.R ###
library(dplyr) x
library(stringr)
library(tidyr)
library(tidytext) x
library(textcat)

# CREATE UNIGRAM TOKENS
tokenizeReview <- function(input) {
  input %>%
  select(asin, overall, reviewTime, reviewerID, review, brand, price, title, scoreNN) %>%
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

# Count for DTM
countWordsDocument <- function(input) { 
  input %>%
    count(asin, reviewerID, word, sort = TRUE) %>%
    arrange(asin, reviewerID)
}
# Apply Count for DTM
#countWordsDocumentCellphone <- countWordsDocument(tokenized_cellphone)

# Count Words without Unwanted Words, if desired
countWords <- function(input, unwanted) { 
    input %>%
      filter(!word %in% unwanted) %>%
      count(word, sort = TRUE) 
}
# Count Top Words excluding unwanted 
wf_cellphone <- countWords(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_headphone <- countWords(tokenized_headphone, c("headphone"))
wf_headphone <- countWords(tokenized_headphone, c(""))
wf_toaster <- countWords(tokenized_toaster, c("toaster", "toast", "buy", "product"))
wf_coffee <- countWords(tokenized_coffee, c("coffee", "buy", "machine", "maker"))

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
# PRÜFEN: IST DOCH DASSELBE?
#countWordsCategory <- function(input, unwanted) {
#  input %>%
#    filter(!brand == "" & !brand == word) %>%
#    filter(!word %in% unwanted) %>%
#    count(categories.0.2, word, sort = TRUE) %>%
#    ungroup()
#}
# Apply countWordsCategory
#wf_cellphone_category <- countWordsCategory(tokenized_cellphone, c("phone", "buy", "product", "2"))
#wf_coffee_category <- countWordsCategory(tokenized_coffee, c("coffee", "buy", "machine", "maker"))
#wf_toaster_category <- countWordsCategory(tokenized_toaster, c("toaster", "toast", "buy", "product"))
#wf_headphone_category <- countWordsCategory(tokenized_headphone, c("headphone", "buy"))

# TF-IDF for the whole Category
# PRÜFEN: FUNKTIONIERT NICHT?
#tf_idf_general <- function(input) {
#  input %>%
#  bind_tf_idf(asin, reviewerID, word, n) %>%
#  arrange(desc(tf_idf))  
#}
#tf_idf_general(wf_headphone)



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

# Create TF-IDF for Overall Rating
popular_tfidf_words <- function(input, undesired) {
  input %>%
  unnest_tokens(word, review) %>%
  distinct() %>%
  filter(!word %in% undesired) %>%
  anti_join(stop_words) %>%
  count(overall, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, overall, n)
}
# Get the top words for every star rating
tfidf_cellphone <- popular_tfidf_words(prep_headphone_brand, c("headphone"))
tfidf_cellphone %>% filter(overall == 1)
tfidf_cellphone %>% filter(overall == 5)
tfidf_cellphone %>% filter(overall == 2)
tfidf_cellphone %>% filter(overall == 3)
tfidf_cellphone %>% filter(overall == 4)
