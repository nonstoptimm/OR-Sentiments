# tidyData.R
### DATA CLEANING IN CLEANING.R ###
library(dplyr) 
library(stringr)
library(tidyr)
library(tidytext) 
library(textcat)
library(ggplot2)

# CREATE UNIGRAM TOKENS
# Unnest the reviews to one word per row
tokenizeReview <- function(input) {
  input %>%
    # select(asin, overall, reviewTime, reviewerID, review, brand, price, title, scoreNN, document) %>%
    unnest_tokens(word, review)
}
# Apply it to datasets
tokenized_headphone <- tokenizeReview(prep_headphone_brand)
tokenized_cellphone <- tokenizeReview(prep_cellphone_brand)
tokenized_coffee <- tokenizeReview(prep_coffee_brand)
tokenized_toaster <- tokenizeReview(prep_toaster_brand)

# REMOVE STOPWORDS
removeStopwords <- function(input){
  input %>%
    anti_join(stop_words) # perform anti-join to stop_words
} 
# Apply to tokenized dataset
tokenized_headphone <- removeStopwords(tokenized_headphone) # Headphones
tokenized_cellphone <- removeStopwords(tokenized_cellphone) # Cellphones
tokenized_coffee <- removeStopwords(tokenized_coffee) # Coffee Makers 
tokenized_toaster <- removeStopwords(tokenized_toaster) # Toaster

# COUNT WORDS OVER WHOLE CORPUS WITHOUT SPECIAL UNWANTED WORDS
# Recommended, as the category name itself would appear too often
countWords <- function(input, unwanted) { 
  input %>%
    filter(!word %in% unwanted) %>%
    count(word, sort = TRUE) 
}
# Count Top Words excluding unwanted 
wf_headphone <- countWords(tokenized_headphone, c("headphone"))
wf_cellphone <- countWords(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_toaster <- countWords(tokenized_toaster, c("toaster", "toast", "buy", "product"))
wf_coffee <- countWords(tokenized_coffee, c("coffee", "buy", "machine", "maker"))

# COUNT WORDS BASED ON BRAND
countWordsBrand <- function(input, unwanted) {
  input %>%
    filter(!brand == "" & !brand == word) %>%
    filter(!word %in% unwanted) %>%
    count(brand, word, sort = TRUE) %>%
    ungroup()
}
# Apply countWordsBrand
wf_headphone_brand <- countWordsBrand(tokenized_headphone, c("headphone", "buy"))
wf_cellphone_brand <- countWordsBrand(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_coffee_brand <- countWordsBrand(tokenized_coffee, c("coffee", "buy", "machine", "maker"))
wf_toaster_brand <- countWordsBrand(tokenized_toaster, c("toaster", "toast", "buy", "product"))

# CALCULATE TOTAL WORDS
# Input must be wf for brands
totalWords <- function(input){
  summarized <- input %>% 
    summarize(total = sum(n))
  summarized <- as.data.frame(rep(as.numeric(summarized), nrow(input)))
  names(summarized) <- "total"
  return(summarized)
} 
# Apply function
headphoneWords <- totalWords(wf_headphone)
# Append it to the word dataframe
headphoneWords <- cbind(wf_headphone, headphoneWords)

# CALCULATE TOTAL WORDS PER BRAND
# Input must be wf for brands
totalWordsBrand <- function(input){
  input %>% 
    group_by(brand) %>% 
    summarize(total = sum(n))
} 
# Apply function
headphoneBrandWords <- totalWordsBrand(wf_headphone_brand)

# Join it together brand
headphoneBrandWords <- left_join(wf_headphone_brand, headphoneBrandWords)

# PLOT WORD DISTRIBUTION 
plotWortDistribution <- function(input, title) {
  input %>% 
    ggplot(aes(n/total)) +
    geom_histogram(show.legend = FALSE) +
    ggtitle(paste("Wort Contribution for whole Category of", title, sep=" ")) +
    xlim(NA, 0.0009)
}
# Apply Function
plotWortDistribution(headphoneWords, "Headphones")

# PLOT WORD DISTRIBUTION FOR BRANDS
# Treshold has to be set, otherwise too many brands
# Optionally, a special brand name can be required as well
plotWortDistributionBrands <- function(input, treshold, individualBrand, title) {
  input %>% 
    filter(total > treshold | brand == individualBrand) %>%
    ggplot(aes(n/total, fill = brand)) +
    geom_histogram(show.legend = FALSE) +
    ggtitle(paste("Wort Contribution regarding different Brands for", title, sep=" ")) +
    xlim(NA, 0.0009) +
    facet_wrap(~brand, ncol = 2, scales = "free_y")
}
# Apply Function
plotWortDistributionBrands(headphoneBrandWords, 280000, "beats", "Headphones")

# CREATE TF-IDF FOR BRAND
createTFIDFbrand <- function(input) {
  input %>%
    bind_tf_idf(word, brand, n)
}
# Apply Function
headphoneBrandTFIDF <- createTFIDFbrand(headphoneBrandWords)

# FILTER AND SORT BY BRAND
filterBrandTFIFT <- function(input, brandname){
  input %>%
    filter(brand == brandname) %>%
    select(-total) %>%
    arrange(desc(tf_idf))
}
# Apply Function
filterBrandTFIFT(headphoneBrandTFIDF, "beats")
filterBrandTFIFT(headphoneBrandTFIDF, "bose")

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

# PLOT TF-IDF
plotTFIDFbrand <- function(input, totalwords) {
  input %>% 
    filter(total > totalwords) %>% 
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(brand) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = brand)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~brand, ncol = 3, scales = "free") +
    coord_flip()
}
# Apply Function
plotTFIDFbrand(headphoneBrandTFIDF, 280000)

# CALCULATE TOTAL WORDS PER PRODUCT
# Input must be wf for brands
#totalWordsProduct <- function(input, brand){
#  input %>% 
#    filter(brand == brand) %>%
#    group_by(title) %>% 
#    summarize(total = sum(n))
#} 
# Apply function
#headphoneProductWords <- totalWordsProduct(wf_headphone_brand)


# REMOVE ADDITIONAL UNWANTED WORDS
# removeUnwanted <- function(input, unwanted){
#  input %>%
#    anti_join(unwanted) # perform anti-join to individual words
#} 
# Apply to tokenized datasets
# nothing yet

# Count for DTM
#countWordsDocument <- function(input) { 
#  input %>%
#    count(asin, reviewerID, word, sort = TRUE) %>%
#    arrange(asin, reviewerID)
#}
# Apply Count for DTM
#countWordsDocumentCellphone <- countWordsDocument(tokenized_cellphone)

