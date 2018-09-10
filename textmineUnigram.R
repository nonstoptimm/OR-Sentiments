# textmineUnigram.R
# Load required packages
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
# "Unwanted" words recommended, as e.g. category name itself would appear too often
countWords <- function(input, unwanted) { 
  input %>%
    filter(!word %in% unwanted) %>%
    count(word, sort = TRUE) 
}
# Count Top Words excluding unwanted 
wf_headphone <- countWords(tokenized_headphone, c("headphone", "buy", "product"))
wf_cellphone <- countWords(tokenized_cellphone, c("phone", "buy", "product", "2", "3", "4", "34"))
wf_toaster <- countWords(tokenized_toaster, c("toaster", "toast", "buy", "product", "2", "4"))
wf_coffee <- countWords(tokenized_coffee, c("coffee", "buy", "machine", "maker", "1", "2", "3", "4", "5", "34"))

# COUNT WORDS BASED ON BRAND
countWordsBrand <- function(input, unwanted) {
  input %>%
    filter(!brand == "" & !brand == word) %>%
    filter(!word %in% unwanted) %>%
    count(brand, word, sort = TRUE) %>%
    ungroup()
}
# Apply countWordsBrand-function
wf_headphone_brand <- countWordsBrand(tokenized_headphone, c("headphone", "buy", "product"))
wf_cellphone_brand <- countWordsBrand(tokenized_cellphone, c("phone", "buy", "product", "2"))
wf_coffee_brand <- countWordsBrand(tokenized_coffee, c("coffee", "buy", "machine", "maker", "1", "2", "3", "4", "5", "34"))
wf_toaster_brand <- countWordsBrand(tokenized_toaster, c("toaster", "toast", "buy", "product", "2", "4"))

# CALCULATE TOTAL WORDS
# Input must be wf for brands
totalWords <- function(input){
  summarized <- input %>% summarize(total = sum(n))
  summarized <- as.data.frame(rep(as.numeric(summarized), nrow(input)))
  names(summarized) <- "total"
  return(summarized)
} 
# Apply totalWords-function
headphoneWords <- totalWords(wf_headphone)
cellphoneWords <- totalWords(wf_cellphone)
toasterWords <- totalWords(wf_toaster)
coffeeWords <- totalWords(wf_coffee)
# Append it to the word dataframe
headphoneWords <- cbind(wf_headphone, headphoneWords)
cellphoneWords <- cbind(wf_cellphone, cellphoneWords)
toasterWords <- cbind(wf_toaster, toasterWords)
coffeeWords <- cbind(wf_coffee, coffeeWords)

# CALCULATE TOTAL WORDS PER BRAND
# Input must be wf for brands
totalWordsBrand <- function(input){
  input %>% 
    group_by(brand) %>% 
    summarize(total = sum(n))
} 
# Apply totalWordsBrand-function
headphoneBrandWords <- totalWordsBrand(wf_headphone_brand)
cellphoneBrandWords <- totalWordsBrand(wf_cellphone_brand)
toasterBrandWords <- totalWordsBrand(wf_toaster_brand)
coffeeBrandWords <- totalWordsBrand(wf_coffee_brand)

# Join it together brand
headphoneBrandWords <- left_join(wf_headphone_brand, headphoneBrandWords)
cellphoneBrandWords <- left_join(wf_cellphone_brand, cellphoneBrandWords)
toasterBrandWords <- left_join(wf_toaster_brand, toasterBrandWords)
coffeeBrandWords <- left_join(wf_coffee_brand, coffeeBrandWords)

# PLOT WORD DISTRIBUTION 
plotWordDistribution <- function(input1, input2, input3, input4, title) {
  input1$Category <- rep("Headphones", nrow(input1))
  input2$Category <- rep("Cellphones", nrow(input2))
  input3$Category <- rep("Toasters", nrow(input3))
  input4$Category <- rep("Coffee Makers", nrow(input4))
  merged <- do.call("rbind", list(input1, input2, input3, input4))
  merged %>% 
    ggplot(aes(n/total)) +
    geom_histogram(show.legend = FALSE) +
    ggtitle(paste("Wort Contribution within each Category", sep="")) +
    xlim(NA, 0.0009) +
    facet_wrap(~Category, ncol = 2, scales = "free_y")
}
# Apply Function
plotWordDistribution(headphoneWords, cellphoneWords, toasterWords, coffeeWords, "XY")
plotWordDistribution(headphoneWords, "Headphones")
plotWordDistribution(cellphoneWords, "Cellphones")
plotWordDistribution(toasterWords, "Toasters")
plotWordDistribution(coffeeBrandWords, "Coffee Makers")

# BRAUCH GLAUB KEINE SAU
# # PLOT WORD DISTRIBUTION FOR BRANDS
# # Treshold has to be set, otherwise too many brands
# # Optionally, a special brand name can be required as well
# plotWortDistributionBrands <- function(input, treshold, individualBrand, title) {
#   input %>% 
#     filter(total > treshold | brand == individualBrand) %>%
#     ggplot(aes(n/total, fill = brand)) +
#     geom_histogram(show.legend = FALSE) +
#     ggtitle(paste("Wort Contribution regarding different Brands for", title, sep=" ")) +
#     xlim(NA, 0.0009) +
#     facet_wrap(~brand, ncol = 2, scales = "free_y")
# }
# # Apply Function
# plotWortDistributionBrands(headphoneBrandWords, 280000, "beats", "Headphones")

# BRAUCH MAN GLAUB NET
# CREATE TF-IDF FOR OVERALL
# createTFIDFbrand <- function(input) {
#   input %>%
#     bind_tf_idf(word, brand, n)
# }
# # Apply Function
# headphoneBrandTFIDF <- createTFIDFbrand(headphoneBrandWords)

# FÜR DEN PLOT
# CREATE TF-IDF FOR BRAND
createTFIDFbrand <- function(input) {
  input %>%
    bind_tf_idf(word, brand, n)
}
# Apply Function
headphoneBrandTFIDF <- createTFIDFbrand(headphoneBrandWords)
# 
# # FILTER AND SORT BY BRAND
# filterBrandTFIFT <- function(input, brandname){
#   input %>%
#     filter(brand == brandname) %>%
#     select(-total) %>%
#     arrange(desc(tf_idf))
# }
# # Apply Function
# filterBrandTFIFT(headphoneBrandTFIDF, "beats")
# filterBrandTFIFT(headphoneBrandTFIDF, "bose")

# IST HIER GLAUBE ICH UNNOETIG
# Function for plotting 
# getSentimentPlot <- function(input) {
#   input %>%
#     group_by(sentiment) %>%
#     top_n(10) %>%
#     ungroup() %>%
#     mutate(word = reorder(word, n)) %>%
#     ggplot(aes(word, n, fill = sentiment)) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~sentiment, scales = "free_y") +
#     labs(y = "Contribution to sentiment",
#          x = NULL) +
#     coord_flip()
# }

# Create TF-IDF
tfidf_words_brand <- function(input, undesired, brandList) {
  input %>% 
    unnest_tokens(word, review) %>%
    distinct() %>%
    filter(!word %in% undesired) %>%
    anti_join(stop_words) %>%
    count(brand, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, brand, n) %>%
    filter(brand %in% top10brands_headphone$brand[1:10]) %>%
    filter(!word %in% brand) %>%
    arrange(desc(tf_idf))
}
# Apply tfidf_words_brand-function
tfidf_headphone <- tfidf_words_brand(prep_headphone_brand, c("headphone"), top10brands_headphone)

# EHER NICHT
# # Create TF-IDF for Overall Rating
# popular_tfidf_words <- function(input, undesired) {
#   input %>%
#     unnest_tokens(word, review) %>%
#     distinct() %>%
#     filter(!word %in% undesired) %>%
#     anti_join(stop_words) %>%
#     count(overall, word, sort = TRUE) %>%
#     ungroup() %>%
#     bind_tf_idf(word, overall, n)
# }
# # Get the top words for every star rating
# tfidf_cellphone <- popular_tfidf_words(prep_headphone_brand, c("headphone"))
# tfidf_cellphone %>% filter(overall == 1)
# tfidf_cellphone %>% filter(overall == 5)
# tfidf_cellphone %>% filter(overall == 2)
# tfidf_cellphone %>% filter(overall == 3)
# tfidf_cellphone %>% filter(overall == 4)

# PLOT TF-IDF
plotTFIDFbrand <- function(input, brandList) {
  input %>% 
    filter(brand %in% brandList$brand[1:9]) %>%
    #filter(total > totalwords) %>% 
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(brand) %>% 
    top_n(8) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = brand)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~brand, ncol = 3, scales = "free") +
    ggtitle("TF-IDF Top-Brands") +
    coord_flip()
}
# Apply Function
plotTFIDFbrand(headphoneBrandTFIDF, top10brands_headphone)
