# TEXT MINING OF UNIGRAMS
# Investigation of unigrams
# textmineUnigram.R
# Load required packages
library(dplyr) 
library(tidytext) # unnest tokens
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

# REMOVE STOPWORDS
removeStopwords <- function(input){
  input %>%
    anti_join(stop_words) # perform anti-join to stop_words
} 
# Apply removeStopwords-function
tokenized_headphone_filtered <- removeStopwords(tokenized_headphone) # Headphones
tokenized_cellphone_filtered <- removeStopwords(tokenized_cellphone) # Cellphones
tokenized_coffee_filtered <- removeStopwords(tokenized_coffee) # Coffee Makers 
tokenized_toaster_filtered <- removeStopwords(tokenized_toaster) # Toaster

# COUNT WORDS OVER WHOLE CORPUS WITHOUT SPECIAL UNWANTED WORDS
# "Unwanted" words recommended, as e.g. category name itself would appear too often
countWords <- function(input, unwanted) { 
  input %>%
    filter(!word %in% unwanted) %>%
    count(word, sort = TRUE) 
}
# Apply countWords-function
wf_headphone <- countWords(tokenized_headphone_filtered, c("headphone", "buy", "product"))
wf_cellphone <- countWords(tokenized_cellphone_filtered, c("phone", "buy", "product", "2", "3", "4", "34"))
wf_toaster <- countWords(tokenized_toaster_filtered, c("toaster", "toast", "buy", "product", "2", "4"))
wf_coffee <- countWords(tokenized_coffee_filtered, c("coffee", "buy", "machine", "maker", "1", "2", "3", "4", "5", "34"))

# COUNT WORDS BASED ON BRAND
countWordsBrand <- function(input, unwanted) {
  input %>%
    filter(!brand == "" & !brand == word) %>%
    filter(!word %in% unwanted) %>%
    count(brand, word, sort = TRUE) %>%
    ungroup()
}
# Apply countWordsBrand-function
wf_headphone_brand <- countWordsBrand(tokenized_headphone, c("headphone", "buy", "product"))  # Headphones
wf_cellphone_brand <- countWordsBrand(tokenized_cellphone, c("phone", "buy", "product", "2")) # Cellphones
wf_coffee_brand <- countWordsBrand(tokenized_coffee, c("coffee", "buy", "machine", "maker", "1", "2", "3", "4", "5", "34"))
wf_toaster_brand <- countWordsBrand(tokenized_toaster, c("toaster", "toast", "buy", "product", "2", "4"))  # Toaster

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
plotWordDistribution <- function(input1, input2, input3, input4) {
  input1$Category <- rep("Headphones", nrow(input1))
  input2$Category <- rep("Cellphones", nrow(input2))
  input3$Category <- rep("Toasters", nrow(input3))
  input4$Category <- rep("Coffee Makers", nrow(input4))
  merged <- do.call("rbind", list(input1, input2, input3, input4))
  merged %>% 
    ggplot(aes(n / total)) +
    geom_histogram(show.legend = FALSE) +
    #ggtitle("Wort Contribution within each Category", sep="") +
    xlab("Relation to Total Amount of Words") + 
    ylab("Word Count") +
    xlim(NA, 0.0009) +
    theme(text = element_text(size = 15, family = "LM Roman 10")) + # Latex Font
    facet_wrap(~ Category, ncol = 2, scales = "free_y")
}
# Apply plotWordDistribution-function
plotWordDistribution(headphoneWords, cellphoneWords, toasterWords, coffeeWords)
plotWordDistribution(headphoneWords, "Headphones")
plotWordDistribution(cellphoneWords, "Cellphones")
plotWordDistribution(toasterWords, "Toasters")
plotWordDistribution(coffeeBrandWords, "Coffee Makers")
