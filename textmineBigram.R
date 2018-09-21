# TEXT MINING OF BIGRAMS
# textBigram.R
# Load required packages
library(dplyr)
library(tidytext) # unnest_tokens
library(ggplot2)
library(tidyr) # spread/gather/separate

# TOKENIZE THE INPUT DATA INTO BIGRAMS
tokenizeBigrams <- function(input) {
  input %>% 
    # n = 2 means bigram
    unnest_tokens(bigram, review, token = "ngrams", n = 2) 
}
# Apply tokenizeBigrams-function
tokenized_headphone_bigram <- tokenizeBigrams(prep_headphone_brand)
tokenized_toaster_bigram <- tokenizeBigrams(prep_toaster_brand)
tokenized_cellphone_bigram <- tokenizeBigrams(prep_cellphone_brand)
tokenized_coffee_bigram <- tokenizeBigrams(prep_coffee_brand)

# COUNTING WORDS (FUNCTION FOR WITH AND WITHOUT STOPWORDS)
countBigram <- function(input) {
  input %>%
    count(bigram, sort = TRUE)
}
# Apply countBigram-function
# No count on the unfiltered data, as it would not make sense (stopwords would appear)
# Application later

# SEPARATE BIGRAMS
# to be able to filter out the stopwords
separateBigrams <- function(input) {
  input %>%
    separate(bigram, c("w1", "w2"), sep = " ")
}
# Apply separateBigrams-function
tokenized_cellphone_bigram <- separateBigrams(tokenized_cellphone_bigram)
tokenized_toaster_bigram <- separateBigrams(tokenized_toaster_bigram)
tokenized_coffee_bigram <- separateBigrams(tokenized_coffee_bigram)
tokenized_headphone_bigram <- separateBigrams(tokenized_headphone_bigram)

# REMOVE STOPWORDS FROM BIGRAMS
filterBigrams <- function(input) {
  input %>%
    filter(!w1 %in% stop_words$word) %>%
    filter(!w2 %in% stop_words$word)
}
# Apply filterBigrams-function
tokenized_headphone_bigram_filtered <- filterBigrams(tokenized_headphone_bigram)
tokenized_cellphone_bigram_filtered <- filterBigrams(tokenized_cellphone_bigram)
tokenized_coffee_bigram_filtered <- filterBigrams(tokenized_coffee_bigram)
tokenized_toaster_bigram_filtered <- filterBigrams(tokenized_toaster_bigram)

# UNITE THE WORDS / GLUE BACK TOGETHER
uniteBigrams <- function(input) {
  input %>% unite(bigram, w1, w2, sep = " ")
}
# Apply uniteBigrams-function
tokenized_headphone_bigram_united <- uniteBigrams(tokenized_headphone_bigram_filtered)
tokenized_cellphone_bigram_united <- uniteBigrams(tokenized_cellphone_bigram_filtered)
tokenized_coffee_bigram_united <- uniteBigrams(tokenized_coffee_bigram_filtered)
tokenized_toaster_bigram_united <- uniteBigrams(tokenized_toaster_bigram_filtered)

# COUNTING WITHOUT STOPWORDS
# Apply countBigram-function again
countBigramCellphone <- countBigram(tokenized_cellphone_bigram_united)
countBigramHeadphone <- countBigram(tokenized_headphone_bigram_united)
countBigramToaster <- countBigram(tokenized_toaster_bigram_united)
countBigramCoffee <- countBigram(tokenized_coffee_bigram_united)

# FILTER FOR NEGATIONS
# Unfiltered dataset has to be used (with stopwords)
filterBigramNot <- function(input) {
  input %>%
  filter(w1 == "not") %>%
  filter(!w2 %in% stop_words$word) %>%
    count(w1, w2, sort = TRUE)
}
# Apply filterBigramNot-function
filterBigramNotCellphone <- filterBigramNot(tokenized_cellphone_bigram)
filterBigramNotCoffee <- filterBigramNot(tokenized_coffee_bigram)
filterBigramNotToaster <- filterBigramNot(tokenized_toaster_bigram)
filterBigramNotHeadphone <- filterBigramNot(tokenized_headphone_bigram)

# PLOT NEGATED TERMS, PRECEDED BY A "NOT"
plotNotWords <- function(input, text, selectBrand) {
  if(selectBrand != "") { # if wanted, brand
    bigrams <- input %>% filter(brand == selectBrand)
    bigrams <- bigrams %>%
      unnest_tokens(bigram, review, token = "ngrams", n = 2)
  } else {
    bigrams <- input %>%
      unnest_tokens(bigram, review, token = "ngrams", n = 2)
  }
  category <- rep("cat", nrow(bigrams))
  bigrams$cat <- category
  bigrams %>%
    count(cat, bigram, sort = TRUE) %>%
    ungroup() %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% c("not")) %>%
    count(word1, word2, wt = n, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    mutate(contribution = score * nn) %>%
    group_by(word1) %>%
    top_n(10, abs(contribution)) %>%
    ungroup() %>%
    mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
    ggplot(aes(word2, contribution, fill = contribution > 0)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ word1, scales = "free", nrow = 3) +
      scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
      xlab("Words preceded by a negation") +
      ylab("Sentiment score * # of occurrences") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      coord_flip() +
      ggtitle(paste("Negate-Words for", text, sep = " ")) +
      theme(text = element_text(size=16), plot.title = element_text(size = 14, face = "bold"))
}
# Apply plotNotWords-function
plotNotWords(prep_headphone_brand, "Headphones", "")
plotNotWords(prep_cellphone_brand, "Cellphones", "")
plotNotWords(prep_toaster_brand, "Toasters", "")  
plotNotWords(prep_coffee_brand, "Coffee", "")
