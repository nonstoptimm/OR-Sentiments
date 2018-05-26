# HISTOGRAMS ON EVALUATION
plotHistogram <- function(input, title) {
  hist(input$overall, main = paste("Histogram of", title, sep = " "))
}
# Apply histogram to datasets
plotHistogram(prep_coffee_brand, "Coffee Reviews")
plotHistogram(prep_toaster_brand, "Toaster Reviews")
plotHistogram(prep_cellphone_brand, "Cellphone Reviews")
#plotHistogram(prep_headphone_brand, "Headphone Reviews")

### TOKENIZE THE INPUT DATA
tokenizeBigram <- function(input) {
  input %>% unnest_tokens(bigram, review, token = "ngrams", n = 2)
}
# Apply it to text data
tokenized_toaster_bigram <- tokenizeBigram(prep_toaster_brand)
# tokenized_headphone_bigram <- tokenizeBigram(prep_headphone_brand)
tokenized_cellphone_bigram <- tokenizeBigram(prep_cellphone_brand)
tokenized_coffee_bigram <- tokenizeBigram(prep_coffee_brand)

### COUNTING WORDS (FUNCTION FOR WITH AND WITHOUT STOPWORDS)
countBigram <- function(input) {
  input %>%
    count(bigram, sort = TRUE)
}
# Apply Bigram Counter
countBigram(tokenized_cellphone_bigram)
# countBigram(tokenized_headphone_bigram)
countBigram(tokenized_coffee_bigram)
countBigram(tokenized_toaster_bigram)

### SEPARATE BIGRAMS
separateBigrams <- function(input) {
  input %>%
    separate(bigram, c("w1", "w2"), sep = " ")
}
# Apply Bigram Separator
tokenized_cellphone_bigram <- separateBigrams(tokenized_cellphone_bigram)
tokenized_toaster_bigram <- separateBigrams(tokenized_toaster_bigram)
tokenized_coffee_bigram <- separateBigrams(tokenized_coffee_bigram)
#tokenized_headphone_bigram <- separateBigrams(tokenized_headphone_bigram)

### FILTER BIGRAMS
filterBigrams <- function(input) {
  input %>%
    filter(!w1 %in% stop_words$word) %>%
    filter(!w2 %in% stop_words$word)
}
# Apply Bigram Filter
tokenized_cellphone_bigram_filtered <- filterBigrams(tokenized_cellphone_bigram)
tokenized_coffee_bigram_filtered <- filterBigrams(tokenized_coffee_bigram)
tokenized_toaster_bigram_filtered <- filterBigrams(tokenized_toaster_bigram)
#tokenized_headphone_bigram_filtered <- filterBigrams(tokenized_headphone_bigram)

#### UNITE THE WORDS / GLUE BACK TOGETHER
uniteBigrams <- function(input) {
  input %>% unite(bigram, w1, w2, sep = " ")
}
# Apply Glue
tokenized_cellphone_bigram_united <- uniteBigrams(tokenized_cellphone_bigram_filtered)
#tokenized_headphone_bigram_united <- uniteBigrams(tokenized_headphone_bigram_filtered)
tokenized_coffee_bigram_united <- uniteBigrams(tokenized_coffee_bigram_filtered)
tokenized_toaster_bigram_united <- uniteBigrams(tokenized_toaster_bigram_filtered)

### COUNTING WITHOUT STOPWORDS
# just apply countBigram again
countBigramCellphone <- countBigram(tokenized_cellphone_bigram_united)
# countBigramHeadphone <- CountBigram(tokenized_headphone_bigram_united)
countBigramToaster <-countBigram(tokenized_toaster_bigram_united)
countBigramCoffee <- countBigram(tokenized_coffee_bigram_united)

### FILTER BIGRAM FOR BATTERY LIFE E.G
filterBigramWord <- function(input, token1, token2) {
  input %>%
  filter(w1 == token1 & w2 == token2) %>%
    count(w1, w2, sort = TRUE)
}
# Apply manual Bigram Filter
filterBigramWord(tokenized_cellphone_bigram, "poor", "performance")

### FILTER FOR NEGATIONS
filterBigramNot <- function(input) {
  input %>%
  filter(w1 == "not") %>%
  filter(!w2 %in% stop_words$word) %>%
    count(w1, w2, sort = TRUE)
}
# Apply filter, unfiltered dataset has to be used (with stopwords)
filterBigramNotCellphone <- filterBigramNot(tokenized_cellphone_bigram)
filterBigramNotCoffee <- filterBigramNot(tokenized_coffee_bigram)
filterBigramNotToaster <- filterBigramNot(tokenized_toaster_bigram)
# filterBigramNotHeadphone <- filterBigramNot(tokenized_headphone_bigram)

## SENTIMENT FOR NOT-WORDS
sentimentNotWords <- function(input) {
  input %>%
  filter(w1 == "not") %>%
    inner_join(get_sentiments("bing"), by = c(w2 = "word")) %>%
    count(w2, score, sort = TRUE) %>%
    ungroup()
}
# Apply sentimentNotWords Function
sentimentNotWords(filterBigramNot(tokenized_cellphone_bigram))

### PLOT THE NEGATE-WORDS
tokenized_bigram_counts <- function(input, selectBrand) {
  if(selectBrand != "") {
    cellphone_bigrams <- input %>% filter(brand == selectBrand)
    cellphone_bigrams <- cellphone_bigrams %>%
      unnest_tokens(bigram, review, token = "ngrams", n = 2)
  } else {
    cellphone_bigrams <- input %>%
      unnest_tokens(bigram, review, token = "ngrams", n = 2)
  }
  cellphone_bigrams %>%
  count(categories.0.2, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")
}
# Apply tokenized_bigram_counts
tokenized_bigram_counts_cellphone <- tokenized_bigram_counts(prep_cellphone_brand, "")
# tokenized_bigram_counts_headphone <- tokenized_bigram_counts(prep_headphone_brand)
tokenized_bigram_counts_coffee <- tokenized_bigram_counts(prep_coffee_brand, "")
tokenized_bigram_counts_toaster <- tokenized_bigram_counts(prep_toaster_brand, "")

# PLOT NEGATE WORDS
plotNotWords <- function(input, text) {
  input %>%
    filter(word1 %in% c("not", "without", "no", "poor")) %>%
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
    ggtitle(paste("Negate-Words for", text, sep = " "))
}
# Apply it to the datasets
#plotNotWords(tokenized_bigram_counts(merged_topic_headphone, ""), "Headphones")
plotNotWords(tokenized_bigram_counts(merged_topic_cellphone, ""), "Cellphones")
plotNotWords(tokenized_bigram_counts(merged_topic_cellphone, "samsung"), "Cellphones, Brand 'Samsung'")
plotNotWords(tokenized_bigram_counts(merged_topic_cellphone, "apple"), "Cellphones, Brand 'Apple'")
plotNotWords(tokenized_bigram_counts(merged_topic_toaster, ""), "Toaster")  
plotNotWords(tokenized_bigram_counts(merged_topic_coffee, ""), "Coffee")  
