
### TOKENIZE THE INPUT DATA
review_bigrams <- current_chunk %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2)

### COUNTING WITH STOPWORDS
review_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- review_bigrams %>%
  separate(bigram, c("w1", "w2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!w1 %in% stop_words$word) %>%
  filter(!w2 %in% stop_words$word)

### COUNTING WITHOUT STOPWORDS
bigram_counts <- bigrams_filtered %>% 
  count(w1, w2, sort = TRUE)

### FILTER FOR BATTERY LIFE E.G
bigrams_filtered %>%
  filter(w1 == "battery" & w2 == "life") %>%
  count(categories.0.2, w1, sort = TRUE)

### GLUE BACK TOGETHER
bigrams_united <- bigrams_filtered %>%
  unite(bigram, w1, w2, sep = " ")


### TF IDF OF BIGRAMS
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf