### Re-Transforming the Word-List to enable an evaluation
current_chunk$word <- unlist(current_chunk$word)
current_chunk$title <- unlist(current_chunk$title)
current_chunk <- as.tibble(current_chunk)
headphones_beats <- as.data.table(headphones_beats)
headphones_beats <- as.tibble(headphones_beats)
headphones_sennheiser <- as.data.table(headphones_sennheiser)
headphones_sennheiser <- as.tibble(headphones_sennheiser)

### Text Preprocessing - Cleaning the raw data
current_chunk_cleaned <- current_chunk %>%
  unnest_tokens(word, reviewText) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

### Text Preprocessing - Cleaning the raw data
tidy_beats <- headphones_beats %>%
  unnest_tokens(word, reviewText) %>%
  anti_join(stop_words)

tidy_sennheiser <- headphones_sennheiser %>%
  unnest_tokens(word, reviewText) %>%
  anti_join(stop_words)

tidy_beats %>%
  count(word, sort = TRUE)

tidy_sennheiser %>%
  count(word, sort = TRUE) 

tidy_beats_count <- tidy_beats %>%
  count(word, sort = TRUE)

tidy_sennheiser_count <- tidy_sennheiser %>%
  count(word, sort = TRUE) 

wordcloud(tidy_sennheiser_count$word, tidy_sennheiser_count$n, max.words = 70, color = "blue")
wordcloud(tidy_beats_count$word, tidy_beats_count$n, max.words = 70, color = "blue")


### SENTI HERE
sennheiser_word_counts <- tidy_sennheiser %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

beats_word_counts <- tidy_sennheiser %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


sennheiser_word_counts %>%
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

beats_word_counts %>%
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




### Re-Transforming the Word-List to enable an evaluation
current_chunk_cleaned$word <- unlist(current_chunk_cleaned$word)
current_chunk_cleaned <- as.tibble(current_chunk_cleaned)


### Count Words
current_chunk_cleaned %>%
  count(word, sort = TRUE)

tidy_beats %>%
  count(word, sort = TRUE)

chunk_by_subcat <- current_chunk_cleaned %>% 
  count(categories.0.2, word, sort = TRUE) %>%
  ungroup()

### CALCULATE IDF
tf_idf <- chunk_by_subcat %>%
  bind_tf_idf(word, categories.0.2, n) %>%
  arrange(desc(tf_idf))

### PLOT IDT
tf_idf %>%
  filter(categories.0.2 %in% c("Computer Components", "Tablets")) %>%
  group_by(categories.0.2) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = categories.0.2)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ categories.0.2, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

### PLOT IDT BIGRAM
bigram_tf_idf %>%
  filter(categories.0.2 %in% c("Computer Components", "Tablets")) %>%
  group_by(categories.0.2) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = categories.0.2)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ categories.0.2, scales = "free") +
  ylab("tf-idf") +
  coord_flip()


### CORRELATION BETWEEN SUBGROUPS
subcat_cors <- chunk_by_subcat %>%
  pairwise_cor(categories.0.2, word, n, sort = TRUE)


### NEGATED WORDS
bigrams_separated %>%
  filter(w1 == "dont") %>%
  count(w1, w2, sort = TRUE)

not_words <- bigrams_separated %>%
  filter(w1 == "not") %>%
  inner_join(AFINN, by = c(w2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()


