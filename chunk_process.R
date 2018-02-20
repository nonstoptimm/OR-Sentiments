### Re-Transforming the Word-List to enable an evaluation
current_chunk$word <- unlist(current_chunk$word)
current_chunk <- as.tibble(current_chunk)


### Text Preprocessing - Cleaning the raw data
current_chunk_cleaned <- current_chunk %>%
  unnest_tokens(word, reviewText) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

### Re-Transforming the Word-List to enable an evaluation
current_chunk_cleaned$word <- unlist(current_chunk_cleaned$word)
current_chunk_cleaned <- as.tibble(current_chunk_cleaned)

### Count Words
current_chunk_cleaned %>%
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

### CORRENALTION BETWEEN SUBGROUPS
subcat_cors <- chunk_by_subcat %>%
  pairwise_cor(categories.0.2, word, n, sort = TRUE)


