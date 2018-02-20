
# Subset the entire dataset to create an easier-to-handle data chunk
# rawdata_subset <- rawdata[1:1000, ]


# Create Corpus from Vector Source
review_corpus <- VCorpus(VectorSource(rawdata_subset$reviewText))

# Function for Corpus Cleaning
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

# Clean Corpus with "clean_corpus"-Function 
clean_corp <- clean_corpus(review_corpus)

# Store as Document Term Matrix
review_dtm <- DocumentTermMatrix(clean_corp)
# Store as Term Document Matrix (for the word cloud)
review_tdm <- TermDocumentMatrix(clean_corp)

# Store as Matrix
review_dtm_m <- as.matrix(review_dtm)
review_tdm_m <- as.matrix(review_tdm)

# RowSums für the Word Cloud
review_words <- rowSums(review_tdm_m)
review_words <- sort(review_words, decreasing = TRUE)
# Dataframe for the Word Cloud
review_freqs <- data.frame(term = names(review_words), num = review_words)
# Create the Wordcloud
wordcloud(review_freqs$term, review_freqs$num, max.words = 70, color = "blue")

# Definition of Tokenizer-Function
tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Store as TOKENIZED Document Term Matrix
bigram_dtm <- DocumentTermMatrix(clean_corp, control = list(tokenize = tokenizer))
bigram_dtm_m <- as.matrix(bigram_dtm)
freq <- colSums(bigram_dtm_m)
bi_words <- names(freq)
wordcloud(bi_words, freq, max.words=15)