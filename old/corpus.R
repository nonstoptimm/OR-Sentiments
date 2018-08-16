# EHER WENIGER
# # CORPUS CREATION
# createCorpus <- function(input) {
#   corpus <- VCorpus(VectorSource(input))
#   return(corpus)
# }
# # Apply createCorpus to the duplocated dataset
# corpus_toaster <- createCorpus(dtm_toaster_brand$review)
# corpus_cellphone <- createCorpus(dtm_cellphone_brand$review)
# corpus_headphone <- createCorpus(dtm_headphone_brand$review)
# corpus_coffee <- createCorpus(dtm_coffee_brand$review)
# 
# 
# # CORPUS CLEANING FUNCTION
# clean_corpus <- function(corpus){
#   corpus <- tm_map(corpus, content_transformer(stripWhitespace))
#   #corpus <- tm_map(corpus, replace_symbol)
#   corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
#   corpus <- tm_map(corpus, PlainTextDocument)
#   #corpus <- tm_map(corpus, content_transformer(removePunctuation))
#   #corpus <- tm_map(corpus, replace_contraction)
#   #corpus <- tm_map(corpus, content_transformer(tolower))
#   #corpus <- tm_map(corpus, content_transformer(stemDocument), language = "english")
#   return(corpus)
# }
# 
# # Apply clean_corpus Function
# clean_corpus_toaster <- clean_corpus(corpus_toaster)
# clean_corpus_coffee <- clean_corpus(corpus_coffee)
# clean_corpus_cellphone <- clean_corpus(corpus_cellphone)
# #clean_corpus_headphone <- clean_corpus(corpus_headphone)
# 
# # CREATE DOCUMENT TERM MATRIX
# createDTM <- function(input) {
#   input <- DocumentTermMatrix(input) 
#   return(input)
# }
# # Apply createDTM Function
# dtm_toaster <- createDTM(clean_corpus_toaster) 
# dtm_cellphone <- createDTM(clean_corpus_cellphone)
# dtm_coffee <- createDTM(clean_corpus_coffee)
# #dtm_headphone <- createDTM(clean_corpus_headphone)
# 
# # CREATE LDA MODEL
# createLDA <- function(input) {
#   # Find the sum of words in each Document to detect if there are empty ones
#   rowTotals <- apply(input , 1, sum) 
#   input <- input[rowTotals> 0, ]
#   # Set parameters for LDA
#   burnin <- 4000
#   iter <- 2000
#   thin <- 500
#   seed <-list(2003,5,63,100001,765)
#   nstart <- 5
#   best <- TRUE
#   # Number of topics
#   k <- 5
#   # Create LDA model using Gibbs sampling
#   ldaOut <-LDA(input,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#   return(ldaOut)
# }
# # Apply createLDA Function to input data
# lda_toaster <- createLDA(dtm_toaster)
# lda_cellphone <- createLDA(dtm_cellphone)
# lda_coffee <- createLDA(dtm_coffee)
# #lda_headphone <- createLDA(dtm_headphone)
# 
# # WORD TOPIC PROBABILITIES
# topic_wtp <- function(input) {
#   tidy(input, matrix = "beta")
# }
# # Apply topic_wtp Function (LDA-Model as input)
# topic_toaster_wtp <- topic_wtp(lda_toaster)
# 
# # TOP TERMS OF WORD TOPIC PROBABILITIES
# topic_wtp_topterms <- function(input) {
#   input %>%
#     group_by(topic) %>%
#     top_n(10, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta)
# }
# # Apply topic_wtp_topterms to WTP dataset
# topic_toaster_wtp_topterms <- topic_wtp_topterms(topic_toaster_wtp)
# 
# # PLOT TOP TERMS FOR EACH TOPIC
# topic_plot_wtp_topterms <- function(input) {
#   input %>%
#     mutate(term = reorder(term, beta)) %>%
#     ggplot(aes(term, beta, fill = factor(topic))) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~ topic, scales = "free") +
#     coord_flip()
# }
# # Apply topic_plot_wtp_topterms Function to topterm dataset
# topic_plot_wtp_topterms(topic_toaster_wtp_topterms)
# 
# # SPREAD BETAS
# # topic_wtp_beta_spread <- 
# #   input %>%
# #     mutate(topic = paste0("topic", topic)) %>%
# #     spread(topic, beta) %>%
# #     filter(topic1 > .001 | topic2 > .001) %>%
# #     mutate(log_ratio = log2(topic2 / topic1))
# # 
# # topic_wtp_beta_spread(topic_toaster_wtp)
# 
# # DOCUMENT TOPIC PROBABILITIES
# topic_atp <- function(input) {
#   tidy(input, matrix = "gamma")
# }
# # Apply topic_atp to LDA model
# topic_toaster_atp <- topic_atp(lda_toaster)
# 
# 
# tidy(AssociatedPress) %>%
#   filter(document == 6) %>%
#   arrange(desc(count))
# 
# ap_documents <- tidy(ap_lda, matrix = "gamma")
# ap_documents
# 
# findFreqTerms(dtm_toaster, 2)
# findAssocs(dtm_toaster, 'my', 0.20)
# df_cellphone <- as.data.frame(inspect(dtm_cellphone))
# df_scale_cellphone <- scale(df_cellphone)
# df_distance <- dist(df_scale_cellphone,method="euclidean")
# df_fit <- hclust(df_distance, method="ward")
# plot(df_fit)
# 
# 
# Review_DTM <- DocumentTermMatrix(clean_corp, control = list(wordLengths = c(2, Inf)))
# inspect(Review_DTM[1:20,1:20])
# DTM <- removeSparseTerms(Review_DTM , 0.990) 
# findFreqTerms(Review_DTM, 3000)
# k<-7
# library(topicmodels)
# 
# control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = T, 
#                           verbose = 0, prefix = tempfile(), 
#                           save = 0, 
#                           keep = 50, 
#                           seed = 980,
#                           nstart = 1, best = T,
#                           delta = 0.1,
#                           iter = 2000, 
#                           burnin = 100, 
#                           thin = 2000) 
# 
# my_first_topic_model <- LDA(Review_DTM, k, method = "Gibbs", control = control_LDA_Gibbs)
# terms(my_first_topic_model, 30)
# 
# # Store as Document Term Matrix
# review_dtm <- DocumentTermMatrix(clean_corp)
# # Store as Term Document Matrix (for the word cloud)
# review_tdm <- TermDocumentMatrix(clean_corp)
# 
# # Store as Matrix
# review_dtm_m <- as.matrix(review_dtm)
# review_tdm_m <- as.matrix(review_tdm)
# 
# # RowSums für the Word Cloud
# review_words <- rowSums(review_tdm_m)
# review_words <- sort(review_words, decreasing = TRUE)
# # Dataframe for the Word Cloud
# review_freqs <- data.frame(term = names(review_words), num = review_words)
# # Create the Wordcloud
# wordcloud(review_freqs$term, review_freqs$num, max.words = 70, color = "blue")
# 
# # Definition of Tokenizer-Function
# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# 
# # Store as TOKENIZED Document Term Matrix
# bigram_dtm <- DocumentTermMatrix(clean_corp, control = list(tokenize = tokenizer))
# bigram_dtm_m <- as.matrix(bigram_dtm)
# freq <- colSums(bigram_dtm_m)
# bi_words <- names(freq)
# wordcloud(bi_words, freq, max.words=15)