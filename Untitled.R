tidy_review <- current_chunk %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "holmes")
  fill()

tidy_review %>%
  count(word, sort = TRUE)

current_chunk %>% select(asin, overall, reviewText)


### CATEGORIZER
# APPLY CATEGORIZER

### CREATE A CHUNK OF THE HUGE RAWDATA
# rawdata_subset <- rawdata[1:100000, ]
# rawdata_subset <- read.csv("input-data/subset.csv")

### EXPORT THE DATA CHUNKS
# write.csv(rawdata_subset, file = "input-data/rawdata_subset.csv")
# write.csv(metadata, file = "input-data/metadata_filtered.csv")

### IMPORT THE DATA CHUNKS
# rawdata_subset <- read.csv("input-data/subset.csv")
# metadata <- read.csv("input-data/metadata_filtered.csv")


merged_branded_full %>% filter(!price)


# Create Corpus from Vector Source
review_corpus <- VCorpus(VectorSource(as.vector(merged_branded_full$review)))

# Subset the entire dataset to create an easier-to-handle data chunk
# rawdata_subset <- rawdata[1:1000, ]

# Create Corpus from Vector Source
corpus <- VCorpus(VectorSource(as.vector(current_chunk$reviewText)))

# EXPORT MERGED CELLPHONE
fwrite(merged_cellphone, "input-data/merged_cellphone.csv")
fwrite(merged_cellphone_brand, "input-data/merged_cellphone_brand.csv")
fwrite(prep_cellphone_brand, "input-data/prep_cellphone_brand.csv")
# EXPORT MERGED HEADPHONE
fwrite(merged_headphone, "input-data/merged_headphone.csv")
fwrite(merged_headphone_brand, "input-data/merged_headphone_brand.csv")
fwrite(prep_headphone_brand, "input-data/prep_headphone_brand.csv")
# EXPORT MERGED COFFEE
fwrite(merged_coffee, "input-data/merged_coffee.csv")
fwrite(merged_coffee_brand, "input-data/merged_coffee_brand.csv")
fwrite(prep_coffee_brand, "input-data/prep_coffee_brand.csv")
# EXPORT MERGED TOASTER
fwrite(merged_toaster, "input-data/merged_toaster.csv")
fwrite(merged_toaster_brand, "input-data/merged_toaster_brand.csv")
fwrite(prep_toaster_brand, "input-data/prep_toaster_brand.csv")

fwrite(prep_headphone_brand, "input-data/prep_headphone_brand.csv")
fwrite(prep_coffee_brand, "input-data/prep_coffee_brand.csv")
fwrite(prep_toaster_brand, "input-data/prep_toaster_brand.csv")
fwrite(prep_cellphone_brand, "input-data/prep_cellphone_brand.csv")

fwrite(merged_topic_cellphone, "input-data/merged_topic_cellphone.csv")
fwrite(merged_topic_samsung, "input-data/merged_topic_samsung.csv")
fwrite(merged_topic_apple, "input-data/merged_topic_apple.csv")
fwrite(merged_topic_toaster, "input-data/prep_coffee_toaster.csv")
fwrite(merged_topic_coffee, "input-data/prep_toaster_coffee.csv")

# IMPORT PREPARED DATASETS
merged_cellphone <- fread("input-data/merged_cellphone.csv", showProgress = TRUE)
merged_cellphone_brand <- fread("input-data/merged_cellphone_brand.csv", showProgress = TRUE)
prep_headphone_brand <- fread("input-data/prep_headphone_brand.csv", showProgress = TRUE)

merged_headphone <- fread("input-data/merged_headphone.csv", showProgress = TRUE)
score_headphone1 <- fread("input-data/score_headphone1.csv", showProgress = TRUE)
score_headphone2 <- fread("input-data/score_headphone2.csv", showProgress = TRUE)
score_headphone3 <- fread("input-data/score_headphone3.csv", showProgress = TRUE)
score_headphone4 <- fread("input-data/score_headphone4.csv", showProgress = TRUE)
merged_headphone_brand <- fread("input-data/merged_headphone_brand.csv", showProgress = TRUE)
merged_headphone_brand <- fread("input-data/prep_headphone_brand.csv", showProgress = TRUE)

merged_toaster <- fread("input-data/merged_toaster.csv", showProgress = TRUE)
merged_toaster_brand <- fread("input-data/merged_toaster_brand.csv", showProgress = TRUE)
merged_toaster_brand <- fread("input-data/prep_toaster_brand.csv", showProgress = TRUE)

merged_coffee <- fread("input-data/merged_coffee.csv", showProgress = TRUE)
merged_coffee_brand <- fread("input-data/merged_coffee_brand.csv", showProgress = TRUE)
merged_coffee_brand <- fread("input-data/prep_coffee_brand.csv", showProgress = TRUE)

score_cellphone <- fread("input-data/score_cellphone.csv", showProgress = TRUE)
score_toaster <- fread("input-data/score_toast.csv", showProgress = TRUE)
score_coffee <- fread("input-data/score_coffee.csv", showProgress = TRUE)
fwrite(score_headphone, "input-data/score_headphone.csv")
score_headphone <- fread("input-data/score_headphone.csv", showProgress = TRUE)


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

# sentimentDetection
# SENTIMENT CONTRIBUTION
sentiContributionsBrand <- function(input) {
  input %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
}
# Apply sentiContributionsBrand Function
sentiContributionsBrand(tokenized_headphone)
sentiContributionsBrand(tokenized_cellphone)
sentiContributionsBrand(tokenized_toaster)
sentiContributionsBrand(tokenized_coffee)

# # RELATIVE IMPORTANCE MOST IMPORTANT TOPICS
# topic1ToTopic2 <- function(topicProbabilities, dtm_input, k) {
#   output <- lapply(1:nrow(dtm_input),function(x) sort(lda_input[x,])[k]/sort(lda_input[x,])[k-1]) 
#   return(output)
# }
# 
# # RELATIVE IMPORTANCE OF SECOND AND THIRD MOST IMPORTANT TOPICS
# topic2ToTopic3 <- function(topicProbabilities, dtm_input, k) {
#   output <- lapply(1:nrow(dtm_input),function(x) sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
#   return(output)
# }

# # CREATE ID FOR TOPIC MODEL TO MERGE THE TOPIC MODEL IT LATER
# createID <- function(input){
#   document <- paste(input$asin, input$reviewerID, sep = "-") 
#   return(document)
# }
# # Apply Function
# dtm_toaster_brand$document <- createID(dtm_toaster_brand)
# dtm_cellphone_brand$document <- createID(dtm_cellphone_brand)
# dtm_coffee_brand$document <- createID(dtm_coffee_brand)
# dtm_headphone_brand$document <- createID(dtm_headphone_brand)

# PRÜFEN WAS DAS IST?
# dtmCreatorNN <- function(input) {
#   input %>%
#     cast_dtm(document, scoreNN, word, n)
# }


