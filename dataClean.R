# dataCleaning.R
# Script to clean the data
# Required Packages
library(tidyverse)
library(textstem)

# SAPPLY TOLOWER
makeLow <- function(input) {
  input$review <- sapply(input$review, tolower)
  input$brand <- sapply(input$brand, tolower)
  return(input)
}
# Apply makeLow Function
merged_cellphone_brand <- makeLow(merged_cellphone_brand)
merged_coffee_brand <- makeLow(merged_coffee_brand)
merged_headphone_brand <- makeLow(merged_headphone_brand)
merged_toaster_brand <- makeLow(merged_toaster_brand)

# DETECT REVIEW LANGUAGE
detectLanguage <- function(input) {
  langProfile <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "french", "spanish", "german", "italian", "portuguese")]
  reviewLanguage <- textcat(input, p = langProfile)
  return(reviewLanguage)
}
# Apply detectLanguage Function
merged_cellphone$reviewLanguage <- detectLanguage(merged_cellphone$review)
merged_cellphone$reviewLanguage <- detectLanguage(merged_cellphone$review)
merged_coffee$reviewLanguage <- detectLanguage(merged_coffee$review)
merged_toaster$reviewLanguage <- detectLanguage(merged_toaster$review)
  
# DELETE ALL NON-ENGLISH REVIEWS
deleteNotEnglish <- function(input) {
  input %>%
    filter(reviewLanguage == "english") %>%
    select(-reviewLanguage)
}
# Apply deleteNotEnglish Function
merged_cellphone <- deleteNotEnglish(merged_cellphone)
merged_headphone <- deleteNotEnglish(merged_headphone)
merged_coffee <- deleteNotEnglish(merged_coffee)
merged_toaster <- deleteNotEnglish(merged_toaster)

# CORRECT CONTRACTIONS
# Here we use the contraction_list imported in importData.R
correctContraction <- function(reviews, contraction_list) {
  for(pattern in 1:nrow(contraction_list)) {
    reviews <- gsub(contraction_list$contraction[pattern], contraction_list$full[pattern], reviews, ignore.case =TRUE)
  }
  return(reviews)
}
# Apply correctContraction Function
merged_cellphone_brand$review <- correctContraction(merged_cellphone_brand$review, contraction_list)
merged_coffee_brand$review <- correctContraction(merged_coffee_brand$review, contraction_list)
merged_headphone_brand$review <- correctContraction(merged_headphone_brand$review, contraction_list)
merged_toaster_brand$review <- correctContraction(merged_toaster_brand$review, contraction_list)

# CORRECT INDIVIDUAL WORDS
correctWord <- function(input, before, after) {
  reviews <- gsub(before, after, input, ignore.case =TRUE)
  return(reviews)
}
# Apply correctWord Function
prep_toaster_brand$review <- correctWord(prep_toaster_brand$review, "toaster", "toast")

# REMOVE PUNCTUATION AND STUFF
removePunctuation <- function(reviews) {
  reviews <- gsub("&#8217;", "'", reviews)
  # Dollar should not be deleted because it might be relevant regarding purchase
  reviews <- gsub("\\$", " dollar ", reviews)
  # Percent should not be deleted because it might be relevant regarding battery gauge
  reviews <- gsub("\\%", " percent ", reviews)
  reviews <- gsub("\\W", " ", reviews)
  return(reviews)
}
# Apply removePunctuation Function
merged_cellphone_brand$review <- removePunctuation(merged_cellphone_brand$review)
merged_coffee_brand$review <- removePunctuation(merged_coffee_brand$review)
merged_headphone_brand$review <- removePunctuation(merged_headphone_brand$review)
merged_toaster_brand$review <- removePunctuation(merged_toaster_brand$review)

# Word Stemming/Lemmatize
lemmatizeText <- function(input) {
  input %>% lemmatize_strings()
}
# Apply to dataset
prep_cellphone_brand$review <- lemmatizeText(prep_cellphone_brand$review)
prep_coffee_brand$review <- lemmatizeText(prep_coffee_brand$review)
prep_toaster_brand$review <- lemmatizeText(prep_toaster_brand$review)
prep_headphone_brand$review <- lemmatizeText(prep_headphone_brand$review)

# Create a copy of the dataset
prep_cellphone_brand <- merged_cellphone_brand
prep_coffee_brand <- merged_coffee_brand
prep_toaster_brand  <- merged_toaster_brand
prep_headphone_brand  <- merged_headphone_brand

# Create a copy for dtm
dtm_cellphone_brand <- prep_cellphone_brand
dtm_coffee_brand <- prep_coffee_brand
dtm_toaster_brand  <- prep_toaster_brand
dtm_headphone_brand  <- prep_headphone_brand