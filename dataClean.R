# DATA CLEANING
# Clean raw review data
# dataCleaning.R
# Load required packages
library(dplyr)
library(tidyverse)
library(textstem)

# MAKE TEXT LOWER-CASE
makeLow <- function(input) {
  input$review <- sapply(input$review, tolower) # make review low
  input$brand <- sapply(input$brand, tolower) # make brand name low
  return(input)
}
# Apply makeLow-function
merged_cellphone_brand <- makeLow(merged_cellphone_brand)
merged_headphone_brand <- makeLow(merged_headphone_brand)
merged_toaster_brand <- makeLow(merged_toaster_brand)
merged_coffee_brand <- makeLow(merged_coffee_brand)

# CORRECT CONTRACTIONS
# Here we use the contraction_list imported in dataImport.R
correctContraction <- function(reviews, contraction_list) {
  # loop through imported contraction list from top to bottom
  for(pattern in 1:nrow(contraction_list)) {
    # in every iteration, the respective term gets substituted
    reviews <- gsub(contraction_list$contraction[pattern], contraction_list$full[pattern], reviews, ignore.case =TRUE)
  }
  return(reviews)
}
# Apply correctContraction-function
merged_cellphone_brand$review <- correctContraction(merged_cellphone_brand$review, contraction_list)
merged_headphone_brand$review <- correctContraction(merged_headphone_brand$review, contraction_list)
merged_toaster_brand$review <- correctContraction(merged_toaster_brand$review, contraction_list)
merged_coffee_brand$review <- correctContraction(merged_coffee_brand$review, contraction_list)

# CORRECT INDIVIDUAL WORDS
# Apply when necessary
correctWord <- function(input, before, after) {
  # individual words mentioned in the function input are substituted
  reviews <- gsub(before, after, input, ignore.case =TRUE)
  return(reviews)
}
# Apply correctWord-function
prep_toaster_brand$review <- correctWord(prep_toaster_brand$review, "toaster", "toast")

# REMOVE PUNCTUATION AND CORRECT FURTHER SIGNS
removePunctuation <- function(reviews) {
  # Substitute colon to a standardized one
  reviews <- gsub("&#8217;", "'", reviews)
  # Dollar should not be deleted because it might be relevant regarding purchase
  reviews <- gsub("\\$", " dollar ", reviews)
  # Percent should not be deleted because it might be relevant regarding battery gauge
  reviews <- gsub("\\%", " percent ", reviews)
  reviews <- gsub("\\W", " ", reviews)
  return(reviews)
}
# Apply removePunctuation-function
merged_cellphone_brand$review <- removePunctuation(merged_cellphone_brand$review)
merged_headphone_brand$review <- removePunctuation(merged_headphone_brand$review)
merged_toaster_brand$review <- removePunctuation(merged_toaster_brand$review)
merged_coffee_brand$review <- removePunctuation(merged_coffee_brand$review)

# SUBSTITUTE "&" IN TITLE AND BRAND
correctAnd <- function(input) {
  input <- gsub("\\&amp;", "&", input)
  return(input)
}
# Apply correctAnd-function
merged_cellphone_brand$title <- gsub("\\&amp;", "&", merged_cellphone_brand$title)
merged_headphone_brand$brand <- gsub("\\&amp;", "&", merged_headphone_brand$brand)
merged_toaster_brand$title <- gsub("\\&amp;", "&", merged_toaster_brand$title)
merged_coffee_brand$brand <- gsub("\\&amp;", "&", merged_coffee_brand$brand)

# WORD LEMMATIZATION
lemmatizeText <- function(input) {
  input %>% 
    lemmatize_strings() # words are transformed to their origin
}
# Apply lemmatizeText-function
prep_cellphone_brand$review <- lemmatizeText(prep_cellphone_brand$review)
prep_coffee_brand$review <- lemmatizeText(prep_coffee_brand$review)
prep_toaster_brand$review <- lemmatizeText(prep_toaster_brand$review)
prep_headphone_brand$review <- lemmatizeText(prep_headphone_brand$review)