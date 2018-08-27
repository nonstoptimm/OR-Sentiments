# categorizeData.R
# Load required packages
library(dplyr)

### CATEGORIZER
# Function to join data
joinData <- function(reviews, metadata) {
  merged <- inner_join(reviews, metadata, by = "asin")
  merged <- as_tibble(merged)
  return(merged)
}

## PHONES
# Filter based on sub-categories
categorizeMetaPhone <- function(input) {
  data <- input %>%
    filter(categories.0.0 == "Cell Phones & Accessories" & categories.0.1 == "Cell Phones" & categories.0.2 == "Unlocked Cell Phones")
  # Remove all category columns
  data$categories.0.0 <- data$categories.0.1 <- data$categories.0.2 <- data$categories.0.3 <- data$categories.0.4 <- NULL
  return(data)  
}
# Apply Categorizer
meta_cellphone <- categorizeMetaPhone(meta_cellphone)
# Apply inner_join
merged_cellphone <- joinData(raw_cellphone, meta_cellphone)

## HEADPHONES
categorizeMetaHeadphones <- function(input) {
  data <- input %>%
    filter(categories.0.0 == "Electronics" & categories.0.1 == "Accessories & Supplies" & categories.0.2 == "Audio & Video Accessories" & categories.0.3 == "Headphones")
  # Remove all category columns
  data$categories.0.0 <- data$categories.0.1 <- data$categories.0.2 <- data$categories.0.3 <- data$categories.0.4 <- NULL
  return(data)
}
# Apply Categorizer
meta_headphone <- categorizeMetaHeadphones(meta_electronics)
# Apply inner_join
merged_headphone <- joinData(raw_headphone, meta_headphone)

## COFFEE MACHINE
categorizeMetaCoffee <- function(input) {
  input %>%
    filter(categories.0.0 == "Home & Kitchen" & categories.0.1 == "Kitchen & Dining" & categories.0.2 == "Coffee, Tea & Espresso" & categories.0.3 == "Coffee Makers")
  # Remove all category columns
  data$categories.0.0 <- data$categories.0.1 <- data$categories.0.2 <- data$categories.0.3 <- data$categories.0.4 <- NULL
  return(data)  
}
# Apply Categorizer
meta_coffee <- categorizeMetaCoffee(meta_homekitchen)
# Apply inner_join
merged_coffee <- joinData(raw_homekitchen, meta_coffee)

## TOASTERS
categorizeMetaToaster <- function(input) {
  input %>%
    filter(categories.0.0 == "Home & Kitchen" & categories.0.1 == "Kitchen & Dining" & categories.0.2 == "Small Appliances" & categories.0.3 == "Ovens & Toasters" & categories.0.4 == "Toasters")
  # Remove all category columns
  data$categories.0.0 <- data$categories.0.1 <- data$categories.0.2 <- data$categories.0.3 <- data$categories.0.4 <- NULL
  return(data)  
}
# Apply Categorizer
meta_toaster <- categorizeMetaToaster(meta_homekitchen)
# Apply inner_join
merged_toaster <- joinData(raw_homekitchen, meta_toaster)

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
    filter(reviewLanguage == "english") %>% # filter for english reviews
    select(-reviewLanguage) # Remove the language column again, as it is no longer needed
}
# Apply deleteNotEnglish Function
merged_cellphone <- deleteNotEnglish(merged_cellphone)
merged_headphone <- deleteNotEnglish(merged_headphone)
merged_coffee <- deleteNotEnglish(merged_coffee)
merged_toaster <- deleteNotEnglish(merged_toaster)

## Branded Reviews Only
categorizeOnlyBranded <- function(input){
  input %>%
    filter(!brand == "")
}
# Apply that only reviews with a brand remain
merged_cellphone_brand <- categorizeOnlyBranded(merged_cellphone)
merged_headphone_brand <- categorizeOnlyBranded(merged_headphone)
merged_coffee_brand <- categorizeOnlyBranded(merged_coffee)
merged_toaster_brand <- categorizeOnlyBranded(merged_toaster)

# CREATE DOCUMENT ID
# To identify each document
createID <- function(input){
  document <- paste(input$asin, input$reviewerID, sep = "-") 
  return(document)
}
# Apply createID Function
merged_cellphone_brand$document <- createID(merged_cellphone_brand)
merged_toaster_brand$document <- createID(merged_toaster_brand)
merged_coffee_brand$document <- createID(merged_coffee_brand)
merged_headphone_brand$document <- createID(merged_headphone_brand)

## Add Sentiment Score to Branded Products
# They have only been calculated for branded products!
merged_cellphone_brand$scoreNN <- addSentiScore(score_cellphone)
merged_toaster_brand$scoreNN <- addSentiScore(score_toaster)
merged_coffee_brand$scoreNN <- addSentiScore(score_coffee)
merged_headphone_brand$scoreNN <- addSentiScore(score_headphone)
