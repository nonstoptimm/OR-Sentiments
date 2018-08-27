# dataPrep.R
library(dplyr)
# PREPARE COLUMNS OF THE RAW REVIEW DATA
prepColumns <- function(input) {
  # Combine two columns
  input$review <- paste(input$summary,input$reviewText, sep=" ")
  # Delete old column
  input$reviewText <- input$summary <- NULL
  # Character Encoding
  input$review <- iconv(input$review, "latin1", "ASCII", sub="")
  # Define reviewTime-column as da date
  input$reviewTime <- as.Date(input$reviewTime, format = '%m %d, %Y')
  # Define overall as integer
  input$overall <- as.integer(input$overall)
  # Substitute "&amp;" in Brand Names
  input$title <- gsub("\\&amp;", " ", input$title)
  # Create ID for each document, combining asin and reviewerID
  input$document <- paste(input$asin, input$reviewerID, sep = "-") 
  return(input)
}
# Apply prepColumns
raw_cellphone <- prepColumns(raw_cellphone)
raw_electronics <- prepColumns(raw_electronics)
raw_homekitchen <- prepColumns(raw_homekitchen)

# REMOVE COLUMNS OF THE REVIEW DATA
# Function to cut the unnecessary review columns
cutReviewColumns <- function(input) { 
  input[, c("asin", "overall", "reviewText", "reviewTime", "reviewerID", "summary")]
}
# Apply to raw review datasets
raw_cellphone <- cutReviewColumns(raw_cellphone)
raw_electronics <- cutReviewColumns(raw_electronics)
raw_homekitchen <- cutReviewColumns(raw_homekitchen)

# REMOVE COLUMNS OF THE META DATA
cutMetaColumns <- function(input) {
  input[, c("asin", "categories.0.0", "categories.0.1", "categories.0.2", "categories.0.3", "description", "title", "price", "categories.0.4", "brand")]
}
# Apply to raw meta datasets
meta_cellphone <- cutMetaColumns(meta_cellphone)
meta_headphones <- cutMetaColumns(meta_headphones)
meta_homekitchen <- cutMetaColumns(meta_homekitchen)

# ROUND AND ADD SENTIMENT SCORE FROM THE DEEP LEARNING MODEL
addSentiScore <- function(sentiment) {
  scoreRounded <- round(sentiment$Score, 5)
  return(scoreRounded)
}
# Apply the function to the dataset
# Add NN-Sentiment Score to the dataset
prep_coffee_brand$scoreNN <- addSentiScore(score_coffee)
prep_toaster_brand$scoreNN <- addSentiScore(score_toaster)
prep_cellphone_brand$scoreNN <- addSentiScore(score_cellphone)
prep_headphone_brand$scoreNN <- addSentiScore(score_headphone)

# CONVERT DATA INTO TIBBLE FORMAT
# Make as tibble, otherwise they can't be proceeded by the tidy tokenizer
makeTibble <- function(input){
  as_tibble(input)
}
prep_headphone_brand <- makeTibble(prep_headphone_brand)
prep_cellphone_brand <- makeTibble(prep_cellphone_brand)
prep_toaster_brand <- makeTibble(prep_toaster_brand)
prep_coffee_brand <- makeTibble(prep_coffee_brand)
