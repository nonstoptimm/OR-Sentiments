# DATA PREPARATION
# dataPrep.R
# Load required packages
library(dplyr)

# PREPARE COLUMNS OF THE RAW REVIEW DATA
prepColumns <- function(input) {
  # Combine two columns
  input$review <- paste(input$summary,input$reviewText, sep=" ")
  # Delete old column
  input$reviewText <- input$summary <- NULL
  # Character encoding
  input$review <- iconv(input$review, "latin1", "ASCII", sub="")
  # Define reviewTime-column as da date
  input$reviewTime <- as.Date(input$reviewTime, format = '%m %d, %Y')
  # Define overall as integer
  input$overall <- as.integer(input$overall)
  # Substitute "&amp;" in Product Title
  input$title <- gsub("\\&amp;", " ", input$title)
  # Substitute "&amp;" in brand names
  input$brand <- gsub("\\&amp;", " ", input$brand)
  # Create ID for each document, combining asin and reviewerID
  input$document <- paste(input$asin, input$reviewerID, sep = "-") 
  return(input)
}
# Apply prepColumns-function
raw_cellphone <- prepColumns(raw_cellphone)
raw_electronics <- prepColumns(raw_electronics)
raw_homekitchen <- prepColumns(raw_homekitchen)

# REMOVE COLUMNS OF THE REVIEW DATA
# Function to cut the unnecessary review columns
cutReviewColumns <- function(input) { 
  input[, c("asin", "overall", "reviewText", "reviewTime", "reviewerID", "summary")]
}
# Apply cutReviewColumns-function to raw review datasets
raw_cellphone <- cutReviewColumns(raw_cellphone)
raw_electronics <- cutReviewColumns(raw_electronics)
raw_homekitchen <- cutReviewColumns(raw_homekitchen)

# REMOVE COLUMNS OF THE META DATA
cutMetaColumns <- function(input) {
  input[, c("asin", "categories.0.0", "categories.0.1", "categories.0.2", "categories.0.3", "description", "title", "price", "categories.0.4", "brand")]
}
# Apply cutMetaColumns-function to raw meta datasets
meta_cellphone <- cutMetaColumns(meta_cellphone)
meta_headphones <- cutMetaColumns(meta_headphones)
meta_homekitchen <- cutMetaColumns(meta_homekitchen)

# ROUND AND ADD SENTIMENT SCORE FROM THE DEEP LEARNING MODEL
addSentiScoreNN <- function(sentiment) {
  scoreRounded <- round(sentiment$Score, 5)
  return(scoreRounded)
}
# Apply addSentiScoreNN-function
prep_coffee_brand$scoreNN <- addSentiScoreNN(score_coffee)
prep_toaster_brand$scoreNN <- addSentiScoreNN(score_toaster)
prep_cellphone_brand$scoreNN <- addSentiScoreNN(score_cellphone)
prep_headphone_brand$scoreNN <- addSentiScoreNN(score_headphone)

# ROUND AND ADD SENTIMENT SCORE FROM THE LEXICON ANALYSIS
addSentiScoreLX <- function(input, sentiment) {
  left_join(input, sentiment, by = "document")
}
# Apply addSentiScoreLX-function
prep_headphone_brand <- addSentiScoreLX(prep_headphone_brand, sentimentReviewHeadphone)
prep_cellphone_brand <- addSentiScoreLX(prep_cellphone_brand, sentimentReviewCellphone)
prep_coffee_brand <- addSentiScoreLX(prep_coffee_brand, sentimentReviewCoffee)
prep_toaster_brand <- addSentiScoreLX(prep_toaster_brand, sentimentReviewToaster)

# ONLY KEEP THE ONES WITH A VALUE FOR "WORDS"
filterScore <- function(input){
  input %>%
    filter(!scoreLX == "")
}
# Apply filterScore-function
prep_headphone_brand1 <- filterScore(prep_headphone_brand)
prep_cellphone_brand1 <- filterScore(prep_cellphone_brand)
prep_coffee_brand1 <- filterScore(prep_coffee_brand)
prep_toaster_brand1 <- filterScore(prep_toaster_brand)

# CONVERT DATA INTO TIBBLE FORMAT
# Make as_tibble, otherwise they can't be proceeded by the tidy tokenizer
prep_headphone_brand <- as_tibble(prep_headphone_brand)
prep_cellphone_brand <- as_tibble(prep_cellphone_brand)
prep_toaster_brand <- as_tibble(prep_toaster_brand)
prep_coffee_brand <- as_tibble(prep_coffee_brand)
