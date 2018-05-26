# SENTIMENT ANALYSIS ON ONLINE REVIEWS #

### LOAD PACKAGES
libraries <- c("dplyr", "tibble", "ndjson", "tm", "dplyr", "tidytext", "ggplot2", "qdap", "wordcloud", "textmineR", "lubridate", "widyr", "ggraph", "igraph")
lapply(libraries, require, character.only = TRUE)

### IMPORT THE RAW DATA ###
raw_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_electronics_reviews.json")
meta_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_electronics_meta.json")
raw_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_cellphones_reviews.json")
meta_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_cellphones_meta.json")
raw_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_homekitchen_reviews.json")
meta_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_homekitchen_meta.json")

### IMPORT THE SENTIMENT SCORES FROM DEEP LEARNING MODEL ###
score_coffee <- read.csv("input-data/score_coffee.csv", header = TRUE, sep = ";")
score_toast <- read.csv("input-data/score_toast.csv", header = TRUE, sep = ";")
score_headphone <- read.csv("input-data/score_headphone.csv", header = TRUE, sep = ";")
score_cellphone <- read.csv("input-data/score_cellphone.csv", header = TRUE, sep = ";")

### IMPORT CONTRACTION LIST
contraction_list <- read.csv("input-data/contractions.csv", header = TRUE, sep = ";")

### ONLY KEEP THE REQUIRED COLUMNS
# Function to cut the unnecessary review columns
cutReviewColumns <- function(input) { 
  input[, c("asin", "overall", "reviewText", "reviewTime", "reviewerID", "summary")]
}
# Apply to raw datasets
raw_cellphone <- cutReviewColumns(raw_cellphone)
raw_electronics <- cutReviewColumns(raw_electronics)
raw_homekitchen <- cutReviewColumns(raw_homekitchen)

# Function to cut the unnecessary metadata columns
cutMetaColumns <- function(input) {
  input[, c("asin", "categories.0.0", "categories.0.1", "categories.0.2", "categories.0.3", "description", "title", "price", "categories.0.4", "brand")]
}
# Apply to raw datasets
meta_cellphone <- cutMetaColumns(meta_cellphone)
meta_headphones <- cutMetaColumns(meta_headphones)
meta_homekitchen <- cutMetaColumns(meta_homekitchen)

### MERGE THE "OVERALL" and "REVIEWTEXT" COLUMN
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
  return(input)
}
# Apply prepColumns
raw_cellphone <- prepColumns(raw_cellphone)
raw_headphones <- prepColumns(raw_headphones)
raw_homekitchen <- prepColumns(raw_homekitchen)

merged_coffee$title <- gsub("\\&amp;", "%", merged_coffee$title)

### CLEANUP
rm(list=ls())
