# SENTIMENT ANALYSIS ON ONLINE REVIEWS #
# dataImport.R
# LOAD PACKAGES
library(data.table)
libraries <- c("dplyr", "tibble", "ndjson", "tm", "dplyr", "tidytext", "ggplot2", "qdap", "wordcloud", "textmineR", "lubridate", "widyr", "ggraph", "igraph")
lapply(libraries, require, character.only = TRUE)

# IMPORT THE RAW DATA ###
raw_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_electronics_reviews.json")
meta_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_electronics_meta.json")
raw_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_cellphones_reviews.json")
meta_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_cellphones_meta.json")
raw_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_homekitchen_reviews.json")
meta_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/raw_homekitchen_meta.json")

# IMPORT CONTRACTION LIST
contraction_list <- read.csv("input-data/contractions.csv", header = TRUE, sep = ";")

# IMPORT INDIVIDUAL STOPWORDS
stopword_list <- read.csv("input-data/stopwords.csv", header = TRUE, sep = ";")

# IMPORT THE SENTIMENT SCORES FROM DEEP LEARNING MODEL ###
score_coffee <- read.csv("input-data/score_coffee.csv", header = TRUE, sep = ";")
score_toast <- read.csv("input-data/score_toast.csv", header = TRUE, sep = ";")
score_headphone <- read.csv("input-data/score_headphone.csv", header = TRUE, sep = ";")
score_cellphone <- read.csv("input-data/score_cellphone.csv", header = TRUE, sep = ";")

# IMPORT PREPARED DATASET
prep_cellphone_brand <- fread("/Volumes/OMEGA/Dataset/prepared_data/prep_cellphone_brand.csv")
prep_coffee_brand <- fread("/Volumes/OMEGA/Dataset/prepared_data/prep_coffee_brand.csv")
prep_toaster_brand  <- fread("/Volumes/OMEGA/Dataset/prepared_data/prep_toaster_brand.csv")
prep_headphone_brand  <- fread("/Volumes/OMEGA/Dataset/prepared_data/prep_headphone_brand.csv")
prep_headphone_brand  <- fread("input-data/prep_headphone_brand.csv")


# IMPORT THE CREATED TOPIC MODELS
LDA_reviews_cellphone <- readRDS("output/LDA_reviews_cellphone.rds")
LDA_reviews_apple <- readRDS("output/LDA_reviews_apple.rds")
LDA_reviews_coffee <- readRDS("output/LDA_reviews_coffee.rds")
LDA_reviews_toaster <- readRDS("output/LDA_reviews_toaster.rds")
LDA_reviews_headphone <- readRDS("output/LDA_reviews_headphone.rds")

# IMPORT THE DATA
xgb_fit <- readRDS("output/xgb_fit1.rds")

# IMPORT THE PREPARED DATA
merged_coffee$title <- gsub("\\&amp;", "%", merged_coffee$title)

# CLEANUP - this command was sometimes really necessary as we're dealing with a huge amount of data
rm(list=ls())
