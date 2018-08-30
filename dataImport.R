# SENTIMENT ANALYSIS ON ONLINE REVIEWS #
# dataImport.R
# LOAD PACKAGES
library(data.table)
library(dplyr)
libraries <- c("dplyr", "tibble", "ndjson", "tm", "dplyr", "tidytext", "ggplot2", "qdap", "wordcloud", "textmineR", "lubridate", "widyr", "ggraph", "igraph")
lapply(libraries, require, character.only = TRUE)

# IMPORT THE RAW DATA
raw_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_electronics_reviews.json")
meta_electronics <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_electronics_meta.json")
raw_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_cellphones_reviews.json")
meta_cellphone <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_cellphones_meta.json")
raw_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_homekitchen_reviews.json")
meta_homekitchen <- ndjson::stream_in("/Volumes/OMEGA/Dataset/rawdata/raw_homekitchen_meta.json")

# IMPORT CONTRACTION/SUBSTITUTION LIST
contraction_list <- read.csv("input-data/contractions.csv", header = TRUE, sep = ";")

# IMPORT INDIVIDUAL STOPWORDS
stopword_list <- read.csv("input-data/stopwords.csv", header = TRUE, sep = ";")
stopword_list <- unlist(stopword_list$word)
stopword_list <- as.character(stopword_list)
stopword_list <- as_tibble(stopword_list)

# IMPORT THE SENTIMENT SCORES FROM DEEP LEARNING MODEL
# Calculated Separately on Azure
score_coffee <- read.csv("input-data/score_coffee.csv", header = TRUE, sep = ";")
score_toast <- read.csv("input-data/score_toast.csv", header = TRUE, sep = ";")
score_headphone <- read.csv("input-data/score_headphone.csv", header = TRUE, sep = ";")
score_cellphone <- read.csv("input-data/score_cellphone.csv", header = TRUE, sep = ";")

# IMPORT PREPARED DATASET
prep_cellphone_brand <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_cellphone_brand-clean.csv"))
prep_coffee_brand <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_coffee_brand-clean.csv"))
prep_toaster_brand  <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_toaster_brand-clean.csv"))
prep_headphone_brand  <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_headphone_brand-clean.csv"))
prep_cellphone_brand <- as_tibble(fread("output/prep_cellphone_brand-clean.csv"))
prep_coffee_brand <- as_tibble(fread("output/prep_coffee_brand-clean.csv"))
prep_toaster_brand  <- as_tibble(fread("output/prep_toaster_brand-clean.csv"))
prep_headphone_brand  <- as_tibble(fread("output/prep_headphone_brand-clean.csv"))
merged_topic_cellphone  <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/old/merged_topic_cellphone.csv"))


# IMPORT THE CREATED TOPIC MODELS
LDA_reviews_cellphone <- readRDS("output/LDA_reviews_cellphone.rds")
LDA_reviews_apple <- readRDS("output/LDA_reviews_apple.rds")
LDA_reviews_coffee <- readRDS("output/LDA_reviews_coffee.rds")
LDA_reviews_toaster <- readRDS("output/LDA_reviews_toaster.rds")
LDA_reviews_headphone <- readRDS("output/LDA_reviews_headphone.rds")

# IMPORT THE XGBOOST MODELS DATA
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone.rds")
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone_noProgress.rds")
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone_noStopwords.rds")
xgbCellphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Cellphone.rds")
xgbToaster <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Toaster.rds")
xgbCoffee <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Coffee.rds")
xgbHeadphone <- xgb.load("output/XG_Headphone")
xgbCellphone <- xgb.load("output/XG_Cellphone")
xgbCoffee <- xgb.load("output/XG_Coffee")
xgbToaster <- xgb.load("output/XG_Toaster")

# CLEANUP - this command was sometimes really necessary as we're dealing with a huge amount of data
rm(list=ls())
