# GENERAL DATA IMPORT
# dataImport.R
# Load required packages
library(dplyr)
library(data.table)
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
# Calculated separately using Microsoft Azure
score_coffee <- read.csv("input-data/score_coffee.csv", header = TRUE, sep = ";")
score_toast <- read.csv("input-data/score_toast.csv", header = TRUE, sep = ";")
score_headphone <- read.csv("input-data/score_headphone.csv", header = TRUE, sep = ";")
score_cellphone <- read.csv("input-data/score_cellphone.csv", header = TRUE, sep = ";")

# IMPORT PREPARED DATASET
prep_cellphone_brand <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_cellphone_brand-clean.csv"))
prep_coffee_brand <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_coffee_brand-clean.csv"))
prep_toaster_brand  <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_toaster_brand-clean.csv"))
prep_headphone_brand  <- as_tibble(fread("/Volumes/OMEGA/Dataset/prepared_data/Prepared_Input_Data/prep_headphone_brand-clean.csv"))
prep_cellphone_brand <- as_tibble(fread("output/prep_cellphone_brand.csv"))
prep_headphone_brand  <- as_tibble(fread("output/prep_headphone_brand.csv"))
prep_toaster_brand  <- as_tibble(fread("output/prep_toaster_brand.csv"))
prep_coffee_brand <- as_tibble(fread("output/prep_coffee_brand.csv"))

# IMPORT PREPARED DATASETS WITHOUT RELEVANT WORDS
prep_cellphone_brand <- as_tibble(fread("output/prep_cellphone_brand-filtered.csv"))
prep_headphone_brand  <- as_tibble(fread("output/prep_headphone_brand-filtered.csv"))
prep_toaster_brand  <- as_tibble(fread("output/prep_toaster_brand-filtered.csv"))
prep_coffee_brand <- as_tibble(fread("output/prep_coffee_brand-filtered.csv"))

# 
top10brands_cellphone <- as_tibble(fread("output/TopBrands/top10brands_cellphone.csv"))
top10brands_headphone <- as_tibble(fread("output/TopBrands/top10brands_headphone.csv"))

# IMPORT PREPARED DATASETS WITHOUT RELEVANT WORDS
merged_topic_cellphone <- as_tibble(fread("output/DataWithTopic/mergedTopicCellphone.csv"))
merged_topic_headphone  <- as_tibble(fread("output/DataWithTopic/mergedTopicHeadphone.csv"))
merged_topic_toaster  <- as_tibble(fread("output/DataWithTopic/mergedTopicToaster.csv"))
merged_topic_coffee <- as_tibble(fread("output/DataWithTopic/mergedTopicCoffee.csv"))

# IMPORT THE CREATED TOPIC MODELS
LDA_reviews_cellphone <- readRDS("output/TopicModels/LDA_reviews_cellphone.rds")
LDA_reviews_coffee <- readRDS("output/TopicModels/LDA_reviews_coffee.rds")
LDA_reviews_toaster <- readRDS("output/TopicModels/LDA_reviews_toaster.rds")
LDA_reviews_headphone <- readRDS("output/TopicModels/2018-08-31-LDA_reviews_headphone.rds")

# IMPORT THE XGBOOST MODELS DATA
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone.rds")
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone_noProgress.rds")
xgbHeadphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Headphone_noStopwords.rds")
xgbCellphone <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Cellphone.rds")
xgbToaster <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Toaster.rds")
xgbCoffee <- readRDS("/Volumes/OMEGA/Dataset/prepared_data/XGBOOST/XG_Coffee.rds")
xgbHeadphone <- xgb.load("output/XGBOOST/XG_Headphone")
xgbCellphone <- xgb.load("output/XGBOOST/XG_Cellphone")
xgbCoffee <- xgb.load("output/XGBOOST/XG_Coffee")
xgbToaster <- xgb.load("output/XGBOOST/XG_Toaster")

# CLEANUP COMMAND
# This command was sometimes really necessary as we're dealing with a huge amount of data
# rm(list=ls())
