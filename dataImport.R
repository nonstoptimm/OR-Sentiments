# GENERAL DATA IMPORT
# Import raw review data as well as prepared data
# dataImport.R
# Load required packages
library(dplyr)
library(data.table)

# IMPORT THE RAW DATA
raw_electronics <- ndjson::stream_in("raw_electronics_reviews.json")
meta_electronics <- ndjson::stream_in("raw_electronics_meta.json")
raw_cellphone <- ndjson::stream_in("raw_cellphones_reviews.json")
meta_cellphone <- ndjson::stream_in("raw_cellphones_meta.json")
raw_homekitchen <- ndjson::stream_in("raw_homekitchen_reviews.json")
meta_homekitchen <- ndjson::stream_in("raw_homekitchen_meta.json")

# IMPORT CONTRACTION/SUBSTITUTION LIST
contraction_list <- read.csv("input-data/ContractionList/contractions.csv", header = TRUE, sep = ";")

# IMPORT THE SENTIMENT SCORES FROM DEEP LEARNING MODEL
# Calculated separately using Microsoft Azure
score_coffee <- read.csv("input-data/ML_SentimentScores/score_coffee.csv", header = TRUE, sep = ";")
score_toast <- read.csv("input-data/ML_SentimentScores/score_toast.csv", header = TRUE, sep = ";")
score_headphone <- read.csv("input-data/ML_SentimentScores/score_headphone.csv", header = TRUE, sep = ";")
score_cellphone <- read.csv("input-data/ML_SentimentScores/score_cellphone.csv", header = TRUE, sep = ";")

# IMPORT PREPARED DATASET
prep_cellphone_brand <- as_tibble(fread("input-data/PreparedData/prep_cellphone_brand.csv"))
prep_headphone_brand  <- as_tibble(fread("input-data/PreparedData/prep_headphone_brand.csv"))
prep_toaster_brand  <- as_tibble(fread("input-data/PreparedData/prep_toaster_brand.csv"))
prep_coffee_brand <- as_tibble(fread("input-data/PreparedData/prep_coffee_brand.csv"))

# IMPORT PREPARED DATASETS WITHOUT EMPTY REVIEWS
prep_cellphone_brand <- as_tibble(fread("input-data/PreparedData/filtered/prep_cellphone_brand-filtered.csv"))
prep_headphone_brand  <- as_tibble(fread("input-data/PreparedData/filtered/prep_headphone_brand-filtered.csv"))
prep_toaster_brand  <- as_tibble(fread("input-data/PreparedData/filtered/prep_toaster_brand-filtered.csv"))
prep_coffee_brand <- as_tibble(fread("input-data/PreparedData/filtered/prep_coffee_brand-filtered.csv"))

# IMPORT LIST OF TOP 10 BRANDS
top10brands_cellphone <- as_tibble(fread("input-data/TopBrands/top10brands_cellphone.csv"))
top10brands_headphone <- as_tibble(fread("input-data/TopBrands/top10brands_headphone.csv"))
top10brands_toaster <- as_tibble(fread("input-data/TopBrands/top10brands_toaster.csv"))
top10brands_coffee <- as_tibble(fread("input-data/TopBrands/top10brands_coffee.csv"))

# IMPORT PREPARED DATASETS INCLUDING TOPIC SCORES
merged_topic_cellphone <- as_tibble(fread("input-data/DataWithTopic/mergedTopicCellphone.csv"))
merged_topic_headphone  <- as_tibble(fread("input-data/DataWithTopic/mergedTopicHeadphone.csv"))
merged_topic_toaster  <- as_tibble(fread("input-data/DataWithTopic/mergedTopicToaster.csv"))
merged_topic_coffee <- as_tibble(fread("input-data/DataWithTopic/mergedTopicCoffee.csv"))

# IMPORT THE CREATED TOPIC MODELS
LDA_reviews_cellphone <- readRDS("input-data/TopicModels/LDA_reviews_cellphone.rds")
LDA_reviews_headphone <- readRDS("input-data/TopicModels/LDA_reviews_headphone.rds")
LDA_reviews_coffee <- readRDS("input-data/TopicModels/LDA_reviews_coffee.rds")
LDA_reviews_toaster <- readRDS("input-data/TopicModels/LDA_reviews_toaster.rds")

# IMPORT THE XGBOOST MODELS DATA
xgbHeadphone <- xgb.load("input-data/XGBOOST/XG_Headphone")
xgbCellphone <- xgb.load("input-data/XGBOOST/XG_Cellphone")

# CLEANUP COMMAND
# This command is sometimes really necessary to clean up the workspace
# rm(list=ls())
