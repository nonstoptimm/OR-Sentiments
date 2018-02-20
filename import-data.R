# SENTIMENT ANALYSIS ON ONLINE REVIEWS #

### LOAD PACKAGES
libraries <- c("tibble", "ndjson", "tm", "dplyr", "tidytext", "ggplot2", "qdap", "wordcloud", "textmineR", "lubridate", "widyr", "ggraph", "igraph")
lapply(libraries, require, character.only = TRUE)

### IMPORT THE RAW DATA ###
rawdata <- ndjson::stream_in("input-data/raw_electronics_reviews.json")
metadata <- ndjson::stream_in("input-data/raw_electronics_meta.json")

### REMOVE NON-NECESSARY COLUMNS
rawdata <- rawdata[, c("asin", "helpful.0", "helpful.1", "overall", "reviewText", "reviewTime", "reviewerID", "summary")]
metadata <- metadata[, c("asin", "categories.0.0", "categories.0.1", "categories.0.2", "categories.0.3", "description", "title", "price", "categories.0.4", "brand")]

### CREATE A CHUNK OF THE HUGE RAWDATA
rawdata_subset <- rawdata[1:100000, ]
rawdata_subset <- read.csv("input-data/subset.csv")

### EXPORT THE DATA CHUNKS
# write.csv(rawdata_subset, file = "input-data/rawdata_subset.csv")
# write.csv(metadata, file = "input-data/metadata_filtered.csv")

### IMPORT THE DATA CHUNKS
rawdata_subset <- read.csv("input-data/subset.csv")
metadata <- read.csv("input-data/metadata_filtered.csv")

### MERGE THE  
merged_branded <- left_join(rawdata_subset, metadata, by = "asin")

### CLEANUP
rm(list=ls())
