# OBJECT WRITER 
# Save generated/proceeded data
# saveData.R
# Load required packages
library(data.table)
library(xgboost)

# PREPARED DATA
# Function to write prepared data and any type of table on the disk as .csv
saveData <- function(input, filename){
  fwrite(input, paste("output/", filename, ".csv", sep=""), sep = ";")  
}
# Apply saveData-function
# Normal Datasets
saveData(prep_cellphone_brand, "prep_cellphone_brand")
saveData(prep_headphone_brand, "prep_headphone_brand")
saveData(prep_toaster_brand, "prep_toaster_brand")
saveData(prep_coffee_brand, "prep_coffee_brand")
# Top-Words
saveData(as.data.frame(topicWordsCellphone), "topicWordsCellphones")
saveData(as.data.frame(topicWordsHeadphones), "topicWordsHeadphones")
saveData(as.data.frame(topicWordsToasters), "topicWordsToasters")
saveData(as.data.frame(topicWordsCoffee), "topicWordsCoffee")
# Top Brands with review count, average price and price group
saveData(scoreCellphoneBrand, "BrandScores/brandScoresCellphone")
saveData(scoreHeadphoneBrand, "BrandScores/brandScoresHeadphone")
saveData(scoreToasterBrand, "BrandScores/brandScoresToaster")
saveData(scoreCoffeeBrand, "BrandScores/brandScoresCoffee")
# Top Brands with review count, average price and price group
saveData(top10brands_cellphone, "TopBrands/top10brands_cellphone")
saveData(top10brands_headphone, "TopBrands/top10brands_headphone")
saveData(top10brands_toaster, "TopBrands/top10brands_toaster")
saveData(top10brands_coffee, "TopBrands/top10brands_coffee")
# Topic-Assigment
saveData(merged_topic_cellphone, "mergedTopicCellphone")
saveData(merged_topic_headphone, "mergedTopicHeadphone")
saveData(merged_topic_toaster, "mergedTopicToaster")
saveData(merged_topic_coffee, "mergedTopicCoffee")

# SAVE TOPIC MODELS
# Function to write topic models on the disk
saveRAW <- function(input, filename) {
  saveRDS(input, paste("output/", filename, ".rds", sep=""))
}
# Apply saveLDA-function
saveRAW(LDA_reviews_cellphone, "LDA_reviews_cellphone")
saveRAW(LDA_reviews_coffee, "LDA_reviews_coffee")
saveRAW(LDA_reviews_toaster, "LDA_reviews_toaster")
saveRAW(LDA_reviews_headphone, "LDA_reviews_headphone")

# SAVE XGBOOST
saveXG <- function(input, filename){
  xgb.save(input, paste("output/", filename, sep=""))
}
# Apply saveXG-function
saveXG(xgbCellphone, "XG_Cellphones")
saveXG(xgbHeadphone, "XG_Headphones1")

