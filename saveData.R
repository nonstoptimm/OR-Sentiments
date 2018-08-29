# saveData.R
# OBJECT WRITER 
# Load required packages
library(data.table)
library(xgboost)

# PREPARED DATA
# Function to write prepared data and any type of table on the disk as .csv
saveData <- function(input, filename){
  fwrite(input, paste("output/", filename, ".csv", sep=""))  
}
# Apply saveData Function
saveData(prep_headphone_brand, "prep_headphone_brand-clean")
saveData(prep_cellphone_brand, "prep_cellphone_brand-clean")
saveData(prep_toaster_brand, "prep_toaster_brand-clean")
saveData(prep_coffee_brand, "prep_coffee_brand-clean")
saveData(importance_vars2, "importance_vars2-noProgress")

# SAVE TOPIC MODELS
# Function to write topic models on the disk
saveRAW <- function(input, filename) {
  saveRDS(input, paste("output/", filename, ".rds", sep=""))
}
# Apply saveLDA Function
saveRAW(LDA_reviews_cellphone, "LDA_reviews_cellphone")
saveRAW(LDA_reviews_apple, "LDA_reviews_apple")
saveRAW(LDA_reviews_samsung, "LDA_reviews_samsung")
saveRAW(LDA_reviews_coffee, "LDA_reviews_coffee")
saveRAW(LDA_reviews_toaster, "LDA_reviews_toaster")
saveRAW(LDA_reviews_headphone, "LDA_reviews_headphone")
saveRAW(xgbCoffee, "XG_Coffee")

# SAVE XGBOOST
saveXG <- function(input, filename){
  xgb.save(input, paste("output/", filename, sep=""))
}
# Apply saveXG Function
saveXG(xgbHeadphones, "XG_Headphones")
saveXG(xgbCellphones, "XG_Cellphones")
saveXG(xgbCoffee, "XG_Coffee")
saveXG(xgbToaster, "XG_Toaster")
