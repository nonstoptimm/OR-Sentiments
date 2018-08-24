# OBJECT WRITER 

# TOPIC MODELS
saveLDA <- function(input, filename) {
  saveRDS(input, paste("output/", filename, ".rds", sep=""))
}
# Apply saveLDA Function
saveLDA(LDA_reviews_cellphone, "LDA_reviews_cellphone")
saveLDA(LDA_reviews_apple, "LDA_reviews_apple")
saveLDA(LDA_reviews_samsung, "LDA_reviews_samsung")
saveLDA(LDA_reviews_coffee, "LDA_reviews_coffee")
saveLDA(LDA_reviews_toaster, "LDA_reviews_toaster")
saveLDA(LDA_reviews_headphone, "LDA_reviews_headphone")
saveLDA(xgb_fit, "xgb_fit1")

# PREPARED DATA
saveData <- function(input, filename){
  fwrite(input, paste("output/", filename, ".csv", sep=""))  
}
# Apply saveData Function
saveData(prep_headphone_brand, "prep_headphone_brand-clean")
saveData(prep_cellphone_brand, "prep_cellphone_brand-clean")
saveData(prep_toaster_brand, "prep_toaster_brand-clean")
saveData(prep_coffee_brand, "prep_coffee_brand-clean")
saveData(importance_vars, "importance_vars")

# Cut all category columns and description, as it is unnecessary
prep_coffee_brand$description <- prep_coffee_brand$categories.0.0 <- prep_coffee_brand$categories.0.1 <- prep_coffee_brand$categories.0.2 <- NULL
prep_toaster_brand$description <- prep_toaster_brand$categories.0.0 <- prep_toaster_brand$categories.0.1 <- prep_toaster_brand$categories.0.2 <- NULL
prep_cellphone_brand$description <- prep_cellphone_brand$categories.0.0 <- prep_cellphone_brand$categories.0.1 <- prep_cellphone_brand$categories.0.2 <- NULL
prep_headphone_brand$description <- prep_headphone_brand$categories.0.0 <- prep_headphone_brand$categories.0.1 <- prep_headphone_brand$categories.0.2 <- NULL