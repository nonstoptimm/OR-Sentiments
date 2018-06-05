### STATISTICAL TESTS
library(dplyr)

# MEAN
calcMeanScore <- function(input) {
  mean_overall <- mean(input$overall)
  mean_score_total <- mean(input$score)
  mean_scores <- c()
  mean_scores <- c(mean_scores, mean_overall, mean_score_total)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarise(mean(scoreNN))
    mean_scores <- c(mean_scores, current_score)
  }
  mean_scores <- round(as.numeric(mean_scores), 5)
  names(mean_scores) <- c("Mean Stars", "Mean Score", "MS1", "MS2", "MS3", "MS4", "MS5")
  return(as.data.frame(mean_scores))
}
# Calculate Means for Sentiment Scores
# Cellphone
calcMeanScore(merged_topic_cellphone)
# fwrite(calcMeanScore(prep_cellphone_brand), "output/mean_cellphone.csv")
# Toaster
calcMeanScore(merged_topic_toaster)
# fwrite(calcMeanScore(prep_toaster_brand), "output/mean_toaster.csv")
# Coffee
calcMeanScore(merged_topic_coffee)
# fwrite(calcMeanScore(merged_topic_coffee), "output/mean_coffee.csv")
# Headphone
calcMeanScore(prep_headphone_brand)
# fwrite(calcMeanScore(merged_topic_coffee), "output/mean_headphone.csv")

# VARIANCE
calcVarScore <- function(input) {
  var_overall <- var(input$scoreNN)
  var_scores <- c()
  var_scores <- c(var_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(var(scoreNN))
    var_scores <- c(var_scores, current_score)
  }
  var_scores <- round(as.numeric(var_scores), 5)
  names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
  return(as.data.frame(var_scores))
}
# Calculate Variance Deviation for Sentiment Scores
# Cellphones
calcVarScore(merged_topic_cellphone)
#fwrite(calcVarScore(merged_topic_cellphone), "output/var_cellphone.csv")
# Toaster
calcVarScore(prep_toaster_brand)
#fwrite(calcVarScore(prep_toaster_brand), "output/var_toaster.csv")
# Coffee
calcVarScore(prep_coffee_brand)
#fwrite(calcVarScore(prep_coffee_brand), "output/var_coffee.csv")
# Headphone
calcVarScore(prep_headphone_brand)
#fwrite(calcVarScore(prep_headphone_brand), "output/var_headphone.csv")


# STANDARD DEVIATION
calcSdScore <- function(input) {
  var_overall <- sd(input$scoreNN)
  sd_scores <- c()
  sd_scores <- c(sd_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(sd(scoreNN))
    sd_scores <- c(sd_scores, current_score)
  }
  sd_scores <- round(as.numeric(sd_scores), 5)
  names(sd_scores) <- c("SdG", "Sd1", "Sd2", "Sd3", "Sd4", "Sd5")
  return(as.data.frame(sd_scores))
}

# Calculate Standard Deviation for Sentiment Scores
# Cellphone
calcSdScore(merged_topic_cellphone)
calcSdScore(merged_topic_apple)
calcSdScore(merged_topic_samsung)
fwrite(sd_cellphone, "output/sd_cellphone.csv")
# Toaster
calcSdScore(prep_toaster_brand)
fwrite(calcSdScore(prep_toaster_brand), "output/sd_toaster.csv")
# Coffee
calcSdScore(prep_coffee_brand)
fwrite(calcSdScore(prep_coffee_brand), "output/sd_coffee.csv")
# Headphone
calcSdScore(prep_headphone_brand)
#fwrite(calcSdScore(prep_headphone_brand), "output/sd_headphone.csv")


# CORRELATION COEFFICIENT
corrCoeff <- function(input) {
  correlation <- cor(input$overall, input$score)
  return(correlation)
}
# Calculate Correlation Coefficient for Sentiment Scores
# Cellphone
corrCoeff(merged_topic_samsung)
#fwrite(corrCoeff(merged_topic_samsung), "output/cor_cellphone.csv")
# Toaster
corrCoeff(prep_toaster_brand)
#fwrite(corrCoeff(prep_toaster_brand), "output/cor_toaster.csv")
# Coffee
corrCoeff(prep_coffee_brand)
#fwrite(corrCoeff(prep_coffee_brand), "output/cor_coffee.csv")
# Headphone
corrCoeff(prep_headphone_brand)
#fwrite(corrCoeff(prep_coffee_brand), "output/cor_headphone.csv")

########## STATISTICAL TESTS ON BRANDS ############
# MEAN
calcMeanScoreBrand <- function(input, brandSelect) {
  input <- input %>% filter(brand == brandSelect)
  mean_overall <- mean(input$overall)
  mean_score_total <- mean(input$score)
  mean_scores <- c()
  mean_scores <- c(mean_scores, mean_overall, mean_score_total)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarise(mean(scoreNN))
    mean_scores <- c(mean_scores, current_score)
  }
  mean_scores <- round(as.numeric(mean_scores), 5)
  names(mean_scores) <- c("Mean Stars", "Mean Score", "MS1", "MS2", "MS3", "MS4", "MS5")
  return(as.data.frame(mean_scores))
}
# Calculate Means for Sentiment Scores
# Cellphone
calcMeanScoreBrand(prep_cellphone_brand, "apple")
#fwrite(calcMeanScoreBrand(prep_cellphone_brand, "apple"), "output/mean_cellphone_apple.csv")
calcMeanScoreBrand(prep_cellphone_brand, "ocean cross")
#fwrite(calcMeanScoreBrand(prep_cellphone_brand, "ocean cross"), "output/mean_cellphone_oceancross.csv")
calcMeanScoreBrand(prep_cellphone_brand, "samsung")
#fwrite(calcMeanScoreBrand(prep_cellphone_brand, "samsung"), "output/mean_cellphone_samsung.csv")
# Toaster
calcMeanScoreBrand(prep_toaster_brand, "")
# Coffee
calcMeanScoreBrand(prep_coffee_brand, "")
# Headphone
calcMeanScoreBrand(prep_headphone_brand, "sennheiser")

# VARIANCE
calcVarScoreBrand <- function(input, brandSelect) {
  input <- input %>% filter(brand == brandSelect)
  var_overall <- var(input$scoreNN)
  var_scores <- c()
  var_scores <- c(var_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(var(scoreNN))
    var_scores <- c(var_scores, current_score)
  }
  var_scores <- round(as.numeric(var_scores), 5)
  names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
  return(as.data.frame(var_scores))
}
# Calculate Variance Deviation for Sentiment Scores
# Cellphones
calcVarScoreBrand(prep_cellphone_brand, "apple")
#fwrite(calcVarScoreBrand(prep_cellphone_brand, "apple"), "output/var_cellphone_apple.csv")
calcVarScoreBrand(dtm_cellphone_brand, "ocean cross")
#fwrite(calcVarScoreBrand(merged_topic_samsung, "samsung"), "output/var_cellphone_apple.csv")
# Toaster
calcVarScoreBrand(prep_toaster_brand, "")
#fwrite(calcVarScoreBrand(prep_toaster_brand, ""), "output/var_toaster.csv")
# Coffee
calcVarScoreBrand(prep_coffee_brand, "")
#fwrite(calcVarScoreBrand(prep_coffee_brand, ""), "output/var_coffee.csv")
# Headphone
calcVarScoreBrand(prep_headphone_brand, "beats")
#fwrite(calcVarScoreBrand(prep_headphone_brand, ""), "output/var_coffee.csv")


# STANDARD DEVIATION
calcSdScoreBrand <- function(input, brandSelect) {
  input <- input %>% filter(brand == brandSelect)
  var_overall <- sd(input$scoreNN)
  sd_scores <- c()
  sd_scores <- c(sd_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(sd(scoreNN))
    sd_scores <- c(sd_scores, current_score)
  }
  sd_scores <- as.numeric(sd_scores)
  names(sd_scores) <- c("SdG", "Sd1", "Sd2", "Sd3", "Sd4", "Sd5")
  return(as.data.frame(sd_scores))
}

# Calculate Standard Deviation for Sentiment Scores
# Cellphone
calcSdScoreBrand(dtm_cellphone_brand, "ocean cross")
fwrite(sd_cellphone_apple, "output/sd_cellphone_apple.csv")
# Toaster
# calcSdScoreBrand(prep_toaster_brand, "")
# Coffee
# calcSdScoreBrand(prep_coffee_brand, "")
# Headphone
calcSdScoreBrand(prep_headphone_brand, "sennheiser")



### TOPIC MODEL STATISTICS ####
# VARIANCE ON TOPICS
calcVarScore <- function(input, topic) {
  var_scores <- c()
  for(star in 1:5) {
    current_score <- input %>% select(topic) %>% summarize(var(topic))
    var_scores <- c(var_scores, current_score)
  }
  var_scores <- round(as.numeric(var_scores), 5)
  names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
  return(as.data.frame(var_scores))
}


