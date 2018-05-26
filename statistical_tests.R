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
mean_cellphone <- calcMeanScore(prep_cellphone_brand)
fwrite(mean_cellphone, "output/mean_cellphone.csv")
# Toaster
mean_toaster <- calcMeanScore(merged_topic_toaster)
fwrite(mean_toaster, "output/mean_toaster.csv")
# Coffee
mean_coffee <- calcMeanScore(merged_topic_coffee)
fwrite(mean_coffee, "output/mean_coffee.csv")

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
var_cellphone <- calcVarScore(merged_topic_cellphone)
fwrite(var_cellphone, "output/var_cellphone.csv")
# Toaster
var_toaster <- calcVarScore(prep_toaster_brand)
fwrite(var_toaster, "output/var_toaster.csv")
# Coffee
var_coffee <- calcVarScore(prep_coffee_brand)
fwrite(var_coffee, "output/var_coffee.csv")

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
sd_cellphone <- calcSdScore(merged_topic_cellphone)
sd_cellphone_apple <- calcSdScore(merged_topic_apple)
sd_cellphone_apple <- calcSdScore(merged_topic_samsung)
fwrite(sd_cellphone, "output/sd_cellphone.csv")
# Toaster
sd_toaster <- calcSdScore(prep_toaster_brand)
fwrite(sd_toaster, "output/sd_toaster.csv")
# Coffee
sd_coffee <- calcSdScore(prep_coffee_brand)
fwrite(sd_coffee, "output/sd_coffee.csv")

# CORRELATION COEFFICIENT
corrCoeff <- function(input) {
  correlation <- cor(input$overall, input$score)
  return(correlation)
}
# Calculate Correlation Coefficient for Sentiment Scores
# Cellphone
cor_cellphone <- corrCoeff(merged_topic_samsung)
fwrite(cor_cellphone, "output/cor_cellphone.csv")
# Toaster
cor_toaster <- corrCoeff(prep_toaster_brand)
fwrite(cor_toaster, "output/cor_toaster.csv")
# Coffee
cor_coffee <- corrCoeff(prep_coffee_brand)
fwrite(cor_coffee, "output/cor_coffee.csv")

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
mean_cellphone_apple <- calcMeanScoreBrand(prep_cellphone_brand, "apple")
fwrite(mean_cellphone_apple, "output/mean_cellphone_apple.csv")
mean_cellphone_oceancross <- calcMeanScoreBrand(prep_cellphone_brand, "ocean cross")
fwrite(mean_cellphone_oceancross, "output/mean_cellphone_oceancross.csv")
mean_cellphone_samsung <- calcMeanScoreBrand(prep_cellphone_brand, "samsung")
fwrite(mean_cellphone_samsung, "output/mean_cellphone_samsung.csv")
# Toaster
calcMeanScoreBrand(prep_toaster_brand, "")
# Coffee
calcMeanScoreBrand(prep_coffee_brand, "")

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
var_cellphone_apple <- calcVarScoreBrand(prep_cellphone_brand, "apple")
fwrite(var_cellphone_apple, "output/var_cellphone_apple.csv")
var_cellphone_apple <- calcVarScoreBrand(merged_topic_samsung, "samsung")
fwrite(var_cellphone_apple, "output/var_cellphone_apple.csv")
# Toaster
#var_toaster <- calcVarScoreBrand(prep_toaster_brand, "")
#fwrite(var_toaster, "output/var_toaster.csv")
# Coffee
#var_coffee <- calcVarScoreBrand(prep_coffee_brand, "")
#fwrite(var_coffee, "output/var_coffee.csv")

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
sd_cellphone_apple <- calcSdScoreBrand(prep_cellphone_brand, "apple")
fwrite(sd_cellphone_apple, "output/sd_cellphone_apple.csv")
# Toaster
# calcSdScoreBrand(prep_toaster_brand, "")
# Coffee
# calcSdScoreBrand(prep_coffee_brand, "")


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


