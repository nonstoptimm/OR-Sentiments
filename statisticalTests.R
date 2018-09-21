# STATISTICAL TESTS
# statisticalTests.R
library(dplyr)

# MIN, MAX, MEDIAN OF ANY NUMERICAL INPUT COLUMN
calcMinMaxMedian <- function(input) {
  minScore <- min(input)
  maxScore <- max(input)
  medScore <- median(input)
  scoreDF <- c()
  scoreDF <- c(minScore, maxScore, medScore)
  names(scoreDF) <- c("Min", "Max", "Median")
  return(as.data.frame(scoreDF))
}
# Apply
calcMinMaxMedian(prep_toaster_brand$scoreLX)

# MEAN FOR SENTIMENT SCORE: NN/OVERALL
calcMeanScoreLX <- function(input) {
  mean_overall <- mean(input$overall)
  mean_score_total <- mean(input$scoreLX)
  mean_scores <- c()
  mean_scores <- c(mean_scores, mean_overall, mean_score_total)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarise(mean(scoreLX))
    mean_scores <- c(mean_scores, current_score)
  }
  mean_scores <- round(as.numeric(mean_scores), 5)
  names(mean_scores) <- c("Mean Stars", "Mean Score", "MS1", "MS2", "MS3", "MS4", "MS5")
  return(as.data.frame(mean_scores))
}
# MEAN FOR SENTIMENT SCORE: LX/OVERALL
calcMeanScoreNN <- function(input) {
  mean_overall <- mean(input$overall)
  mean_score_total <- mean(input$scoreNN)
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
# Headphone
calcMeanScoreLX(prep_headphone_brand)
calcMeanScoreNN(prep_headphone_brand)
# fwrite(calcMeanScore(merged_topic_coffee), "output/mean_headphone.csv")
# Cellphone
calcMeanScoreLX(prep_cellphone_brand)
calcMeanScoreNN(prep_cellphone_brand)
# fwrite(calcMeanScore(prep_cellphone_brand), "output/mean_cellphone.csv")
# Toaster
calcMeanScoreLX(prep_toaster_brand)
calcMeanScoreNN(prep_toaster_brand)
# fwrite(calcMeanScore(prep_toaster_brand), "output/mean_toaster.csv")
# Coffee
calcMeanScoreLX(prep_coffee_brand)
calcMeanScoreNN(prep_coffee_brand)
# fwrite(calcMeanScore(merged_topic_coffee), "output/mean_coffee.csv")

# VARIANCE FOR SENTIMENT SCORE: LX & NN
calcVarScoreLX <- function(input) {
  var_overall <- var(input$scoreLX)
  var_scores <- c()
  var_scores <- c(var_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(var(scoreLX))
    var_scores <- c(var_scores, current_score)
  }
  var_scores <- round(as.numeric(var_scores), 5)
  names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
  return(as.data.frame(var_scores))
}
calcVarScoreNN <- function(input) {
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
# Headphone
calcVarScoreLX(prep_headphone_brand)
calcVarScoreNN(prep_headphone_brand)
#fwrite(calcVarScore(prep_headphone_brand), "output/var_headphone.csv")
# Cellphones
calcVarScoreLX(prep_cellphone_brand)
calcVarScoreNN(prep_cellphone_brand)
#fwrite(calcVarScore(prep_cellphone_brand), "output/var_cellphone.csv")
# Toaster
calcVarScoreLX(prep_toaster_brand)
calcVarScoreNN(prep_toaster_brand)
#fwrite(calcVarScore(prep_toaster_brand), "output/var_toaster.csv")
# Coffee
calcVarScoreLX(prep_coffee_brand)
calcVarScoreNN(prep_coffee_brand)
#fwrite(calcVarScore(prep_coffee_brand), "output/var_coffee.csv")

# STANDARD DEVIATION SCORE: LX & NN
calcSdScoreLX <- function(input) {
  var_overall <- sd(input$scoreLX)
  sd_scores <- c()
  sd_scores <- c(sd_scores, var_overall)
  for(star in 1:5) {
    current_score <- input %>% filter(overall == star) %>% summarize(sd(scoreLX))
    sd_scores <- c(sd_scores, current_score)
  }
  sd_scores <- round(as.numeric(sd_scores), 5)
  names(sd_scores) <- c("SdG", "Sd1", "Sd2", "Sd3", "Sd4", "Sd5")
  return(as.data.frame(sd_scores))
}
calcSdScoreNN <- function(input) {
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
# Headphone
calcSdScoreLX(prep_headphone_brand)
calcSdScoreNN(prep_headphone_brand)
# fwrite(calcSdScore(prep_headphone_brand), "output/sd_headphone.csv")
# Cellphone
calcSdScoreLX(prep_cellphone_brand)
calcSdScoreNN(prep_cellphone_brand)
#fwrite(sd_cellphone, "output/sd_cellphone.csv")
# Toaster
calcSdScoreLX(prep_toaster_brand)
calcSdScoreNN(prep_toaster_brand)
# fwrite(calcSdScore(prep_toaster_brand), "output/sd_toaster.csv")
# Coffee
calcSdScoreLX(prep_coffee_brand)
calcSdScoreNN(prep_coffee_brand)
# fwrite(calcSdScore(prep_coffee_brand), "output/sd_coffee.csv")

# CORRELATION COEFFICIENT: SCORE LX & NN
corrCoeffLX <- function(input) {
  correlation <- cor(input$overall, input$scoreLX)
  return(correlation)
}
corrCoeffNN <- function(input) {
  correlation <- cor(input$overall, input$scoreNN)
  return(correlation)
}
# Calculate Correlation Coefficient for Sentiment Scores
# Headphone
corrCoeffLX(prep_headphone_brand)
corrCoeffNN(prep_headphone_brand)
# fwrite(corrCoeff(prep_coffee_brand), "output/cor_headphone.csv")
# Cellphone
corrCoeffLX(prep_cellphone_brand)
corrCoeffNN(prep_cellphone_brand)
# fwrite(corrCoeff(merged_topic_samsung), "output/cor_cellphone.csv")
# Toaster
corrCoeffLX(prep_toaster_brand)
corrCoeffNN(prep_toaster_brand)
# fwrite(corrCoeff(prep_toaster_brand), "output/cor_toaster.csv")
# Coffee
corrCoeffLX(prep_coffee_brand)
corrCoeffNN(prep_coffee_brand)
# fwrite(corrCoeff(prep_coffee_brand), "output/cor_coffee.csv")

# ########## STATISTICAL TESTS ON BRANDS ############
# # MEAN
# calcMeanScoreBrand <- function(input, brandSelect) {
#   input <- input %>% filter(brand == brandSelect)
#   mean_overall <- mean(input$overall)
#   mean_score_total <- mean(input$scoreNN)
#   mean_scores <- c()
#   mean_scores <- c(mean_scores, mean_overall, mean_score_total)
#   for(star in 1:5) {
#     current_score <- input %>% filter(overall == star) %>% summarise(mean(scoreNN))
#     mean_scores <- c(mean_scores, current_score)
#   }
#   mean_scores <- round(as.numeric(mean_scores), 5)
#   names(mean_scores) <- c("Mean Stars", "Mean Score", "MS1", "MS2", "MS3", "MS4", "MS5")
#   return(as.data.frame(mean_scores))
# }
# # Calculate Means for Sentiment Scores
# # Cellphone
# calcMeanScoreBrand(prep_cellphone_brand, "apple")
# # fwrite(calcMeanScoreBrand(prep_cellphone_brand, "apple"), "output/mean_cellphone_apple.csv")
# calcMeanScoreBrand(prep_cellphone_brand, "ocean cross")
# # fwrite(calcMeanScoreBrand(prep_cellphone_brand, "ocean cross"), "output/mean_cellphone_oceancross.csv")
# calcMeanScoreBrand(prep_cellphone_brand, "samsung")
# # fwrite(calcMeanScoreBrand(prep_cellphone_brand, "samsung"), "output/mean_cellphone_samsung.csv")
# # Toaster
# calcMeanScoreBrand(prep_toaster_brand, "")
# # Coffee
# calcMeanScoreBrand(prep_coffee_brand, "")
# # Headphone
# calcMeanScoreBrand(prep_headphone_brand, "sennheiser")
# 
# # VARIANCE
# calcVarScoreBrand <- function(input, brandSelect) {
#   input <- input %>% filter(brand == brandSelect)
#   var_overall <- var(input$scoreNN)
#   var_scores <- c()
#   var_scores <- c(var_scores, var_overall)
#   for(star in 1:5) {
#     current_score <- input %>% filter(overall == star) %>% summarize(var(scoreNN))
#     var_scores <- c(var_scores, current_score)
#   }
#   var_scores <- round(as.numeric(var_scores), 5)
#   names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
#   return(as.data.frame(var_scores))
# }
# # Calculate Variance Deviation for Sentiment Scores
# # Cellphones
# calcVarScoreBrand(prep_cellphone_brand, "apple")
# # fwrite(calcVarScoreBrand(prep_cellphone_brand, "apple"), "output/var_cellphone_apple.csv")
# calcVarScoreBrand(dtm_cellphone_brand, "ocean cross")
# # fwrite(calcVarScoreBrand(merged_topic_samsung, "samsung"), "output/var_cellphone_apple.csv")
# # Toaster
# calcVarScoreBrand(prep_toaster_brand, "")
# # fwrite(calcVarScoreBrand(prep_toaster_brand, ""), "output/var_toaster.csv")
# # Coffee
# calcVarScoreBrand(prep_coffee_brand, "")
# # fwrite(calcVarScoreBrand(prep_coffee_brand, ""), "output/var_coffee.csv")
# # Headphone
# calcVarScoreBrand(prep_headphone_brand, "beats")
# # fwrite(calcVarScoreBrand(prep_headphone_brand, ""), "output/var_coffee.csv")
# 
# 
# # STANDARD DEVIATION
# calcSdScoreBrand <- function(input, brandSelect) {
#   input <- input %>% filter(brand == brandSelect)
#   var_overall <- sd(input$scoreNN)
#   sd_scores <- c()
#   sd_scores <- c(sd_scores, var_overall)
#   for(star in 1:5) {
#     current_score <- input %>% filter(overall == star) %>% summarize(sd(scoreNN))
#     sd_scores <- c(sd_scores, current_score)
#   }
#   sd_scores <- as.numeric(sd_scores)
#   names(sd_scores) <- c("SdG", "Sd1", "Sd2", "Sd3", "Sd4", "Sd5")
#   return(as.data.frame(sd_scores))
# }
# 
# # Calculate Standard Deviation for Sentiment Scores
# # Cellphone
# calcSdScoreBrand(dtm_cellphone_brand, "ocean cross")
# fwrite(sd_cellphone_apple, "output/sd_cellphone_apple.csv")
# # Toaster
# # calcSdScoreBrand(prep_toaster_brand, "")
# # Coffee
# # calcSdScoreBrand(prep_coffee_brand, "")
# # Headphone
# calcSdScoreBrand(prep_headphone_brand, "sennheiser")
# 
# 
# ### TOPIC MODEL STATISTICS ####
# # VARIANCE ON TOPICS
# calcVarScore <- function(input, topic) {
#   var_scores <- c()
#   for(star in 1:5) {
#     current_score <- input %>% select(topic) %>% summarize(var(topic))
#     var_scores <- c(var_scores, current_score)
#   }
#   var_scores <- round(as.numeric(var_scores), 5)
#   names(var_scores) <- c("VarG", "Var1", "Var2", "Var3", "Var4", "Var5")
#   return(as.data.frame(var_scores))
# }