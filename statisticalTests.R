# STATISTICAL TESTS
# statisticalTests.R
library(dplyr)

# MIN, MAX, MEDIAN OF ANY NUMERICAL INPUT COLUMN
calcMinMaxMedianMean <- function(input) {
  minScore <- min(input)
  maxScore <- max(input)
  medScore <- median(input)
  meanScore <- mean(input)
  scoreDF <- c()
  scoreDF <- c(minScore, maxScore, medScore, meanScore)
  names(scoreDF) <- c("Min", "Max", "Median", "Mean")
  return(as.data.frame(scoreDF))
}
# Apply calcMinMaxMedian-function
calcMinMaxMedianMean(prep_headphone_brand$scoreLX)
calcMinMaxMedianMean(prep_cellphone_brand$scoreLX)
calcMinMaxMedianMean(prep_toaster_brand$scoreLX)
calcMinMaxMedianMean(prep_coffee_brand$scoreLX)
calcMinMaxMedianMean(prep_headphone_brand$scoreNN)
calcMinMaxMedianMean(prep_cellphone_brand$scoreNN)
calcMinMaxMedianMean(prep_toaster_brand$scoreNN)
calcMinMaxMedianMean(prep_coffee_brand$scoreNN)

# MEAN FOR SENTIMENT SCORE: 
# LX/OVERALL
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
# ML/OVERALL
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
# Apply calcMeanScoreLX/NN-function
# Cellphone
calcMeanScoreLX(prep_cellphone_brand)
calcMeanScoreNN(prep_cellphone_brand)
# Headphone
calcMeanScoreLX(prep_headphone_brand)
calcMeanScoreNN(prep_headphone_brand)
# Toaster
calcMeanScoreLX(prep_toaster_brand)
calcMeanScoreNN(prep_toaster_brand)
# Coffee
calcMeanScoreLX(prep_coffee_brand)
calcMeanScoreNN(prep_coffee_brand)

# VARIANCE FOR SENTIMENT SCORE: LX & NN
# LX
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
# ML
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
# Apply calcVarScoreLX/NN-function
# Headphone
calcVarScoreLX(prep_headphone_brand)
calcVarScoreNN(prep_headphone_brand)
# Cellphones
calcVarScoreLX(prep_cellphone_brand)
calcVarScoreNN(prep_cellphone_brand)
# Toaster
calcVarScoreLX(prep_toaster_brand)
calcVarScoreNN(prep_toaster_brand)
# Coffee
calcVarScoreLX(prep_coffee_brand)
calcVarScoreNN(prep_coffee_brand)

# STANDARD DEVIATION SCORE: LX & NN
# LX
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
# ML
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
# Apply calcSdScoreLX/NN-function
# Cellphone
calcSdScoreLX(prep_cellphone_brand)
calcSdScoreNN(prep_cellphone_brand)
# Headphone
calcSdScoreLX(prep_headphone_brand)
calcSdScoreNN(prep_headphone_brand)
# Toaster
calcSdScoreLX(prep_toaster_brand)
calcSdScoreNN(prep_toaster_brand)
# Coffee
calcSdScoreLX(prep_coffee_brand)
calcSdScoreNN(prep_coffee_brand)

# CORRELATION COEFFICIENT: SCORE LX & NN
# LX
corrCoeffLX <- function(input) {
  correlation <- cor(input$overall, input$scoreLX)
  return(correlation)
}
# ML
corrCoeffNN <- function(input) {
  correlation <- cor(input$overall, input$scoreNN)
  return(correlation)
}
# Calculate Correlation Coefficient for Sentiment Scores
# Cellphone
corrCoeffLX(prep_cellphone_brand)
corrCoeffNN(prep_cellphone_brand)
# Headphone
corrCoeffLX(prep_headphone_brand)
corrCoeffNN(prep_headphone_brand)
# Toaster
corrCoeffLX(prep_toaster_brand)
corrCoeffNN(prep_toaster_brand)
# Coffee
corrCoeffLX(prep_coffee_brand)
corrCoeffNN(prep_coffee_brand)
