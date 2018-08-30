# SENTIMENT OVER TIME
# timeSeries.R
# Load required packages
library(dplyr)
library(lubridate)

# CREATE TIME SERIES FOR SENTIMENT SCORE AND OVERALL RATING
createTimeSeries <- function(input, brandSelect){
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  if(brandSelect == "n"){
  # Aggregate
    aggregated <- input %>% group_by(reviewYear, reviewMonth) %>% summarise(AvgScore=mean(scoreNN), AvgStar=mean(overall))
  } else {
    aggregated <- input %>% group_by(brand, reviewYear, reviewMonth) %>% summarise(AvgScore=mean(scoreNN), AvgStar=mean(overall))
  }
  # Glue date back together and remove year+month
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep="-")
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTimeSeries Function
timeHeadphone <- createTimeSeries(prep_headphone_brand, "n")
timeHeadphoneBrand <- createTimeSeries(prep_headphone_brand, "y")
timeHeadphoneBrandBeats <- timeHeadphoneBrand %>% filter(brand == "beats")
timeHeadphoneBrandSennheiser <- timeHeadphoneBrand %>% filter(brand == "sennheiser")

# CREATE TIME SERIES FOR AVERAGE TOPIC DISTRIBUTION
createTopicSeries <- function(input, brandSelect){
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  if(brandSelect == "n"){
    # Aggregate
    aggregated <- input %>% group_by(reviewYear, reviewMonth) %>% summarise(T1=mean(T1), T2=mean(T2), T3=mean(T3), T4=mean(T4), T5=mean(T5))
  } else {
    aggregated <- input %>% group_by(brand, reviewYear, reviewMonth) %>% summarise(T1=mean(T1), T2=mean(T2), T3=mean(T3), T4=mean(T4), T5=mean(T5))
  }
  # Glue date back together and remove year+month
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep="-")
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
