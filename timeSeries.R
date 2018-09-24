# SENTIMENT OVER TIME
# timeSeries.R
# Load required packages
library(dplyr)
library(lubridate)
library(ggplot2)

# CREATE TIME SERIES FOR SENTIMENT SCORE AND OVERALL RATING
createTimeSeries <- function(input, brandSelect){
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  if(brandSelect == "n"){
  # Aggregate
    aggregated <- input %>% 
      group_by(reviewYear, reviewMonth) %>% 
      summarise(AvgScore=mean(scoreNN), AvgStar=mean(overall)) %>% 
      ungroup()
  } else {
    aggregated <- input %>% 
      group_by(brand, reviewYear, reviewMonth) %>% 
      summarise(AvgScore=mean(scoreNN), AvgStar=mean(overall)) %>% 
      ungroup()
  }
  # Glue date back together and remove $year + $month
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep="-")
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTimeSeries-function
timeHeadphone <- createTimeSeries(prep_headphone_brand, "n")
timeHeadphoneBrand <- createTimeSeries(prep_headphone_brand, "y")
timeHeadphoneBrandBeats <- timeHeadphoneBrand %>% filter(brand == "beats")
timeHeadphoneBrandSennheiser <- timeHeadphoneBrand %>% filter(brand == "sennheiser")

# PLOT TIME SERIES FOR AVERAGE SENTIMENT SCORE
plotTimeSeries <- function(input, brandList, begin, end){
  input <- input %>%
    filter(brand %in% brandList & date >= begin & date <= end)
  ggplot(data = input, aes(x = date, group = brand, color = brand)) + 
    geom_line(aes(y = AvgScore), size = 1.5) + 
    theme_classic() + 
    labs(title = "Sentiment over Time", y = "Average Sentiment", x = "Period") + 
    #geom_vline(xintercept = 5, color = "red") + 
    #coord_cartesian(xlim = c(1.6, 7)) + 
    scale_colour_hue(name = "Brands", labels = brandList) 
}
# Apply plotTimeSeries-function
plotTimeSeries(timeHeadphoneBrand, top10brands_headphone$brand, "2014-01", "2014-09")

# CREATE TIME SERIES FOR AVERAGE TOPIC DISTRIBUTION
createTopicSeries <- function(input, brandSelect){
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  if(brandSelect == "n"){
    # Aggregate
    aggregated <- input %>% 
      group_by(reviewYear, reviewMonth) %>% 
      summarise(T1=mean(T1), T2=mean(T2), T3=mean(T3), T4=mean(T4), T5=mean(T5))
  } else {
    aggregated <- input %>% 
      group_by(brand, reviewYear, reviewMonth) %>% 
      summarise(T1=mean(T1), T2=mean(T2), T3=mean(T3), T4=mean(T4), T5=mean(T5))
  }
  # Glue date back together and remove year+month
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep="-")
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
