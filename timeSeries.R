# SENTIMENT OVER TIME
# timeSeries.R
# Load required packages
library(dplyr)
library(ggplot2)
library(lubridate) # formatting dates

# CREATE TIME SERIES FOR SENTIMENT SCORE AND OVERALL RATING
createTimeSeries <- function(input, brandList){
  # Review time as date
  input$reviewTime <- as.Date(input$reviewTime)
  # Get months
  input$reviewMonth <- format(input$reviewTime, "%m")
  # Get year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  # Aggregate Data
  aggregated <- input %>%
    filter(brand %in% brandList$brand) %>%
    group_by(brand, reviewYear, reviewMonth) %>% 
    summarise(AvgScore = mean(scoreNN), AvgStar = mean(overall),n = n()) %>% 
    ungroup()
  # Glue date back together and remove $year + $month
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep="-")
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTimeSeries-function
timeHeadphoneBrand <- createTimeSeries(merged_topic_headphone, top10brands_headphone)
timeCellphoneBrand <- createTimeSeries(merged_topic_cellphone, top10brands_cellphone)

# PLOT TIME SERIES FOR AVERAGE SENTIMENT SCORE
plotTimeSeries <- function(input, brandList, category, begin, end, ylim, filterSelect){
  # Filter for the products and time range
  input <- input %>%
    filter(brand %in% brandList$brand & date >= begin & date <= end)
  if(is.character(filterSelect)){
    input <- input %>%
      filter(brand %in% filterSelect)
    brandList <- brandList %>%
      filter(brand %in% filterSelect)
  }
  # Date as factor to place the vertical line for product launch
  input$date <- as.factor(input$date)
  # Plot data
  ggplot(data = input, aes(x = date, group = brand, color = brand)) + 
    geom_line(aes(y = AvgScore), size = 1) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Average Sentiment Over Time for", category, sep = " "), y = "Average Sentiment", x = "Period") + 
    ylim(ylim) + 
    scale_colour_hue(name = "Brands", labels = brandList$properBrand) 
}
# Apply plotTimeSeries-function
plotTimeSeries(timeHeadphoneBrand, top10brands_headphone, "Headphones", "2013-01", "2014-03", c(-0.5,0.7), 0)
plotTimeSeries(timeHeadphoneBrand, top10brands_headphone, "Headphones", "2013-01", "2014-03", c(-0.5,0.7), 0)
plotTimeSeries(timeCellphoneBrand, top10brands_cellphone, "Cellphones", "2011-01", "2014-07", c(-1.7,0.7), 0)
plotTimeSeries(timeCellphoneBrand, top10brands_cellphone, "Cellphones", "2011-02", "2014-07", c(-0.4,0.5), c("apple", "samsung")) + 
  # Add lines for product launch dates
  # iPhone 5
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-10")), color="red") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-10"), label = "Launch Apple iPhone 5", y = 0), color = "red", angle = 90, vjust = 3, hjust = 0) +
  # Samsung S3
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-05")), color="blue") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-05"), label = "Launch Samsung S3", y = 0), color= "blue", angle = 90, vjust = -4, hjust = 1)

# PRODUCT-BASED TIME SERIES FOR SENTIMENT
# We merge the same products with different colors to one product
productComparisonCellphone <- merged_topic_cellphone %>%
  mutate(title = replace(title, title == "Samsung Galaxy S3 i9300 16GB - Factory Unlocked International Version Blue", "Galaxy S3")) %>%
  mutate(title = replace(title, title == "Samsung Galaxy S3 i9300 16GB - Factory Unlocked International Version White", "Galaxy S3")) %>%
  mutate(title = replace(title, title == "Apple iPhone 5, Black 16GB (Unlocked)", "iPhone 5")) %>%
  mutate(title = replace(title, title == "Apple iPhone 5 16GB (White) - Unlocked", "iPhone 5"))

# CREATE TIME SERIES FOR PRODUCT-BASED-SENTIMENT SCORE
createProductTimeSeries <- function(input, brandList){
  # Review time as date
  input$reviewTime <- as.Date(input$reviewTime)
  # Get months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  # Aggregate data
  aggregated <- input %>%
    filter(brand %in% brandList$brand) %>%
    group_by(brand, title, reviewYear, reviewMonth) %>% 
    summarise(AvgScore = mean(scoreNN), AvgStar = mean(overall), n=n()) %>% 
    ungroup()
  # Glue date back together 
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep = "-")
  # and remove $year + $month
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTimeSeries-function
timeHeadphoneProduct <- createProductTimeSeries(merged_topic_headphone, top10brands_headphone)
timeCellphoneProduct <- createProductTimeSeries(productComparisonCellphone, top10brands_cellphone) # corrected data set

# PLOT TIME SERIES FOR AVERAGE SENTIMENT SCORE OF PRODUCTS
# PRODUCTS HAVE TO BE SELECTED, OTHERWISE TOO LONG LIST
plotProductTimeSeries <- function(input, productList, title, begin, end, ylim){
  # Filter for the products and time range
  input <- input %>% 
    filter(title %in% productList & date >= begin & date <= end)
  # Date as factor to place the vertical line for product launch
  input$date <- as.factor(input$date)
  # Plot data
  ggplot(data = input, aes(x = date, group = title, color = title)) + 
    geom_line(aes(y = AvgScore), size = 1) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Average Sentiment Over Time for", title, sep = " "), y = "Average Sentiment", x = "Period") + 
    scale_colour_hue(name = "Products", labels = productList) +
    ylim(ylim)
}
# Apply plotTimeSeries-function
plotProductTimeSeries(timeCellphoneProduct, c("iPhone 5", "Galaxy S3"), "Apple iPhone 5 vs. Samsung S3", "2012-05", "2014-07", c(-1,1))
  # # Add lines for product launch dates
  # # iPhone 5
  # geom_vline(aes(xintercept = which(levels(date) %in% "2012-10")), color = "red") +
  # geom_label(stat = "identity", aes(x = which(levels(date) %in% "2012-10"), label = "Launch Apple iPhone 5", y = 0), color = "red", angle = 90) +
  # # Samsung S3
  # geom_vline(aes(xintercept = which(levels(date) %in% "2012-05")), color = "blue") +
  # geom_label(stat = "identity", aes(x = which(levels(date) %in% "2012-05"), label = "Launch Samsung S3", y = 0), color= "blue", angle = 90)

# CREATE TIME SERIES FOR AVERAGE TOPIC DISTRIBUTION
createTopicSeries <- function(input, brandSelect){
  # Review time as date
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  # Filter for Brand
  input <- input %>% filter(brand %in% brandSelect$brand)
  # Aggregate
  aggregated <- input %>% 
    group_by(brand, reviewYear, reviewMonth, mainTopic) %>% 
    summarise(AvgScore = mean(scoreNN), AvgStar = mean(overall), n=n()) %>%
    ungroup()
  # Glue date back together
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep = "-")
  # and remove $year + $month
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTopicSeries-function
topicSeriesHeadphone <- createTopicSeries(merged_topic_headphone, top10brands_headphone)
topicSeriesCellphone <- createTopicSeries(merged_topic_cellphone, top10brands_cellphone)

# PLOT TIME SERIES FOR AVERAGE SENTIMENT SCORE
plotTopicSeries <- function(input, brandSelect, brandText, begin, end, ylim){
  # Filter for the products and time range
  input <- input %>%
    filter(brand == brandSelect, date >= begin & date <= end)
  # mainTopic as factor for x-axis
  input$mainTopic <- as.factor(input$mainTopic)
  # Plot data
  ggplot(data = input, aes(x = date, group = mainTopic, color = mainTopic)) + 
    geom_line(aes(y = AvgScore), size = 1) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Topic-Sentiment over Time for", brandText, sep = " "), y = "Average Sentiment", x = "Period") + 
    ylim(ylim) + 
    scale_colour_hue(name = "MainTopic", labels = c(1,2,3,4,5)) 
}
# Apply plotTimeSeries-function
plotTopicSeries(topicSeriesHeadphone, "beats", "Beats (Headphones)", "2012-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesHeadphone, "bose", "Bose (Headphones)", "2013-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesHeadphone, "sennheiser", "Sennheiser (Headphones)", "2013-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesCellphone, "apple", "Apple (Cellphones)", "2011-01", "2014-07", c(-1.1,1))
plotTopicSeries(topicSeriesCellphone, "samsung", "Samsung (Cellphones)", "2013-01", "2014-03", c(-1,1))

# CREATE TIME SERIES FOR AVERAGE TOPIC DISTRIBUTION FOR BRANDS
createProductTopicSeries <- function(input, brandSelect){
  # Review time as date
  input$reviewTime <- as.Date(input$reviewTime)
  # Get Months
  input$reviewMonth <- format(as.Date(input$reviewTime), "%m")
  # Get Year
  input$reviewYear <- format(input$reviewTime, format="%Y")
  # Filter for Brand
  input <- input %>% filter(brand %in% brandSelect$brand)
  # Aggregate
  aggregated <- input %>% 
    group_by(brand, title, reviewYear, reviewMonth, mainTopic) %>% 
    summarise(AvgScore = mean(scoreNN), AvgStar = mean(overall), n = n()) %>%
    ungroup()
  # Glue date back together
  aggregated$date <- paste(aggregated$reviewYear, aggregated$reviewMonth, sep = "-")
  # and remove $year + $month
  aggregated$reviewYear <- aggregated$reviewMonth <- NULL
  # Return aggregated data
  return(aggregated)
}
# Apply createTopicSeries-function
topicProductSeriesHeadphone <- createProductTopicSeries(merged_topic_headphone, top10brands_headphone)
topicProductSeriesCellphone <- createProductTopicSeries(merged_topic_cellphone, top10brands_cellphone)
topicProductSeriesCellphone <- createProductTopicSeries(productComparisonCellphone, top10brands_cellphone) # with corrected product data

# PLOT TOPIC-TIME SERIES FOR PRODUCT
plotProductTopicSeries <- function(input, productSelect, brandText, begin, end, ylim){
  # Filter for the products and time range
  input <- input %>%
    filter(title == productSelect, date >= begin & date <= end)
  # mainTopic as factor for x-axis
  input$mainTopic <- as.factor(input$mainTopic)
  # Plot data
  ggplot(data = input, aes(x = date, group = mainTopic, color = mainTopic)) + 
    geom_line(aes(y = AvgScore), size = 1) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Topic-Sentiment over Time for", brandText, sep = " "), y = "Average Sentiment", x = "Period") + 
    ylim(ylim) + 
    scale_colour_hue(name = "MainTopic", labels = c(1, 2, 3 , 4, 5)) 
}
# Apply plotTopicSeries-function
plotProductTopicSeries(topicProductSeriesCellphone, "Galaxy S3", "Samsung S3", "2012-05", "2013-12", c(-2,2))
plotProductTopicSeries(topicProductSeriesCellphone, "iPhone 5", "iPhone 5", "2012-10", "2013-12", c(-2,2))

plotProductTopicSeries(topicProductSeriesCellphone, "Apple iPhone 5s, Space Gray 16GB (Unlocked)", "iPhone 5s", "2012-01", "2014-12", c(-2,2))
plotProductTopicSeries(topicProductSeriesCellphone, "Apple iPhone 3G 8GB Black - Factory Unlocked", "iPhone 3G", "2010-01", "2014-12", c(-2,2))
plotProductTopicSeries(topicProductSeriesCellphone, "Sony Xperia SL LT26II Unlocked Android Phone--U.S. Warranty (Black)", "Sony Xperia SL", "2010-01", "2014-12", c(-2,2))
plotProductTopicSeries(topicProductSeriesCellphone, "Samsung Galaxy S3 GT-i8190 Mini Blue 8GB factory Unlocked 3G 900/1900/2100", "Samsung S3", "2012-01", "2014-12", c(-2,2))
plotProductTopicSeries(topicProductSeriesCellphone, "Sony Xperia Z C6603 Black Factory Unlocked LTE BANDS 1/3/5/7/8/20 International version - Original Sony phone", "Sony XPERIA Z", "2013-03", "2014-07", c(-2,2))
plotProductTopicSeries(topicProductSeriesHeadphone, "Beats Solo HD RED Edition On-Ear Headphones (Discontinued by Manufacturer)", "Beats Solo HD", "2011-01", "2012-01", c(-2,2))
plotProductTopicSeries(topicProductSeriesHeadphone, "Sennheiser  RS120 On-Ear 926MHz Wireless RF Headphones with Charging Cradle", "Sennheiser RS120", "2013-01", "2014-07", c(-2,2))
plotProductTopicSeries(topicProductSeriesHeadphone, "Bose IE2 Audio Headphones", "Bose IE2", "2012-01", "2013-03", c(-2,2))
