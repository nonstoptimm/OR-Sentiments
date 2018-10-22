# SENTIMENT OVER TIME SIMULATION
# Creation of time series plots for cellphones
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
    labs(y = "Average Sentiment", x = "Time Period") + 
    ylim(ylim) + 
    theme(text = element_text(size = 12, family = "LM Roman 10")) +
    scale_colour_hue(name = "Brands", labels = brandList$properBrand)
}
# Apply plotTimeSeries-function
plotTimeSeries(timeCellphoneBrand, top10brands_cellphone, "Cellphones", "2012-01", "2013-12", c(-1.7,1), 0)
plotTimeSeries(timeCellphoneBrand, top10brands_cellphone, "Cellphones (Apple vs. Samsung)", "2012-01", "2013-12", c(-0.4,0.5), c("apple", "samsung")) + 
  # Add lines for product launch dates
  # iPhone 5
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-10")), color="firebrick") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-10"), label = "Launch Apple iPhone 5", y = 0), color = "red", angle = 90, vjust = 3, hjust = 0, family = "LM Roman 10", label.size = 0.1) +
  # Samsung S3
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-05")), color="dodgerblue4") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-05"), label = "Launch Samsung S3", y = 0), color= "blue", angle = 90, vjust = -4, hjust = 0, family = "LM Roman 10", label.size = 0.1)

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
    labs(y = "Average Sentiment", x = "Time Period") + 
    scale_colour_hue(name = "Products") +
    theme(text = element_text(size = 12, family = "LM Roman 10")) +
    ylim(ylim)
}
# Apply plotTimeSeries-function
plotProductTimeSeries(timeCellphoneProduct, c("Galaxy S3", "iPhone 5"), "Apple iPhone 5 vs. Samsung Galaxy S3", "2012-05", "2013-12", c(-1,1)) +
  # Add lines for product launch dates
  # iPhone 5
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-10")), color = "red") +
  geom_label(stat = "identity", aes(x = which(levels(date) %in% "2012-10"), label = "Launch Apple iPhone 5", y = 0), color = "red", angle = 90, vjust = 3, hjust = 0, family = "LM Roman 10") +
  # Samsung S3
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-05")), color = "blue") +
  geom_label(stat = "identity", aes(x = which(levels(date) %in% "2012-05"), label = "Launch Samsung S3", y = 0), color= "blue", angle = 90, vjust = -4, hjust = 0, family = "LM Roman 10")

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
topicSeriesCellphone <- createTopicSeries(merged_topic_cellphone, top10brands_cellphone)

# PLOT TIME SERIES FOR AVERAGE SENTIMENT SCORE
plotTopicSeries <- function(input, brandSelect, brandText, begin, end, ylim, labels){
  # Filter for the products and time range
  input <- input %>%
    filter(brand == brandSelect, date >= begin & date <= end)
  input$date <- as.factor(input$date)
  # mainTopic as factor for x-axis
  input$mainTopic <- as.factor(input$mainTopic)
  # Plot data
  ggplot(data = input, aes(x = date, group = mainTopic, color = mainTopic)) + 
    geom_line(aes(y = AvgScore), size = 1) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Average Sentiment", x = "Time Period") + 
    scale_colour_hue(name = "MainTopic", labels = labels) +
    theme(text = element_text(size = 12, family = "LM Roman 10")) +
    ylim(ylim)
}
# Apply plotTimeSeries-function
plotTopicSeries(topicSeriesHeadphone, "beats", "Beats (Headphones)", "2012-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesHeadphone, "bose", "Bose (Headphones)", "2013-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesHeadphone, "sennheiser", "Sennheiser (Headphones)", "2013-01", "2014-03", c(-1,1))
plotTopicSeries(topicSeriesCellphone, "apple", "Apple (Cellphones)", "2012-01", "2013-12", c(-1.7,1.5), c("Purchase", "Software", "Hardware", "Brand Attitude", "Battery Life")) +
  # Add lines for product launch dates
  # iPhone 5
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-10")), color="red") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-10"), label = "Launch Apple iPhone 5", y = 0), color = "red", angle = 90, vjust = -3, hjust = 0, family = "LM Roman 10")
plotTopicSeries(topicSeriesCellphone, "samsung", "Samsung (Cellphones)", "2012-01", "2013-12", c(-1.7,1.5), c("Purchase", "Software", "Hardware", "Brand Attitude", "Battery Life")) +
  # Samsung S3
  geom_vline(aes(xintercept = which(levels(date) %in% "2012-05")), color="blue") +
  geom_label(stat = 'identity', aes(x = which(levels(date) %in% "2012-05"), label = "Launch Samsung S3", y = 0), color= "blue", angle = 90, vjust = -3, hjust = 0, family = "LM Roman 10")

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
topicProductSeriesCellphone <- createProductTopicSeries(merged_topic_cellphone, top10brands_cellphone)
topicProductSeriesCellphone <- createProductTopicSeries(productComparisonCellphone, top10brands_cellphone) # with corrected product data

# PLOT TOPIC-TIME SERIES FOR PRODUCT
plotProductTopicSeries <- function(input, productSelect, brandText, begin, end, ylim, labels){
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
    labs(y = "Average Sentiment Score", x = "Time Period") + 
    ylim(ylim) + 
    theme(text = element_text(size = 12, family = "LM Roman 10")) +
    scale_colour_hue(name = "MainTopic", labels = labels) 
}
# Apply plotProductTopicSeries-function
plotProductTopicSeries(topicProductSeriesCellphone, "Galaxy S3", "Samsung Galaxy S3", "2012-05", "2013-12", c(-2,2), c("Purchase", "Software", "Hardware", "Satisfaction", "Battery Life"))
plotProductTopicSeries(topicProductSeriesCellphone, "iPhone 5", "Apple iPhone 5", "2012-10", "2014-05", c(-2,2), c("Purchase", "Software", "Hardware", "Satisfaction", "Battery Life"))