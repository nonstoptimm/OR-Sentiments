# EXPLORATORY DATA ANALYSIS
# exploreData.R
# Load required packages
library(dplyr)
library(ggplot2)

# COUNT UNIQUE HITS FOR VARIABLES 
countHit <- function(input){
  # Count hitcount for every variable  
  sapply(input, function(x) length(unique(x)))
}
# Apply countHit-function
# Prepared Datasets
countHit(prep_headphone_brand)
countHit(prep_cellphone_brand)
countHit(prep_coffee_brand)
countHit(prep_toaster_brand)
# Raw Metadata
countHit(meta_cellphone)
countHit(meta_electronics)
countHit(meta_homekitchen)
# Raw Review Data
countHit(raw_electronics)
countHit(raw_headphone)
countHit(raw_homekitchen)

# PLOT BARCHART
plotBarchart <- function(input1, input2, input3, input4){
  merged <- do.call("rbind", list(input1, input2, input3, input4)) # append all rows
  merged <- merged %>% 
    group_by(category, overall) %>% 
    summarise(count=n()) %>% 
    mutate(perc = count/sum(count)) %>% # Calculate Percentage
    ungroup()
  ggplot(merged, aes(x = factor(category), y = perc * 100, fill = factor(overall))) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Category", y = "Percent", fill = "Star Rating") +
    scale_fill_brewer(palette="RdYlGn") +
    # ggtitle("Percentage distribution of Star Ratings") +
    theme(text = element_text(size = 15, family = "LM Roman 10")) + # Latex Font
    coord_flip()
}
# Apply plotBarchart-function
plotBarchart(prep_coffee_brand, prep_toaster_brand, prep_cellphone_brand, prep_headphone_brand)

# PLOT HISTORGAM FOR OVERALL RATING
# Not included in Thesis document, as substituted by plotBarchart, but insightful
plotHistogram <- function(input, title, xdesc, .) {
  ggplot(input, aes(.)) +
    geom_histogram(binwidth = 0.5) +
    labs(x = xdesc, y = "n") +
    geom_vline(aes(xintercept=mean(.), color="mean"), size=1) +
    scale_color_manual(name = "Statistics", values = c(mean = "red")) +
    # theme(text = element_text(size = 12, family = "LM Roman 10")) + # Latex Font
    ggtitle(paste("Histogram of ", title, sep = ""))
}
# Apply plotHistogram-function
plotHistogram(prep_headphone_brand, "Overall Rating for Headphones", "Overall Rating", prep_headphone_brand$overall)
plotHistogram(prep_cellphone_brand, "Overall Rating for Cellphones", "Overall Rating", prep_cellphone_brand$overall)
plotHistogram(prep_coffee_brand, "Overall Rating for Coffee Makers", "Overall Rating", prep_coffee_brand$overall)
plotHistogram(prep_toaster_brand, "Overall Rating for Toaster", "Overall Rating", prep_toaster_brand$overall)

# DETECT MOST COMMON BRANDS
countBrands <- function(input){
  input <- input %>%
    group_by(brand) %>% 
      summarise(reviewCount = n(), meanStar = round(mean(overall),2)) %>% 
        arrange(desc(reviewCount))
  return(input[1:10,])
}
# Apply countBrands-function
top10brands_headphone <- countBrands(prep_headphone_brand)
top10brands_cellphone <- countBrands(prep_cellphone_brand)
top10brands_coffee <- countBrands(prep_coffee_brand)
top10brands_toaster <- countBrands(prep_toaster_brand)

# ADD PROPER BRAND NAMES
# Looks better for plots, separate column, as lower cased has to be used for joining
addProperNames <- function(input, properNames) {
  input$properBrand <- properNames
  input <- input[, c("properBrand", "brand", "reviewCount", "meanStar")]
  return(input)
}
# Apply addProperNames-function
top10brands_cellphone <- addProperNames(top10brands_cellphone, c("Samsung", "Apple", "Blackberry", "Motorola", "HTC", "LG", "Nokia", "Blu", "Sony", "Ocean Cross"))
top10brands_headphone <- addProperNames(top10brands_headphone, c("Sennheiser", "Sony", "Panasonic", "JVC", "JLAB", "Skullcandy", "Koss", "Beats", "Bose", "Audio-Technica"))
top10brands_toaster <- addProperNames(top10brands_toaster, c("Hamilton Beach", "Cuisinart", "T-Fal", "Black & Decker", "Oster", "Back to Basics", "Proctor Silex", "Waring", "West Bend", "Breville"))
top10brands_coffee <- addProperNames(top10brands_coffee, c("Cuisinart", "Keurig", "Mr. Coffee", "Hamilton Beach", "Black & Decker", "Bunn", "Bodum", "Aeropress", "Zojirushi", "Krups"))

# DETECT PRICES AND SET TRESHOLDS FOR SEGMENT GROUPS
averagePrice <- function(input, top_brands){
  input <- input %>%
    select(brand, title, price) %>%
    filter(brand %in% top_brands$brand[1:10]) %>%
    group_by(brand, title) %>%
    distinct() %>% 
    ungroup() %>%
    group_by(brand) %>%
    summarize(avgPrice = mean(na.omit(price))) # in case there are missing values
  input$avgPrice <- round(input$avgPrice, 2)
  return(input)
}
# Apply averagePrice-function
avgPriceCellphone <- averagePrice(prep_cellphone_brand, top10brands_cellphone)
avgPriceHeadphone <- averagePrice(prep_headphone_brand, top10brands_headphone)
avgPriceToaster <- averagePrice(prep_toaster_brand, top10brands_toaster)
avgPriceCoffee <- averagePrice(prep_coffee_brand, top10brands_coffee)

# SET PRICE GROUPS
# After exploration, the tresholds have to be set manually within the function
setPriceGroups <- function(brandList, brandPrices, tLow, tHigh) {
  brandPrices$priceGroup <- sapply(brandPrices$avgPrice, function(x) ifelse(x < tLow, "Low", ifelse(x < tHigh, "Medium", "High")))
  brandPrices$priceGroup <- as.factor(brandPrices$priceGroup)
  brandPrices$priceGroup <- factor(brandPrices$priceGroup,levels(brandPrices$priceGroup)[c(2,3,1)])
  brandList %>% 
    arrange(brand) %>% 
    left_join(brandPrices, by = "brand")
}
# Apply setPriceGroups-function
top10brands_cellphone <- setPriceGroups(top10brands_cellphone, avgPriceCellphone, 100, 300)
top10brands_headphone <- setPriceGroups(top10brands_headphone, avgPriceHeadphone, 30, 100)
top10brands_toaster <- setPriceGroups(top10brands_toaster, avgPriceToaster, 50, 100)
top10brands_coffee <- setPriceGroups(top10brands_coffee, avgPriceCoffee, 50, 100)

# DETECT MOST POPULAR PRODUCTS
countBrandsProduct <- function(input){
  input <- input %>%
    group_by(title) %>% 
    summarise(reviewCount = n()) %>% 
    arrange(desc(reviewCount))
  return(input[1:10,])  
}
# Apply countBrandsProduct-function
top10products_headphone <- countBrandsProduct(prep_headphone_brand)
top10products_cellphone <- countBrandsProduct(prep_cellphone_brand)
top10products_coffee <- countBrandsProduct(prep_coffee_brand)
top10products_toaster <- countBrandsProduct(prep_toaster_brand)

# DETECT ALL PRODUCTS FROM A BRAND
# No special use, just for exploration
detectProducts <- function(input, brandSelect){
  input %>% 
    filter(brand == brandSelect) %>%
    group_by(title) %>% 
    summarize(n = n()) %>%
    arrange(desc(n))
}
# Apply detectProducts-function
detectProducts(prep_cellphone_brand, "apple")
detectProducts(prep_cellphone_brand, "samsung")