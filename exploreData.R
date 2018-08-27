# exploreData.R
library(dplyr)
library(ggplot2)

# COUNT UNIQUE HITS FOR VARIABLES 
countHit <- function(input){
  # Count hitcount for every variable  
  sapply(input, function(x) length(unique(x)))
}
# APPLY FORMULA
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

# Plot Pretty Histograms
plotHistogram <- function(input, title, xdesc, .) {
  ggplot(input, aes(.)) +
    geom_histogram(binwidth = 0.5) +
    labs(x = xdesc, y = "n") +
    geom_vline(aes(xintercept=mean(.), color="mean"), size=1) +
    scale_color_manual(name = "Statistics", values = c(mean = "red")) +
    ggtitle(paste("Histogram of ", title, sep = ""))
}
# Apply Formula
plotHistogram(prep_headphone_brand, "Overall Rating for Headphones", "Overall Rating", prep_headphone_brand$overall)
plotHistogram(prep_cellphone_brand, "Overall Rating for Cellphones", "Overall Rating", prep_cellphone_brand$overall)
plotHistogram(prep_coffee_brand, "Overall Rating for Coffee Makers", "Overall Rating", prep_coffee_brand$overall)
plotHistogram(prep_toaster_brand, "Overall Rating for Toaster", "Overall Rating", prep_toaster_brand$overall)

# DETECT MOST COMMON BRANDS
countBrands <- function(input){
  input %>%
    group_by(brand) %>% 
      summarise(reviewCount = n()) %>% 
        arrange(desc(reviewCount))
}
# See What's In it
top10brands_headphone <- countBrands(prep_headphone_brand)
countBrands(prep_cellphone_brand)
countBrands(prep_coffee_brand)
countBrands(prep_toaster_brand)

# DETECT MOST POPULAR PRODUCTS
countBrandsProduct <- function(input){
  input %>%
    group_by(title) %>% 
    summarise(reviewCount = n()) %>% 
    arrange(desc(reviewCount))
}
# Apply Function
countBrandsProduct(prep_headphone_brand)
countBrandsProduct(prep_coffee_brand)
countBrandsProduct(prep_toaster_brand)
countBrandsProduct(prep_cellphone_brand)

