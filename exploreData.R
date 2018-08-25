# exploreData.R
library(dplyr)
prep_headphone_brand <- as_tibble(prep_headphone_brand)

# Most Common Brands
countBrands <- function(input){
  input %>%
    group_by(brand) %>% 
      summarise(reviewCount = n()) %>% 
        arrange(desc(reviewCount))
}
# See What's In it
countBrands(prep_cellphone_brand)
countBrands(prep_headphone_brand)
countBrands(prep_coffee_brand)
countBrands(prep_toaster_brand)

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
    scale_color_manual(name = "statistics", values = c(mean = "red")) +
    ggtitle(paste("Histogram of ", title, sep = ""))
}
# Apply Formula
plotHistogram(prep_headphone_brand, "Overall Rating for Headphones", "Overall Rating", prep_headphone_brand$overall)
plotHistogram(prep_cellphone_brand, "Overall Rating for Cellphones", "Overall Rating", prep_headphone_brand$overall)
plotHistogram(prep_coffee_brand, "Overall Rating for Coffee Makers", "Overall Rating", prep_headphone_brand$overall)