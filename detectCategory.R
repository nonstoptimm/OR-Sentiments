# detectCategory.R
# Script to detect the category dimensions, just for exploration
# Load required packages
library(dplyr)

### DEFINITION OF FILTER FUNCTIONS
# Extract Headcategories (categories 0.0)
merged_branded_full %>% 
  distinct(categories.0.0) %>% 
  pull(categories.0.0)

# Extract Subcategories (categories 0.1)
extract_cat01 <- function(cat00) { 
  merged_branded_full %>% 
  filter(categories.0.0 == cat00)  %>% 
  distinct(categories.0.1) %>% 
  pull(categories.0.1)
}
# Extract all Subcategories of the Super-Category
extract_cat01("Electronics")

# Extract Sub-Subcategories (categories 0.2)
extract_cat02 <- function(cat00, cat01) { 
  merged_branded_full %>% 
    filter(categories.0.0 == cat00)  %>% 
    filter(categories.0.1 == cat01) %>% 
    distinct(categories.0.2) %>% 
    pull(categories.0.2)
}
# Extract all Sub-Categories of the two Super-Categories
extract_cat02("Electronics", "Computers & Accessories")
extract_cat02("Electronics", "Accessories & Supplies")

# Extract Sub-Sub-Subcategories (categories 0.3)
extract_cat03 <- function(cat00, cat01, cat02) { 
  merged_branded_full %>% 
  filter(categories.0.0 == cat00)  %>% 
  filter(categories.0.1 == cat01) %>%
  filter(categories.0.2 == cat02) %>% 
  distinct(categories.0.3) %>% 
  pull(categories.0.3)
}
# Extract all Sub-Categories of the three Super-Categories
extract_cat03("Electronics", "Accessories & Supplies", "Audio & Video Accessories")

# SUBCATEGORIES OF "CELLPHONE & ACCESSORIES
# Extract Subcategories of "Cell Phones & Accessories" (categories 0.1)
merged_branded_full %>% 
  filter(categories.0.0 == "Cell Phones & Accessories")  %>%
  distinct(categories.0.1) %>% 
  pull(categories.0.1)

# Extract Subcategories of "Computers & Accessories" (categorie 0.2)
beats_list <- merged_branded_full %>% 
  filter(categories.0.0 == "Electronics")  %>% 
  filter(brand == "Beats")  %>% 
  select(title, categories.0.1, categories.0.2, categories.0.3, categories.0.4) %>% 
  distinct()
  
# Extract Subcategories of "Cell Phones & Accessories" (categorie 0.2)
merged_branded_full %>% 
  filter(categories.0.0 == "Cell Phones & Accessories")  %>% 
  filter(categories.0.1 == "Cell Phones") %>% 
  distinct(categories.0.2) %>% 
  pull(categories.0.2)

# Extract Subcategories of "Cell Phones & Accessories" (categorie 0.2)
merged_branded_full %>% 
  filter(categories.0.0 == "Electronics")  %>% 
  filter(categories.0.1 == "eBook Readers & Accessories") %>% 
  distinct(categories.0.2) %>%
  pull(categories.0.2)