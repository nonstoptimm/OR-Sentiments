### CATEGORIZATION

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

# Extract Sub-Subcategories (categories 0.2)
extract_cat02 <- function(cat00, cat01) { 
  merged_branded_full %>% 
    filter(categories.0.0 == cat00)  %>% 
    filter(categories.0.1 == cat01) %>% 
    distinct(categories.0.2) %>% 
    pull(categories.0.2)
}

# Extract Sub-Sub-Subcategories (categories 0.3)
extract_cat03 <- function(cat00, cat01, cat02) { 
  merged_branded_full %>% 
  filter(categories.0.0 == cat00)  %>% 
  filter(categories.0.1 == cat01) %>%
  filter(categories.0.2 == cat02) %>% 
  distinct(categories.0.3) %>% 
  pull(categories.0.3)
}



### EXECUTE FUNCTIONS HERE
extract_cat01("Electronics")
extract_cat02("Electronics", "Computers & Accessories")
extract_cat02("Electronics", "Accessories & Supplies")
extract_cat03("Electronics", "Accessories & Supplies", "Audio & Video Accessories")

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

# Where is Samsung? Where is Apple?
merged_branded_full %>% 
  filter(brand == "Beats")  %>%
  distinct(categories.0.1) %>%
  pull(categories.0.1)

  filter(categories.0.2 == "MP3 Players & Accessories")  %>%
  distinct(title) %>% 
  pull(title)

# Extract 
extract_current_chunk <- function(cat00, cat01) { 
  merged_branded_full %>% 
  filter(categories.0.0 == "Electronics" & categories.0.1 == "Computers & Accessories")
}

current_chunk <- merged_branded_full %>% 
  filter(categories.0.0 == "Electronics" & categories.0.1 == "Accessories & Supplies" & categories.0.2 == "Audio & Video Accessories" & categories.0.3 == "Headphones")
current_chunk_dup <- current_chunk

merged_branded %>% 
  group_by(categories.0.1) %>% 
  filter(categories.0.0 == "Electronics")  %>% 
  filter(categories.0.1 == "Computers & Accessories") %>% 
  filter(categories.0.2 == "Laptops")

### TELEVISION

merged_branded %>% 
  group_by(categories.0.1) %>% 
  filter(categories.0.0 == "Electronics")  %>% 
  filter(categories.0.1 == "Television & Video") %>% 
  distinct(categories.0.2 == "Television") %>% 
  pull(categories.0.2)


raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()