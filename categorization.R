

### CATEGORIZATION
# Extract Subcategories of "Computers & Accessories"
merged_branded %>% 
  group_by(categories.0.1) %>% 
  filter(categories.0.0 == "Electronics")  %>% 
  filter(categories.0.1 == "Computers & Accessories") %>% 
  distinct(categories.0.2) %>% 
  pull(categories.0.2)


current_chunk <- merged_branded %>% 
  filter(categories.0.0 == "Electronics" & categories.0.1 == "Computers & Accessories")


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