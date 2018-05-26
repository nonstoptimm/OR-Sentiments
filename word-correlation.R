library(ggraph)
library(igraph)

# WORD CORRELATIONS
brand_cors <- function(input) {
  input %>% 
    filter(n >= 100) %>%
    pairwise_cor(brand, word, n, sort = TRUE)
}
# Apply function
brand_cors(wf_cellphone_brand)
brand_cors(wf_coffee_brand)
brand_cors(wf_toaster_brand)
brand_cors(wf_headphone_brand)

# Set seed for plot
set.seed(10)
# PLOT WORD CORRELATIONS
plotCors <- function(input) {
  input %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
}
# Apply function
plotCors(brand_cors(wf_cellphone_brand))
plotCors(brand_cors(wf_coffee_brand))
plotCors(brand_cors(wf_toaster_brand))
plotCors(brand_cors(wf_headphone_brand))