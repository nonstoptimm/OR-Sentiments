# TOPIC-ORIENTED SENTIMENT ANALYSIS
# Analyze created topics with regard to sentiment score
# analyzeTopics.R
# Load required packages
library(dplyr)
library(ggplot2)

# CREATE BOXPLOT FOR TOPIC-SENTIMENT
topicBoxplot <- function(input, category, topBrands, brandSelect, ylim, labels){
  input$mainTopic <- as.factor(input$mainTopic) # as factor for boxplot
  if(brandSelect != "") {
  properBrand <- topBrands %>% # as we want to have pretty names
    filter(brand == brandSelect) %>%
    select(properBrand)
  input <- input %>% 
    filter(brand == brandSelect)
  category <- paste(category, ' (Brand \"', properBrand, '\")', sep = "")
  }
  ggplot(input) + 
      aes(x = mainTopic, y = scoreNN) + 
      xlab("Main Topic") +
      ylab("Sentiment Score (ML-based)") +
      geom_boxplot() +
      # ggtitle(paste("Topic-oriented Sentiment-Scores for", category, sep = " ")) +
      theme(text = element_text(size = 15, family = "LM Roman 10")) +
      scale_x_discrete(labels = labels) +
      ylim(ylim)
}
# Apply topicBoxplot-function
topicBoxplot(merged_topic_cellphone, "Cellphones", "", "", c(-4.3,2.5), c("Purchase", "Software", "Hardware", "Brand", "Battery"))
# For Every Cellphone-Brand
lapply(top10brands_cellphone$brand, function(brandSelect) topicBoxplot(merged_topic_cellphone, "Cellphone", top10brands_cellphone, brandSelect, c(-4.3,2.5), c("Purchase", "Software", "Hardware", "Brand", "Battery")))

topicBoxplot(merged_topic_headphone, "Headphones", "", "", c(-4.3,2.5), c("Hardware", "Purchase", "User Experience", "Brand", "Aesthetics"))
# For Every Headphone-Brand
lapply(top10brands_headphone$brand, function(brandSelect) topicBoxplot(merged_topic_headphone, "Headphone", top10brands_headphone, brandSelect, c(-4.3,2.5), c("Hardware", "Purchase", "User Experience", "Brand", "Aesthetics")))

topicBoxplot(merged_topic_toaster, "Toasters", "", "", c(-4.1,3))
# For Every Toaster-Brand
lapply(top10brands_toaster$brand, function(brandSelect) topicBoxplot(merged_topic_toaster, "Toaster", top10brands_toaster, brandSelect, c(-4.1,3)))

topicBoxplot(merged_topic_coffee, "Coffee", "", "", c(-4.4,2.7))
# For Every Coffee Maker-Brand
lapply(top10brands_coffee$brand, function(brandSelect) topicBoxplot(merged_topic_coffee, "Coffee", top10brands_coffee, brandSelect, c(-4.1,3)))

# MEAN-SCORE FOR EACH TOPIC
meanScoreTopic <- function(input, brandSelect){
  if(brandSelect != "") {
    input <- input %>% 
      filter(brand == brandSelect)
  }
  input %>% 
    group_by(mainTopic) %>%
    summarise(AvgScore=mean(scoreNN))
}
# Apply meanScoreTopic-function
topicScoreCellphone <- lapply(top10brands_cellphone$brand, function(brandSelect) meanScoreTopic(merged_topic_cellphone, brandSelect))
topicScoreHeadphone <- lapply(top10brands_headphone$brand, function(brandSelect) meanScoreTopic(merged_topic_headphone, brandSelect))
topicScoreToaster <- lapply(top10brands_toaster$brand, function(brandSelect) meanScoreTopic(merged_topic_toaster, brandSelect))
topicScoreCoffee <- lapply(top10brands_coffee$brand, function(brandSelect) meanScoreTopic(merged_topic_coffee, brandSelect))

# KRUSKAL WALLIS TEST
kwTest <- function(input){
  input$mainTopic <- as.factor(input$mainTopic)
  kruskal.test(scoreNN ~ mainTopic, data = input)
}
# Apply kwTest-function combined with sample_n (70 data sets)
kwTest(sample_n(merged_topic_cellphone, 70))
kwTest(sample_n(merged_topic_headphone, 70))
kwTest(sample_n(merged_topic_toaster, 70))
kwTest(sample_n(merged_topic_coffee, 70))
