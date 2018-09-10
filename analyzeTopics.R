# TOPIC-ORIENTED SENTIMENT ANALYSIS
# analyzeTopics.R
# Load required packages
library(dplyr)
library(ggplot2)

# CREATE BOXPLOT FOR TOPIC-SENTIMENT
topicBoxplot <- function(input, category, brand){
  input$mainTopic <- as.factor(input$mainTopic)
  if(brand == ""){
    plot <- ggplot(input, aes(x=mainTopic, y=scoreNN)) + 
      geom_boxplot() +
      ggtitle(paste("Topic-oriented Sentiment-Score", category, sep = " ")) +
      ylim(-3, 3)
  }
  plot <- input %>% 
    filter(brand == brand) %>%
    ggplot(input, aes(x=mainTopic, y=scoreNN)) + 
    geom_boxplot() +
    ggtitle(paste("Topic-oriented Sentiment-Score for", brand, category, sep = " ")) +
    ylim(-3, 3)
  return(plot)
}
# Apply topicBoxplot-function
topicBoxplot(merged_topic_cellphone, "Cellphones", "apple")
apple <- merged_topic_cellphone %>% filter(brand == "apple")
samsung <- merged_topic_cellphone %>% filter(brand == "samsung")
lenovo <- merged_topic_cellphone %>% filter(brand == "lenovo")
sony <- merged_topic_cellphone %>% filter(brand == "sony")
google <- merged_topic_cellphone %>% filter(brand == "google")

topicBoxplot(apple)
topicBoxplot(samsung)
topicBoxplot(lenovo)
topicBoxplot(sony)
topicBoxplot(google)
topicBoxplot(nokia)

# SCORE-DENSITY-PLOT FOR EACH TOPIC
densityTopic <- function(input, category, topic){
  input %>% 
    filter(mainTopic == topic) %>%
    ggplot(diamonds, aes(scoreNN)) +
      geom_density() +
      ggtitle(paste("Density plot of ", category, "-Topic Nr.", topic, sep=""))
}
# Apply densityTopic-function
densityTopic()

# KOLMOGOROV-TEST
ksTestTopic <- function(input, topic){
  input <- input %>% filter(mainTopic == topic)
  test <- ks.test(input$scoreNN, "pnorm")  
}
# Apply kTestTopic-function
ksTestTopic()

# MEAN-SCORE FOR EACH TOPIC
meanScoreTopic <- function(input){
  input %>% 
    group_by(mainTopic) %>%
    summarise(AvgScore=mean(scoreNN))
}
# Apply meanScoreTopic-function

# KRUSKAL WALLIS TEST
kwTest <- function(input){
  input %>%
    
}
# Apply kwTest-function
kwTest()