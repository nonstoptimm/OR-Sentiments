# TOPIC-ORIENTED SENTIMENT ANALYSIS
# analyzeTopics.R
# Load required packages
library(dplyr)
library(ggplot2)

# CREATE BOXPLOT FOR TOPIC-SENTIMENT
topicBoxplot <- function(input){
  input$overall <- as.factor(input$overall)
  plot <- ggplot(input, aes(x=mainTopic, y=scoreNN)) + 
    geom_boxplot() +
    ggtitle("Topic-oriented Sentiment-Score")
  return(plot)
}
# Apply topicBoxplot-function
topicBoxplot()

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