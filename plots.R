# GGPLOTS
library(ggplot2)

# Histogram Price in $
ggplot(data=metadata, aes(metadata$price, na.rm = TRUE)) + 
  geom_histogram() +
  theme(strip.text=element_text(size=11)) +
  labs(title="Prices for Smartphones") +
  labs(x="Price  in US$", y="Amount of Products")

# Density Histogram Price 
ggplot(data=metadata, aes(metadata$price, na.rm = TRUE)) + 
  geom_histogram(aes(y =..density..)) +
  geom_density(col = 4) + 
  theme(strip.text=element_text(size=11)) +
  labs(title="Prices for Smartphones") +
  labs(x="Price in $", y="Amount of Products")

# Histogram Star Ratings
ggplot(data=merged_branded_full, aes(overall, na.rm = TRUE)) + 
  geom_histogram() +
  #geom_density() + 
#  theme(strip.text=element_text(size=11)) +
  labs(title="Distrubition of Star Ratings") +
  labs(x="Price", y="Amount of Products")

# SENTIMENT COUNT PLOT
plotSentiCount <- function(input, title) {
  input %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle(title) +
  coord_flip()
}
# Plot
plotSentiCount(sentiment_coffee_bing, "Sentiment Count for Coffee Products")

full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = chart_level )) +
  ylab("Song Count") +
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# Plot Word Frequency
plotWordFrequency <- function(input, unwanted) {
  input %>%
  filter(!word %in% unwanted) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = "blue") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") +
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()
}

# Plot Lexical Diversity
diversity_plot <- lexDiv_Cellphone %>%
  ggplot(aes(reviewTime, lex_diversity)) +
  geom_point(color = "blue",
             alpha = .4,
             size = 4,
             position = "jitter") +
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = year, y = lex_diversity), se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = "blue") +
  theme_classic() +
  theme_lyrics()