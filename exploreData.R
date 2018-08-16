# exploreData.R

# Plot Pretty Histograms
plotHistogram <- function(input, title, xdesc, .) {
  ggplot(input, aes(.)) +
    geom_histogram(binwidth = 0.5) +
    labs(x = xdesc, y = "n") +
    geom_vline(aes(xintercept=mean(.), color="mean"), size=1) +
    scale_color_manual(name = "statistics", values = c(mean = "red")) +
    ggtitle(paste("Histogram of ", title, sep = ""))
}
plotHistogram(prep_headphone_brand, "Overall Rating for Headphones", "Overall Rating", prep_headphone_brand$overall)
