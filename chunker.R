

rawdata <- ndjson::stream_in("input-data/raw_electronics_reviews.json")
rawdata <- rawdata[, c("asin", "helpful.0", "helpful.1", "overall", "reviewText", "reviewTime", "reviewerID", "summary")]
rawdata_subset <- rawdata[1:100000, ]
rawdata_subset$reviewText <- as.character(rawdata_subset$reviewText)
write.csv(rawdata_subset, file = "input-data/rawdata_subset.csv")

metadata <- ndjson::stream_in("input-data/raw_electronics_meta.json")
metadata <- metadata[, c("asin", "categories.0.0", "categories.0.1", "categories.0.2", "categories.0.3", "description", "title", "price", "categories.0.4", "brand")]
write.csv(metadata, file = "input-data/metadata_filtered.csv")

