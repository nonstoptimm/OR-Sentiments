# TF-IDF ON CATEGORY
tf_idf_category <- function(input) {
  input %>%
    bind_tf_idf(word, asin, n) %>%
    filter(n >= 20) %>%
    arrange(desc(tf_idf))
}
tf_idf_category(wf_cellphone)

# TF-IDF BASED ON ANY BRAND
tf_idf_brand <- function(input) {
  input %>%
    bind_tf_idf(word, brand, n) %>%
    filter(n >= 20 & !brand == word) %>%
    arrange(desc(tf_idf))
}
tf_idf_brand(wf_cellphone_brand)
  
# TF-IDF BASED ON SPECIAL BRAND
tf_idf_selectbrand <- function(input, filter) {
  input %>%
    bind_tf_idf(word, brand, n) %>%
    filter(n >= 20 & !brand == word) %>%
    arrange(desc(tf_idf)) %>%
    filter(brand == filter)
}
tf_idf_selectbrand(wf_cellphone_brand, "lenovo")

# Herausfinden, welche positiven sentimentwörter mit brands in verbindung stehen
tf_idf_sentiment <- function(input, filter) {
  input %>%
    bind_tf_idf(word, brand, n) %>%
    arrange(desc(tf_idf)) %>%
    filter(word %in% get_sentiments("afinn"))
}
tf_idf_sentiment(wf_cellphone_brand, "lenovo")