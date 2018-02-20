### Pseudocode
# init
libraries <- c("tm", "dplyr", "class")
lapply(libraries, require, character.only = TRUE)

# Set options
options(stringsAsFactors = FALSE)

candidates <- c("bush", "obama")
pathname <- "input-data/speeches"
filelist <- list.files("input-data/speeches", full.names = TRUE)
df <- lapply(filelist, fread)

# clean text
cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus, stripWhitespace)
  corpus.tmp <- tm_map(corpus, tolower)
  corpus.tmp <- tm_map(corpus, removeWords, stopwords("english"))
  return(corpus.tmp)
}

# build TDM
generateTDM <- function(cand, path){
  s.dir <- sprintf("%s/%s", path, candidates)
  s.cor <- VCorpus(DirSource(directory = s.dir), readerControl = list(reader=readPlain))
  s.cor.cl <- CleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand, tdm = s.tdm)
}

tdm <- lapply(candidates, generateTDM, path = pathname)

