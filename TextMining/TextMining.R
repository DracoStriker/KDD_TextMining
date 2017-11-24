library(tm)
library(SnowballC)
library(FSelector)

#setwd('GitHub/KDD_TextMining/TextMining')
#setwd("C:/Users/Miguel N?brega/ProjetosGit/KDD_TextMining/TextMining")

# Substitute symbols by whitespaces 
makewhitespace <- content_transformer(function (d, pattern) gsub(pattern, " ", d))

# Text cleaning
preprocess.simple <- function(d) {
  
  # words to lower case
  d <- tm_map (d, content_transformer(tolower))
  
  # clean symbols
  d <- tm_map (d, makewhitespace, "/")
  d <- tm_map (d, makewhitespace, "@")
  d <- tm_map (d, makewhitespace, "\\|")
  
  # remove stop words (174)
  d <- tm_map (d, removeWords, stopwords (kind= "en"))
  
  # remove punctuation
  d <- tm_map (d, removePunctuation, preserve_intra_word_dases = TRUE)
  
  # remove digits
  d <- tm_map (d, removeNumbers)
  
  # word stemming
  #d <- tm_map (d, stemDocument)
  
  # remove white spaces
  d <- tm_map (d, stripWhitespace)
  
  d
}

# Create the corpus
mistery.train <- Corpus(DirSource("mistery"), readerControl = list(reader=readPlain, language="en"))
romance.train <- Corpus(DirSource("romance"), readerControl = list(reader=readPlain, language="en"))

# Print lenght of folders
length(mistery.train) #2985
length(romance.train) #3884

# Pre-processing 
mistery.train <- preprocess.simple(mistery.train)
romance.train <- preprocess.simple(romance.train)

# Concat corpus
overall.train <- Corpus(VectorSource(c(content(mistery.train), content(romance.train))))

# Generate document term matrix
# Words between 4 and 15 letters
# 3 global occurences
# 2 local ocurrences
# TfIdf weight
dtm <- DocumentTermMatrix(overall.train, control = list(WordLengths = c(4, 15), bounds = list(global=c(3, Inf), local=c(2, Inf)), weighting = weightTfIdf))

# remove sparse terms (95% sparcity)
dtm <- removeSparseTerms(dtm, 0.95)

# MAtrix statistics
#dim(dtm)
#colnames (dtm) [1:20]

# Document term matrix to dataframe
train.d <- as.data.frame(as.matrix(dtm))

# Generate lexicon
#lexicon <- names(train.d)

# Class vector
c.vector <- c(c(rep(0, length(mistery.train)), c(rep(1, length(romance.train)))))

# Bind class feature
train.d <- cbind(train.d, class=c.vector)

# Information gain
#info.terms <- information.gain(class ~., train.d)
#which(info.terms$attr_importance > info.min)
#rownames(info.terms)
