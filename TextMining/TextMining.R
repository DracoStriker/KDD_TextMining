library(tm)
library(SnowballC)
library(FSelector)
library(quanteda)
library(caret)

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
  d <- tm_map (d, removeWords, stopwords("english"))
  
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
mistery <- Corpus(DirSource("mistery"), readerControl = list(reader=readPlain, language="en"))
romance <- Corpus(DirSource("romance"), readerControl = list(reader=readPlain, language="en"))

#create Train and Test set for group1
mistery.ri <- sort(sample(length(mistery), length(mistery)*.7))
mistery.train<-mistery[c(mistery.ri)]
mistery.test<-mistery[c(-mistery.ri)]


#create Train and Test set for group1
romance.ri <- sort(sample(length(romance), length(romance)*.7))
romance.train<-romance[c(romance.ri)]
romance.test<-romance[c(-romance.ri)] 

# Print lenght of datasets
length(mistery.train) #2089
length(mistery.test) #896
length(romance.train) #2718
length(romance.test) #1166


# Pre-processing 
mistery.train <- preprocess.simple(mistery.train)
romance.train <- preprocess.simple(romance.train)

# Concat corpus train
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
lexicon <- names(train.d)

# Class vector
train.c <- c(c(rep("Mistery", length(mistery.train)), c(rep("Romance", length(romance.train)))))

# Bind class feature
train.dc <- cbind(train.d, class=train.c)

# Shuffle data
train.dc <- train.dc[sample(nrow(train.dc)),]

# Information gain
#info.terms <- information.gain(class ~., train.d)
#which(info.terms$attr_importance > info.min)
#rownames(info.terms)

# Test set

#Merge Test sets
overall.test <-Corpus(VectorSource(c(content(mistery.test), content(romance.test))))

#test set with the same lexicon
test.d <- as.data.frame(as.matrix(DocumentTermMatrix(overall.test, control = list(dictionary = lexicon))))

#class labels for the test set
test.c <- c(c(rep("Mistery", length(mistery.test)), c(rep("Romance", length(romance.test)))))


#-----Classification
#Dtree
set.seed(565)
dtree <- train (train.d, train.c, method ='rpart')

#confusion matrix
conf.mx.dtree <-table(test.c, predict(dtree, test.d))
tp.dtree <- conf.mx [1,1]
fp.dtree <- conf.mx [2,1]
tn.dtree <- conf.mx [2,2]
fn.dtree <- conf.mx [1,2]

