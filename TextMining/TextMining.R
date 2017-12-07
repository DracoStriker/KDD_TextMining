library(tm)
library(SnowballC)
library(FSelector)
library(quanteda)
library(caret)
library(RWeka)
library(e1071)
library(wordcloud2)

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
  
  # remove punctuation
  d <- tm_map (d, removePunctuation, preserve_intra_word_dases = TRUE)
  
  # remove digits
  d <- tm_map (d, removeNumbers)
  
  # word stemming
  #d <- tm_map (d, stemDocument)
  
  # remove white spaces
  d <- tm_map (d, stripWhitespace)
  
  # remove stop words (174)
  d <- tm_map (d, removeWords, stopwords("english"))
  
  # remove common words
  #d <- tm_map (d, removeWords, c("film", "movie", "one", "really", "story",
  #                               "like", "even", "time", "can", "cast",
  #                               "good", "work", "character", "see", "now",
  #                               "just", "characters"))
  
  d
}

# Create the corpus
mistery <- Corpus(DirSource("mistery"), readerControl = list(reader=readPlain, language="en"))
romance <- Corpus(DirSource("romance"), readerControl = list(reader=readPlain, language="en"))

# Pre-processing 
mistery <- preprocess.simple(mistery)
romance <- preprocess.simple(romance)

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

# Concat corpus train
overall.train <- Corpus(VectorSource(c(content(mistery.train), content(romance.train))))

# Generate document term matrix
# Words between 4 and 15 letters
# 3 global occurences
# 2 local ocurrences
# TfIdf weight
dtm <- DocumentTermMatrix(overall.train, control = list(WordLengths = c(4, 15), bounds = list(global=c(2, Inf), local=c(2, Inf)), weighting = weightTfIdf))

# remove sparse terms (95% sparcity)
dtm <- removeSparseTerms(dtm, 0.99)

# MAtrix statistics
#dim(dtm)
#colnames (dtm) [1:20]

# Document term matrix to dataframe
train.d <- as.data.frame(as.matrix(dtm))

# Shuffle train data
#set.seed(423)
#train.d <- train.d[sample(1:nrow(train.d)),]

# Generate lexicon
lexicon <- names(train.d)

# Class vector
train.c <- c(c(rep("Mistery", length(mistery.train)), c(rep("Romance", length(romance.train)))))

# Shuffle class data (same seed as train data)
#set.seed(423)
#train.c <- train.c[sample(1:nrow(train.c)),]

# Bind class feature
train.dc <- cbind(train.d, class=train.c)

# Shuffle data
#train.dc <- train.dc[sample(nrow(train.dc)),]

#Remove zero lines! 
sum.train <- (c(rep(0, nrow(train.dc))))
train.dc <- cbind(train.dc, sum.train)
train.dc$sum.train <- rowSums(train.dc[1:(ncol(train.dc)-2)])
train.dc<- train.dc[which(train.dc$sum.train>0),]
train.dc<-within(train.dc, rm(sum.train))

#class vector 
train.c <- as.character(train.dc[,ncol(train.dc)])
train.d<-within(train.dc, rm(class))

############################
## Test Set
############################

# Merge Test sets
overall.test <- Corpus(VectorSource(c(content(mistery.test), content(romance.test))))

# Test set with the same lexicon
test.d <- as.data.frame(as.matrix(DocumentTermMatrix(overall.test, control = list(dictionary = lexicon))))

# Class labels for the test set

test.c <- c(c(rep("Mistery", length(mistery.test)), c(rep("Romance", length(romance.test)))))

############################
## Training
############################

set.seed(565)

# k-Nearest Neighbor
knn <- train(train.d, train.c, method = 'knn')

# Naive Bayes
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
nbayes <- NB(class ~., train.dc)
table(test.c, predict(nbayes, test.d))

# Neural Networks
nnets <- train(train.d, train.c, method = 'nnet')

# Support Vector Machine (Radial Kernel)
svmRad <- train(train.d, train.c, method = 'svmRadial')

# Support Vector Machine (Linear Kernel)
svmLin2 <- train(train.d, train.c, method = 'svmLinear2')

# Decision Tree
dtree <- train (train.d, train.c, method ='rpart')

############################
## Testing
############################

run.test <- function(prediction, test.c) {
  model.table <- table(test.c, prediction)
  print(model.table)
  right <- model.table[1,1]+model.table[2,2]
  wrong <- model.table[1,2]+model.table[2,1]
  accuracy <- right/(right + wrong)
  accuracy
}

knn.prediction <- predict(knn, test.d)
nbayes.prediction <- predict(nbayes, test.d)
nnets.prediction <- predict(nnets, test.d)
svmRad.prediction <- predict(svmRad, test.d)
svmLin2.prediction <- predict(svmLin2, test.d)
dtree.prediction <- predict(dtree, test.d)

voting.table <- data.frame(col1=c(nbayes.prediction), col2=c(nnets.prediction), col3=c(dtree.prediction))
voting.prediction <- rowSums(voting.table)
voting.prediction <- sapply(voting.prediction, function(x) ifelse(x <= 4, "Mystery", "Romance"))

run.test(knn.prediction, test.c)
run.test(nbayes.prediction, test.c)
run.test(nnets.prediction, test.c)
run.test(svmRad.prediction, test.c)
run.test(svmLin2.prediction, test.c)
run.test(dtree.prediction, test.c)
run.test(voting.prediction, test.c)

# Information gain
#info.terms <- information.gain(class ~., train.d)
#which(info.terms$attr_importance > info.min)
#rownames(info.terms)

#WORD CLOUD-------------
romance.train.dtm = DocumentTermMatrix(romance.train)
romance.train.df = as.data.frame(as.matrix(romance.train.dtm))
sort.romance.train.df = sort(colSums(romance.train.df), decreasing = TRUE)
romance.train.wc = data.frame(word = names(sort.romance.train.df), freq = sort.romance.train.df)
wordcloud2(head(romance.train.wc, 100), color = 'random-dark', fontWeight = 'normal', fontFamily = 'Consolas')

mistery.train.dtm = DocumentTermMatrix(mistery.train)
mistery.train.df = as.data.frame(as.matrix(mistery.train.dtm))
sort.mistery.train.df = sort(colSums(mistery.train.df), decreasing = TRUE)
mistery.train.wc = data.frame(word = names(sort.mistery.train.df), freq = sort.mistery.train.df)
wordcloud2(head(mistery.train.wc, 100), color = 'random-dark', fontWeight = 'normal', fontFamily = 'Consolas')
