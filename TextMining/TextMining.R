library(tm)
library(Rstem)
library(SnowballC)

#setwd('GitHub/KDD_TextMining/TextMining')
setwd("C:/Users/Miguel Nóbrega/ProjetosGit/KDD_TextMining/TextMining")

#Create the corpus
mistery.train <- Corpus(DirSource("mistery"), readerControl = list(reader=readPlain, language="en"))
romance.train <- Corpus(DirSource("romance"), readerControl = list(reader=readPlain, language="en"))

#lenght of folders
length(mistery.train) #2985
length(romance.train) #3884

#pre-processing 
mistery.train.p <- preprocess.simple(mistery.train)
romance.train.p <- preprocess.simple(romance.train)

#o stemming está a remover palavras que podem ser úteis.. por exemplo dectetive estava a ser substituido por detect..

#creating document-term matrix
DocumentTermMatrix(mistery.train.p)

#palavras entre 4 e 15 letras, 3 occorencias globais e 2 ocorrencias locais
DocumentTermMatrix (mistery.train.p, control = list(WordLengths = c(4,15), bounds = list(global=c(3,Inf), local=c(2,Inf))))

dtm <- DocumentTermMatrix(mistery.train.p, control = list (bounds = list(global = c(2,Inf))))
dim(dtm)
colnames (dtm) [1:20]

#-------
txt <- readLines('text/romance/tt0034583_0.txt')

# split text by spaces
words <- strsplit(txt, ' ')

# words to lower case
words <- sapply(words, tolower)

# remove stop words
words <- setdiff(words, stopwords(kind = 'en'))

# remove punctuation
words <- gsub('[[:punct:]]+', '', words)

# remove digits
words <- gsub('[[:digit:]]+', '', words)

# word steming
words <- wordStem(words, language='english')

# remove empty words
words <- words[words != ""]

# aggregate words
words <- table(words)

##-----FUNCTIONS--------
preprocess.simple <- function(d){
  d<-tm_map (d, content_transformer(tolower))            #all lower case
  d<-tm_map (d, makewhitespace, "/")
  d<-tm_map (d, makewhitespace, "@")
  d<-tm_map (d, makewhitespace, "\\|")
  d<-tm_map (d, removeWords, stopwords (kind= "en"))     #remove stop words (174)
  d<-tm_map (d, removePunctuation, preserve_intra_word_dases = TRUE )  #removeponctuation
  d<-tm_map (d, removeNumbers)    #remove numbers
  #d<-tm_map (d, stemDocument)     #word stemmer
  d<-tm_map (d, stripWhitespace)  #remove whitespaces
}

#function to substitute symbols by whitespaces 
makewhitespace <- content_transformer(function (d, pattern) 
    gsub(pattern, " ", d))

