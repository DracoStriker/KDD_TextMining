library(tm)
library(Rstem)

#setwd('GitHub/KDD_TextMining/TextMining')

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
