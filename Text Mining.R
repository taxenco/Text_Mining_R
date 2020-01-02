install.packages('tm')
install.packages("wordcloud") 
library(wordcloud) 
library(tm)
setwd('C:/Users/carlo/Desktop/Data science/ASDM/textmining')
dataset<-readLines("TMwithR.txt")

#Inspect the dataset 

names(dataset)
head(dataset)
tail(dataset)
summary(dataset)
str(dataset)

#create corpus
mycorpus <-Corpus(VectorSource(dataset))
mycorpus

# inspect the items in corpus.
inspect(mycorpus[1])
inspect(mycorpus[2])
inspect(mycorpus[3])
inspect(mycorpus[8])
mycorpus[8]

#lean up and convert the document in lower alphabets
mycorpus <-tm_map(mycorpus,tolower)
inspect(mycorpus[8])

# remove punctuations
mycorpus <-tm_map(mycorpus,removePunctuation)
inspect(mycorpus[8])

#remove numbers 
mycorpus <-tm_map(mycorpus,removeNumbers)

#remove stop words 

dataclean <-tm_map(mycorpus,removeWords,stopwords("english"))
inspect(dataclean[8])

#remove those white spaces

dataclean <-tm_map(dataclean,stripWhitespace)
inspect(dataclean[8])

#Document-Term Matrix (DTM). DTM is a matrix that lists all occurrences of words in the corpus

dtm <-TermDocumentMatrix(dataclean,control = list(minWordLength=c(1,Inf)))
dtm

#find frequent terms in a document-term or term-document matrix.
findFreqTerms(dtm,lowfreq = 10)

# Words frequency in the document matrix
termFrequency <-rowSums(as.matrix(dtm))
termFrequency

# frequency greater than 15. 

termFrequency <-subset(termFrequency,termFrequency>=15)
termFrequency

#Plot results 
barplot(termFrequency,las=2,col=rainbow(20))
# word cloud
wordfreq<-sort(termFrequency,decreasing = TRUE)
wordcloud(words = names(wordfreq),freq=wordfreq,max.words=100,min.freq = 5,random.order = F)
wordcloud(words = names(wordfreq),freq=wordfreq,max.words=100,min.freq = 5,random.order = F,colors = rainbow(20))
wordcloud(words = names(wordfreq),scale = c(6,.05),freq=wordfreq,max.words=100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))
wordcloud(words = names(wordfreq),freq=wordfreq,max.words=100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))
wordcloud(words = names(wordfreq),rot.per=0.50,scale = c(6,.05),freq=wordfreq,max.words=100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))

#Word distribution of 50 most frequent words.
barplot(wordfreq[1:50], xlab = "term", ylab = "frequency", las=2, col=heat.colors(50))