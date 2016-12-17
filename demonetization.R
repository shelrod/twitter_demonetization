
library(syuzhet)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)



demonitization <- read.csv("C:/Data Analysis Kaggle/Twitter Demonitization/demonetization-tweets.csv" , header = TRUE , stringsAsFactors = FALSE)

mySentiment <- get_nrc_sentiment(demonitization$text)

demonitization <- cbind(demonitization, mySentiment)

sentimentTotals <- data.frame(colSums(demonitization[,c(16:25)]))

#Renaming the columns to "Count"
names(sentimentTotals) <- "count"

#Renaming the columns to "Sentiment" and binding "Count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)

#Renaming the rownames to NULL 
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")



wordCorpus <- Corpus(VectorSource(demonitization$text))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))

wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("the", "with", "has", "4yo"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, stemDocument)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = wordCorpus, scale=c(5,0.5), max.words=20, random.order=FALSE,
          rot.per=0.5, use.r.layout=FALSE, colors=pal)
