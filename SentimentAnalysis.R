#Setting the working directory

setwd("~/R_Twitter_SentimentAnalysis")

#Importing the libraries

library(devtools)
install_github('sentiment140', 'okugami79')
library(sentiment)
library(RTextTools)


library(twitteR)
library(plyr)
library(stringr)
library(plotly)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(ggplot2)
library(igraph)
library(sentimentr)
library(fpc)

#Twitter Authentication

Consumer_API_Key = "ZHfRWBUACBlTJZpErSbke4zgk"
Consumer_API_Secret = "QUImAEPtcQVoPfvfiyYMBAAnWP9ByZlkhuruJV9OBKQj9lAnXa"
Access_Token = "304496682-gE3DE3PrHuDvb4J412xsDeowYDCvRPP3NgON9QaA"
Access_Token_Secret = "anTKDpacUHe8CuPV3yICUEKzTfDwvlV5VKE07OFwqZOBR"

setup_twitter_oauth(Consumer_API_Key, Consumer_API_Secret, Access_Token, Access_Token_Secret)

#Getting tweets from twitter using a keyword

SampleTweets = searchTwitter('President', lang="en", n=250, resultType="recent")
SampleTweets_Text = sapply(SampleTweets, function(x) x$getText())

#Preparing the Text for sentimental analysis

# remove retweet entities
SampleTweets_Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", SampleTweets_Text)
# remove at people
SampleTweets_Text = gsub("@\\w+", "", SampleTweets_Text)
# remove punctuation
SampleTweets_Text = gsub("[[:punct:]]", "", SampleTweets_Text)
# remove numbers
SampleTweets_Text = gsub("[[:digit:]]", "", SampleTweets_Text)
# remove html links
SampleTweets_Text = gsub("http\\w+", "", SampleTweets_Text)

# remove unnecessary spaces
SampleTweets_Text = gsub("[ \t]{2,}", "", SampleTweets_Text)
SampleTweets_Text = gsub("^\\s+|\\s+$", "", SampleTweets_Text)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
SampleTweets_Text = sapply(SampleTweets_Text, try.error)

# remove NAs in some_txt
SampleTweets_Text = SampleTweets_Text[!is.na(SampleTweets_Text)]
names(SampleTweets_Text) = NULL

#Performing the Sentiment Analysis

Neg.words = scan(file = "~/R_Twitter_SentimentAnalysis/Documentation/negative-words.txt", what = "character")
Pos.words = scan(file = "~/R_Twitter_SentimentAnalysis/Documentation/positive-words.txt", what = "character")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

Sentiment_Score = score.sentiment(SampleTweets_Text, Pos.words, Neg.words)

#Barplot of the number of words

positive = sum(Sentiment_Score$score > 0)
negative = sum(Sentiment_Score$score < 0)
neutral = sum(Sentiment_Score$score == 0)

SentimentAnalysis = c(positive, neutral, negative)

