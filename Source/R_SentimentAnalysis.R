#Setting the working directory

setwd("G:\\Masters\\Data Analytics with R\\Project\\Twitter_SentimentAnalysis")

#Importing the libraries

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
library(fpc)

#Twitter Authentication

Consumer_API_Key = "ZHfRWBUACBlTJZpErSbke4zgk"
Consumer_API_Secret = "QUImAEPtcQVoPfvfiyYMBAAnWP9ByZlkhuruJV9OBKQj9lAnXa"
Access_Token = "304496682-gE3DE3PrHuDvb4J412xsDeowYDCvRPP3NgON9QaA"
Access_Token_Secret = "anTKDpacUHe8CuPV3yICUEKzTfDwvlV5VKE07OFwqZOBR"

setup_twitter_oauth(Consumer_API_Key, Consumer_API_Secret, Access_Token, Access_Token_Secret)

#Location information for downloading the tweets

N=2000  # tweets to request from each query
S=250  # radius in miles

lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

########cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul


#Getting tweets from twitter using a keyword

Tweet = do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('President',
                                                                      lang="en", n=N, resultType="recent",
                                                                      geocode = paste(lats[i], lons[i], paste0(S,"mi"),sep=","))))

#Getting the Latituted information of the tweets

TweetLat = sapply(Tweet, function(x) as.numeric(x$getLatitude()))
TweetLat = sapply(TweetLat, function(z) ifelse(length(z)==0,NA,z))  

#Getting the longitude information of the tweets

TweetLon=sapply(Tweet, function(x) as.numeric(x$getLongitude()))
TweetLon=sapply(TweetLon, function(z) ifelse(length(z)==0,NA,z))

#Getting the time stamp of the tweets

TweetDate = lapply(Tweet, function(x) x$getCreated())
TweetDate = sapply(TweetDate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

#Getting the Text Information

TweetText = sapply(Tweet, function(x) x$getText())
TweetText = unlist(TweetText)

#Getting the Retweet Information

isretweet = sapply(Tweet, function(x) x$getIsRetweet())
retweeted = sapply(Tweet, function(x) x$getRetweeted())
retweetcount = sapply(Tweet, function(x) x$getRetweetCount())

#Getting the favourite count

favoritecount = sapply(Tweet, function(x) x$getFavoriteCount())
favorited = sapply(Tweet, function(x) x$getFavorited())

TweetData=as.data.frame(cbind(tweet = TweetText, date = TweetDate, lat = TweetLat, lon = TweetLon,
                         isretweet = isretweet, retweeted = retweeted, retweetcount = retweetcount, favoritecount = favoritecount, favorited = favorited))

#Getting the Address of the tweets

TweetData_Geo = filter(TweetData, !is.na(lat),!is.na(lon))
LonLat=select(TweetData,lon,lat)

result <- do.call(rbind, lapply(1:nrow(LonLat),
                                function(i) revgeocode(as.numeric(LonLat[i,1:2]))))

#Seperating the address information

TweetData1 = lapply(result,  function(x) unlist(strsplit(x,",")))
address = sapply(TweetData1,function(x) paste(x[1:3],collapse=''))
city = sapply(TweetData1,function(x) x[2])
stzip = sapply(TweetData1,function(x) x[3])
zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
state = str_extract(stzip,"[:alpha:]{2}")
TweetData1 = as.data.frame(list(address=address, city=city, zipcode=zipcode, state=state))

#Concatenating this data to the original one

TweetData_Geo = cbind(TweetData, TweetData1)

#Text Mining on Tweets

TweetInfo=TweetData_Geo$tweet
TweetList=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
TweetList=lapply(tweet, function(x) gsub("htt.*",' ',x))
TweetInfo=unlist(TweetInfo)
TweetData_Geo$tweet= TweetInfo

positives = scan('positive-words.txt',what = 'character',comment.char = ';')
negatives = scan('negative-words.txt',what = 'character',comment.char = ';')


#Calculating the Sentiment Score

sentiment_scores = function(tweets, positive_words, negative_words, .progress='none')
  {
    scores = laply(tweets, function(tweet, positive_words, negative_words)
      {
        tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
        tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
        tweet = gsub('\d+', '', tweet)          # remove digits
                   
        #Let's have error handling function when trying tolower
         tryTolower = function(x)
           {
        #create missing value
         y = NA
        #tryCatch error
          try_error = tryCatch(tolower(x), error=function(e) e)
          #if not an error
            if (!inherits(try_error, "error"))
              y = tolower(x)
                #result
                 return(y)
            }
        # use tryTolower with sapply
        tweet = sapply(tweet, tryTolower)
        # split sentence into words with str_split function from stringr package
        word_list = str_split(tweet, "\s+")
        words = unlist(word_list)
        # compare words to the dictionaries of positive & negative terms
        positive.matches = match(words, positive_words)
        negative.matches = match(words, negative_words)
        # get the position of the matched term or NA
        # we just want a TRUE/FALSE
        positive_matches = !is.na(positive_matches)
        negative_matches = !is.na(negative_matches)
        # final score
        score = sum(positive_matches) - sum(negative_matches)
        return(score)
    }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}

score = sentiment_scores(TweetInfo, positives, negatives, .progress='text')
TweetData_Geo$score=score


# Create corpus
corpus=Corpus(VectorSource(TweetData$tweet))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

Keyword.text = lapply(Keyword,function(t)t$getText())
Keyword.text

Keyword.text = data.frame(Keyword.text)







