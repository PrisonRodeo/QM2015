##################################################
# Various analyses for the QM workshop, Oxford, 
# January 8, 2015.
#
# setwd("~/Dropbox/Oxford 2015")
##################################################
# Preliminaries: Packages:

library(httr)
library(twitteR)
library(tm)
library(wordcloud)

# Options:
options(scipen=999)
options(digits=5)

##################################################
# Google Trends of "big data"
#

BDTrend<-read.csv(url("http://dl.dropboxusercontent.com/u/7698439/BigDataTrends.csv"),
                  header=TRUE,stringsAsFactors=FALSE)
# Local:
# BDTrend<-read.csv("url("BigDataTrends.csv"),
#      header=TRUE,stringsAsFactors=FALSE)
#
# Note: Can also use tweaked version of data created
# by these four lines:
#
# require(devtools) 
# install_github('googletrend','okugami79')
# library(googletrend) 
# BDTrendData<- gettrend(keyword='"big data"',simple=TRUE,plot=FALSE)

BDTrend$weekend<-substr(BDTrend$date,start=14,stop=23)
BDTrend$weekend<-as.Date(BDTrend$weekend)
t<-seq(1:nrow(BDTrend))
fit<-lm(BDTrend$score~t+I((t^2)/100))

pdf("BDTrend.pdf",6,5)
par(mar=c(4,4,2,2))
plot(BDTrend$weekend,BDTrend$score,t="l",lwd=2,
     xlab="Year",ylab="Normalized Frequency")
lines(BDTrend$weekend,fit$fitted.values,lwd=2,lty=2,col="red")
title(main="Google Trends Figures for 'big data'")
dev.off()

##############################
# Pulling data from Twitter. Note that (a) you need
# to created a dummy Twitter app, (b) this needs to be
# run in the console, not RStudio, (c) the PIN 
# has to be typed, not cut-and-pasted, and (d) the
# auth codes have been changed here to protect my
# Twitter account. See here for some detailed instructions:
#
# http://decisionstats.com/2013/09/11/using-twitter-data-with-r/

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
APIKey<-"ieDpmBzQ1khjVlbrR5RoAZL09"
APISecret<-"PnhfyeA3XbNOxMswN68z9hxThm5e5Z1AQ9dTYjvQllf0VGQQwk"
AccessToken<-"15040999-XQt9uVi5l1gLzMeDp86mcy9BA0vZgbCzTpobLD9op"
AccessSecret<-"XDd0mPEb9uPcFbqD7VeRVkJ09nWQ83PA5asbCz874x8kL"

twitCred <- OAuthFactory$new(consumerKey=APIKey,
                             consumerSecret=APISecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake

# Check handshake:

registerTwitterOAuth(twitCred)

# Pull data:

KaneTweets<-searchTwitter('Kane',geocode='51.603211,-0.066430,10mi',
              since='2014-12-31',n=5000, retryOnRateLimit=1)

KaneTweetsData<-twListToDF(KaneTweets) # Turn into data frame

# Cumulative frequency:

pdf("KaneCumFreq.pdf",7,6)
with(KaneTweetsData, plot(created,as.factor(created),t="l",
                     lwd=2,main="",xlab="Date",
                     ylab="Cumulative Tweets"))
dev.off()

# Histogram:

pdf("KaneRTHistogram.pdf",7,6)
with(KaneTweetsData, hist(retweetCount,breaks=16,main="",
                     ylab="Frequency",xlab="Number of Retweets",
                     col="grey"))
dev.off()

# Word cloud created using Wordle, because I'm lazy; could also
# use the new wordcloud R package
