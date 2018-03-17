#install.packages("curl")
library(curl) 

library(twitteR)
library(ROAuth)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(scales)
## Install Packages for replacemen of package sentiment
#install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer)
#install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org",dependencies=TRUE)
library(tm.plugin.sentiment)


## Connect to twitter 
setup_twitter_oauth('XX', 'XX', access_token=NULL, access_secret=NULL)



#Let's collect some tweets containing the term "Football" 
# harvest some tweets
autonomous_tweets = searchTwitter("Football", n=197, lang="en")
autonomous_tweets[[118]]=NULL
autonomous_txt = sapply(autonomous_tweets, function(x) x$getText()) 
summary(autonomous_txt)
# Remove retweets
autonomous_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", autonomous_txt)

head(autonomous_txt)


#Remove email addresses from tweets extracted:                                                        
  
  autonomous_txt = gsub("@\\w+", "", autonomous_txt)

#Remove punctuations:                                                                                                    
  
autonomous_txt = gsub("[[:punct:]]", "", autonomous_txt)
#x <- "a1~????????????????????????????????????????????????????????????????????????"
#gsub("[[:punct:]]", " ", x)

#Because we are only interested in mining the texts, we should remove numbers as well:      
  
autonomous_txt = gsub("[[:digit:]]", "", autonomous_txt)

#Also remove website links from the tweets extracted:                                      
  
autonomous_txt = gsub("http\\w+", "", autonomous_txt)

#Trim texts by removing unnecessary spaces:                                                                
  
autonomous_txt = gsub("[ \t]{2,}", "", autonomous_txt) #Removes spaces
autonomous_txt = gsub("^\\s+|\\s+$", "", autonomous_txt) #Removes tabs
autonomous_txt = gsub("???","", autonomous_txt)#Removes 
#Remove NA terms and column headers:                                                                    
  
autonomous_txt = autonomous_txt[!is.na(autonomous_txt)]
names(autonomous_txt) = NULL

## Remove non english words
NonEng <- grep("autonomous_txt", iconv(autonomous_txt, "latin1", "ASCII", sub="autonomous_txt"))
autonomous_txt<-autonomous_txt[-NonEng]

## Convert text into corpus
text.corpus <- Corpus(VectorSource(autonomous_txt))

tm_tag_score<-tm_term_score

#install.packages("devtools")

library(devtools)

#install_github("mannau/tm.plugin.sentiment")

## Score Function

#install.packages("score")

library(score)

text.corpus <- score(text.corpus)
meta(text.corpus) 


sentiment_result <- meta(text.corpus)
#writeLines(as.character(ncorpus), con="corpus.txt")
#ncorpus
#sentiment_result <- ncorpus

sentiment_result <- stack(sentiment_result)

uni <- unique(sentiment_result[, "values"])
uni

## Histogram

ggplot(sentiment_result,aes(sentiment_result$values)) +
      geom_histogram(col="orange",fill="green",alpha=0.2) + xlab("Polarity") + ylab("Count") + 
      ggtitle("Histogram for Polarity") + scale_y_continuous(breaks=pretty_breaks(n=10),limits=c(0,1000))


newcorpus <- tm_map(text.corpus, stemDocument)
 nncorpus <- tm_map(newcorpus, content_transformer(tolower))

 
 # Words to be removed 
 nncorpus <- tm_map(nncorpus, removeWords, 
                    c("to","the","this","that",
                       "just","our","me","you","have","not",
                       "are","get","from","and","for","i","may","with",
                       "which","what","how"))
 
 
 ## Plot word cloud
 wordcloud(nncorpus,scale = c(3,.5), max.words=70, random.order = FALSE, colors=brewer.pal(12,"Paired"))
 

 