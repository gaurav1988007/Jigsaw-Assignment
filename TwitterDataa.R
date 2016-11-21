
# TwitteR Sentimets Analysis ----------------------------------------------
# Installing Required packages for Twitter sentiments analysis
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("twitteR")


# Loading all required libraries ------------------------------------------

library("devtools")
library("twitteR")
library("plyr")
library("stringr")

# Setting Twitter Authentication ------------------------------------------
# Generated all the credential from personal twitter  account  ------------
# setting all the keys to the R object

api_key <- "r5cyBkpIrWs9UVAo9MsqtodfR" 

api_secret <- "DvI0oR29tzG8HiuTUTM9WRxFNi3wE3AbrorsGYuyClnqrlXili"

access_token <- "361304989-tEteoDf7gWelG5B7xr4v37WBaMknOsjb28UPKi9p"

access_token_secret <- "LHOI0xcIj1JWD4jBAPGSRfyzsZvrCSAN6a0AFsftQPCvo"

# calling setup_twitter_oauth() function to establish twitter connection with R
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Pulling twitter data from R , hash tag "Abortion"
tweets = searchTwitter("#trump", n=500)

# Creating R data frame after reading "ABortion" data from Twitter
#tweets$text1 <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
usableText=str_replace_all(df$text,"[^[:graph:]]", " ") 

df <- do.call("rbind", lapply(usableText, as.data.frame))
df1 <- do.call("rbind", lapply(tweets, as.data.frame))


colnames(df) <- c("tweet")

final_df <-cbind(df,df1) 
final_df <- final_df[,-2]
# changing columns name ---------------------------------------------------

colnames(final_df) <- c("tweet", "favorited", "favorited_count", "replyToSN", "created",
                  "truncated", "replyToSID", "id", "replyToUID", "statusSource", "screenName",
                  "retweetCount", "isRetweet", "retweeted", "longitude", "latitude")

# sanity check of DF - structure of the data ------------------------------

colnames(final_df) # checking columns 
nrow(final_df) # Number of rows in df
ncol(final_df) # Number of coumns in df
str(final_df) # Structure of the data
summary(final_df) # Summary of the data, levels, min,max, mean & median of df.


# Data Transformation - df ------------------------------------------------
# Splitting date & time into different columns  
final_df$date <- as.Date(final_df$created , "%Y-%m-%d")
final_df$time <- format(final_df$created, "%H:%M:%S")

# Calculating positive negative counts in tweets
Tweets.text = laply(tweets,function(t)t$getText())

pos = scan('D:/LocalGit/Jigsaw-Assignment/positive-words.txt', what='character', comment.char=';')
neg = scan('D:/LocalGit/Jigsaw-Assignment/negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('/d+', '', sentence)
    
    # and convert to lower case:
       
       sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '/s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
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


analysis = score.sentiment(Tweets.text, pos, neg)
table(analysis$score)


mean(analysis$score)
hist(analysis$score)

