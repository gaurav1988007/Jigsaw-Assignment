
# TwitteR Sentimets Analysis ----------------------------------------------
# Installing Required packages for Twitter sentiments analysis
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("twitteR")


# Loading all required libraries ------------------------------------------

library(devtools)
library("twitteR")
library("plyr")

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
tweets = searchTwitter("#abortion", n=200)

# Creating R data frame after reading "ABortion" data from Twitter
df <- do.call("rbind", lapply(tweets, as.data.frame))

# changing columns name ---------------------------------------------------

colnames(df) <- c("tweets", "favorited", "favorited_count", "replyToSN", "created",
                  "truncated", "replyToSID", "id", "replyToUID", "statusSource", "screenName",
                  "retweetCount", "isRetweet", "retweeted", "longitude", "latitude")

# sanity check of DF - structure of the data ------------------------------

colnames(df) # checking columns 
nrow(df) # Number of rows in df
ncol(df) # Number of coumns in df
str(df) # Structure of the data
summary(df) # Summary of the data, levels, min,max, mean & median of df.


# Data Transformation - df ------------------------------------------------
# Splitting date & time into different columns  
df$date <- as.Date(df$created , "%Y-%m-%d")
df$time <- format(df$created, "%H:%M:%S")
