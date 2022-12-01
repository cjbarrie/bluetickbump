library(academictwitteR)
library(dplyr)
library(fst)

#get all accounts tweets
usernames <- read.csv("data/processed/blue-verified-collected.csv") 
# #remove accounts in ranked list for all verified analysis
# ranks <- read.csv("data/raw/blue-ranked.csv")
# ranks <- ranks$username
# 
# usernames <- usernames %>%
#   filter(!x %in% ranks)

usernames <- unique(usernames$x)
usernames <- usernames[939:943]

for (i in seq_along(usernames)) {
  
  cat("\nGetting tweets for ", usernames[[i]], " number ", i, " of", length(usernames), "\n")
  
  get_all_tweets(
    users = usernames[[i]],
    start_tweets = "2022-11-23T00:00:00Z",
    end_tweets = "2022-11-29T00:00:00Z",
    data_path = "data/tweets/02_update/",
    bind_tweets = F,
    n = Inf,
  )
  
}

#get ranked accounts tweets
usernames <- read.csv("data/processed/blue-ranked-collected.csv") 
usernames <- unique(usernames$x)
usernames <- usernames[2:53]

for (i in seq_along(usernames)) {
  
  cat("\nGetting tweets for ", usernames[[i]], " number ", i, " of", length(usernames), "\n")
  
  get_all_tweets(
    users = usernames[[i]],
    start_tweets = "2022-11-23T00:00:00Z",
    end_tweets = "2022-11-29T00:00:00Z",
    data_path = "data/rankedtweets/02_update/",
    bind_tweets = F,
    n = Inf,
  )
  
}