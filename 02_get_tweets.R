library(academictwitteR)
library(dplyr)

#get all accounts tweets

blueversamp <- readRDS("data/processed/blue-verified-sample.rds")

usernames <- unique(blueversamp$username)

for (i in seq_along(usernames)) {
  
  cat("\nGetting tweets for ", usernames[[i]], " number ", i, " of", length(usernames), "\n")
  
  get_all_tweets(
    users = usernames[[i]],
    start_tweets = "2022-05-17T00:00:00Z",
    end_tweets = "2022-11-16T00:00:00Z",
    data_path = "data/tweets/",
    bind_tweets = F,
    n = Inf,
  )
  
}

# get ranked account tweets

blueveranksamp <- readRDS("data/processed/blue-ranked-sample.rds")

usernames <- unique(blueveranksamp$username)

for (i in seq_along(usernames)) {
  
  cat("\nGetting tweets for ", usernames[[i]], " number ", i, " of", length(usernames), "\n")
  
  get_all_tweets(
    users = usernames[[i]],
    start_tweets = "2022-05-17T00:00:00Z",
    end_tweets = "2022-11-16T00:00:00Z",
    data_path = "data/rankedtweets/",
    bind_tweets = F,
    n = Inf,
  )
  
}