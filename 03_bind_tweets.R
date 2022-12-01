library(academictwitteR)
library(dplyr)
library(ggplot2)
library(fst)

# bind updated tweets for all accounts sample
tweetsdf <- bind_tweets("data/tweets/", output_format = "tidy")
tweetsdf$date <- as.Date(tweetsdf$created_at)

saveRDS(tweetsdf, "data/processed/blue-verified-tweets.rds")
write_fst(tweetsdf, "data/processed/blue-verified-tweets.fst")

# bind updated tweets for ranked accounts sample
tweetsdf <- bind_tweets("data/rankedtweets/", output_format = "tidy")
tweetsdf$date <- as.Date(tweetsdf$created_at)

saveRDS(tweetsdf, "data/processed/blue-ranked-tweets.rds")
write_fst(tweetsdf, "data/processed/blue-ranked-tweets.fst")

# get lists of collected usernames
##all accounts sample
bluevertweets <- read_fst("data/processed/blue-verified-tweets.fst")
usernames <- unique(bluevertweets$user_username)
rm(bluevertweets)
write.csv(usernames, "data/processed/blue-verified-collected.csv", row.names = F)

##ranked accounts sample
blueranktweets <- read_fst("data/processed/blue-ranked-tweets.fst")
usernames <- unique(blueranktweets$user_username)
rm(blueranktweets)
write.csv(usernames, "data/processed/blue-ranked-collected.csv", row.names = F)
 
