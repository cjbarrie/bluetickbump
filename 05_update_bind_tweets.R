library(academictwitteR)
library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)

options(scipen = 999)

# bind updated tweets for all accounts sample update
tweetsdfall <- bind_tweets("data/tweets/01_update/", output_format = "tidy")
tweetsdfall$date <- as.Date(tweetsdfall$created_at)

saveRDS(tweetsdfall, "data/processed/blue-verified-tweets1.rds")
write_fst(tweetsdfall, "data/processed/blue-verified-tweets1.fst")

tweetsdfall <- bind_tweets("data/tweets/02_update/", output_format = "tidy")
tweetsdfall$date <- as.Date(tweetsdfall$created_at)

saveRDS(tweetsdfall, "data/processed/blue-verified-tweets2.rds")
write_fst(tweetsdfall, "data/processed/blue-verified-tweets2.fst")

# bind updated tweets for ranked accounts sample update
tweetsdfranked <- bind_tweets("data/rankedtweets/01_update", 
                              output_format = "tidy")
tweetsdfranked$date <- as.Date(tweetsdfranked$created_at)

saveRDS(tweetsdfranked, "data/processed/blue-ranked-tweets1.rds")
write_fst(tweetsdfranked, "data/processed/blue-ranked-tweets1.fst")

tweetsdfranked <- bind_tweets("data/rankedtweets/02_update", 
                              output_format = "tidy")
tweetsdfranked$date <- as.Date(tweetsdfranked$created_at)

saveRDS(tweetsdfranked, "data/processed/blue-ranked-tweets2.rds")
write_fst(tweetsdfranked, "data/processed/blue-ranked-tweets2.fst")

##bind original and updated all accounts sample
tdf1 <- read_fst("data/processed/blue-verified-tweets.fst")
tdf2 <- read_fst("data/processed/blue-verified-tweets1.fst")
tdf3 <- read_fst("data/processed/blue-verified-tweets2.fst")
tdf <- rbind(tdf1, tdf2, tdf3)
saveRDS(tdf, "data/processed/blue-verified-tweetsALL.rds")
write_fst(tdf, "data/processed/blue-verified-tweetsALL.fst")

##bind original and updated ranked accounts sample
tdf1 <- read_fst("data/processed/blue-ranked-tweets.fst")
tdf2 <- read_fst("data/processed/blue-ranked-tweets1.fst")
tdf3 <- read_fst("data/processed/blue-ranked-tweets2.fst")
tdf <- rbind(tdf1, tdf2, tdf3)
saveRDS(tdf, "data/processed/blue-ranked-tweetsALL.rds")
write_fst(tdf, "data/processed/blue-ranked-tweetsALL.fst")