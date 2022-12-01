library(readr)
library(dplyr)
options(scipen=999)

# get sample of all accounts
accounts <- read_csv("data/raw/blue-verified.csv")
colnames(accounts) <- c("id", "username", "followers")

set.seed(123L)

accounts_sample <- accounts %>%
  sample_n(1000)

saveRDS(accounts_sample, "data/processed/blue-verified-sample.rds")

# get top 1000 sample of ranked accounts
accounts_ranked <- read_csv("data/raw/blue-ranked.csv")

accounts_ranked_sample <- accounts_ranked[1:1000,]

saveRDS(accounts_ranked_sample, "data/processed/blue-ranked-sample.rds")
