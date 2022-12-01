library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)
library(fixest)
library(zoo)
library(tsibble)
library(modelsummary)

tdf <- read_fst("data/processed/blue-ranked-tweetsALL.fst")
ids <- tdf %>%
  pull(tweet_id)
write.table(ids, "public_data/tweetIDs-ranked.txt", col.names = F, row.names = F,
            quote = F)

tdf$username <- tdf$user_username

tdf_tweetdays <- tdf %>%
  group_by(username) %>%
  tidyr::complete(date = seq.Date(min(as.Date("2022-05-17")), max(as.Date("2022-11-29")), 
                           by="day")) %>%
  mutate(obs = 1,
         rts = ifelse(is.na(retweet_count), 1, 1 +retweet_count), #note use of started log
         likes = ifelse(is.na(like_count), 1, 1 +like_count)) %>% #note use of started log
  group_by(date, username) %>%
  summarise(sum_tweets = sum(obs), 
            sum_rts = sum(rts),
            sum_likes = sum(likes)) %>%
  arrange(username, date)

tdf_tweetdays <- tdf_tweetdays %>%
  mutate(yearmon = as.Date(as.yearmon(date)),
         yearwk = as.Date(yearweek(date))) %>%
  filter(date <= "2022-11-27")

tdf_tweetdays$musk <- ifelse(tdf_tweetdays$date>="2022-10-27" & tdf_tweetdays$date <"2022-11-09", 1, 0)
tdf_tweetdays$twitblue <- ifelse(tdf_tweetdays$date>="2022-11-09", 1, 0)

saveRDS(tdf_tweetdays, "data/processed/tdf_tweetdays-ranked.rds")

tdf_tweetdays_public <- tdf_tweetdays

tdf_tweetdays_public$userid <- sapply(tdf_tweetdays_public$username, 
                                      digest::digest, 
                                      algo = "md5")

tdf_tweetdays_public <- tdf_tweetdays_public %>%
  select(-username)

saveRDS(tdf_tweetdays_public, "public_data/tdf_tweetdays-ranked.rds")

# plot all data
tdfsums <- tdf %>%
  group_by(username, date) %>%
  summarise(sum_rts = sum(retweet_count),
            sum_likes = sum(like_count))
  

tdfsums$logsum_rts <- log(tdfsums$sum_rts)
tdfsums$logsum_likes <- log(tdfsums$sum_likes)
tdfsums$rtfac <- cut(tdfsums$logsum_rts, 
                     breaks=c(-Inf, 1.5, 3,4.5,6,7.5,9,10.5,12,13.15, Inf), 
                     labels=c("0-1.5","1.5-3","3-4.5", 
                              "4.5-6", "6-7.5", "7.5-9",
                              "9-10.5", "10.5-12", "12-13.5", "13.5-15"))
tdfsums$likefac <- cut(tdfsums$logsum_likes, 
                       breaks=c(-Inf, 1.5, 3,4.5,6,7.5,9,10.5,12,13.15, Inf), 
                       labels=c("0-1.5","1.5-3","3-4.5", 
                                "4.5-6", "6-7.5", "7.5-9",
                                "9-10.5", "10.5-12", "12-13.5", "13.5-15"))
tdfsums %>%
  ggplot(aes(date,username, fill = rtfac)) +
  geom_tile(size=0.6) +
  guides(fill=guide_legend(title=paste0("Retweets (logged)"),
                           nrow=1)) +
  labs(x="Month | Week\n ",y="User",title="") + #add line break on y-axis so red vline lines up across top and bottom panels
  scale_x_date(date_labels="%b \n", date_breaks="month",
               date_minor_breaks = "week") +
  # scale_fill_brewer(na.value="white", palette = "PiYG") + 
  scale_fill_grey(start = 1, end=0, na.value = "white") + 
  theme_tufte(base_size=10, base_family = "Helvetica") +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.key=element_rect(colour="black"),
        axis.text.x=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_blank(),
        axis.title.y=element_text(size=15),
        axis.ticks=element_line(size=0.1),
        plot.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  geom_vline(aes(xintercept = as.integer(as.Date("2022-10-27"))),
             linetype="dotdash", colour = "black", size=.5) +
  geom_vline(aes(xintercept = as.integer(as.Date("2022-11-09"))),
             linetype="longdash", colour = "black", size=1)

ggsave("plots/alldatrts-ranked.png", 
       width=250, height = 200, 
       dpi=300, units="mm", bg = "white")

tdfsums %>%
  ggplot(aes(date,username, fill = likefac)) +
  geom_tile(size=0.6) +
  guides(fill=guide_legend(title=paste0("Likes (logged)"),
                           nrow=1)) +
  labs(x="Month | Week\n ",y="User",title="") + #add line break on y-axis so red vline lines up across top and bottom panels
  scale_x_date(date_labels="%b \n", date_breaks="month",
               date_minor_breaks = "week") +
  theme_tufte(base_size=10, base_family = "Helvetica") +
  # scale_fill_brewer(na.value="white", palette = "PiYG") + 
  scale_fill_grey(start = 1, end=0, na.value = "white") + 
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.key=element_rect(colour="black"),
        axis.text.x=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_blank(),
        axis.title.y=element_text(size=15),
        axis.ticks=element_line(size=0.1),
        plot.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  geom_vline(aes(xintercept = as.integer(as.Date("2022-10-27"))),
             linetype="dotdash", colour = "black", size=.5) +
  geom_vline(aes(xintercept = as.integer(as.Date("2022-11-09"))),
             linetype="longdash", colour = "black", size=1)

ggsave("plots/alldatlikes-ranked.png", 
       width=250, height = 200, 
       dpi=300, units="mm", bg = "white")


# tdfsums %>%
#   mutate(all = 1) %>%
#   filter(date>="2022-10-27" & date <"2022-11-09") %>%
#   group_by(all) %>%
#   summarise(median = median(sum_rts))


# tdf$musk <- ifelse(tdf$date>="2022-10-27" & tdf$date <"2022-11-09", 1, 0)
# tdf$twitblue <- ifelse(tdf$date>="2022-11-09", 1, 0)
# tdf$muskblue <- ifelse(tdf$date>="2022-11-09", 2,
#                        ifelse(tdf$date>="2022-10-27" & tdf$date <"2022-11-09", 1, 0))
# 
# tdf %>%
#   sample_n(1000000) %>%
#   ggplot() +
#   geom_point(aes(date,log(retweet_count), group = muskblue)) +
#   geom_smooth(aes(date, log(retweet_count), group = muskblue)) +
#   geom_vline(aes(xintercept = as.integer(as.Date("2022-10-27"))),
#              linetype="dotdash", colour = "black", size=.5) +
#   geom_vline(aes(xintercept = as.integer(as.Date("2022-11-09"))),
#              linetype="longdash", colour = "black", size=1)
#   