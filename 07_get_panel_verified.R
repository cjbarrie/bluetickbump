library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)
library(fixest)
library(zoo)
library(tsibble)
library(modelsummary)

tdf <- read_fst("data/processed/blue-verified-tweetsALL.fst")
ids <- tdf %>%
  pull(tweet_id)
write.table(ids, "data/tweetIDs-verified.txt", col.names = F, row.names = F,
            quote = F)

tdf$username <- tdf$user_username

#remove accounts in ranked list for all verified analysis
ranks <- read.csv("data/raw/blue-ranked.csv")
ranks <- ranks$username

tdf <- tdf %>%
  filter(!username %in% ranks)

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

saveRDS(tdf_tweetdays, "data/processed/tdf_tweetdays-verified.rds")

tdf_tweetdays_public <- tdf_tweetdays

tdf_tweetdays_public$userid <- sapply(tdf_tweetdays_public$username, 
                                      digest::digest, 
                                      algo = "md5")

tdf_tweetdays_public <- tdf_tweetdays_public %>%
  select(-username)

saveRDS(tdf_tweetdays_public, "public_data/tdf_tweetdays-verified.rds")

# tdf_tweetdays$muskblue <- ifelse(tdf_tweetdays$date>="2022-10-27" & tdf_tweetdays$date <"2022-11-03", 1,
#                                  ifelse(tdf_tweetdays$date>="2022-11-03" & tdf_tweetdays$date <"2022-11-10", 2,
#                                         ifelse(tdf_tweetdays$date>="2022-11-12" & tdf_tweetdays$date <"2022-11-17", 3,
#                                                ifelse(tdf_tweetdays$date>="2022-11-17" & tdf_tweetdays$date <"2022-11-24", 4,
#                                                       ifelse(tdf_tweetdays$date>="2022-11-24", 5, 0)))))
# 
# tdf_tweetdays$muskblue <- as.factor(tdf_tweetdays$muskblue)
# 
# baseline1 <- feols(log(sum_rts) ~ muskblue + log(sum_tweets) |
#                      username + yearmon,
#                    data = tdf_tweetdays)

# baseline1 <- feols(log(sum_rts) ~ twitblue + musk + log(sum_tweets) |
#                      username + yearmon,
#                    data = tdf_tweetdays)
# 
# baseline2 <- feols(log(sum_likes) ~ twitblue + musk + log(sum_tweets) |
#                      username + yearmon,
#                    data = tdf_tweetdays)

# robust1 <- feols(log(sum_rts) ~ twitblue + musk + log(sum_tweets) |
#                                 username + yearmon,
#                               data = tdf_tweetdays)
# 
# robust2 <- feols(log(sum_likes) ~ twitblue + musk + log(sum_tweets) |
#                      username + yearmon,
#                    data = tdf_tweetdays)

# linearHypothesis(robust1, c("musk = twitblue" ))


# baseline1 <- fepois(sum_rts ~ twitblue + sum_tweets |
#                      username + yearmon,
#                    data = tdf_tweetdays)

# baseline1 <- fenegbin(sum_rts ~ twitblue + musk + log(sum_tweets) |
#                      username + yearmon,
#                    data = tdf_tweetdays)

# gm <- tibble::tribble(
#   ~raw,        ~clean,          ~fmt,
#   # "r.squared", "R2",            2,
#   "nobs",      "Observations",             0,
#   "FE: username",  "User fixed effect",      0,
#   "FE: yearmon", "Year-month fixed effect", 0)
# 
# modelsummary(models = list("Retweets" = baseline1, 
#                            "Likes" = baseline2), 
#              output = "latex",
#              coef_rename = c(
#                "twitblue" = "Paid verification",
#                "musk" = "Musk acquisition",
#                "log(sum_rts)" = "Retweets (logged sum)",
#                "log(sum_tweets)" = "Tweets (logged sum)"),
#              gof_map = gm, 
#              notes = list('Outcomes are logged retweets and likes',
#                           'Standard errors clustered by user'),
#              title = 'Effect of paid verification on general user engagement',
#              stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001))

# now with "regular" standard-errors
# png(
#   "plots/efplot-all.png",
#   width = 200,
#   height = 150,
#   units = 'mm',
#   res = 200
# )
# est_std1 = summary(baseline1, se = "iid")
# # #exponentiate and get percentage
# # est_std1$coefficients <- (exp(est_std1$coefficients) -1)*100
# coefplot(est_std1, 
#          dict = c(twitblue="Paid verification", musk = "Musk acquisition"),
#          keep = c("Paid verification", "Musk acquisition"),
#          x.shift = -.2,
#          add = F, pch=15, ylim = c(0,.8), main = "Effect on user engagement",
#          lab.cex = 1.25, pt.cex = 2, cex.axis = 2)
# 
# est_std2 = summary(baseline2, se = "iid")
# coefplot(est_std2, 
#          dict = c(twitblue="Paid verification", musk = "Musk acquisition"),
#          keep = c("Paid verification", "Musk acquisition"),
#          x.shift = .2,
#          add = T, col = 2, pch=15, ylim = c(0,.8),
#          lab.cex = 1.25, pt.cex = 2)
# 
# legend("topright", col = 1:2, pch = 20, lwd = 1,
#        legend = c("Retweets", "Likes"), cex = 1)
# dev.off() 

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

ggsave("plots/alldatlikes-verified.png", 
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

ggsave("plots/alldatlikes-verified.png", 
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
