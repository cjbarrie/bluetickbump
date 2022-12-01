library(dplyr)
library(ggplot2)
library(tidylog)
library(fixest)
library(modelsummary)
library(scales)

##change variable "username" to "userid" if using public repo data

tdf_tweetdays <- readRDS("data/processed/tdf_tweetdays-verified.rds")

tdf_tweetdays$muskblue <- ifelse(tdf_tweetdays$date>="2022-10-28" & tdf_tweetdays$date <"2022-10-31", 1,
                                 ifelse(tdf_tweetdays$date>="2022-10-31" & tdf_tweetdays$date <"2022-11-03", 2,
                                        ifelse(tdf_tweetdays$date>="2022-11-03" & tdf_tweetdays$date <"2022-11-06", 3,
                                               ifelse(tdf_tweetdays$date>="2022-11-06" & tdf_tweetdays$date <"2022-11-09", 4,
                                                      ifelse(tdf_tweetdays$date>="2022-11-09" & tdf_tweetdays$date <"2022-11-12", 5,
                                                             ifelse(tdf_tweetdays$date>="2022-11-12" & tdf_tweetdays$date <"2022-11-15", 6,
                                                                    ifelse(tdf_tweetdays$date>="2022-11-15" & tdf_tweetdays$date <"2022-11-18", 7,
                                                                           ifelse(tdf_tweetdays$date>="2022-11-18" & tdf_tweetdays$date <"2022-11-21", 8,
                                                                                  ifelse(tdf_tweetdays$date>="2022-11-21" & tdf_tweetdays$date <"2022-11-24", 9,
                                                                                         ifelse(tdf_tweetdays$date>="2022-11-24" & tdf_tweetdays$date <"2022-11-27", 10, 0))))))))))
tdf_tweetdays$muskblue <- as.factor(tdf_tweetdays$muskblue)

tdf_tweetdays$pmusk <- ifelse(tdf_tweetdays$date=="2022-10-27", 1, 0)
tdf_tweetdays$muskacq <- ifelse(tdf_tweetdays$date>="2022-10-27", 1, 0)


baseline1 <- feols(log(sum_rts) ~ muskacq + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

baseline2 <- feols(log(sum_likes) ~ muskacq + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  # "r.squared", "R2",            2,
  "nobs",      "Observations",             0,
  "FE: username",  "User fixed effect",      0)

modelsummary(models = list("Retweets" = baseline1,
                           "Likes" = baseline2),
             output = "latex",
             coef_rename = c(
               "muskacq" = "Post-Musk acquisition",
               "log(sum_rts)" = "Retweets (logged sum)",
               "log(sum_tweets)" = "Tweets (logged sum)"),
             gof_map = gm,
             notes = list('Outcomes are logged retweets and likes',
                          'Standard errors clustered by user'),
             title = 'Effect of Musk acquisition on general user engagement',
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001))



baseline3 <- feols(log(sum_rts) ~ pmusk + muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

baseline4 <- feols(log(sum_likes) ~ pmusk +  muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

png(
  "plots/efplot-verified.png",
  width = 300,
  height = 150,
  units = 'mm',
  res = 200
)
est_std1 = summary(baseline3, se = "iid")
# #exponentiate and get percentage
# est_std1$coefficients <- (exp(est_std1$coefficients) -1)*100
coefplot(est_std1, 
         dict = c(muskblue1="Oct 28-Oct 31", muskblue2 = "Oct 31-Nov 03", muskblue3="Nov 03-Nov 06", muskblue4 = "Nov 06-Nov 09",
                  muskblue5="Nov 09-Nov 12", muskblue6 = "Nov 12-Nov 15", muskblue7="Nov 15-Nov 18", muskblue8 = "Nov 18-Nov 21", 
                  muskblue9="Nov 21-Nov 24", muskblue10 = "Nov 24-Nov 27"),
         keep = c("Oct 28-Oct 31","Oct 31-Nov 03","Nov 03-Nov 06","Nov 06-Nov 09",
                  "Nov 09-Nov 12","Nov 12-Nov 15","Nov 15-Nov 18","Nov 18-Nov 21",
                  "Nov 21-Nov 24","Nov 24-Nov 27"),
         ylim= c(0,1.5),
         add = F, pch=15, main = "Effect on user post engagement",
         lab.cex = .75, pt.cex = 1.5, cex.axis = 1)


est_std1 = summary(baseline4, se = "iid")
# #exponentiate and get percentage
# est_std1$coefficients <- (exp(est_std1$coefficients) -1)*100
coefplot(est_std1, 
         dict = c(muskblue1="Oct 28-Oct 31", muskblue2 = "Oct 31-Nov 03", muskblue3="Nov 03-Nov 06", muskblue4 = "Nov 06-Nov 09",
                  muskblue5="Nov 09-Nov 12", muskblue6 = "Nov 12-Nov 15", muskblue7="Nov 15-Nov 18", muskblue8 = "Nov 18-Nov 21", 
                  muskblue9="Nov 21-Nov 24", muskblue10 = "Nov 24-Nov 27"),
         keep = c("Oct 28-Oct 31","Oct 31-Nov 03","Nov 03-Nov 06","Nov 06-Nov 09",
                  "Nov 09-Nov 12","Nov 12-Nov 15","Nov 15-Nov 18","Nov 18-Nov 21",
                  "Nov 21-Nov 24","Nov 24-Nov 27"),
         ylim= c(0,1.5),
         add = T, col = 2, pch=15,
         lab.cex = .75, pt.cex = 1.5, cex.axis = 1)

legend("topright", col = 1:2, pch = 20, lwd = 1,
       legend = c("Retweets", "Likes"), cex = .8)
abline(v=c(5), col=  alpha(c("blue"), .4), lty=c(2), lwd=c(3))
dev.off() 