library(dplyr)
library(ggplot2)
library(tidylog)
library(fixest)
library(modelsummary)
library(scales)

tdf_tweetdays <- readRDS("data/processed/tdf_tweetdays-verified.rds")


tdf_tweetdays$muskblue <- ifelse(tdf_tweetdays$date>="2022-10-27" & tdf_tweetdays$date <"2022-10-30", 1,
                                 ifelse(tdf_tweetdays$date>="2022-10-30" & tdf_tweetdays$date <"2022-11-02", 2,
                                        ifelse(tdf_tweetdays$date>="2022-11-02" & tdf_tweetdays$date <"2022-11-05", 3,
                                               ifelse(tdf_tweetdays$date>="2022-11-05" & tdf_tweetdays$date <"2022-11-08", 4,
                                                      ifelse(tdf_tweetdays$date>="2022-11-08" & tdf_tweetdays$date <"2022-11-11", 5,
                                                             ifelse(tdf_tweetdays$date>="2022-11-11" & tdf_tweetdays$date <"2022-11-14", 6,
                                                                    ifelse(tdf_tweetdays$date>="2022-11-14" & tdf_tweetdays$date <"2022-11-17", 7,
                                                                           ifelse(tdf_tweetdays$date>="2022-11-17" & tdf_tweetdays$date <"2022-11-20", 8,
                                                                                  ifelse(tdf_tweetdays$date>="2022-11-20" & tdf_tweetdays$date <"2022-11-23", 9,
                                                                                         ifelse(tdf_tweetdays$date>="2022-11-23" & tdf_tweetdays$date <"2022-11-26", 10,
                                                                                                ifelse(tdf_tweetdays$date>="2022-11-26" & tdf_tweetdays$date <"2022-11-29", 11, 0)))))))))))
tdf_tweetdays$muskblue <- as.factor(tdf_tweetdays$muskblue)

baseline1 <- feols(log(sum_rts) ~ muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

baseline2 <- feols(log(sum_likes) ~ muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

png(
  "plots/efplot-verified.png",
  width = 300,
  height = 150,
  units = 'mm',
  res = 200
)
est_std1 = summary(baseline1, se = "iid")
# #exponentiate and get percentage
# est_std1$coefficients <- (exp(est_std1$coefficients) -1)*100
coefplot(est_std1, 
         dict = c(muskblue1="Oct 27-Oct 30", muskblue2 = "Oct 30-Nov 02", muskblue3="Nov 02-Nov 05", muskblue4 = "Nov 05-Nov 08",
                  muskblue5="Nov 08-Nov 11", muskblue6 = "Nov 11-Nov 14", muskblue7="Nov 14-Nov 17", muskblue8 = "Nov 17-Nov 20", 
                  muskblue9="Nov 20-Nov 23", muskblue10 = "Nov 23-Nov 26", muskblue11="Nov 26-Nov 29"),
         keep = c("Oct 27-Oct 30","Oct 30-Nov 02","Nov 02-Nov 05","Nov 05-Nov 08",
                  "Nov 08-Nov 11","Nov 11-Nov 14","Nov 14-Nov 17","Nov 17-Nov 20",
                  "Nov 20-Nov 23","Nov 23-Nov 26","Nov 26-Nov 29"),
         ylim= c(0,1),
         add = F, pch=15, main = "Effect on user post engagement",
         lab.cex = 1.25, pt.cex = 2, cex.axis = 2)


est_std1 = summary(baseline2, se = "iid")
coefplot(est_std1, 
         dict = c(muskblue1="Oct 27-Oct 30", muskblue2 = "Oct 30-Nov 02", muskblue3="Nov 02-Nov 05", muskblue4 = "Nov 05-Nov 08",
                  muskblue5="Nov 08-Nov 11", muskblue6 = "Nov 11-Nov 14", muskblue7="Nov 14-Nov 17", muskblue8 = "Nov 17-Nov 20", 
                  muskblue9="Nov 20-Nov 23", muskblue10 = "Nov 23-Nov 26", muskblue11="Nov 26-Nov 29"),
         keep = c("Oct 27-Oct 30","Oct 30-Nov 02","Nov 02-Nov 05","Nov 05-Nov 08",
                  "Nov 08-Nov 11","Nov 11-Nov 14","Nov 14-Nov 17","Nov 17-Nov 20",
                  "Nov 20-Nov 23","Nov 23-Nov 26","Nov 26-Nov 29"),
         ylim= c(0,1),
         add = T, col = 2, pch=15,
         lab.cex = 1.25, pt.cex = 2)

legend("topright", col = 1:2, pch = 20, lwd = 1,
       legend = c("Retweets", "Likes"), cex = .8)
abline(v=c(5), col=  alpha(c("blue"), .4), lty=c(2), lwd=c(3))
dev.off() 