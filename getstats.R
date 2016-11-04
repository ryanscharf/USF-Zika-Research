#get top 20 images, mentions, tweets, users, countries,
#library(plyr, dplyr)
library(tidyr)


getstats <- function(s, q)
{
  #most common countries | $country
  countrycount <- plyr::count(s$country)
  countrycount <- na.omit(countrycount)
  countrycount <<- head(arrange(countrycount, desc(freq)), n = q)
  
  #most mentioned users | $mentioned_users
  s <- separate(s, col = mentioned_users, into = c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14"), sep = ";", extra = "merge", fill = "right")
  mentions <- plyr::count(s$mention1)
  mentions <- plyr::rename(mentions,c("freq" = "mention1"))
  m2 <- plyr::count(s$mention2)
  m2 <- plyr::rename(m2,c("freq" = "mention2"))
  mentions <- merge(mentions, m2, by="x", all=TRUE)
  m3 <- plyr::count(s$mention3)
  m3 <- plyr::rename(m3,c("freq" = "mention3"))
  mentions <- merge(mentions, m3, by="x", all=TRUE)
  m4 <- plyr::count(s$mention4)
  m4 <- plyr::rename(m4,c("freq" = "mention4"))
  mentions <- merge(mentions, m4, by="x", all=TRUE)
  m5 <- plyr::count(s$mention5)
  m5 <- plyr::rename(m5,c("freq" = "mention5"))
  mentions <- merge(mentions, m5, by="x", all=TRUE)
  m6 <- plyr::count(s$mention6)
  m6 <- plyr::rename(m6,c("freq" = "mention6"))
  mentions <- merge(mentions, m6, by="x", all=TRUE)
  m7 <- plyr::count(s$mention7)
  m7 <- plyr::rename(m7,c("freq" = "mention7"))
  mentions <- merge(mentions, m7, by="x", all=TRUE)
  m8 <- plyr::count(s$mention8)
  m8 <- plyr::rename(m8,c("freq" = "mention8"))
  mentions <- merge(mentions, m8, by="x", all=TRUE)
  m9 <- plyr::count(s$mention9)
  m9 <- plyr::rename(m9,c("freq" = "mention9"))
  mentions <- merge(mentions, m9, by="x", all=TRUE)
  m10 <- plyr::count(s$mention10)
  m10 <- plyr::rename(m10,c("freq" = "mention10"))
  mentions <- merge(mentions, m10, by="x", all=TRUE)
  m11 <- plyr::count(s$mention11)
  m11 <- plyr::rename(m11,c("freq" = "mention11"))
  mentions <- merge(mentions, m11, by="x", all=TRUE)
  m12 <- plyr::count(s$mention12)
  m12 <- plyr::rename(m12,c("freq" = "mention12"))
  mentions <- merge(mentions, m12, by="x", all=TRUE)
  m13 <- plyr::count(s$mention13)
  m13 <- plyr::rename(m13,c("freq" = "mention13"))
  mentions <- merge(mentions, m13, by="x", all=TRUE)
  m14 <- plyr::count(s$mention14)
  m14<- plyr::rename(m14,c("freq" = "mention14"))
  mentions <- merge(mentions, m14, by="x", all=TRUE)
  mentions$total_mentions <- rowSums(mentions[,2:14], na.rm = TRUE)
  mentions <- mentions[!is.na(mentions[,1]),]
  mentions <- mentions[, c(1,16)]
  mentions <- head(arrange(mentions,desc(total_mentions)), n = q)
  mentions<<-mentions
  
  #mentions <- plyr::count(s$mentioned_users)
  #mentions <- na.omit(mentions)
  #mentions <<- head(arrange(mentions, desc(freq)), n = q)
  
  #most frequent tweeters | $screen_name
  usrs <- plyr::count(s$screen_name)
  usrs <<- head(arrange(usrs, desc(freq)), n = q)
  
  #most common tweets | $text
  tweets <- plyr::count(s$text)
  tweets <<- head(arrange(tweets, desc(freq)), n = q)
  
  #most retweeted users | $rt_screen_name
  rt <- plyr::count(s$rt_screen_name)
  rt <- na.omit(rt)
  rt <- head(arrange(rt, desc(freq)), n = q)
  rtusers <<- rt

}

