#get top 20 images, mentions, tweets, users, countries, hashtags
#s is your target data set
#q is how many rowss you want returned in your tables

library(tidyr)

getstats <- function(s, q)
{
  #most common countries | $country
  countrycount <- plyr::count(s$country)
  countrycount <- na.omit(countrycount)
  countrycount <<- head(arrange(countrycount, desc(freq)), n = q)
  
  #most mentioned users | $mentioned_users
  s <- separate(s, col = mentioned_users, into = c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14"), sep = ";", extra = "merge", fill = "right")
  
  mentions <- apply(s[,22:35], 2, FUN=function(x) plyr::count(x))
  mentions <- do.call(rbind.data.frame, mentions)
  mentions <- plyr::ddply(mentions,"x",numcolwise(sum))
  mentions <- mentions[!is.na(mentions[,1]),]
  
  mentions <- head(arrange(mentions,desc(freq)), n = q)
  mentions <<- mentions
  
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
  
  #hashtags
  s <- separate(s, col = hashes, into = c("hash1", "hash2", "hash3", "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", "hash12", "hash13", "hash14", "hash15", "hash16", "hash17", "hash18", "hash19", "hash20"), sep = ";", extra = "merge", fill = "right")

  hashes<- apply(s[,37:56], 2, FUN=function(x) plyr::count(x))
  hashes<- do.call(rbind.data.frame, hashes)
  hashes<- plyr::ddply(hashes,"x",numcolwise(sum))
  hashes <- hashes[!is.na(hashes[,1]),]
  
  hashes <- head(arrange(hashes,desc(freq)), n = q)
  hashes<<-hashes
}

