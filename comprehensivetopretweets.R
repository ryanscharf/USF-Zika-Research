#conglomeration of spaghetti scripts used, needs cleaning
#generating revised, expanded version of topretweets table
#generates 3 tables of the top retweeted text, image, and video tweets

aetrt <- plyr::count(ae$text)
aetrt <- head(arrange(aetrt, desc(freq)), n = 100)
aetrt1 <- ae[ae$text %in% aetrt$x, ] %>% group_by(text) %>% top_n(n=1, retweet_count) %>% top_n(n=1, favorite_count) %>% top_n(n=1, datetime)
aetrt1 <- arrange(aetrt1, desc(retweet_count))
#aetrt1 <- aetrt1[aetrt1$retweet_count == which.max(aetrt1$retweet_count), ]


bftrt <- plyr::count(bf$text)
bftrt <- head(arrange(bftrt, desc(freq)), n = 100)
bftrt1 <- bf[bf$text %in% bftrt$x, ] %>% group_by(text) %>% top_n(n=1, retweet_count) %>% top_n(n=1, favorite_count) %>% top_n(n=1, datetime)
bftrt1 <- arrange(bftrt1, desc(retweet_count))

cdtrt <- plyr::count(cd$text)
cdtrt <- head(arrange(cdtrt, desc(freq)), n = 100)
cdtrt1 <- cd[cd$text %in% cdtrt$x, ] %>% group_by(text) %>% top_n(n=1, retweet_count) %>% top_n(n=1, favorite_count) %>% top_n(n=1, datetime)
cdtrt1 <- arrange(cdtrt1, desc(retweet_count))

write.csv(aetrt1, "tbl1000.csv")
write.csv(bftrt1, "tbl1001.csv")
write.csv(cdtrt1, "tbl1002.csv")
