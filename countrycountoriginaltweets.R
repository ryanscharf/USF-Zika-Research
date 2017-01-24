#generating table 5, top countries, based off of the original tweets

#choose only tweets that have no retweets
ae1 <- ae[ae$retweet_count == 0,]
bf1 <- bf[bf$retweet_count == 0,]
cd1 <- cd[cd$retweet_count == 0,]

#count values
aeocountries<- plyr::count(ae1$country)
aeocountries <- na.omit(aeocountries)
aeocountries <- arrange(aeocountries, desc(freq))

bfocountries<- plyr::count(bf1$country)
bfocountries <- na.omit(bfocountries)
bfocountries <- arrange(bfocountries, desc(freq))

cdocountries<- plyr::count(cd1$country)
cdocountries <- na.omit(cdocountries)
cdocountries <- arrange(cdocountries, desc(freq))
