#conglomeration of spaghetti scripts used; needs cleaning
#generating tables for groupings of images, texts, and video for revised version of table 2

##separating mentions{
cd <- separate(cd, col = mentioned_users, into = c("mention1", "mention2", "mention3", "mention4", "mention5", "mention6", "mention7", "mention8", "mention9", "mention10", "mention11", "mention12", "mention13", "mention14"), sep = ";", extra = "merge", fill = "right")


mentionscd <- apply(ae[,22:35], 2, FUN=function(x) plyr::count(x))
mentionscd <- do.call(rbind.data.frame, mentionscd)
mentionscd <- plyr::ddply(mentionscd,"x",numcolwise(sum))
mentionscd <- mentionscd[!is.na(mentionscd[,1]),]
mentionscd <- head(arrange(mentionscd,desc(freq)), n = 100)

mentionsbf <- apply(bf[,22:35], 2, FUN=function(x) plyr::count(x))
mentionsbf <- do.call(rbind.data.frame, mentionsbf)
mentionsbf <- plyr::ddply(mentionsbf,"x",numcolwise(sum))
mentionsbf <- mentionsbf[!is.na(mentionsbf[,1]),]
mentionsbf <- head(arrange(mentionsbf,desc(freq)), n = 100)

mentionscd <- apply(cd[,22:35], 2, FUN=function(x) plyr::count(x))
mentionscd <- do.call(rbind.data.frame, mentionscd)
mentionscd <- plyr::ddply(mentionscd,"x",numcolwise(sum))
mentionscd <- mentionscd[!is.na(mentionscd[,1]),]
mentionscd <- head(arrange(mentionscd,desc(freq)), n = 100)


write.xlsx(mentionsae, "mentions.xlsx", sheetName="tfreq")
write.xlsx(mentionsbf, "mentions.xlsx", sheetName="ifreq", append = TRUE)
write.xlsx(mentionscd, "mentions.xlsx", sheetName="vfreq", append = TRUE)

##}


ae1 <- ae[ae$retweet_count != 0,]
  
  
aeretweets <- plyr::count(ae$text)
aeretweets <- head(arrange(aeretweets,desc(freq)), n = 100)
bfretweets <- plyr::count(bf$text)
bfretweets <- head(arrange(bfretweets,desc(freq)), n = 100)
bf2 <- plyr::count(cd$text)
bf2 <- head(arrange(bf2,desc(freq)), n = 100)
topretweets <- plyr::count(zq$text)
topretweets <- head(arrange(topretweets,desc(freq)), n = 100)

ae1 <- sample_n(ae,100)
ae1 <- ae1[,c(4, 5,7,8,9,13,15,17,38,39,40)]
bf1 <- sample_n(bf,100)
bf1 <- bf1[,c(4, 5,7,8,9,13,15,17,38,39,40)]
cd1 <- sample_n(cd,100)
cd1 <- cd1[,c(4, 5,7,8,9,13,15,17,38,39,40)]








aeretweets <- plyr::count(ae$text)
aeretweets <- arrange(aeretweets,desc(freq))
aeretweets <- aeretweets[aeretweets$freq == 1,]
ae1 <- ae[!(ae$text %in% aeretweets$x),]

bfretweets <- plyr::count(bf$text)
bfretweets <- arrange(bfretweets,desc(freq))
bfretweets <- bfretweets[bfretweets$freq == 1,]
bf1 <- bf[!(bf$text %in% bfretweets$x),]

bf2 <- plyr::count(cd$text)
bf2 <- arrange(bf2,desc(freq))
bf2 <- bf2[bf2$freq == 1,]
cd1 <- cd[!(cd$text %in% bf2$x),]
#

ae2 <- plyr::count(ae$rt_screen_name)
ae2<- ae2[complete.cases(ae2),]
ae2 <- head(arrange(ae2,desc(freq)), n = 100)

bf2 <- plyr::count(bf$rt_screen_name)
bf2<- bf2[complete.cases(bf2),]
bf2 <- head(arrange(bf2,desc(freq)), n = 100)

cd2 <- plyr::count(cd$rt_screen_name)
cd2<- cd2[complete.cases(cd2),]
cd2 <- head(arrange(cd2,desc(freq)), n = 100)

zq2 <- plyr::count(zq$rt_screen_name)
zq2 <- zq2[complete.cases(zq2),]
zq2 <- head(arrange(zq2, desc(freq)), n = 100)
zqtoppostertweets <- zq[zq$screen_name %in% zq2$x,]


aaaa <- plyr::count(zq$text)
aaaa <- arrange(aaaa, desc(freq))
aaaa <- aaaa[aaaa$freq == 1,]
zqretweets <- zq[!(zq$text %in% aaaa$x),]
zqretweets1 <- zqretweets[zqretweets$screen_name %in% zq2$x,]
bbbb <- zqretweets[zqretweets$rt_screen_name %in% zq2$x,]



t2u <- zq[zq$retweet_count == 0,]
