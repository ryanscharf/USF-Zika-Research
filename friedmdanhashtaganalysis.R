#My code for an analysis of hastags on zika related tweets
#http://ryanscharf.com/an-analysis-of-hashtags-on-zika-related-tweets/

zq1 <- separate(zqimage, col = hashes, into = c("hash1", "hash2", "hash3", 
                                                "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", 
                                                "hash12", "hash13", "hash14", "hash15", "hash16", "hash17", "hash18", "hash19",
                                                "hash20"), sep = ";", extra = "merge", fill = "right")

hashes<- apply(zq1[,24:43], 2, FUN=function(x) plyr::count(x))
hashes<- do.call(rbind.data.frame, hashes)
hashes<- plyr::ddply(hashes,"x",numcolwise(sum))
hashes <- hashes[!is.na(hashes[,1]),]
hashes <- head(arrange(hashes,desc(freq)), n = 100)

require(dplyr)
zqimage <- zq[grep("photo/1", zq$parsed_media_url),]
zqimage <- zqimage %>% group_by(text) %>% filter(retweet_count == max(retweet_count), favorite_count == max(favorite_count))
zqimage <- arrange(zqimage, desc(retweet_count))

zqimage <- zqimage[!is.na(zqimage$hashes), ]

zqimage$hashhash <- NA
zqimage$hashhash <- ifelse(grepl("bees", zqimage$hashes, ignore.case = TRUE), "bees", 
                           ifelse(grepl("mosquito", zqimage$hashes, ignore.case = TRUE), "mosquito", 
                                  ifelse(grepl("zika", zqimage$hashes, ignore.case = TRUE), "zika", "Other")))

fit <- aov(zqimage$retweet_count ~ hashhash, data = zqimage)
