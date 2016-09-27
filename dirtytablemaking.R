library(plyr)
library(reshape2)

#creating the tables the right way
activeagents <- zika1[zika1$screen_name %in% usrs[,1], ]
activeagents <- activeagents[, c(4,26, 7, 9)]
#activeagents <- arrange(activeagents, desc(usrs$screen_name), n = 100)

aa <- usrs
aa <- rename(aa, c("x" = "screen_name"))


#aaa <- plyr::count(activeagents2$screen_name)
#aaa <- rename(aaa,c("x" = "screen_name", "freq" = "both"))
#aa1 <- merge(aa, aaa, by="screen_name", all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]
#aa1 <- rename(aa1, c("both.y" = "both"))
#aa1$both.x<-NULL

# get text only
activeagents2<-activeagents[is.na(activeagents[,2]) & is.na(activeagents[,3]),]
aaa <- plyr::count(activeagents2$screen_name)
aaa <- rename(aaa,c("x" = "screen_name", "freq" = "text"))
aa1 <- merge(aa, aaa, by="screen_name", all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get urls
activeagents2 <- activeagents[is.na(activeagents[,2]) & !is.na(activeagents[,3]),]
aaa <- plyr::count(activeagents2$screen_name)
aaa <- rename(aaa,c("x" = "screen_name", "freq" = "url"))
aa1 <- merge(aa1, aaa, by="screen_name", all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get photos
activeagents2 <- activeagents[grep("photo/1", activeagents$parsed_media_url),]
aaa <- plyr::count(activeagents2$screen_name)
aaa <- rename(aaa,c("x" = "screen_name", "freq" = "photos"))
aa1 <- merge(aa1, aaa, by="screen_name", all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get videos
activeagents2 <- activeagents[grep("video/1", activeagents$parsed_media_url),]
aaa <- plyr::count(activeagents2$screen_name)
aaa <- rename(aaa,c("x" = "screen_name", "freq" = "video"))
aa1 <- merge(aa1, aaa, by="screen_name", all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

aa1 <- head(arrange(aa1, desc(freq)), n = 100)
