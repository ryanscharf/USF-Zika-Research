
######must fix renaming by field arg

analysistable <- function(field, obj, numb)
{
library(plyr)
library(reshape2)
  
#creating the tables the right way
activeagents <- fl[fl$field %in% obj[,1], ]
activeagents <- activeagents[, c(4,26, 7, numb)]
#activeagents <- arrange(activeagents, desc(obj$field), n = 100)

aa <- obj
aa <- plyr::rename(aa, c("x" = field))


# get text only
activeagents2<-activeagents[is.na(activeagents[,2]) & is.na(activeagents[,3]),]
aaa <- plyr::count(activeagents2$field)
aaa <- plyr::rename(aaa,c("x" = field, "freq" = "text"))
aa1 <- merge(aa, aaa, by=field, all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get urls
activeagents2 <- activeagents[is.na(activeagents[,2]) & !is.na(activeagents[,3]),]
aaa <- plyr::count(activeagents2$field)
aaa <- plyr::rename(aaa,c("x" = field, "freq" = "url"))
aa1 <- merge(aa1, aaa, by=field, all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get photos
activeagents2 <- activeagents[grep("photo/1", activeagents$parsed_media_url),]
aaa <- plyr::count(activeagents2$field)
aaa <- plyr::rename(aaa,c("x" = field, "freq" = "photos"))
aa1 <- merge(aa1, aaa, by=field, all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

#get videos
activeagents2 <- activeagents[grep("video/1", activeagents$parsed_media_url),]
aaa <- plyr::count(activeagents2$field)
aaa <- plyr::rename(aaa,c("x" = field, "freq" = "video"))
aa1 <- merge(aa1, aaa, by=field, all=TRUE)
#aa1 <- aa1[!is.na(aa1[,2]),]

aa1 <- head(arrange(aa1, desc(freq)), n = 100)
aa1 <- aa1[,c(3,5,6,4,1,2)]
aa1 <<- aa1
}
