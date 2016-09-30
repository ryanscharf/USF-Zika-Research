#table 5
tbl_5 <- function(src)
{
tbl5 <<- data.frame(matrix(ncol = 2, nrow = 5))
rownames(tbl5) <- c("Text only", "image", "video", "url", "total")
colnames(tbl5) <- c("#", "avg")

zq <- src[,c(4,26,7,5)]
zq$retweet_count[zq$retweet_count== 0] <- NA
zq<- zq[!is.na(zq$retweet_count),]
tbl5[5,1] <- length(zq[,1])
zq1 <-zq
#text
zq1<-zq1[is.na(zq1[,2]) & is.na(zq1[,3]),]
tbl5[1,1] <- length(zq1[,1])
tbl5[1,2] <- tbl5[1,1] / tbl5[5,1]
zq1 <- zq
#url
zq1 <- zq1[is.na(zq1[,2]) & !is.na(zq1[,3]),]
tbl5[4,1] <- length(zq1[,1])
tbl5[4,2] <- tbl5[4,1] / tbl5[5,1]
zq1 <- zq
#image
zq1 <- zq1[grep("photo/1", zq1$parsed_media_url),]
tbl5[2,1] <- length(zq1[,1])
tbl5[2,2] <- tbl5[2,1] / tbl5[5,1]
zq1 <- zq
#video
zq1 <- zq1[grep("video/1", zq1$parsed_media_url),]
tbl5[3,1] <- length(zq1[,1])
tbl5[3,2] <- tbl5[3,1] / tbl5[5,1]

tbl5 <<- tbl5
}