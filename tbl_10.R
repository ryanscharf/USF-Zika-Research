#src is your source data set
#generate table 10; analysis of most retweets

tbl_10<-function(src)
{
tbl10 <- src[src$text %in% topretweets$text, ]
tbl10 <- arrange(tbl10, desc(retweet_count))
tbl10 <- tbl10[!duplicated(tbl10$text),]
tbl10 <- tbl10[,c(9,13, 4, 27, 26,7,5,6,15)]
tbl10 <<- tbl10[,c(3, 5,6, 2, 9, 7,8)]
}