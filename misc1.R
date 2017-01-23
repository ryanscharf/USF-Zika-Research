#conglomeration of spaghetti scripts used
#trying to generate table 5 in a better way

  tbl5 <- data.frame(matrix(ncol = 2, nrow = 4))
  rownames(tbl5) <- c("Text", "image", "video", "top 100")
  colnames(tbl5) <- c("#", "avg")
  
  #zq <- src[,c(4,26,7,5)]
  zq$retweet_count[zq$retweet_count== 0] <- NA
  zq<- zq[!is.na(zq$retweet_count),]
  tbl5[5,1] <- length(zq[,1])
  zq1 <-zq
  
  #text
  cd1$retweet_count[cd1$retweet_count== 0] <- NA
  cd1<- cd1[!is.na(cd1$retweet_count),]
  tbl5[1,1] <- length(cd1$tweet_id)
  
  #image
  ae1$retweet_count[ae1$retweet_count== 0] <- NA
  ae1<- ae1[!is.na(ae1$retweet_count),]
  tbl5[2,1] <- length(ae1$tweet_id)
  
  #video
  bf1$retweet_count[bf1$retweet_count== 0] <- NA
  bf1<- bf1[!is.na(bf1$retweet_count),]
  tbl5[3,1] <- length(bf1$tweet_id)
  