#creates a series of tables of hashtag popularity by day and through the entire duration of data collection
#must have hashtags expanded by usage of getstats()

aug25 <- hashchart[hashchart$cleandate == "2016-08-25",]
aug25 <- apply(aug25[,2:21], 2, FUN=function(x) plyr::count(x))
aug25 <- do.call(rbind.data.frame, aug25)
aug25 <- plyr::ddply(aug25,"x",numcolwise(sum))
aug25 <- aug25[!is.na(aug25[,1]),]
rownames(aug25) = NULL
colnames(aug25) <- c("x", "2016-8-25")

aug26 <- hashchart[hashchart$cleandate == "2016-08-26",]
aug26 <- apply(aug26[,2:21], 2, FUN=function(x) plyr::count(x))
aug26 <- do.call(rbind.data.frame, aug26)
aug26 <- plyr::ddply(aug26,"x",numcolwise(sum))
aug26 <- aug26[!is.na(aug26[,1]),]
rownames(aug26) = NULL
colnames(aug26) <- c("x", "2016-8-26")

aug27 <- hashchart[hashchart$cleandate == "2016-08-27",]
aug27 <- apply(aug27[,2:21], 2, FUN=function(x) plyr::count(x))
aug27 <- do.call(rbind.data.frame, aug27)
aug27 <- plyr::ddply(aug27,"x",numcolwise(sum))
aug27 <- aug27[!is.na(aug27[,1]),]
rownames(aug27) = NULL
colnames(aug27) <- c("x", "2016-8-27")

aug28 <- hashchart[hashchart$cleandate == "2016-08-28",]
aug28 <- apply(aug28[,2:21], 2, FUN=function(x) plyr::count(x))
aug28 <- do.call(rbind.data.frame, aug28)
aug28 <- plyr::ddply(aug28,"x",numcolwise(sum))
aug28 <- aug28[!is.na(aug28[,1]),]
rownames(aug28) = NULL
colnames(aug28) <- c("x", "2016-8-28")

aug29 <- hashchart[hashchart$cleandate == "2016-08-29",]
aug29 <- apply(aug29[,2:21], 2, FUN=function(x) plyr::count(x))
aug29 <- do.call(rbind.data.frame, aug29)
aug29 <- plyr::ddply(aug29,"x",numcolwise(sum))
aug29 <- aug29[!is.na(aug29[,1]),]
rownames(aug29) = NULL
colnames(aug29) <- c("x", "2016-8-29")

aug30 <- hashchart[hashchart$cleandate == "2016-08-30",]
aug30 <- apply(aug30[,2:21], 2, FUN=function(x) plyr::count(x))
aug30 <- do.call(rbind.data.frame, aug30)
aug30 <- plyr::ddply(aug30,"x",numcolwise(sum))
aug30 <- aug30[!is.na(aug30[,1]),]
rownames(aug30) = NULL
colnames(aug30) <- c("x", "2016-8-30")

aug31 <- hashchart[hashchart$cleandate == "2016-08-31",]
aug31 <- apply(aug31[,2:21], 2, FUN=function(x) plyr::count(x))
aug31 <- do.call(rbind.data.frame, aug31)
aug31 <- plyr::ddply(aug31,"x",numcolwise(sum))
aug31 <- aug31[!is.na(aug31[,1]),]
rownames(aug31) = NULL
colnames(aug31) <- c("x", "2016-8-31")

sep1 <- hashchart[hashchart$cleandate == "2016-09-01",]
sep1 <- apply(sep1[,2:21], 2, FUN=function(x) plyr::count(x))
sep1 <- do.call(rbind.data.frame, sep1)
sep1 <- plyr::ddply(sep1,"x",numcolwise(sum))
sep1 <- sep1[!is.na(sep1[,1]),]
rownames(sep1) = NULL
colnames(sep1) <- c("x", "2016-9-01")

sep2 <- hashchart[hashchart$cleandate == "2016-09-02",]
sep2 <- apply(sep2[,2:21], 2, FUN=function(x) plyr::count(x))
sep2 <- do.call(rbind.data.frame, sep2)
sep2 <- plyr::ddply(sep2,"x",numcolwise(sum))
sep2 <- sep2[!is.na(sep2[,1]),]
rownames(sep2) = NULL
colnames(sep2) <- c("x", "2016-9-02")

sep3 <- hashchart[hashchart$cleandate == "2016-09-03",]
sep3 <- apply(sep3[,2:21], 2, FUN=function(x) plyr::count(x))
sep3 <- do.call(rbind.data.frame, sep3)
sep3 <- plyr::ddply(sep3,"x",numcolwise(sum))
sep3 <- sep3[!is.na(sep3[,1]),]
rownames(sep3) = NULL
colnames(sep3) <- c("x", "2016-9-03")

sep4 <- hashchart[hashchart$cleandate == "2016-09-04",]
sep4 <- apply(sep4[,2:21], 2, FUN=function(x) plyr::count(x))
sep4 <- do.call(rbind.data.frame, sep4)
sep4 <- plyr::ddply(sep4,"x",numcolwise(sum))
sep4 <- sep4[!is.na(sep4[,1]),]
rownames(sep4) = NULL
colnames(sep4) <- c("x", "2016-9-04")

sep5 <- hashchart[hashchart$cleandate == "2016-09-05",]
sep5 <- apply(sep5[,2:21], 2, FUN=function(x) plyr::count(x))
sep5 <- do.call(rbind.data.frame, sep5)
sep5 <- plyr::ddply(sep5,"x",numcolwise(sum))
sep5 <- sep5[!is.na(sep5[,1]),]
rownames(sep5) = NULL
colnames(sep5) <- c("x", "2016-9-05")

#combines each day into one master table
hashtable <- Reduce(function(x,y) merge(x,y, all=TRUE), list(aug25,aug26,aug27,aug28,aug29,aug30,aug31,sep1,sep2,sep3,sep4,sep5))
hashtable$total <- rowSums(hashtable[,2:13], na.rm=FALSE)
hashtable$total <- rowSums(hashtable[,2:13], na.rm=TRUE)
hashtable <- arrange(hashtable, desc(total))
