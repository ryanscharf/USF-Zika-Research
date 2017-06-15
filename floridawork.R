library(dplyr)
library(plyr)
library(ggplot2)
library(ggthemes)
library(Cairo)
library(tidyr)


#import a list of directories for the extracted file
dirs <- list.dirs('~/zika/USF-Zika-Research/raw month', recursive=FALSE)
#append the file to the end of each directory
dirs <- paste(dirs, "/data.txt", sep="")
#apply the parse function to the list of files
parsed30d <- lapply(dirs, parseTweets_mod)
#turns the list of data frames into one data frame
parsed30d <- bind_rows(parsed30d)

#dplyr method to parse tweets; slower by 1.4% than lapply method
dirs <- data.frame(dirs)
parsed30d <- as.data.frame(dirs) %>% rowwise() %>% do(parseTweets_mod(paste(.$dirs)))

#turning NAs to 0s for favorite count and retweet count
parsed30d$retweet_count[is.na(parsed30d$retweet_count)] <- 0
parsed30d$favorite_count[is.na(parsed30d$favorite_count)] <- 0


#cleaning up the date field
parsed30d$cleandate <- strptime(parsed30d$datetime, "%Y-%m-%d %H:%M:%S")
parsed30d$cleandate <- format(parsed30d$cleandate, "%Y-%m-%d")

#get rid of duplicate tweetids, 266 duplicate tweet_ids
dup30 <- plyr::count(parsed30d$tweet_id)
dup30 <- dup30[dup30$freq > 1, ]
parsed30d <- parsed30d[!(parsed30d$tweet_id %in% dup30$x), ]

#subset data to be only tweets dealing with Florida
#flexpr <- c("fl","FL", "Fl", "Fla","FLA", "fla","Florida", "FLORIDA", "florida")
#fl30d <- parsed30d[grep(paste(flexpr, collapse = "|"), parsed30d, fixed = TRUE),]
fl30d <- parsed30d[grep('(\\bfl\\b)|(\\bfla\\b)|(\\bflorida\\b)', parsed30d$text, perl=T, ignore.case = T), ]
#doublechecking grep outcome
fl30d1 <- parsed30d[grep("fl|FL|Fl|Fla|FLA|fla|Florida|FLORIDA|florida", parsed30d$text), ]
notfl <- fl30d1[!(fl30d1$tweet_id %in% fl30d$tweet_id), ]
#cleanup
rm(fl30d1, notfl)

#graphing tweet volume per day
CairoWin()
flddates <- plyr::count(fl30d$cleandate)
ggplot(flddates, aes(x=x, y = freq, group = 1)) + geom_line() + labs(x = "Date", y = "Number of Tweets") + ggtitle("Number of Posts Per Day") +
 theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12),
                     panel.grid.major = element_line(color = "grey", size = .25)) 
ggsave(file = "30dposts.png", type="cairo-png")

#graph cumulative percentages of total
CairoWin()
ggplot(flddates, aes(x = x, y = 100*cumsum(freq)/sum(freq), group = 1)) +  geom_line() + labs(x = "Date", y = "Percentage of Total (%)") + ggtitle("Cumulative Sum of Tweets (%)") +
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12),
                      panel.grid.major = element_line(color = "grey", size = .25)) 
ggsave(file = "30dcumsums.png", type="cairo-png")

#add in a mean line
CairoWin()
ggplot(flddates, aes(x = x, y = 100*cumsum(freq)/sum(freq), group = 1)) +  geom_line() + labs(x = "Date", y = "Percentage of Total (%)") + ggtitle("Cumulative Sum of Tweets (%) w/ Mean") +
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12),
                      panel.grid.major = element_line(color = "grey", size = .25)) + geom_abline(intercept = 0, slope = 100/length(flddates$x), color = "blue")
ggsave(file = "30dcumsumsmean.png", type="cairo-png")

#start hashtag analysis
fl30dh <- separate(fl30d, col = hashes, into = c("hash1", "hash2", "hash3", 
                                                "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", 
                                                "hash12", "hash13", "hash14", "hash15", "hash16", "hash17", "hash18", "hash19",
                                                "hash20"), sep = ";", extra = "merge", fill = "right")
#transalate all hashtags to lowercase and count.  Nearly 14% of hashes (326) had varying cases.
flhashes <- apply(fl30dh[,24:43], 2, FUN=function(x) plyr::count(stringi::stri_trans_tolower(x)))
flhashes <- do.call(rbind.data.frame, flhashes)
flhashes <- plyr::ddply(flhashes,"x",numcolwise(sum))
flhashes <- flhashes[!is.na(flhashes[,1]), ]
flhashes <- arrange(flhashes, desc(freq))
#the 95th percentile of hashes are those that occur 27 times or more.  106 hashes meet this criteria.
flhashes <- flhashes[flhashes$freq > 26, ]
flhashes <- arrange(flhashes,desc(freq))

#shaping data for a stacked area plot of hashtag ifnormation
flhashsap <- fl30dh[ ,24:46]
flhashsap <- flhashsap[ ,-c(21:22)]
flhashsap <- flhashsap[!(is.na(flhashsap$hash1)), ] #looking only for tweets that have hashes. includes 12,109 tweets
flhashsap <- as.data.frame(sapply(flhashsap,tolower))

#going to make a loop to make the data more managable
xz <- colnames(flhashsap)
flhashsap1 <- data.frame(flhashsap$cleandate, flhashsap$hash1)
colnames(flhashsap1) <- c("date", "hashtag")

for(i in 2:20) {
  tempdf <- data.frame(flhashsap$cleandate, flhashsap[[i]])
  colnames(tempdf) <- c("date", "hashtag")
  flhashsap1 <<- rbind(flhashsap1, tempdf)
    } 
rm(xz)
flhashsap1 <- flhashsap1[!(is.na(flhashsap1$hashtag)), ]
flhashsap1 <- flhashsap1 %>% group_by(date, hashtag) %>% tally()
flhashsapc <- spread(flhashsap1, key = hashtag,  value = n)       #
flhashsap1 <- flhashsap1[flhashsap1$hashtag %in% flhashes[1:10,1], ]
#even the most popular hashtags don't appear every day, which causes holes in graphs, there were 15
flhashsap1<-merge(flhashsap1,expand.grid(date=unique(flhashsap1$date),hashtag=unique(flhashsap1$hashtag),stringsAsFactors=F),all.y=T)
flhashsap1$n[is.na(flhashsap1$n)] <- 0
flhashsap1$date <- strptime(flhashsap1$date, "%Y-%m-%d")
flhashsap1$date <- format(flhashsap1$date, "%m-%d")

#100% stacked area plot of top 10 hashtags
CairoWin()
ggplot(flhashsap1, aes(x = date, y = n, group = hashtag)) + geom_area(colour="black", size=.4, aes(color = hashtag, fill = hashtag, group = hashtag), position = "fill") +
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12)) +
  scale_fill_viridis(option = "plasma", discrete = TRUE) + labs(x = "Date", y = "Percentage of Total") +
  ggtitle("100% Stacked Area Chart of the 10 Most Common Hashtags") + scale_y_continuous(labels = scales::percent, expand = c(0,0)) + scale_x_discrete(expand = c(0,0))
ggsave(file = "top10hashtags_100stackedareaplot.png", type="cairo-png")

#stacked area plot of top 10 hashtags
CairoWin()
ggplot(flhashsap1, aes(x = date, y = n, group = hashtag)) + geom_area(colour="black", size=.4, aes(color = hashtag, fill = hashtag, group = hashtag), position = "stack") +
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12),  
  panel.grid.major = element_line(color = "grey", size = .25)) + scale_fill_viridis(option = "plasma", discrete = TRUE) + labs(x = "Date", y = "Number of Hashtags") +
  ggtitle("Stacked Area Chart of the 10 Most Common Hashtags") + scale_y_continuous(breaks = seq(0,1500, by = 200), expand = c(0,0), limits = c(0, 1500)) +
  scale_x_discrete(expand = c(0,0))
ggsave(file = "top10hashtags_stackedareaplot.png", type = "cairo-png")

#Line charts of top 10 hashtags
CairoWin()
ggplot(flhashsap1, aes(x = date, y = n, group = hashtag)) + geom_line(aes(group = hashtag, color = hashtag)) +
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12),  
                      panel.grid.major = element_line(color = "grey", size = .25)) + scale_color_viridis(option = "plasma", discrete = TRUE, direction = -1) + labs(x = "Date", y = "Number of Hashtags") +
  ggtitle("Line Chart of the 10 Most Common Hashtags") + scale_y_continuous(breaks = seq(0,600, by = 100), expand = c(0,0), limits = c(0, 600)) +
  scale_x_discrete(expand = c(0,0))
ggsave(file = "top10hashtags_linechart.png", type = "cairo-png")

#scatter plot of 95th percentile of hashtag occurances.  Fairly uninformative.
CairoWin()
ggplot(flhashes, aes(reorder(x,-freq),freq)) + geom_point() + theme_few() + labs(x = NULL, y = "Number of Tweets") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_abline(intercept = 27, slope = 0, color = "blue")
ggsave(file = "hashtagdistribution.png", type = "cairo-png")

flhashsapc <- t(flhashsapc)
flhashsapc <- data.frame(flhashsapc)
colnames(flhashsapc) <- as.character(unlist(flhashsapc[1, ]))
flhashsapc <- flhashsapc[-1, ]

indx <- sapply(flhashsapc, is.factor)
flhashsapc[indx] <- lapply(flhashsapc[indx], function(x) as.numeric(as.character(x)))
flhashsapc$hashtag <- rownames(flhashsapc)
flhashsapc <- flhashsapc[,c(ncol(flhashsapc),1:(ncol(flhashsapc)-1))]

flhashsapc <- flhashsapc %>% replace(is.na(.), 0 ) %>% mutate(total = rowSums(.[2:29])) %>% arrange(desc(total))