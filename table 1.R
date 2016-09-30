reeetweets <- fl %>% group_by(text) %>% summarise(retweet_count = max(retweet_count))
reeetweets[reeetweets == 0] <- NA
reeetweets<- na.omit(reeetweets)


##
#number of unique text only tweets
bss <- fl[is.na(fl[,7]) & is.na(fl[,26]),]
#number of unique text only agents
length(unique(bss$screen_name))
#numberb ofo unique text only coutnries
length(unique(bss$country))

##
#number of unique url only tweets
bss <- fl[is.na(fl[,7]) & !is.na(fl[,26]),]
#number of unique url only agents
length(unique(bss$screen_name))
#numberb ofo unique url only coutnries
length(unique(bss$country))

##
#number of unique, photo only tweets
bss <- fl[grep("photo/1", fl$parsed_media_url),]
#number of unique photo only agents
length(unique(bss$screen_name))
#number of unique photo only countries
length(unique(bss$country))

##
#number of unique, video only tweets
bss <- fl[grep("video/1", fl$parsed_media_url),]
#number of unique video only agents
length(unique(bss$screen_name))
#number of unique video only countries
length(unique(bss$country))
