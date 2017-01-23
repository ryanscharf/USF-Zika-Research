#reeetweets <- zika1 %>% group_by(text) %>% summarise(retweet_count = max(retweet_count))
#reeetweets[reeetweets == 0] <- NA
#reeetweets<- na.omit(reeetweets)

#original script for creating table 1

#number of  text only tweets
bss <- zika1[is.na(zika1[,7]) & is.na(zika1[,26]),]
length(bss[,1])
#number of unique text only agents
length(unique(bss$screen_name))
#numberb ofo unique text only coutnries
length(unique(bss$country))

#number of url only tweets
bss <- zika1[!is.na(zika1[,7]) & is.na(zika1[,26]),]  # is.na(zika1[,7] calculate tweets with NO url. I would think replacing "bss" with different variable name would be a good idea. what do you think?
length(bss[,1])
#number of unique url only agents
length(unique(bss$screen_name))
#numberb ofo unique url only coutnries
length(unique(bss$country))

#number of tweets that have photos embedded
bss <- zika1[grep("photo/1", zika1$parsed_media_url),] 
length(bss[,1])
#number of unique photo only agents
length(unique(bss$screen_name))
#number of unique photo only countries
length(unique(bss$country))

#number of tweets with videos embedded
bss <- zika1[grep("video/1", zika1$parsed_media_url),]
length(bss[,1])
#number of unique video only agents
length(unique(bss$screen_name))
#number of unique video only countries
length(unique(bss$country))

#get numbers for mixed things
bss <- zika1[!is.na(zika1[,7]) & !is.na(zika1[,26]),]
length(bss[,1])
length(unique(bss$screen_name))
length(unique(bss$country))
