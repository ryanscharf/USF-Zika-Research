#get quanity (q) of top retweets from source (s)
#library(plyr)
#library(dplyr)

toprtt <- function(s, q)
{
  require(dplyr)
topretweets <<- s %>% group_by(text) %>% summarise(retweet_count = max(retweet_count))

topretweets <<- head(plyr::arrange(topretweets, desc(retweet_count)), n = q)
}

