#get top (q) retweets from source (s)
#library(plyr)
#library(dplyr)
#devtools::use_package("plyr", "imports")
#devtools::use_package("dplyr", "imports")


toprtt <- function(s, q)
{
  require(dplyr)
topretweets <<- s %>% group_by(text) %>% summarise(retweet_count = max(retweet_count))

topretweets <<- head(plyr::arrange(topretweets, desc(retweet_count)), n = q)
}

