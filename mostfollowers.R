#get max number of followers from parsed_zika
#s is your target data set
#q is how many rowss you want returned in your tables

mostfollowers <- function(s, q)
{
  require(dplyr)
  
sub <- subset(s, select = c(screen_name, followers_count))

topfollowers <- sub %>% group_by(screen_name) %>% summarise(followers = max(followers_count))

topfollowers <<- head(arrange(topfollowers, desc(followers)), n = q)
}
