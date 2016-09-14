#most favorited tweets

mostfavs <- function(s,q)
{
    require(dplyr)
    favs <- s %>% group_by(text) %>% summarise(favorite_count = max(favorite_count))
    
    favs <<- head(plyr::arrange(favs, desc(favorite_count)), n = q)
  
}