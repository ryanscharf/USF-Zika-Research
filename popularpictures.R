#get the most (q) tweeted photos from (s) source.  IE. zika
#have to have dplyr unloaded, causes some conflict

##sub <- subset(parsed_zika_12days, !is.na(parsed_media_type));
##toppics <- sub %>% group_by(parsed_media_url) %>% summarise(pictures = max(parsed_media_url))


getpics <- function(s, q)
{
  require(plyr)
topphotos <- plyr::count(s$parsed_media_url)
topphotos <- na.omit(topphotos)
topphotos <<- head(arrange(topphotos, desc(freq)), n = q)
}