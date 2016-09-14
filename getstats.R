#get top 20 images, mentions, tweets, users, countries,
#library(plyr)
#have to have dplyr disabled, causes some conflict
#detach(dplyr)


getstats <- function(s, q)
{
#most common countries | $country
countrycount <- plyr::count(s$country)
countrycount <- na.omit(countrycount)
countrycount <<- head(arrange(countrycount, desc(freq)), n = q)

#most mentioned users | $mentioned_users
mentions <- plyr::count(s$mentioned_users)
mentions <- na.omit(mentions)
mentions <<- head(arrange(mentions, desc(freq)), n = q)

#most frequent tweeters | $screen_name
usrs <- plyr::count(s$screen_name)
usrs <<- head(arrange(usrs, desc(freq)), n = q)

#most common tweets | $text
tweets <- plyr::count(s$text)
tweets <<- head(arrange(tweets, desc(freq)), n = q)

#most retweeted users | $rt_screen_name
rt <- plyr::count(s$rt_screen_name)
rt <- na.omit(rt)
rt <- head(arrange(rt, desc(freq)), n = q)
rtusers <<- rt

}

