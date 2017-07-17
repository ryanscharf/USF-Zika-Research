library(twitteR)
library(rjson)
library(httr)

setup_twitter_oauth("LIewVbg8raOPeNjMBXhryHM4e", "xhXGtGtGj4ByhFWWmvPv4yPBaUml5a0mIt7GYMOXXaXAfJkSYz",
                    "128995370-Lhfdwntzb6E8HsJEI3xmvvIgWWu3tKus2Xfga5ZF", "ZWaUXf7met4kgHDZ05cMwZQjYpRjPnlyqfCQYC65qAsNh")

twitlist <- function(listowner, listname){
  
  api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
                    listname, "&owner_screen_name=", listowner, "&count=5000")
  response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
  
  response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  users.names <- sapply(response.list$users, function(i) i$name)
  users.screennames <- sapply(response.list$users, function(i) i$screen_name)
  
  return(data.frame(users.screennames, users.names))
}


#twitlistdf <- function(df){

#  apply(df, 1, twitlist, arg1 = df$owner, arg2 = df$list)}
#a <- mapply(twitlist, paste0(twitlists[1]), paste0(twitlists[2]))

a <- NULL
for (i in 1:length(twitlists$X1)) {
  b <- twitlist(twitlists[i,1], twitlists[i,2])
  a <- rbind(b,a)}

getfriendslist <- function(handl) {
  tempname <- deparse(substitute(handl))
  user <- getUser(tempname)
  #friends <- lookupUsers(user$getFriendIDs())
  friends.names <- sapply(friends,name)
  friends.screennames <- sapply(friends, screenName)
  friends <- data.frame(friends.screennames, friends.names)
  tempname <-  paste(tempname, "_friends", sep = "")
  assign(tempname, friends, envir = globalenv())
}

getfollowerslist <- function(handl) {
  tempname <- deparse(substitute(handl))
  # user <- getUser(tempname)
  followers <- lookupUsers(user$getFollowers())
  followers.names <- sapply(followers,name)
  followers.screennames <- sapply(followers, screenName)
  followers <- data.frame(followers.screennames, followers.names)
  tempname <-  paste(tempname, "_followers", sep = "")
  assign(tempname, followers, envir = globalenv())
}

countstats <- function(handl, df) {
  z <- deparse(substitute(handl))
  a <- nrow(df[grep(paste0("\\b",noquote(z),"\\b"), df$screen_name, ignore.case = T), ])
  b <- nrow(df[grep(paste0("\\b",noquote(z),"\\b"), df$rt_screen_name, ignore.case = T), ])
  c <- nrow(parsed30d[grep(paste0("\\b",noquote(z),"\\b"), parsed30d$mentioned_users, ignore.case = T), ])
  cat(paste(z, " - tweets:", a, ", retweets: ", b, ", mentions: ", c))
}

