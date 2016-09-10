#initial R script to parse 
#streaming twitter API
#Thomas Keller
#thomas.e.keller@gmail.com

#code and parser based off streamR parseTweets function
#https://github.com/pablobarbera/streamR/blob/master/streamR/R/parseTweets.R

#helper function not meant for the light of day
#breaks out list columns of tweets



unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3 & field[1]!="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (field[1]=="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
  }
  
  if (length(field)==4 && field[2]!="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
  }
  if (length(field)==4 && field[2]=="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (length(field)==6 && field[2]=="bounding_box"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) 
      x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
  }
  return(vect)
}

#small extra functions to parse the two more complicated fields that return lists themselves...user mentions
#right now merging into a single string with ";" as a separator
parse_user=function(user_mentions){
  num_mention=length(user_mentions)
  if(num_mention==0){return(NA)}
  else if(num_mention==1){
    return(user_mentions[[1]]$screen_name) 
  }
  else{return(paste(sapply(1:length(user_mentions),function(x) user_mentions[[x]]$screen_name),collapse=';'))}
}

parse_id=function(user_mentions){
  num_mention=length(user_mentions)
  if(num_mention==0){return(NA)}
  else if(num_mention==1){
    return(user_mentions[[1]]$id_str) 
  }
  else{return(paste(sapply(1:length(user_mentions),function(x)
    user_mentions[[x]]$id_str),collapse=';'))}
}

parse_hash=function(hashtags){
  num_hash=length(hashtags)
  if(num_hash==0){return(NA)}
  else if(num_hash==1){
    return(hashtags[[1]]$text) 
  }
  else{return(paste(sapply(1:length(hashtags),function(x)
    hashtags[[x]]$text),collapse=';'))}
}

parse_media_type=function(media){
  num_media=length(media)
  if(num_media==0){return(NA)}
  else if(num_media==1){
    return(media[[1]]$type) 
  }
  else{return(paste(sapply(1:length(media),function(x)
    media[[x]]$type),collapse=';'))}
}

parse_media_url=function(media){
  num_media=length(media)
  if(num_media==0){return(NA)}
  else if(num_media==1){
    return(media[[1]]$expanded_url) 
  }
  else{return(paste(sapply(1:length(media),function(x)
    media[[x]]$expanded_url),collapse=';'))}
}

######### 
#main parser
#code
########


library(streamR)

parseTweets_mod=function(jsonfile){
  tweet_list=readTweets(jsonfile)
  tot_tweet=length(tweet_list)
  tweet_list=lapply(tweet_list, function(x) if(x$lang=='en' & x$user$followers_count>=25 & x$user$friends_count>=100 ) return(x))
  tweet_list=tweet_list[!sapply(tweet_list,is.null)] # culls out the nulled elements
  #reweet=lapply(tweet_list,function(x) if(is.null(x$retweeted_status)==TRUE) return(NULL) else(return(x)))
  #retweet=retweet[!sapply(retweet,is.null)]
  print(paste('After simple spam filtering',length(tweet_list),'tweets remain' , 100*length(tweet_list)/tot_tweet,'%'))
  place_lat_1 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 1, 2))
  place_lat_2 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 2, 2)) 
  place_lat = sapply(1:length(tweet_list), function(x) 
    mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE))
  place_lon_1 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 1, 1))
  place_lon_2 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 3, 1))
  place_lon = sapply(1:length(tweet_list), function(x) 
    mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE))
  
  mentions=lapply(1:length(tweet_list),function(x) tweet_list[[x]]$entities$user_mentions)
  hashes=lapply(1:length(tweet_list),function(x) tweet_list[[x]]$entities$hash)
  media=lapply(1:length(tweet_list),function(x) tweet_list[[x]]$entities$media)
  parsed_user=sapply(1:length(mentions), function(x) parse_user(mentions[[x]]))
  parsed_id=sapply(1:length(mentions), function(x) parse_id(mentions[[x]]))
  parsed_hash=sapply(1:length(hashes),function(x) parse_hash(hashes[[x]]))
  parsed_media_type=sapply(1:length(media),function(x) parse_media_type(media[[x]]))
  parsed_media_url=sapply(1:length(media),function(x) parse_media_url(media[[x]]))
  text=sapply(tweet_list, function(x) ifelse(is.null(x$retweeted_status), x$text, x$retweeted_status$text))
  timestamp_ms = unlistWithNA(tweet_list, 'timestamp_ms')
  datetime =as.POSIXct(as.numeric(as.character(timestamp_ms))/1000, origin='1970-01-01',tz='EST')
  df=data.frame(
    timestamp_ms = timestamp_ms,
    datetime=datetime,
    tweet_id = unlistWithNA(tweet_list, "id_str"),
    text=iconv(text,to='UTF-8', sub = "byte"), 
    retweet_count = unlistWithNA(tweet_list, c('retweeted_status','retweet_count')),
    favorite_count = unlistWithNA(tweet_list, c('retweeted_status','favorite_count')),
    expanded_url = unlistWithNA(tweet_list, c('entities', 'urls', 1, 'expanded_url')),
    friends_count = unlistWithNA(tweet_list, c('user', 'friends_count')),
    screen_name = unlistWithNA(tweet_list, c('user', 'screen_namtwe')),
    user_id_str = unlistWithNA(tweet_list, c('user', 'id_str')),
    in_reply_to_screen_name = unlistWithNA(tweet_list, ('in_reply_to_screen_name')),
    in_reply_to_user_id = unlistWithNA(tweet_list, ('in_reply_to_user_id_str')),
    rt_screen_name = unlistWithNA(tweet_list,c('retweeted_status','user','screen_name')),
    rt_screen_id = unlistWithNA(tweet_list,c('retweeted_status','user','id_str')),
    country = unlistWithNA(tweet_list, c('place', 'country')),
    full_name = unlistWithNA(tweet_list, c('place', 'full_name')),
    followers_count = unlistWithNA(tweet_list, c('user', 'followers_count')),
    #place_lat_1 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 1, 2)),
    #place_lat_2 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 2, 2)),
    place_lat = sapply(1:length(tweet_list), function(x) 
      mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE)),
    #place_lon_1 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 1, 1)),
    #place_lon_2 = unlistWithNA(tweet_list, c('place', 'bounding_box', 'coordinates', 1, 3, 1)),
    place_lon = sapply(1:length(tweet_list), function(x) 
      mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE)),
    lat = unlistWithNA(tweet_list, c('geo', 'coordinates', 1)),
    lon = unlistWithNA(tweet_list, c('geo', 'coordinates', 2)),
    mentioned_users=parsed_user,
    mentioned_id=parsed_id,
    hashes=parsed_hash,
    parsed_media_type=parsed_media_type,
    parsed_media_url=parsed_media_url,
    stringsAsFactors=F
  )
  return(df)
}

args<-commandArgs(TRUE)
tweets=parseTweets_mod(args[1])
first=args[3]
tweets$retweet_count[is.na(tweets$retweet_count)]=0
tweets$favorite_count[is.na(tweets$favorite_count)]=0
if(first=='1') {
  write.table(tweets,file=args[2],quote=T,row.names=F,append=T,sep=',',qmethod='double')
  } else {
  write.table(tweets,file=args[2],quote=T,col.names=F,row.names=F,append=T,sep=',',qmethod='double')
  }
#clean up the NA's in favorite/retweets, should be 0's if "retweeted_status" is not filled


