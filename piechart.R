##stuff that starts with a url
##url <- zika1[is.na(zika1[,26]),]
##url <- url[grepl('^http', url$text),]
##url <- url[-grepl("[[:blank:]]", url$text),]
#generates values to create a nested pi chart to help visualize the relationship
#between subgroups of the data 

zq <- zika1
zqimage <- zq[grep("photo/1", zq$parsed_media_url),]
zqimageurl <- zqimage[!is.na(zqimage[,7]),]
zqimage <- zqimage[!(zqimage$tweet_id %in% zqimageurl$tweet_id),]
zqremainder <- zq[!(zq$tweet_id %in% zqimage$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqimageurl$tweet_id),]

zqvideo <- zqremainder[grep("video/1", zqremainder$parsed_media_url),]
zqvideourl <- zqvideo[!is.na(zqvideo[,7]),]
zqvideo <- zqvideo[!(zqvideo$tweet_id %in% zqvideourl$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqvideo$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqvideourl$tweet_id),]

zqurl <- zqremainder[!is.na(zqremainder[,7]),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqurl$tweet_id),]

g <- zqremainder[grep("http", zqremainder$text),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% g$tweet_id),]

zqtext <- zqremainder

length(zqvideo$tweet_id) + length(zqvideourl$tweet_id) + length(zqimage$tweet_id) + length(zqimageurl$tweet_id) + 
  length(zqurl$tweet_id) + length(g$tweet_id) + length(zqtext$tweet_id)

zqremainder <- zq
zqremainder <- zq[!(zq$tweet_id %in% zqimageurl$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqimage$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqvideo$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqvideourl$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqtext$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% zqurl$tweet_id),]
zqremainder <- zqremainder[!(zqremainder$tweet_id %in% g$tweet_id),]
