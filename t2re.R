t2re <-function(s, q)
{
  require(dplyr)
  require(xlsx)
  
zq <- s[is.na(s[,7]) & is.na(s[,26]),]
tfreq <- plyr::count(zq$rt_screen_name)
tfreq <- na.omit(tfreq)
tfreq <<- head(arrange(tfreq, desc(freq)), n = q)

zq <- s[grep("photo/1", s$parsed_media_url),]
ifreq <- plyr::count(zq$rt_screen_name)
ifreq <- na.omit(ifreq)
ifreq <<- head(arrange(ifreq, desc(freq)), n = q)

zq <- s[grep("video/1", s$parsed_media_url),]
vfreq <- plyr::count(zq$rt_screen_name)
vfreq <- na.omit(vfreq)
vfreq <<- head(arrange(vfreq, desc(freq)), n = q)

zq <- s[!is.na(s[,7]) & is.na(s[,26]),]
ufreq <- plyr::count(zq$rt_screen_name)
ufreq <- na.omit(ufreq)
ufreq <<- head(arrange(ufreq, desc(freq)), n = q)
}