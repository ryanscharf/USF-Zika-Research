#table 5 excluding tweets that haven't been retweeted
t2u <- zq[zq$retweet_count == 0,]
t2uae <- ae[ae$retweet_count == 0, ]
t2ubf <- bf[bf$retweet_count == 0, ]
t2ucd <- cd[cd$retweet_count == 0, ]

tfreq <- plyr::count(t2ucd$screen_name)
tfreq <- na.omit(tfreq)
tfreq <- head(arrange(tfreq, desc(freq)), n = 100)

ifreq <- plyr::count(t2uae$screen_name)
ifreq <- na.omit(ifreq)
ifreq <- head(arrange(ifreq, desc(freq)), n = 100)

vfreq <- plyr::count(t2ubf$screen_name)
vfreq <- na.omit(vfreq)
vfreq <- head(arrange(vfreq, desc(freq)), n = 100)

ufreq <- plyr::count(t2u$screen_name)
ufreq <- na.omit(ufreq)
ufreq <- head(arrange(ufreq, desc(freq)), n = 100)