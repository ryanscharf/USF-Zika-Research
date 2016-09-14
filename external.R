#top external links
#requires plyr
#must unload dplyr
getext <- function(s,q)
ext <- plyr::count((s$expanded_url))
ext <- na.omit(ext)

ext <- head(arrange(ext, desc(freq)), n = q)
