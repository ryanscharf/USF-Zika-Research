#top external links
library(plyr, dplyr)

ext <- count(parsed_zika_12days$expanded_url)
ext <- na.omit(ext)

ext <- head(arrange(ext, desc(freq)), n = 20)
