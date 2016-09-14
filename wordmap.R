
makecloud <- function(s)
{
require(tm)
require(SnowballC)
require(wordcloud)

ti <- Corpus(VectorSource(s$text))
ti <- tm_map(ti, PlainTextDocument)
ti <- tm_map(ti, stripWhitespace)
ti <- tm_map(ti, tolower)
ti <- tm_map(ti, removeNumbers)
ti <- tm_map(ti, removePunctuation)
ti <- tm_map(ti, removeWords, stopwords("english"))


wordcloud(ti, scale = c(8,0.5), max.words = 300, min.freq = 25, random.order = FALSE, rot.per =.35, colors=brewer.pal(8,"YlOrRd"))
}