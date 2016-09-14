exportxlsx <- function()
{
  require(xlsx)
  write.xlsx(countrycount, file = "stats.xlsx", sheetName="countrycount")
  write.xlsx(mentions, file = "stats.xlsx", sheetName="mentions", append = TRUE)
  write.xlsx(rtusers, file = "stats.xlsx", sheetName="rtusers", append = TRUE)
  write.xlsx(topfollowers, file = "stats.xlsx", sheetName="topfollowers", append = TRUE)
  write.xlsx(topphotos, file = "stats.xlsx", sheetName="topphotos", append = TRUE)
  write.xlsx(topretweets, file = "stats.xlsx", sheetName="topretweets", append = TRUE)
  write.xlsx(tweets, file = "stats.xlsx", sheetName="tweets", append = TRUE)
  write.xlsx(favs, file = "stats.xlsx", sheetName="favs", append = TRUE)
  write.xlsx(usrs, file = "stats.xlsx", sheetName="usrs", append = TRUE)
  
}