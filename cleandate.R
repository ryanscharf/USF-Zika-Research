#clean dates and 

cleandate <- function(s)
{
  s$cleandate <<- strptime(s$datetime, "%Y-%m-%d %H:%M:%S")
  s$cleandate <<- format(zika1$cleandate, "%Y-%m-%d")
  }