getall <- function(s,q, y = NULL)
{
  getstats(s,q)
  getpics(s,q)
  mostfavs(s,q)
  mostfollowers(s,q)
  toprtt(s,q)
  
  if(isTRUE(y)) exportxlsx()
  }