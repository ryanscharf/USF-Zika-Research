#calls many different fucntions to create a set of basic tables
#s is your target data set
#q is how many rowss you want returned in your tables

getall <- function(s,q, y = NULL)
{
  getstats(s,q)
  getpics(s,q)
  mostfavs(s,q)
  mostfollowers(s,q)
  toprtt(s,q)
  
  if(isTRUE(y)) exportxlsx()
  }