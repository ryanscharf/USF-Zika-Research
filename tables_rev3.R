#3rd revision of table generation, usable for multiple tables & in conjunction with exportxlsx2.r


  tfreq <- plyr::count(cd$screen_name) 
  tfreq <- na.omit(tfreq)
  tfreq <- arrange(tfreq, desc(freq))
  
  ifreq <- plyr::count(ae$screen_name)
  ifreq <- na.omit(ifreq)
  ifreq <- arrange(ifreq, desc(freq))
  
  vfreq <- plyr::count(bf$screen_name)
  vfreq <- na.omit(vfreq)
  vfreq <- arrange(vfreq, desc(freq))
  
  write.csv(ifreq, "ifreq.csv")
  write.csv(vfreq, "vfreq.csv")
  write.csv(usrs, "usrs.csv")
  
  write.xlsx(tfreq, "statsre.xlsx", sheetName="tfreq")
  write.xlsx2(ifreq, "statsre.xlsx", sheetName="ifreq", append = TRUE)
  write.xlsx2(vfreq, "statsre.xlsx", sheetName="vfreq", append = TRUE)
  write.xlsx2(usrs, "statsre.xlsx", sheetName="freq", append = TRUE)
  