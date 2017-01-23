#3rd revision of table generation, usable for multiple tables & in conjunction with exportxlsx2.r


  tfreq <- plyr::count(cd$country) 
  tfreq <- na.omit(tfreq)
  tfreq <- head(arrange(tfreq, desc(freq)), n = 100)
  
  ifreq <- plyr::count(ae$country)
  ifreq <- na.omit(ifreq)
  ifreq <- head(arrange(ifreq, desc(freq)), n = 100)
  
  vfreq <- plyr::count(bf$country)
  vfreq <- na.omit(vfreq)
  vfreq <- head(arrange(vfreq, desc(freq)), n = 100)
  
  write.xlsx(tfreq, "statsre.xlsx", sheetName="tfreq")
  write.xlsx(ifreq, "statsre.xlsx", sheetName="ifreq", append = TRUE)
  write.xlsx(vfreq, "statsre.xlsx", sheetName="vfreq", append = TRUE)
  write.xlsx(countrycount, "statsre.xlsx", sheetName="freq", append = TRUE)
  