#a more generic way to export an xlsx sheet.  meant to be used for frequency tables, but is flexible.
exportxlsx2 <- function()
{
  write.xlsx(tfreq, "statsre.xlsx", sheetName="tfreq")
  write.xlsx(ifreq, "statsre.xlsx", sheetName="ifreq", append = TRUE)
  write.xlsx(vfreq, "statsre.xlsx", sheetName="vfreq", append = TRUE)
  write.xlsx(ufreq, "statsre.xlsx", sheetName="ufreq", append = TRUE)
  write.xlsx(mentions, "statsre.xlsx", sheetName="freq", append = TRUE)
}

