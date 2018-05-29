# 
# divaridata<-luecsv("divari.csv")
# peliData <- luecsv("pelit.csv")
# 
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)



create_data_for_win_disribution <- function(peliData_ja_pfi, divaridata) {

maxTurnaus <- peliData_ja_pfi[, max(TurnausNo)]
all_data <- NULL
for (loop_no in (maxTurnaus - 10):(maxTurnaus-1)) {
  kierrosData <- create_forecast_data_for_stats(peliData_ja_pfi, divaridata, loop_no)
  kierrosData[, TurnausNo := loop_no]
  all_data <- rbind(all_data, kierrosData)
  print(paste0("kierros", loop_no))
}
return(all_data)
# kierrosData[Pakka==1 & Vastustajan_Pakka == 8 ]
# all_data[Pakka==8 & Vastustajan_Pakka == 1 & TurnausNo==19]
}