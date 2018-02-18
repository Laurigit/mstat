peliData <- luecsv("pelit.csv")
# LP<-1
# MP<-6
# LMull <- 0
# MMull <- 0
# Aloittaja <- 0

maxturnaus <- peliData[!is.na(Voittaja), max(TurnausNo)]-1
tulostaulu <- NULL
for (TNo in 5:maxturnaus) {
  pakat<-omaReadJson("./external_files/")
  pfi_data<-pakkaUutuusProsentti(pakat)
  peliData_SS <- peliData[TurnausNo<=TNo]
  pelidata_joined_pakkatiedot<-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData_SS)

  #ennusta
  ennustePelit <- peliData[TurnausNo == (TNo + 1), .(Laurin_pakka, Martin_pakka, 
                                                     Laurin_mulligan,
                                                     Martin_mulligan,
                                                     Mull_diff = Martin_mulligan - Laurin_mulligan, 
                                                     Aloittaja,
                                                     VS_peli_bool = 1,
                                                     peli_ID, Voittaja)]
  ennustePelit[, ennuste := voittoEnnuste(Laurin_pakka, Martin_pakka,
                                          pelidata_joined_pakkatiedot,
                                          Laurin_mulligan,
                                          Martin_mulligan,
                                          Aloittaja), by = peli_ID]
  
  tulostaulu <- rbind(tulostaulu, ennustePelit)

}

tulostaulu[, ennusteMittari := abs(Voittaja- ennuste)]

summary(tulostaulu[ennuste >0.7 | ennuste < 0.3])
