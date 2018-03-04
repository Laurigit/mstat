#peliData <- luecsv("pelit.csv")
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
  pelidata_joined_pakkatiedot<-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)

  #ennusta
  ennustePelit <- pelidata_joined_pakkatiedot[TurnausNo == (TNo + 1), .(Laurin_pakka, Martin_pakka, 
                                                     Laurin_mulligan,
                                                     Martin_mulligan,
                                                     Mull_diff = Martin_mulligan - Laurin_mulligan, 
                                                     Aloittaja,
                                                     VS_peli_bool = 1,
                                                     peli_ID, Voittaja,
                                                     laurin_kortti_lkm,
                                                     hinta_lauri,
                                                     hinta_martti,
                                                     martin_kortti_lkm
                                                     )]
  ennustePelit[, ennuste := voittoEnnuste(Laurin_pakka, Martin_pakka,
                                          pelidata_joined_pakkatiedot,
                                          ifelse(is.na(Laurin_mulligan),0,Laurin_mulligan),
                                          ifelse(is.na(Martin_mulligan),0,Martin_mulligan),
                                          Aloittaja,
                                          hinta_lauri,
                                          laurin_kortti_lkm,
                                          hinta_martti,
                                          martin_kortti_lkm), by = peli_ID]
  
  tulostaulu <- rbind(tulostaulu, ennustePelit)

}

tulostaulu[, ennusteMittari := abs(Voittaja- ennuste)]

summary(tulostaulu)
