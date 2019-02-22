#getUusi_Peli_ID
#required_data("ADM_PELIT")
# PakkaLeft<-6
# PakkaRight <-9
# getUusi_Peli_ID(ADM_PELIT, PakkaLeft, PakkaRight)
getUusi_Peli_ID <- function(ADM_PELIT, PakkaLeft, PakkaRight) {

  max_turnaus <- max(ADM_PELIT[, Turnaus_NO])
  #otetaan pienin pelaamaton peli
  peli_min<- suppressWarnings(ADM_PELIT[is.na(Voittaja) &
                         Turnaus_NO == max_turnaus &
                         Pakka_ID == PakkaLeft &
                         Vastustajan_Pakka_ID == PakkaRight, min(Peli_ID)])
 # op("peli_min")
  #jos ei oo, niin otetaan suurin pelattu
  peli_max <- suppressWarnings(ADM_PELIT[!is.na(Voittaja) &
                          Pakka_ID == PakkaLeft &
                          Vastustajan_Pakka_ID == PakkaRight, max(Peli_ID)])
 # op("peli_max")
  result_peli <- ifelse(is.infinite(peli_min), peli_max, peli_min)
  #jos ei oo ikinä pelannu eikä myöskään oo ohjelmassa, niin palauta NA
  result_peli_valid <- ifelse(is.infinite(result_peli), NA, result_peli)
 # op("result_peli_valid")
  #print(result_peli_valid)  
}


