#tasuripeli
# kumpiJohtaa, Lauri, Tasan, Martti
tasuripeli_ID <- function(kumpiJohtaa, pfi_data, peliData) {
peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)



# LP<-1
# MP<-6
# LMull <- 0
# MMull <- 0
# Aloittaja <- 0

  peliData_SS <- peliData[!is.na(Voittaja)]
  pelidata_joined_pakkatiedot<-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData_SS)

  
    peliData_SS <- peliData[is.na(Voittaja)]
  
  
  #ennusta
  ennustePelit <- peliData_SS[, .(Laurin_pakka, Martin_pakka, 
                                                     Laurin_mulligan,
                                                     Martin_mulligan,
                                                     Mull_diff = Martin_mulligan - Laurin_mulligan, 
                                                     Aloittaja,
                                                     VS_peli_bool = 1,
                                                     peli_ID, Voittaja)]
  ennustePelit[, ennuste := voittoEnnuste(Laurin_pakka, Martin_pakka,
                                          pelidata_joined_pakkatiedot,
                                          0,
                                          0,
                                          Aloittaja), by = peli_ID]
  
  ennustePelit_aggr <- ennustePelit[, .(sum_ennuste = sum(ennuste), peli_ID = min(peli_ID)), by = .(Laurin_pakka, Martin_pakka)]

  ennustePelit_aggr[, erotus := sum_ennuste - 1]
  Martti_johtaa <- ennustePelit_aggr[which.min(erotus), peli_ID]
  Lauri_johtaa <- ennustePelit_aggr[which.max(erotus), peli_ID]
  Tasan  <- ennustePelit_aggr[which.min(abs(erotus)), peli_ID]
lopputulos <-  switch(kumpiJohtaa,
         Martti = Martti_johtaa,
         Tasan = Tasan,
         Lauri = Lauri_johtaa)
return(lopputulos)
}
