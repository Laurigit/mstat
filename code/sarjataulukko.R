
#sarjataulukko total


sarjataulukkoKaikki<-function(input_bo_mode=FALSE,input_turnaus=1,input_total=FALSE,input_divari=NA,input_Laurin_pakka=NA,input_Martin_pakka=NA,input_moving_average=NA) {
  
pelidata_temp_all<-bo_data_conv(input_bo_mode)
  
  #ota pois pelaamattomat pelit
  pelidata_temp<-pelidata_temp_all[!is.na(Voittaja)]
  
  #valitaan turnausnumero tai kaikki
  if(input_total!=TRUE) {
    pelidata_all<-pelidata_temp[TurnausNo==input_turnaus]
  } else {
    pelidata_all<-pelidata_temp
    #nolladivari = kaikki pelit
    pelidata_all[,Divari:=0]
  }
  
  #filteroi vaan yhden divarin data
  if(!is.na(input_divari) & input_total==FALSE) {
    pelidata_divari<-pelidata_all[Divari==input_divari]
  } else {
    pelidata_divari <-pelidata_all
  }
  

  if(!is.na(input_Laurin_pakka)&!is.na(input_Martin_pakka)) {
    pelidata_vs<-pelidata_divari[Laurin_pakka==input_Laurin_pakka&Martin_pakka==input_Martin_pakka]
  } else {
    pelidata_vs<-pelidata_divari
  }


if(is.na(input_moving_average)) {
  pelidata<-pelidata_vs
  
}else {
  #montako pelattu
  pelit_kpl<-nrow(pelidata_vs)
  pelidata<-pelidata_vs[(pelit_kpl-min(pelit_kpl,input_moving_average)+1):pelit_kpl]
  
}

  
  
  Laurinstats<-pelidata[,.(Voitot=sum(Lauri_voitti,na.rm=TRUE),Pelit=sum(ifelse(is.na(Voittaja),0,1)),Omistaja=1),by=.(Pakka=Laurin_pakka,Divari)]
  Martinstats <- pelidata[,.(Voitot=sum(Martti_voitti,na.rm=TRUE),Pelit=sum(ifelse(is.na(Voittaja),0,1)),Omistaja=2),by=.(Pakka=Martin_pakka,Divari)]
  append<-rbind(Laurinstats,Martinstats)
  append[,Tappiot:=Pelit-Voitot]
  pakkatiedot<-luecsv("divari.csv")[,.(Omistaja,Pakka,Nimi)]
  #joinaa Nimi
  setkeyv(pakkatiedot,c("Omistaja","Pakka"))
  setkeyv(append,c("Omistaja","Pakka"))
  joinapakka <- pakkatiedot[append]
  joinapakka[,':='(Voitto_pct=Voitot/Pelit)]
  
  
  #perakkaiset voitot
  perakkaiset_lauri<-pelidata[!is.na(Voittaja),.(sequence(rle(as.character(Voittaja))$lengths),Voittaja),by=Laurin_pakka]
  perakkaiset_martti<-pelidata[!is.na(Voittaja),.(sequence(rle(as.character(Voittaja))$lengths),Voittaja),by=Martin_pakka]
  pl<-perakkaiset_lauri[,.(Putki=ifelse(Voittaja==0,V1,-V1),Pakka=Laurin_pakka,Omistaja=1)]
  pm<-perakkaiset_martti[,.(Putki=ifelse(Voittaja==1,V1,-V1),Pakka=Martin_pakka,Omistaja=2)]
  
  #liitaputket
  liitaputki<-rbind(pl,pm)
  nykyputki<-liitaputki[, .SD[c(.N)], by=.(Pakka,Omistaja),.SDcols=c("Putki")]
  setkeyv(nykyputki,c("Omistaja","Pakka"))
  setkeyv(joinapakka,c("Omistaja","Pakka"))
  
  
  
  joinedputki<-joinapakka[nykyputki]
  tulos<-NULL
  tulos$divarit <-sort(unique(joinedputki[,Divari]))
  tulos$sarjataulukko<-joinedputki[,.(Nimi,Pelit,Voitot,Tappiot,Voitto_pct=round(Voitto_pct*100,0),Putki)][order(-Voitot)]
  #pisin voittoputk
  isoin_putki<-liitaputki[, .SD[which.max(Putki)]]
  setkeyv(isoin_putki,c("Omistaja","Pakka"))
  joinputkipakka <-pakkatiedot[isoin_putki]
  tulos$ison_putki<-joinputkipakka[,.(Nimi,Putki)]
  
  
  
  #putki, 
  
  
  #eniten katkonut voittoputkia
  #liitaputki[,putkikatki:=ifelse(Putki>2 & Voittaja=1 & Omistaja=)]
  #eniten jatkanus tappioputkia
  return(tulos)
}