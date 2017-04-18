# pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# inputDivariData<-luecsv("divari.csv")
# inputPeliData<-luecsv("pelit.csv")
# inputTurnausSaanto<-luecsv("turnaussaanto.csv")

funcLisakortit<-function(inputPeliData,inputDivariData,inputTurnausSaanto,includeCurrentTurnaus=TRUE){
  
turnaussaanto<- inputTurnausSaanto
#levita saannot
turnauksia<-data.table(TurnausNoSeq=1:1000)
#setwd("C:/Users/Lauri/Documents/R/mstat2/code/omawd")
levite<-data.table(expand.grid(1:10000,1:100))
setnames(levite,c("Var1","Var2"),c("TurnausNo","Divari"))
joinsaanto <- turnaussaanto[levite,on=c("TurnausNo","Divari")]
joinsaanto<-joinsaanto[order(TurnausNo,Divari)]
#korvaa NA:t seuraavalla

joinsaanto[,lisakortit_per_voitto:=na.locf(lisakortit_per_voitto),by=Divari]

joinLisakortit<-joinsaanto[inputPeliData,on=c("TurnausNo","Divari")]


pelidata_joined_pakkatiedot<- funcLiitaPelit_ja_Pysyvyys(pfi_data,joinLisakortit)
#eti joka pakan max pakkaform_id
laurin_maxpfi<-pelidata_joined_pakkatiedot[,.(Laurin_pakka_form_id=max(Laurin_pakka_form_id,na.rm=TRUE)),by=Laurin_pakka]
#eti joka pakan pienin peliID, millon pfi=1
minPeliID<-pelidata_joined_pakkatiedot[laurin_maxpfi,on=c("Laurin_pakka_form_id")][,.(min_peliID=min(peli_ID)),by=Laurin_pakka]
laurin_versio<-pelidata_joined_pakkatiedot[minPeliID,on=c("Laurin_pakka")]
#rivit, mssä peliID on < min_peli__ID
laurin_versio_lisakortit_current<-laurin_versio[peli_ID<min_peliID][,.(Lisakortit=sum(as.numeric(lisakortit_per_voitto)*Lauri_voitti,na.rm=TRUE)),
                                                            ,by=.(Pakka=Laurin_pakka)]
#korvaa puuttuvat
paikkaus_lauri<-inputDivariData[Omistaja==1,.(Pakka,Lisakortit_vara=0)]
laurin_versio_lisakortit_current_paikattu<-laurin_versio_lisakortit_current[paikkaus_lauri,on=c("Pakka")]
laurin_versio_lisakortit_current_paikattu[,':=' (Lisakortit=ifelse(is.na(Lisakortit),Lisakortit_vara,Lisakortit),Lisakortit_vara=NULL)]
#martille sama
martin_maxpfi<-pelidata_joined_pakkatiedot[,.(Martin_pakka_form_id=max(Martin_pakka_form_id,na.rm=TRUE)),by=Martin_pakka]
#eti joka pakan pienin peliID, millon pfi=1
minPeliID<-pelidata_joined_pakkatiedot[martin_maxpfi,on=c("Martin_pakka_form_id")][,.(min_peliID=min(peli_ID)),by=Martin_pakka]
martin_versio<-pelidata_joined_pakkatiedot[minPeliID,on=c("Martin_pakka")]
#rivit, mssä peliID on < min_peli__ID
martin_versio_lisakortit_current<-martin_versio[peli_ID<min_peliID][,.(Lisakortit=sum(as.numeric(lisakortit_per_voitto)*Martti_voitti,na.rm=TRUE)),
                                                                    ,by=.(Pakka=Martin_pakka)]
#korvaa puuttuvat
paikkaus_lauri<-inputDivariData[Omistaja==2,.(Pakka,Lisakortit_vara=0)]
martin_versio_lisakortit_current_paikattu<-martin_versio_lisakortit_current[paikkaus_lauri,on=c("Pakka")]
martin_versio_lisakortit_current_paikattu[,':=' (Lisakortit=ifelse(is.na(Lisakortit),Lisakortit_vara,Lisakortit),Lisakortit_vara=NULL)]


#palauta lisakortit
lisakortit_lauri<-joinLisakortit[,.(Lisakortit=sum(as.numeric(lisakortit_per_voitto)*Lauri_voitti,na.rm=TRUE)),
                            ,by=.(Pakka=Laurin_pakka)]
lisakortit_lauri[,Omistaja:="Lauri"]
lisakortit_martti<-joinLisakortit[,.(Lisakortit=sum(as.numeric(lisakortit_per_voitto)*Martti_voitti,na.rm=TRUE)),
                                 ,by=.(Pakka=Martin_pakka)]
lisakortit_martti[,Omistaja:="Martti"]

append<-rbind(lisakortit_lauri,lisakortit_martti)
nimitiedot<-inputDivariData[,.(Nimi,Omistaja=Omistaja_nimi,Pakka)]
joinnimi<-nimitiedot[append,on=c("Pakka","Omistaja")]

tulos<-NULL
tulos$data<-joinnimi
#yhdista data
append_cur_lisakortit<-rbind(laurin_versio_lisakortit_current_paikattu[,Omistaja:="Lauri"],martin_versio_lisakortit_current_paikattu[,Omistaja:="Martti"])
#ja joinaa nimi
append_cur_lisakortit<-append_cur_lisakortit[nimitiedot,on=c("Pakka","Omistaja")]
tulos$current_lisakortit<-append_cur_lisakortit

return(tulos)
}