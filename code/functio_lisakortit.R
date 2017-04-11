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

if(includeCurrentTurnaus==FALSE){
 #kato onko keskeneräistä turnausta
  keskenturnaus<-joinLisakortit[is.na(Voittaja),.N,by=TurnausNo]
#ota pois datasta
  joinLisakortit<-joinLisakortit[!TurnausNo %in% (keskenturnaus[,TurnausNo])]
}
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


return(joinnimi)
}