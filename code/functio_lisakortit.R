funcLisakortit<-function(inputPelidata,divariData){
turnaussaanto<-data.table(read.csv("turnaussaanto.csv",sep=";",fileEncoding="UTF-8-BOM"))
#levita saannot
turnauksia<-data.table(TurnausNoSeq=1:1000)
#setwd("C:/Users/Lauri/Documents/R/mstat2/code/omawd")
levite<-data.table(expand.grid(1:10000,1:100))
setnames(levite,c("Var1","Var2"),c("TurnausNo","Divari"))
joinsaanto <- turnaussaanto[levite,on=c("TurnausNo","Divari")]
#korvaa NA:t seuraavalla
library(zoo)
joinsaanto[,lisakortit_per_voitto:=na.locf(lisakortit_per_voitto)]

joinLisakortit<-joinsaanto[inputPeliData,on=c("TurnausNo","Divari")]
#palauta lisakortit
lisakortit_lauri<-joinLisakortit[,.(Lisakortit=sum(lisakortit_per_voitto*Lauri_voitti,na.rm=TRUE)),
                            ,by=.(Pakka=Laurin_pakka)]
lisakortit_lauri[,Omistaja:="Lauri"]
lisakortit_martti<-joinLisakortit[,.(Lisakortit=sum(lisakortit_per_voitto*Martti_voitti,na.rm=TRUE)),
                                 ,by=.(Pakka=Martin_pakka)]
lisakortit_martti[,Omistaja:="Martti"]

append<-rbind(lisakortit_lauri,lisakortit_martti)
nimitiedot<-inputDivariData[,.(Nimi,Omistaja=Omistaja_nimi,Pakka)]
joinnimi<-nimitiedot[append,on=c("Pakka","Omistaja")]


return(joinnimi)
}