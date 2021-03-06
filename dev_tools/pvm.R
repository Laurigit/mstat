

importEx<-data.table(read.table("C:/Users/Lauri/Documents/R/mstat2/code/import_to_R.csv",sep=";",header=TRUE,stringsAsFactors = FALSE))


importEx[900:950,Martti_voitti]

setnames(importEx,"ï..Divari","Divari")
str(importEx)

#ota vaan mukaan turnaukset numerolla yli 34
importEx<-importEx[TurnausNo>34]

#korjaa voittaja ja aloittaja
importEx[,':=' (Voittaja = Voittaja - 1, Aloittaja = Aloittaja - 1)]

#laske superturnauksen kierros'
importEx[,Kierros:=NULL]
importEx[,Kierros:=1]

importEx[,Ottelu_no:=NULL]
importEx[,Ottelu_no:=seq_len(.N),by=.(TurnausNo,Laurin_pakka,Martin_pakka)]
importEx[,Ottelu_ID_apu:=ifelse(Ottelu_no==1,1,0)]
importEx[,apupariID:=paste0(Laurin_pakka,Martin_pakka)]
importEx<-importEx[order(TurnausNo,apupariID)]
importEx[,Ottelu_ID:=cumsum(Ottelu_ID_apu)]
importEx[,':=' (Aloitusaika=as.integer(as.ITime(strptime(Aloitusaika,format="%H.%M.%S"))), 
                Aloituspvm=as.integer(as.IDate(strptime(Aloituspvm,format="%d.%m.%Y"))), 
                Lopetusaika=as.integer(as.ITime(strptime(Lopetusaika,format="%H.%M.%S"))), 
                Lopetuspvm=as.integer(as.IDate(strptime(Lopetuspvm,format="%d.%m.%Y"))))]
importEx[,Ottelu_ID_apu:=NULL]
importEx[,Superturnaus:=NULL]
importEx[,apupariID:=NULL]
importEx[,peli_ID:=seq_len(.N)]
importEx[,Martti_voitti:=ifelse(Lauri_voitti==0,1,0)]
setnames(importEx,"Laurin_Humala","Laurin_humala")

write.csv2(importEx,"pelit.csv",row.names = FALSE)
as.numeric(as.IDate("2014-12-22"))
showTime<-as.IDate(16426,origin="1970-01-01")
