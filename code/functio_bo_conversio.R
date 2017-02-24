bo_data_conv <-function(input_bo_mode=FALSE,pelidata) {
#kaikkipelit<-luecsv("pelit.csv")
kaikkipelit<-pelidata
kaikkipelit[,':=' (sumlaurinvoito=sum(Lauri_voitti,na.rm=TRUE),summarttivoitti=sum(Martti_voitti,na.rm=TRUE),sumPelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),maxKierros=max(Kierros)),by=Ottelu_ID]
kaikkipelit[,':=' (Otteluvoittaja=ifelse(sumlaurinvoito/maxKierros>0.5,0,ifelse(summarttivoitti/maxKierros>0.5,1,ifelse(sumPelit/maxKierros>0.5,0.5,NA))),
                   pelikesto=aikaero(Aloitusaika,Lopetusaika,Aloituspvm,Lopetuspvm),
                   enemmanMulliganeja=ifelse(Laurin_mulligan>Martin_mulligan,-1,ifelse(Martin_mulligan>Laurin_mulligan,1,0)))]

kaikkipelit[ , Ottelun_aloittaja:=.SD[which.min(Kierros)], by = Ottelu_ID,.SDcols="Aloittaja"]



kaikkiottelut <-kaikkipelit[!is.na(Otteluvoittaja),.(Divari=max(Divari),
                                                     Laurin_pakka=max(Laurin_pakka),
                                                     Martin_pakka=max(Martin_pakka),
                                                     Lauri_voitti=max(ifelse(Otteluvoittaja==0,1,ifelse(Otteluvoittaja==1,0,0.5))),
                                                     Martti_voitti=max(ifelse(Otteluvoittaja==1,1,ifelse(Otteluvoittaja==0,0,0.5))),
                                                     Ottelun_pelit_kpl=max(Kierros),
                                                     BO_mode=max(BO_mode),
                                                     Best_of_N=max(maxKierros),
                                                     TurnausNo=max(TurnausNo),
                                                     Ottelun_aloittaja=min(Ottelun_aloittaja),
                                                     Voittaja=max(Otteluvoittaja),
                                                     Avg_kesto=mean(pelikesto),
                                                     Mulligan_ero=sum(enemmanMulliganeja,na.rm=TRUE),
                                                     Laurin_humala=mean(Laurin_humala),
                                                     Martin_humala=mean(Martin_humala),
                                                     Laurin_arvosana=mean(Laurin_arvosana),
                                                     Martin_arvosana=mean(Martin_arvosana),
                                                     Aloituspvm=min(Aloituspvm),
                                                     Aloitusaika=min(Aloitusaika),
                                                     Lopetuspvm=max(Lopetuspvm),
                                                     Lopetusaika=max(Lopetusaika),
                                                     peli_ID=min(Ottelu_ID)), #vähän purkkaa, mutta käytetään ku joinataan pakkoformeja
                            by=Ottelu_ID]


#valitaan, kumpaa modea katellaan
if(input_bo_mode==TRUE) {
  pelidata_temp <-kaikkiottelut 
}else{
  pelidata_temp<-kaikkipelit
}
return(pelidata_temp)

}
