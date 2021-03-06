# setwd("~/R/mstat2/code/omawd")
# peliData<-luecsv("pelit.csv")
# input_bo_mode=FALSE
# input_pfiMA=TRUE
# input_Laurin_pakka=NA
# input_Martin_pakka=NA
# input_moving_average=5
# input_turnaus<-3
# pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# divariData<-luecsv("divari.csv")
tilastoMurskain<-function(divariData,peliData,pfi_data,input_bo_mode=FALSE,input_moving_average=5,input_pfiMA=NA) {
#save(list=c("divariData","peliData","pfi_data"),file="pelidataa.Rdata")
#pysäytä jos nulleja
if(is.null(divariData)|
   is.null(peliData)|
   is.null(input_bo_mode)|
   #is.null(input_turnaus)|
   #is.null(input_total)|
   #is.null(input_divari)|
   #is.null(input_Laurin_pakka)|
   #is.null(input_Martin_pakka)|
   is.null(input_moving_average)|
   is.null(input_pfiMA)|
   is.null(pfi_data)) {
  tulos<-NULL
  tulos$transposed<-NA
  return(tulos)
}


  pelidata_joined_pakkatiedot<- funcLiitaPelit_ja_Pysyvyys(pfi_data,peliData)

#print(paste(input_bo_mode,input_turnaus,input_total,input_divari,input_Laurin_pakka,input_Martin_pakka,input_moving_average,input_pfiMA,pfi_data))



#kasaa data
Lauridata<-pelidata_joined_pakkatiedot[,.(peli_ID,
                                          Omistaja=1,
                                          Vastustajan_omistaja=2,
                                          Aloituspvm=as.IDate(Aloituspvm,origin="1970-01-01"),
                                          Aloitusaika,
                                          Pysyvyys_pct=Laurin_pysyvyys_pct,
                                          Vastustajan_pysyvyys_pct=Martin_pysyvyys_pct,
                                          Historiakerroin=Laurin_pysyvyys_pct*Martin_pysyvyys_pct,
                                          pakka_form_id=Laurin_pakka_form_id,
                                          vastustajan_pakka_form_id=Martin_pakka_form_id,
                                          Hinta=hinta_lauri,
                                          Vastustajan_hinta=hinta_martti,
                                          Divari,
                                          Pakka=Laurin_pakka,
                                          Vastustajan_pakka=Martin_pakka,
                                          Kierros,
                                          Ottelu_ID,
                                          Ottelu_no,
                                          BO_mode,
                                          TurnausNo,
                                          Aloitti=ifelse(Aloittaja==0,1,0),
                                          Voitti=ifelse(Voittaja==0,1,0),
                                          Kesto=aikaero(Aloitusaika,Lopetusaika,Aloituspvm,Lopetuspvm)/60,
                                          Mulliganit=Laurin_mulligan,
                                          Vastustajan_mulliganit=Martin_mulligan,
                                          Arvosana=Laurin_arvosana,
                                          Vastustajan_arvosana=Martin_arvosana,
                                          Humala=Laurin_humala,
                                          Vastustajan_humala=Martin_humala,
                                          Landit=Laurin_landit,
                                          Vastustajan_landit=Martin_landit,
                                          Vuoroarvio,
                                          Kasikortit=Laurin_kasikortit,
                                          Vastustajan_kasikortit=Martin_kasikortit,
                                          Lifet=Laurin_lifet,
                                          Vastustajan_lifet=Martin_lifet
                                          )]

Marttidata<-pelidata_joined_pakkatiedot[,.(peli_ID,
                                          Omistaja=2,
                                          Vastustajan_omistaja=1,
                                          Aloituspvm=as.IDate(Aloituspvm,origin="1970-01-01"),
                                          Aloitusaika,
                                          Pysyvyys_pct=Martin_pysyvyys_pct,
                                          Vastustajan_pysyvyys_pct=Laurin_pysyvyys_pct,
                                          Historiakerroin=Laurin_pysyvyys_pct*Martin_pysyvyys_pct,
                                          pakka_form_id=Martin_pakka_form_id,
                                          vastustajan_pakka_form_id=Laurin_pakka_form_id,
                                          Hinta=hinta_martti,
                                          Vastustajan_hinta=hinta_lauri,
                                          Divari,
                                          Pakka=Martin_pakka,
                                          Vastustajan_pakka=Laurin_pakka,
                                          Kierros,
                                          Ottelu_ID,
                                          Ottelu_no,
                                          BO_mode,
                                          TurnausNo,
                                          Aloitti=ifelse(Aloittaja==1,1,0),
                                          Voitti=ifelse(Voittaja==1,1,0),
                                          Kesto=aikaero(Aloitusaika,Lopetusaika,Aloituspvm,Lopetuspvm)/60,
                                          Mulliganit=Martin_mulligan,
                                          Vastustajan_mulliganit=Laurin_mulligan,
                                          Arvosana=Martin_arvosana,
                                          Vastustajan_arvosana=Laurin_arvosana,
                                          Humala=Martin_humala,
                                          Vastustajan_humala=Laurin_humala,
                                          Landit=Martin_landit,
                                          Vastustajan_landit=Laurin_landit,
                                          Vuoroarvio,
                                          Kasikortit=Martin_kasikortit,
                                          Vastustajan_kasikortit=Laurin_kasikortit,
                                          Lifet=Martin_lifet,
                                          Vastustajan_lifet=Laurin_lifet
)]

omapakkanimi<-divariData[,.(Omistaja,Pakka,Nimi)]
vihunpakkanimi<-divariData[,.(Vastustajan_omistaja=Omistaja,Vastustajan_pakka=Pakka,Vastustajan_nimi=Nimi)]
appendKasa<-rbind(Lauridata,Marttidata)
pelatutpelit<-appendKasa[!is.na(Voitti)][order(Aloituspvm,Aloitusaika)]
#joinNimi
pelatutNimi<-omapakkanimi[pelatutpelit, on=c("Omistaja","Pakka")]
#joinVastustajan nimi
pelatutNimet<-vihunpakkanimi[pelatutNimi,on=c("Vastustajan_omistaja","Vastustajan_pakka")]

pelatutNimet[,':=' (Omistaja=ifelse(Omistaja==1,"Lauri","Martti"),
                    Vastustajan_omistaja=ifelse(Omistaja==1,"Lauri","Martti")
                    )]


#rivi talteen ja sorttaa
pelatutNimet[,rivi:=seq_len(.N)]
pelatutNimet[,pariPeliNumero:=seq_len(.N),by=.(Nimi,Vastustajan_nimi)]
pelatutNimet[,pariPeliID:=paste(Pakka,Vastustajan_pakka)]
pelatutNimet<-pelatutNimet[order(TurnausNo,pariPeliID,pariPeliID,pariPeliNumero)]

pelatutNimet[,':=' (pakkaPeliNoTurnaus=seq_len(.N)),by=.(TurnausNo,pariPeliID)]
pelatutNimet[,':=' (pakkaPeliNoTurnaus=ceiling(pakkaPeliNoTurnaus/2))]
pelatutNimet[,':=' (MulliganKPI=ifelse(Mulliganit>Vastustajan_mulliganit,-1,ifelse(Mulliganit<Vastustajan_mulliganit,1,0)))]
#sorttaa takasi
pelatutNimet<-pelatutNimet[order(rivi)][,rivi:=NULL]

#putki
#perakkaiset voitot
perakkaiset_lauri<-pelatutNimet[,.(sequence(rle(as.character(Voitti))$lengths),Voitti,peli_ID),by=.(Nimi)]
putket<-perakkaiset_lauri[,.(Putki=ifelse(Voitti==1,V1,-V1),Nimi,peli_ID)]
perakkaiset_vs<-pelatutNimet[,.(sequence(rle(as.character(Voitti))$lengths),Voitti,peli_ID),by=.(Nimi,Vastustajan_nimi)]
putket_vs<-perakkaiset_vs[,.(Putki_VS=ifelse(Voitti==1,V1,-V1),Nimi,Vastustajan_nimi,peli_ID)]
#join putki
pelatutNimet<-putket_vs[pelatutNimet,on=c("peli_ID","Nimi","Vastustajan_nimi")]
pelatutNimet<-putket[pelatutNimet,on=c("peli_ID","Nimi")]
#jion putki_VS

#pfi
pelatutNimet[,':=' (Voitti_PFI=Voitti*Historiakerroin)]


turnaus_data<-data.table(pelatutNimet[,.(Voitot=sum(Voitti),
                                         Hinta=mean(Hinta),
                                         MulliganKPI=sum(MulliganKPI)),
                                      by=.(Divari,Nimi,TurnausNo)])

turnaus_data[,divariSijoitus:=rank(-Voitot),by=.(Divari,TurnausNo)]
turnaus_data[,turnausSijoitus:=rank(-Voitot+Divari*1000),by=.(TurnausNo)]

turnaus_data[,':=' (turnausVoittaja=ifelse(turnausSijoitus==1,1,0))]

tulos<-NULL
tulos$turnaus<-turnaus_data

kumulative_data<-pelatutNimet[,.(Omistaja,
                                 Vastustajan_omistaja,
                                 Nimi,
                                 Aloituspvm,
                                 Vastustajan_nimi,
                                 Divari,
                                 TurnausNo,
                                 Aloitti,
                                 Voitti,
                                 Putki,
                                 Putki_VS,
                                 MA_voitti=round(rollmean(Voitti,input_moving_average,align=c("left"),fill=c("extend","extend","extend")),2),
                                 Vuoroarvio,
                                 pakkaPeliNoTurnaus,
                                 Hinta,
                                 Pysyvyys_pct,
                                 Humala,
                                 Vastustajan_humala,
                                 Mulliganit,
                                 peli_ID,
                                 pariPeliNumero,
                                 pakka_form_id
                                 )]
#pakkapelinumero
kumulative_data[,pakkaPeliNumero:=seq_len(.N),by=.(Nimi)]

#ota aikadatasta kaikki crossdata-sarakkeet pois



tulos$aikasarja<-kumulative_data
#poista cross_datasta kaikki aikasarakkeet ja kategorisoi kesto

pelatutNimet[,':=' (peli_ID=NULL,
                    pakka_form_id=NULL,
                    vastustajan_pakka_form_id=NULL,
                    Ottelu_ID=NULL,
                    Ottelu_no=NULL,
                    BO_mode=NULL,
                    Historiakerroin=NULL,
                    Pysyvyys_pct=NULL,
                    Vastustajan_hinta=NULL,
                    Vastustajan_pysyvyys_pct=NULL,
                    Hinta=NULL,
                    Divari=NULL,
                    Kierros=NULL,
                    TurnausNo=NULL,
                    Aloitusaika=NULL,
                    Kesto_cat=kategorisoi(Kesto/60),
                    Humala_cat=kategorisoi(Humala,c(Humala,Vastustajan_humala)),
                    Vastustajan_humala=kategorisoi(Vastustajan_humala,c(Humala,Vastustajan_humala)),
                    Life_cat=cut(Lifet,breaks=c(-1,0,4,9,14,19,20,21),include.lowest=TRUE),
                    Vastustajan_cat=cut(Vastustajan_lifet,breaks=c(-1,0,4,9,14,19,20,21),include.lowest=TRUE),
                    pariPeliNumero=NULL
                    
                    )]
tulos$cross<-pelatutNimet
return(tulos)
}

