# setwd("~/R/mstat2/code/omawd")
# peliData<-luecsv("pelit.csv")
# input_bo_mode=FALSE
# input_total=TRUE
# input_pfiMA=TRUE
# input_divari=NA
# input_Laurin_pakka=NA
# input_Martin_pakka=NA
# input_moving_average=NA
# input_turnaus<-3
# pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# divariData<-luecsv("divari.csv")
tilastoMurskain<-function(divariData,peliData,pfi_data,input_bo_mode=FALSE,input_moving_average=NA,input_pfiMA=NA) {
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



#jos sekä laurin ja martin pakka valittu, tulee vs statsit. Jos vain toinen, niin tulee sen pakan omat statsit

pelidata_temp_all<-peliData

#print(paste(input_bo_mode,input_turnaus,input_total,input_divari,input_Laurin_pakka,input_Martin_pakka,input_moving_average,input_pfiMA,pfi_data))

pysyvyys_pct<-as.data.table(pfi_data)

#joinaa pysyvyys_pct divariin

pysyvyys_pct[,':=' (dt_alku=oma_timedate(pvm,kello),dt_loppu=oma_timedate(pvm_end,kello_end))]
laurin_pakat<-pysyvyys_pct[omistaja=="L",.(Laurin_pakka_form_id=id,Laurin_pakka=pakkanumero,dt_alku,dt_loppu,pysyvyys_pct,hinta_lauri=hinta)]
martin_pakat<-pysyvyys_pct[omistaja=="M",.(Martin_pakka_form_id=id,Martin_pakka=pakkanumero,dt_alku,dt_loppu,pysyvyys_pct,hinta_martti=hinta)]
pelidata_dt<-pelidata_temp_all[,':=' (pelidt_alku=oma_timedate(Aloituspvm,Aloitusaika),pelitdt_loppu=oma_timedate(Lopetuspvm,Lopetusaika))]

joiniID_and_pct_lauri<-laurin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","Laurin_pakka==Laurin_pakka")]
joiniID_and_pct_lauri<-joiniID_and_pct_lauri[,.(peli_ID,Laurin_pysyvyys_pct=pysyvyys_pct,Laurin_pakka_form_id,hinta_lauri)]
#joinaa viela martti
joiniID_and_pct_martti<-martin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","Martin_pakka==Martin_pakka")]

joiniID_and_pct_martti<-joiniID_and_pct_martti[,.(peli_ID,Martin_pysyvyys_pct=pysyvyys_pct,Martin_pakka_form_id,hinta_martti)]
#joinaa tulokset
setkey(joiniID_and_pct_lauri,peli_ID)
setkey(joiniID_and_pct_martti,peli_ID)
setkey(pelidata_temp_all,peli_ID)


pelidata_joined_pakkatiedot<-joiniID_and_pct_lauri[joiniID_and_pct_martti][pelidata_temp_all]
pelidata_joined_pakkatiedot[,':=' (pelidt_alku=NULL,pelitdt_loppu=NULL)]

#kasaa data
Lauridata<-pelidata_joined_pakkatiedot[,.(peli_ID,
                                          Omistaja=1,
                                          Vastustajan_omistaja=2,
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
                                          Kesto=aikaero(Aloitusaika,Lopetusaika,Aloituspvm,Lopetuspvm),
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
                                          Kesto=aikaero(Aloitusaika,Lopetusaika,Aloituspvm,Lopetuspvm),
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
pelatutpelit<-appendKasa[!is.na(Voitti)]
#joinNimi
pelatutNimi<-omapakkanimi[pelatutpelit, on=c("Omistaja","Pakka")]
#joinVastustajan nimi
pelatutNimet<-vihunpakkanimi[pelatutNimi,on=c("Vastustajan_omistaja","Vastustajan_pakka")]






kumulative_data<-pelatutNimet[,.(Nimi,Hinta,Divari,TurnausNo,Aloitti,Voitti,Kesto,Arvosana,Vastustajan_arvosana,Vuoroarvio,Pakka,Vastustajan_pakka)]
#pakkapelinumero
kumulative_data[,pakkaPeliNumero:=seq_len(.N),by=.(Pakka)]
#paripelinumero
kumulative_data[,pariPeliNumero:=seq_len(.N),by=.(Pakka,Vastustajan_pakka)]
#
tulos<-NULL
tulos$cumulative<-kumulative_data
#poista pivotista turhat sarakkeet
pelatutNimet[,':=' (peli_ID=NULL,
                    pakka_form_id=NULL,
                    vastustajan_pakka_form_id=NULL,
                    Ottelu_ID=NULL,
                    Ottelu_no=NULL,
                    BO_mode=NULL)]
tulos$cross<-pelatutNimet
return(tulos)
}

