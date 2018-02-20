# peliData <- luecsv("pelit.csv")
# LP<-5
# MP<-5
# LMull <- 0
# MMull <- 0
# Aloittaja <- 0
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
funcLiitaPelit_ja_Pysyvyys<-function(pfi_data,peliData) {
pelidata_temp_all<-peliData
pysyvyys_pct<-as.data.table(pfi_data)

#joinaa pysyvyys_pct divariin

pysyvyys_pct[,':=' (dt_alku=oma_timedate(pvm,kello),dt_loppu=oma_timedate(pvm_end,kello_end))]
laurin_pakat<-pysyvyys_pct[omistaja=="L",.(Laurin_pakka_form_id = id,
                                           Laurin_pakka = pakkanumero,
                                           dt_alku,
                                           dt_loppu,
                                           pysyvyys_pct,
                                           hinta_lauri = hinta,
                                           laurin_kortti_lkm = kortti_lkm)]
martin_pakat<-pysyvyys_pct[omistaja=="M",.(Martin_pakka_form_id = id,
                                           Martin_pakka = pakkanumero
                                           ,dt_alku,
                                           dt_loppu,
                                           pysyvyys_pct,
                                           hinta_martti = hinta,
                                           martin_kortti_lkm = kortti_lkm)]
pelidata_dt<-pelidata_temp_all[,':=' (pelidt_alku=oma_timedate(Aloituspvm,Aloitusaika),pelitdt_loppu=oma_timedate(Lopetuspvm,Lopetusaika))]

#tilapäinen pelidtalku ja loppu
pelidata_dt[is.na(pelidt_alku), ':=' (pelidt_alku = 4000000000, pelitdt_loppu=4000000001)]


joiniID_and_pct_lauri<-laurin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","Laurin_pakka==Laurin_pakka")]
joiniID_and_pct_lauri<-joiniID_and_pct_lauri[,.(peli_ID,Laurin_pysyvyys_pct=pysyvyys_pct,Laurin_pakka_form_id,hinta_lauri,laurin_kortti_lkm)]
#joinaa viela martti
joiniID_and_pct_martti<-martin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","Martin_pakka==Martin_pakka")]

joiniID_and_pct_martti<-joiniID_and_pct_martti[,.(peli_ID,Martin_pysyvyys_pct=pysyvyys_pct,Martin_pakka_form_id,hinta_martti,martin_kortti_lkm)]
#joinaa tulokset
setkey(joiniID_and_pct_lauri,peli_ID)
setkey(joiniID_and_pct_martti,peli_ID)
setkey(pelidata_temp_all,peli_ID)
pelidata_joined_pakkatiedot<-joiniID_and_pct_lauri[joiniID_and_pct_martti][pelidata_temp_all]
pelidata_joined_pakkatiedot[,':=' (pelidt_alku=NULL,pelitdt_loppu=NULL)]
return(pelidata_joined_pakkatiedot)
}