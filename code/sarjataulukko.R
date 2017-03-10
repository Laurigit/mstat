
#sarjataulukko total
#peliData2<-kaikkipelit
#pfi_data2<-  as.data.table(pakkaUutuusProsentti(pakat))
#setwd("~/R/mstat2/code/omawd")
# peliData<-luecsv("pelit.csv")


# input_bo_mode=FALSE
# input_total=FALSE

# input_pfiMA=FALSE
# input_divari=1
# input_Laurin_pakka=1
# input_Martin_pakka=1
# input_moving_average=NA

#input_pfiMA=FALSE
# input_turnaus<-3

# pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# divariData<-luecsv("divari.csv")

sarjataulukkoKaikki<-function(divariData,peliData,input_bo_mode=FALSE,input_turnaus=1,input_total=FALSE,input_divari=NA,input_Laurin_pakka=NA,input_Martin_pakka=NA,input_moving_average=NA,input_pfiMA=FALSE,pfi_data=NA) {
#pysäytä jos nulleja
  if(is.null(divariData)|
     is.null(peliData)|
     is.null(input_bo_mode)|
     is.null(input_turnaus)|
     is.null(input_total)|
     is.null(input_divari)|
     is.null(input_Laurin_pakka)|
     is.null(input_Martin_pakka)|
     is.null(input_moving_average)|
     is.null(input_pfiMA)|
     is.null(pfi_data)) {
    tulos<-NULL
    tulos$transposed<-NA
    return(tulos)
  }
    
  
  
#jos sekä laurin ja martin pakka valittu, tulee vs statsit. Jos vain toinen, niin tulee sen pakan omat statsit
  
pelidata_temp_all<-bo_data_conv(input_bo_mode,peliData)

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
  #ota pois pelaamattomat pelit
  pelidata_temp<-pelidata_joined_pakkatiedot[!is.na(Voittaja)]
  nimipaate<-NULL
  

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
  
  ##korjaa warningeja pois
  if(is.null(input_Laurin_pakka)) {input_Laurin_pakka<-NA}
  if(is.null(input_Martin_pakka)) {input_Martin_pakka<-NA}
#vs statsit 
  if(!is.na(input_Laurin_pakka)&!is.na(input_Martin_pakka)) {
      pelidata_vs<-pelidata_divari[Laurin_pakka==input_Laurin_pakka&Martin_pakka==input_Martin_pakka]
      nimipaate<-paste("VS",nimipaate,sep="")
  } else {
    pelidata_vs<-pelidata_divari
  }
  
  
#Laurin pakan statsit
  if(!is.na(input_Laurin_pakka) & is.na(input_Martin_pakka)) {
    pelidata_vs<-pelidata_divari[Laurin_pakka==input_Laurin_pakka]
    nimipaate<-paste("Deck",nimipaate,sep="") 
    
  }
  
  #Martin pakan statsit
  if(is.na(input_Laurin_pakka) & !is.na(input_Martin_pakka)) {
    
    pelidata_vs<-pelidata_divari[Martin_pakka==input_Martin_pakka]
    nimipaate<-paste("Deck",nimipaate,sep="") 
    
  }
  
  
  
if(is.na(input_moving_average)) {
  pelidata<-pelidata_vs
  
}else {
  #montako pelattu
  pelit_kpl<-nrow(pelidata_vs)
  pelidata<-pelidata_vs[(pelit_kpl-min(pelit_kpl,input_moving_average)+1):pelit_kpl]
  nimipaate<-paste("MA",nimipaate,sep="")
}
  
#pfiMA
  
if(input_pfiMA==TRUE) {
  pelidata[,':=' (Lauri_voitti=Lauri_voitti*Laurin_pysyvyys_pct,Martti_voitti=Martti_voitti*Martin_pysyvyys_pct)]
  #nimipaate<-paste("pfiMA",nimipaate,sep="")
}
  tulos<-NULL

  
  Lauripelaajastats<-pelidata[,.(Voitot_Lauri=round(sum(Lauri_voitti,na.rm=TRUE),1),Pelit=round(sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),1)),by=.(Divari)]
  Martinpelaajastats <- pelidata[,.(Voitot_Martti=round(sum(Martti_voitti,na.rm=TRUE),1),Pelit=round(sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),1)),by=.(Divari)]
  joinpelaajastats<-Lauripelaajastats[Martinpelaajastats,on=c("Divari","Pelit")]
  tulos$pelaajastats<-joinpelaajastats
  
  Laurinstats<-pelidata[,.(Voitot=sum(Lauri_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=1,Hinta=mean(hinta_lauri)),by=.(Pakka=Laurin_pakka,Divari)]
  Martinstats <- pelidata[,.(Voitot=sum(Martti_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=2,Hinta=mean(hinta_martti)),by=.(Pakka=Martin_pakka,Divari)]
  append<-rbind(Laurinstats,Martinstats)
  append[,Tappiot:=Pelit-Voitot]
  pakkatiedot<-divariData[,.(Omistaja,Pakka,Nimi)]
  #joinaa Nimi
  setkeyv(pakkatiedot,c("Omistaja","Pakka"))
  setkeyv(append,c("Omistaja","Pakka"))
  joinapakka <- pakkatiedot[append]
  joinapakka[,':='(Voitto_pct=Voitot/Pelit)]
  
  #turnausvoittolaskentaa
  minmax_divari_per_turnaus <-pelidata[,.(min_div=(min(Divari)),maxdiv=max(Divari)),by=TurnausNo]
  #voitot per pelaaja
  Voitot_perTurnaus<-pelidata[,.(Voitot=sum(Lauri_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=1,Hinta=mean(hinta_lauri)),by=.(Divari,TurnausNo)]
  
  
  Laurinstats<-pelidata[,.(Voitot=sum(Lauri_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=1,Hinta=mean(hinta_lauri)),by=.(Pakka=Laurin_pakka,Divari,TurnausNo)]
  Martinstats <- pelidata[,.(Voitot=sum(Martti_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=2,Hinta=mean(hinta_martti)),by=.(Pakka=Martin_pakka,Divari,TurnausNo)]
  
  #print(paste("pelidata ennen gmax"))
  #print(pelidata)
  #print("pelidata jalkeen gmas")
  max_pakkaform_by_laurin_pakka<-pelidata[,.(laurin_pfi=max(Laurin_pakka_form_id,na.rm=TRUE)),by=Laurin_pakka]
  max_pakkaform_by_martin_pakka  <-pelidata[,.(martin_pfi=max(Martin_pakka_form_id,na.rm=TRUE)),by=Martin_pakka]                                                 
  
  setkey(max_pakkaform_by_laurin_pakka,laurin_pfi)
  setkey(max_pakkaform_by_martin_pakka,martin_pfi)
  setkey(pelidata,Laurin_pakka_form_id)
  pfilauri<-pelidata[max_pakkaform_by_laurin_pakka][,.(Pelit=sum(ifelse(is.na(Voittaja),0,1)),Voitot=sum(Lauri_voitti,na.rm=TRUE),Tappiot=sum(Martti_voitti,na.rm=TRUE),Omistaja=1),by=.(Pakka=Laurin_pakka)]
  setkey(pelidata,Martin_pakka_form_id)
  pfimartti<-pelidata[max_pakkaform_by_martin_pakka][,.(Pelit=sum(ifelse(is.na(Voittaja),0,1)),Voitot=sum(Martti_voitti,na.rm=TRUE),Tappiot=sum(Lauri_voitti,na.rm=TRUE),Omistaja=2),by=.(Pakka=Martin_pakka)]
  append_pfi<-rbind(pfilauri,pfimartti)
  setkeyv(append_pfi,c("Omistaja","Pakka"))
  joinpfipakka <- pakkatiedot[append_pfi]
  pfiresult<-joinpfipakka[,.(Nimi,Voitot,Tappiot)][order(-Tappiot,-Voitot)]

  tulos$pfi<-pfiresult
  #transponoi
  cols<-names(pfiresult)[2:length(names(pfiresult))]
  pfiresult[, (cols):=lapply(.SD, as.double),.SDcols=cols]
  
  #convertointi valmis
  transposed<-melt(pfiresult,id.vars=c("Nimi"),variable.name=c("Tilasto"))
  all_rows<-data.table(dcast(transposed,Tilasto~Nimi,fun.aggregate = sum))
  valitut_sarakkeet<-all_rows[Tilasto %in% c("Voitot","Tappiot")]
  tulos$pfi_trans<-cbind(selite="pfi",valitut_sarakkeet)
  
  
  #perakkaiset voitot
  perakkaiset_lauri<-pelidata[!is.na(Voittaja),.(sequence(rle(as.character(Voittaja))$lengths),Voittaja),by=Laurin_pakka]
  perakkaiset_martti<-pelidata[!is.na(Voittaja),.(sequence(rle(as.character(Voittaja))$lengths),Voittaja),by=Martin_pakka]
  pl<-perakkaiset_lauri[,.(Putki=ifelse(Voittaja==0,V1,ifelse(Voittaja==1,-V1,0)),Pakka=Laurin_pakka,Omistaja=1)]
  pm<-perakkaiset_martti[,.(Putki=ifelse(Voittaja==1,V1,ifelse(Voittaja==0,-V1,0)),Pakka=Martin_pakka,Omistaja=2)]
  
  #liitaputket
  liitaputki<-rbind(pl,pm)
  nykyputki<-liitaputki[, .SD[c(.N)], by=.(Pakka,Omistaja),.SDcols=c("Putki")]
  setkeyv(nykyputki,c("Omistaja","Pakka"))
  setkeyv(joinapakka,c("Omistaja","Pakka"))
  
  
  
  joinedputki<-joinapakka[nykyputki]
  
  tulos$divarit <-sort(unique(joinedputki[,Divari]))
  

  
  
  
  if(!is.na(input_Laurin_pakka)&is.na(input_Martin_pakka)) {
    joinedputki_filt  <- joinedputki[(Omistaja==1 & Pakka==input_Laurin_pakka)]
  
  } else if (is.na(input_Laurin_pakka)&!is.na(input_Martin_pakka)){
    
    joinedputki_filt  <- joinedputki[(Omistaja==2 & Pakka==input_Martin_pakka) ]

  }else {
    joinedputki_filt<-joinedputki
  }
  sarjataulukkotulos<-joinedputki_filt[,.(Nimi,Pelit=round(Pelit,1),Voitot=round(Voitot,1),Tappiot=round(Tappiot,1),Voitto_pct=round(Voitto_pct*100,0),Putki,Hinta)][order(-Voitot)]
  
  #jos vs_statsit ei oo päällä, mutta pakkanumerot on annettu, niin palauta vaan niiden kahden pakan tulokset

  
  tulos$sarjataulukko<-sarjataulukkotulos

  #colnames( tulos$sarjataulukko) <- paste(colnames(tulos$sarjataulukko),nimipaate, sep = "_")
  
  #pisin voittoputk
  isoin_putki<-liitaputki[, .SD[which.max(Putki)]]
  setkeyv(isoin_putki,c("Omistaja","Pakka"))
  joinputkipakka <-pakkatiedot[isoin_putki]
  tulos$ison_putki<-joinputkipakka[,.(Nimi,Putki)]
  
  #transponoitu_tilastot
  #convertoi numeroksi sarakkeet
  cols<-names(sarjataulukkotulos)[2:length(names(sarjataulukkotulos))]
  sarjataulukkotulos[, (cols):=lapply(.SD, as.double),.SDcols=cols]

  #convertointi valmis
  transposed<-melt(sarjataulukkotulos,id.vars=c("Nimi"),variable.name=c("Tilasto"))
  all_rows<-data.table(dcast(transposed,Tilasto~Nimi))
  valitut_sarakkeet<-all_rows[Tilasto %in% c("Voitot","Voitto_pct","Putki")]

  #valitut_sarakkeet[,selite:=nimipaate]
  #tulos$transposed<-valitut_sarakkeet[!is.na(Tilasto),.(Tilasto,selite,laurieka,marttieka)]
  if(!is.null(nimipaate)) {
    tulos$transposed<-cbind(selite=nimipaate,valitut_sarakkeet)
  } else {
    tulos$transposed<-valitut_sarakkeet
    
    }


  tulos$laurin_voitto_pct<-joinedputki_filt[Omistaja==1,Voitto_pct]
  
  
  #eniten katkonut voittoputkia
  #eniten jatkanus tappioputkia
  return(tulos)
}

