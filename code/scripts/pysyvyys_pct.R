
pakkaUutuusProsentti <-function(pakat) {
  pakka_comb<-pakat$meta[,.N,by=.(omistaja,pakkanumero)]
  
  tuloslkaikkipakat<-NULL
  
  for(riviloop in 1:nrow(pakka_comb)) {
    rividata<-pakka_comb[riviloop]
    pakkafilter<-pakat$meta[omistaja==rividata[,omistaja] &pakkanumero==rividata[,pakkanumero]][order(pvm,kello)]  

  

  #eti kaikki pakkakombinaatiot
  
  
  #joinaa uusin kaikkien muitten kanssa
  #eti uusimman pakan ID
    uusinpakka_id<-pakkafilter[nrow(pakkafilter),id]
    uusinpakka_kortit <-data.table(id=pakat$pakat[[uusinpakka_id]]$list$cards$id,card_count=pakat$pakat[[uusinpakka_id]]$list$cards$count)
    setkey(uusinpakka_kortit,id)
    tulosdata<-NULL
    for(pakkaid in pakkafilter[,id]) {
  #joinaa uusimman pakan kanssa
    vertailupakka_kortit<-data.table(id=pakat$pakat[[pakkaid]]$list$cards$id,card_count_vertailu=pakat$pakat[[pakkaid]]$list$cards$count)
    setkey(vertailupakka_kortit,id)
    joino<-vertailupakka_kortit[uusinpakka_kortit]
    #replace NA with 0
    joino[,card_count_vertailu:=ifelse(is.na(card_count_vertailu),0,card_count_vertailu)]
    #jos on samaa korttia, mutta eri maarat, niin ala laske mukaan enempaa, kuin on uusimmassa pakassa
    joino[,max_card_vertailu:=pmin(card_count_vertailu,card_count)]
    aggr<-joino[,.(card_count_uusin=sum(card_count),card_count_samat_vanhassa=sum(max_card_vertailu))]
    aggr[,pysyvyys_pcs:=card_count_samat_vanhassa/card_count_uusin]
   
    tulosrivi <-data.table(id=pakkaid,pysyvyys_pct=aggr[,pysyvyys_pcs])
    tulosdata<-rbind(tulosdata,tulosrivi)
    }
    tuloslkaikkipakat<-rbind(tuloslkaikkipakat,tulosdata)
  }
  #joinaa pysyvyys alkuun
  setkey(tuloslkaikkipakat,id)
  setkey(pakat$meta,id)
  joinaa_pct<-tuloslkaikkipakat[pakat$meta]
  return(joinaa_pct)
}

