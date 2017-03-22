omaReadJson<-function(folder,optionaldoesNothingbutDontDelme=NA) {#check pfi_data reactive why
  pakkalista<-    list.files(folder)
  tulos<-NULL
  pakat<-NULL
  counter <-0
  pakkametataulu<-NULL
  kierros_count<-100
  lauri_counter<-0
  martti_counter<-0
  for (pakka in 1:kierros_count){ 
    counter<-counter+1
    pakkanimi<-substr(pakka,1,nchar(pakka)-5)
    
    #kierrospakka<-fromJSON(paste(folder,pakka,sep=""))
    kierrospakka<-NULL
    
    kierrospakka$omistaja <-ifelse(pakka<kierros_count/2,"L","M")
    if (kierrospakka$omistaja=="L") {lauri_counter<-lauri_counter+1} else {martti_counter<-martti_counter+1}
    kierrospakka$pakkanumero <-pakka %% 7+1
    if(lauri_counter <8 & kierrospakka$omistaja =="L" | martti_counter<8 & kierrospakka$omistja=="M" ) {
      kierrospakka$pvm<-as.IDate("2014-12-20")
    } else {
      kierrospakka$pvm<-as.IDate(runif(1,min=10,max=700)+16426,origin="1970-01-01")
    }
    kierrospakka$kello<-as.ITime("05:01:01")
    kierrospakka$list$cards<-data.table(count=ceiling(runif(20,min=0,max=4)),id=ceiling(runif(20,min=0,max=40)))
    kierrospakka$price$med<-ceiling(runif(1,min=10,max=200))
    pakat[[counter]]<-kierrospakka
    tulos$pakat<-pakat
    uusmetarivi<- data.table(id=counter,
                             omistaja=kierrospakka$omistaja,
                             pakkanumero=kierrospakka$pakkanumero,
                             pvm=kierrospakka$pvm,kello=kierrospakka$kello,hinta=kierrospakka$price$med)
    
    pakkametataulu<-as.data.table(rbind(pakkametataulu,uusmetarivi))
    #laske voimassaolon päättyminen
    #pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"))]
    
  }
  pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"),kello_end=shift(kello,1,type="lead")),by=.(omistaja,pakkanumero)]
  #fix na ending to future
  pakkametataulu[,':=' (pvm_end=ifelse(is.na(pvm_end),as.IDate("2100-01-01"),pvm_end),kello_end=ifelse(is.na(kello_end),1,kello_end))]
  tulos$meta<-pakkametataulu
  print("PAKAT PÄIVITETTY")
  return(tulos)
}
