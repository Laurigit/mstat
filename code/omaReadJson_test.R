omaReadJson<-function(folder,optionaldoesNothingbutDontDelme=NA) {#check pfi_data reactive why
  pakkalista<-    list.files(folder)
  tulos<-NULL
  pakat<-NULL
  counter <-0
  pakkametataulu<-NULL
  kierros_count<-42
  for (pakka in 1:kierros_count){ 
    counter<-counter+1
    pakkanimi<-substr(pakka,1,nchar(pakka)-5)
    
    #kierrospakka<-fromJSON(paste(folder,pakka,sep=""))
    kierrospakka<-NULL
    #parsi pakkatiedot
    splitti<-strsplit(pakkanimi,"_")
    splitti<-splitti[[1]]
    kierrospakka$omistaja <-splitti[1]
    kierrospakka$pakkanumero <-as.numeric(splitti[2])
    kierrospakka$pvm<-as.IDate(splitti[3])
    kierrospakka$kello<-(as.numeric(splitti[4]))
    
    kierrospakka$omistaja <-ifelse(pakka<kierros_count/2,"L","M")
    kierrospakka$pakkanumero <-pakka %% 7+1
    kierrospakka$pvm<-as.IDate(paste0("2016-",ceiling(runif(1,min=0,max=12)),"-",ceiling(runif(1,min=1,max=28))))
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
<<<<<<< HEAD
=======

>>>>>>> 44b119b6983e6b7a77f4718040caf8bbaccc32a9
