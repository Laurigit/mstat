#folder<-"C:/Users/laurilepisto/Documents/R/shiny/matka/mstat/code/omawd/"
omaReadJson<-function(folder,optionaldoesNothingbutDontDelme=NA) {#check pfi_data reactive why
  pakkalista<-    list.files(folder)
  tulos<-NULL
  pakat<-NULL
  counter <-0
  pakkametataulu<-NULL
  for (pakka in pakkalista){ 
    if (substr(pakka, nchar(pakka)-5+1, nchar(pakka))==".json") {
      counter<-counter+1
      pakkanimi<-substr(pakka,1,nchar(pakka)-5)
      kierrospakka<-fromJSON(paste(folder,pakka,sep=""))
      #parsi pakkatiedot
      splitti<-strsplit(pakkanimi,"_")
      splitti<-splitti[[1]]
      kierrospakka$omistaja <-splitti[1]
      kierrospakka$pakkanumero <-as.numeric(splitti[2])
      kierrospakka$pvm<-as.IDate(splitti[3])
      kierrospakka$kello<-(as.numeric(splitti[4]))
      
      pakat[[counter]]<-kierrospakka
      tulos$pakat<-pakat
      uusmetarivi<- data.table(id=counter,omistaja=splitti[1],pakkanumero=as.numeric(splitti[2]),pvm=as.IDate(splitti[3]),kello=as.numeric(splitti[4]),hinta=kierrospakka$price$med)
      
      pakkametataulu<-as.data.table(rbind(pakkametataulu,uusmetarivi))
      #laske voimassaolon päättyminen
      #pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"))]
    }
  }
  pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"),kello_end=shift(kello,1,type="lead")),by=.(omistaja,pakkanumero)]
  #fix na ending to future
  pakkametataulu[,':=' (pvm_end=ifelse(is.na(pvm_end),as.IDate("2100-01-01"),pvm_end),kello_end=ifelse(is.na(kello_end),1,kello_end))]
  tulos$meta<-pakkametataulu
  print("PAKAT PÄIVITETTY")
  return(tulos)
}

