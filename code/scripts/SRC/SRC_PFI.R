#SRC_PFI

pakkalista <- list.files("./external_files")
pakat <- NULL
counter <-0
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
    
  }
}
SRC_PFI <- pakat


