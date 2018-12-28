#SRC_PFI

pakkalista_raw <- data.table(filenimi = list.files("./external_files"))
filtered <- pakkalista_raw[ (substr(filenimi, nchar(filenimi)-5+1, nchar(filenimi))==".json")]
filtered[, pvm_with_json := word(filenimi, 3, 3, sep = "_") ]
filtered[, pvm := word(pvm_with_json, 1, 1, sep = ".json") ]
setorder(filtered, pvm)
pakkalista <- filtered[, filenimi]
folder <-"./external_files/"
pakat <- NULL
counter <- 0
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
    kierrospakka$pakka_form_id <- counter
    
    pakat[[counter]]<-kierrospakka
    
  }
}
SRC_PFI <- pakat


