omaReadJson<-function(folder) {
  pakkalista<-    list.files(folder)
  
  pakat<-NULL
  counter <-0
  for (pakka in pakkalista){ 
    counter<-counter+1
 
    pakat[[counter]]<-fromJSON(paste(folder,pakka,sep=""))
    print(pakat[[counter]])
  }
  
}
