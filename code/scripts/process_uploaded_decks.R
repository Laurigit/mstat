
# folder_in <- "C:/Users/Lauri/Documents/R/mstat2/pakat/"
# folder_out <- "C:/Users/Lauri/Documents/R/mstat2/pakat/processed/"
# filename_in<-"L_1.json"
process_uploaded_decks<-function(filelist,folder_out) {
filelist <-data.table(filelist)
  #lue json-tiedosto
  #json_pakka <- fromJSON(paste(folder_in,filename_in,sep=""))
valid_teksti<-NULL

  for (rivi in 1:nrow(filelist)){ 
  
    pakka <-filelist[rivi]
    tiedostoalku<-substr(pakka$name,1,nchar(pakka$name)-5)
    #remove json ending

    splitnimi <- strsplit(tiedostoalku,"_")

    #validointi
    if(splitnimi[[1]][1]=="L"|splitnimi[[1]][1]=="M") {omistaja<-TRUE} else {omistaja<-FALSE}
    if(!is.na(as.numeric(splitnimi[[1]][2]))) {pakkanumero<-TRUE} else {pakkanumero<-FALSE}
    if(length(splitnimi[[1]])<3) {pituus<-TRUE} else {pituus <-FALSE}
   # print(paste(omistaja,pakkanumero,pituus," ",splitnimi[[1]][1]," ",(as.numeric(splitnimi[[1]][2]))))
    
   
    if(omistaja==TRUE & pakkanumero==TRUE & pituus==TRUE) {
    aikaleima<-file.info(paste(pakka$datapath))$mtime
    pvm<-as.IDate(aikaleima)
    sekunnit<-as.integer(as.ITime(aikaleima))
    
    tiedostopaate <- substr(pakka$name,nchar(pakka$name)-4,nchar(pakka$name))
    tiedostonimi<-paste0("./decks_unzipped/",tiedostoalku,"_",pvm,"_",sekunnit,tiedostopaate)

    write.table(read.table(file=pakka$datapath),file=tiedostonimi,row.names=FALSE,col.names = FALSE,quote=FALSE)
    
    
    #syscommand <-paste("copy ", pakka$datapath," ",folder_out,tiedostonimi,sep="")
  
    #repl_slash <-gsub("/","\\\\",syscommand)

    #shell(paste(repl_slash),intern=TRUE)
    valid_teksti<-paste(valid_teksti,"ok: ",pakka$name,"\n")

    } else {
      valid_teksti<-paste(valid_teksti,"FAILED: ",pakka$name,"\n")

    }
    
  }
return(valid_teksti)
  
}
