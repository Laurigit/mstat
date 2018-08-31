
# folder_out <-"./external_files"
# folder_in <- "./external_files/"
# filename_in<-"L_7.json"
# pakka <- file.info(paste(folder_in,filename_in,sep=""))
# filelist <- "L_7"
# pakka <- data.frame(name = "L_7.json", size = 7790, type ="", datapath = paste(folder_in,filename_in,sep=""), stringsAsFactors = FALSE)
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
    aikaleima <- now(tz = "EET")
    
    repl_pattern <- paste0(', "load_datetime":{"datetime":"', aikaleima, '"}')
    
    file_content <- fread(paste0("./external_files/", pakka$name), header = FALSE, sep = "¤",
                          stringsAsFactors = FALSE)
    len_of_string <- nchar(file_content[1])
    start_string <- str_sub(file_content, 1, len_of_string -1)
    result_string <- paste0(start_string, repl_pattern, "}")
    
    
    tiedostopaate <- substr(pakka$name,nchar(pakka$name)-4,nchar(pakka$name))
    tiedostonimi_illegal <- paste0("./external_files/",tiedostoalku,"_",aikaleima, tiedostopaate)
    tiedostonimi <- gsub(":", ".", tiedostonimi_illegal)

    write.table(result_string,
                file = tiedostonimi,
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE)
    
    
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
