#one time. Conv manastack deck file names
required_functions("convSecsToTime")
pakkalista <- list.files(folder)
tulos <- NULL
pakat <- NULL
counter <-0
pakkametataulu<-NULL
for (pakka in pakkalista){ 
  if (substr(pakka, nchar(pakka)-5+1, nchar(pakka))==".json") {
    counter<-counter+1
    pakkanimi<-substr(pakka,1,nchar(pakka)-5)
    splitti<-strsplit(pakkanimi,"_")
    splitti<-splitti[[1]]
    omistaja <-splitti[1]
    pakkanumero <-as.numeric(splitti[2])
    pvm<-as.IDate(splitti[3])
    kello<-(as.numeric(splitti[4]))
    datetime <- convSecsToTime(kello, as.integer(pvm), tzfix = 60*60*3)
    repl_pattern <- paste0(', "load_datetime":{"datetime":"', datetime, '"}')
    
    file_content <- fread(paste0("./external_files/", pakka), header = FALSE, sep = "¤",
                               stringsAsFactors = FALSE)
    len_of_string <- nchar(file_content[1])
    start_string <- str_sub(file_content, 1, len_of_string -1)
    result_string <- paste0(start_string, repl_pattern, "}")
    
    
    tiedostopaate <- ".json"
    tiedostonimi_illegal <- paste0("./external_files/",omistaja, "_", pakkanumero,"_",datetime, tiedostopaate)
    tiedostonimi <- gsub(":", ".", tiedostonimi_illegal)
   
    write(result_string, tiedostonimi)

    #laske voimassaolon päättyminen
    #pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"))]
  }
}
