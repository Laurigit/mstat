GLOBAL_test_mode <- TRUE


library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(lubridate)
library(DT)
library(reshape2)
library(jsonlite)
library(rdrop2)
library(zoo)
library(rpivotTable)
library(rvest)
#library(shinythemes)
#options(shiny.error=browser)
options(max.print=1000000)
options(DT.fillContainer = FALSE) 
options(DT.autoHideNavigation = FALSE) 
#setwd("C:/Users/laurilepisto/Documents/R/shiny/r2")
#setwd("C:/Users/Lauri/Documents/R/mstat2/code")
#setwd("E:/Pikkuohjelmat/mstat/mstat/code")
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# read it back with readRDS
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#drop_acc(dtoken = token)

#source all files in folder

# source("./scripts/sarjataulukko.R", local = TRUE)
# source("./scripts/functio_bo_conversio.R", local = TRUE)
# source("./scripts/process_uploaded_decks.R", local = TRUE)
# source("./scripts/omaReadJson.R", local = TRUE)
# source("./scripts/pysyvyys_pct.R", local = TRUE)
# source("./scripts/turnausVoitot.R", local = TRUE)
# source("./scripts/functio_lisakortit.R", local = TRUE)
# source("./scripts/tilastomurskain.R", local = TRUE)
# source("./scripts/funcLiitaPelit_ja_Pysyvyys.R", local = TRUE)
# source("./scripts/saavutusLaskenta.R", local = TRUE)

# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)
# source("./scripts/", local = TRUE)


# dirname <-  './omawd'
# if (dir.exists(path=dirname)) {
#   setwd(dirname) 
# }



luecsvalku<-function() {
  print(getwd())
  
  test_mode <- FALSE
  if(exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == TRUE) {
      test_mode <- TRUE
    }
  }
  
  if(test_mode == FALSE) {
    
  
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "divari.csv"), dest = "./drop_download/", sep=";",stringsAsFactors = FALSE,dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "pelit.csv"), dest = "./drop_download/", sep=";",stringsAsFactors = FALSE,dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "temp_data_storage.csv"), dest = "./drop_download/", sep=";",stringsAsFactors = FALSE,dtoken = token))
  print("temp data storage")
  print(tulos)
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "turnaussaanto.csv"), dest = "./drop_download/", sep=";",stringsAsFactors = FALSE,dtoken = token))
  #jsonit <- as.data.table(drop_dir("mstat/processed/", dtoken = token))
  #for(pakka in jsonit[,path]) {
  #  print(substring(pakka,2))
  drop_download(path = "mstat/processed/json.zip",
                local_path = "./drop_download/",
                overwrite = TRUE,
                dtoken = token)
  #delete prev decks in case of testing
  do.call(file.remove, list(list.files("./decks_unzipped", full.names = TRUE)))
  
  unzip(zipfile = "./drop_download/json.zip",
        exdir = "./decks_unzipped")
    #}
  
  #tilastoasetukset
  drop_download(path = "mstat/csv/tilastoAsetukset.R",
                local = "./rdata/",
                overwrite = TRUE,
                dtoken = token)
  drop_download("mstat/csv/saavutusAsetukset.R",
                local = "./rdata/",
                overwrite = TRUE,
                dtoken = token)
  }
}
luecsvalku()

kircsv<-function(datataulu, tiedostonimi) {
  test_mode <- FALSE
  if(exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == TRUE) {
      test_mode <- TRUE
    }
  }
  
  write.table(x=datataulu,file=tiedostonimi,sep=";",row.names = FALSE,dec=",")
  if (test_mode == FALSE) {
  drop_upload(tiedostonimi, "mstat/csv/", mode = "overwrite" ,dtoken = token)
  }
}



luecsv<-function(tiedostonimi) {
  tulos <-as.data.table(read.csv(tiedostonimi,sep=";",stringsAsFactors = FALSE,dec=",",fileEncoding="UTF-8-BOM"))
  return(tulos)
}
kircsv2<-function(datataulu,tiedostonimi) {
  tulos <- write.table(x=datataulu,file=tiedostonimi,sep=";",dec=",",row.names = FALSE)
}

#pakkaa jsonit ja laheta
zipAndSend<-function(){

  tiedostot<- as.data.table(dir(path = "./decks_unzipped/"))
  
  tiedostot[,paate:= substr(tiedostot[,V1], nchar(tiedostot[,V1])-5+1, nchar(tiedostot[,V1]))]
  json_files<-tiedostot[paate==".json",paste0(V1)]
  if (length(json_files)>0){
    setwd("./decks_unzipped")
      zip(zipfile = "../drop_upload/json.zip",
          files=json_files)
    setwd("..")
    test_mode <- FALSE
    if(exists("GLOBAL_test_mode")) {
      if (GLOBAL_test_mode == TRUE) {
        test_mode <- TRUE
      }
    }
    if (test_mode == FALSE) {
    drop_upload("./drop_upload/json.zip", "mstat/processed/", mode = "overwrite" ,dtoken = token)
    }
  }

}

paivitaSliderit<-function(input_peli_ID,session) {
  kaikkipelit<-luecsv("./drop_download/pelit.csv")
  laurin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Laurin_pakka])
  martin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Martin_pakka])
  
  updateSelectInput(session,"select_laurin_pakka",selected=  laurin_pakka)
  updateSelectInput(session,"select_martin_pakka",selected=  martin_pakka)
  
}

aikaero<-function(aika,loppuaika,pvm,loppupvm){
  return(((loppupvm-pvm)*60*60*24+loppuaika-aika))
  
}


#oma_timedate
oma_timedate<-function(pvm,aika) {
  tulos<-as.integer(pvm)*24*60*60+as.integer(aika)
  return(tulos)
  
}

kategorisoi<-function(arvoVektori,kategorisointiVektori=NULL,pct_vektori=c(0.2, 0.4, 0.6, 0.8)) {
  if(is.null(kategorisointiVektori)) {
    kategorisointiVektori<-arvoVektori
  }
  kvantiilit <-quantile(kategorisointiVektori,pct_vektori)
  #laske, että niitä on 4 erilaista
  eriKvant<-unique(kvantiilit)
  if(length(eriKvant)==4) {
    #lisää minimi ja maksimi
    minHavainto<-min(kategorisointiVektori)
    maxHavainto<-max(kategorisointiVektori)
    tulos<-cut(arvoVektori,breaks=sort(unique(c(minHavainto,as.numeric(kvantiilit),maxHavainto))),include.lowest=TRUE)
    return (tulos)
    
  } else {
    varaTulos<-cut(arvoVektori,sort(unique(kategorisointiVektori)),include.lowest=TRUE)
    return(varaTulos)
  }
}

saveR_and_send <-function(rdatasetti,RdataTallenna,RdataTiedostonimi){
  
  assign(RdataTallenna,rdatasetti)
  print(get(RdataTallenna))
  print("ladattu")
  save(list=RdataTallenna,file=RdataTiedostonimi)
  test_mode <- FALSE
  if(exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == TRUE) {
      test_mode <- TRUE
    }
  }
  if (test_mode == FALSE) {
  drop_upload(RdataTiedostonimi, "mstat/csv/", mode = "overwrite",dtoken = token)
  }
  
  print("tallennettu")

  #load("tilastoAsetukset.R")
  print("ladattu taas ja nyt tulostetaan")
  print(get(RdataTallenna))
}

list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname, "[[", seq_along(obj), "]] = ", obj,
          sep = "", collapse = "\n")
  } else {
    paste(listname, "$", names(obj), " = ", obj,
          sep = "", collapse = "\n")
  }
}



load("./rdata/tilastoAsetukset.R")
load("./rdata/saavutusAsetukset.R")
print("Ladattu")