#options are prod, test, dev
GLOBAL_test_mode <- "test"
dir.create("./rdata/", showWarnings = FALSE)
dir.create("./drop_download/", showWarnings = FALSE)
dir.create("./decks_unzipped/", showWarnings = FALSE)
dir.create("./drop_upload/", showWarnings = FALSE)


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



load_data_from_DB <- function(download_folder, drop_box_folder, drop_box_unit_test_folder) {
  test_mode <- "prod"
  if (exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      test_mode <- "dev"
    } else if (GLOBAL_test_mode == "test") {
      drop_box_folder <- drop_box_unit_test_folder
    }
  }
  
  if (test_mode == "prod") {
    
  
  tulos <- as.data.table(drop_read_csv(paste0(drop_box_folder, "divari.csv"),
                                       dest = download_folder,
                                       sep = ";",
                                       stringsAsFactors = FALSE,
                                       dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0(drop_box_folder, "pelit.csv"),
                                       dest = download_folder,
                                       sep = ";",stringsAsFactors = FALSE,
                                       dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0(drop_box_folder, "temp_data_storage.csv"),
                                       dest = download_folder,
                                       sep = ";",
                                       stringsAsFactors = FALSE,
                                       dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0(drop_box_folder, "turnaussaanto.csv"), 
                                       dest = download_folder,
                                       sep = ";",
                                       stringsAsFactors = FALSE,
                                       dtoken = token))
  drop_download(path = paste0(drop_box_folder,"json.zip"),
                local_path = download_folder,
                overwrite = TRUE,
                dtoken = token)
  #delete prev decks in case of testing
  do.call(file.remove, list(list.files("./decks_unzipped", full.names = TRUE)))
  
  unzip(zipfile = paste0(download_folder, "json.zip"),
        exdir = "./decks_unzipped")
  
  #tilastoasetukset
  drop_download(path = paste0(drop_box_folder,"tilastoAsetukset.R"),
                local = download_folder,
                overwrite = TRUE,
                dtoken = token)
  drop_download(paste0(drop_box_folder, "saavutusAsetukset.R"),
                local = download_folder,
                overwrite = TRUE,
                dtoken = token)
  #move .R -files to /rdata
  rfilelist <- data.table(
    filename=dir(download_folder))[,
                                      file_end :=  substr(filename, nchar(filename)-1, nchar(filename))][file_end==".R",
                                                     paste0(download_folder,filename)]
  file.copy(from = rfilelist, to = "./rdata", overwrite = TRUE)
  
  
  }
}
load_data_from_DB("./drop_download/")



kircsv <- function(datataulu, tiedostonimi, upload = TRUE) {
  test_mode <- "prod"
  upload_folder <- "mstat/all_data/"
  if(exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      test_mode <- "dev"
    } else if (GLOBAL_test_mode = "test") {
      upload_folder <- "mstat/all_data_for_test/"
    }
  }
  
  write.table(x=datataulu,file=tiedostonimi,sep=";",row.names = FALSE,dec=",")
  if (test_mode == "prod" & upload == TRUE) {
  drop_upload(tiedostonimi, upload_folder, mode = "overwrite" ,dtoken = token)
  }
}



luecsv<-function(tiedostonimi) {
  tulos <-as.data.table(read.csv(tiedostonimi,sep=";",stringsAsFactors = FALSE,dec=",",fileEncoding="UTF-8-BOM"))
  return(tulos)
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




load("./rdata/tilastoAsetukset.R")
load("./rdata/saavutusAsetukset.R")
print("Ladattu")
