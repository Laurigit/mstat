#options are prod, test, dev
GLOBAL_test_mode <- "prod"
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


#download_folder <- "./drop_download/"
#drop_box_folder <- "mstat/all_data/"
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
    #delete prev decks in case of testing
    do.call(file.remove, list(list.files("./decks_unzipped", full.names = TRUE)))
    do.call(file.remove, list(list.files("./drop_download", full.names = TRUE)))
    do.call(file.remove, list(list.files("./drop_upload", full.names = TRUE)))
    do.call(file.remove, list(list.files("./rdata", full.names = TRUE)))
    
    
    drop_download(path = paste0(drop_box_folder,"all_files.zip"),
                  local_path = download_folder,
                  overwrite = TRUE,
                  dtoken = token)
    unzip(zipfile = paste0(download_folder, "all_files.zip"),
          exdir = "./drop_download")
    
    

  unzip(zipfile = paste0(download_folder, "json.zip"),
        exdir = "./decks_unzipped")
  
  #move .R -files to /rdata
  rfilelist <- data.table(
    filename=dir(download_folder))[,
                                      file_end :=  substr(filename, nchar(filename)-1, nchar(filename))][file_end==".R",
                                                     paste0(download_folder,filename)]
  file.copy(from = rfilelist, to = "./rdata", overwrite = TRUE)
  
  
  }
}
load_data_from_DB(download_folder = "./drop_download/", drop_box_folder = "mstat/all_data/",
                  drop_box_unit_test_folder = "mstat/all_data_static/")



kircsv <- function(datataulu, tiedostonimi, upload = TRUE) {
  test_mode <- "prod"
  upload_folder <- "mstat/all_data/"
  if(exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      test_mode <- "dev"
    } else if (GLOBAL_test_mode == "test") {
      upload_folder <- "mstat/all_data_for_test/"
    }
  }
  
  write.table(x = datataulu,
              file = tiedostonimi,
              sep = ";",
              row.names = FALSE,
              dec = ",")
  

  if (test_mode == "prod" & upload == TRUE) {
    #copy to upload
    file.copy(from = tiedostonimi, to = "./drop_upload/")
    zip_all_and_send()
  }
}

zip_all_and_send <- function() {
#copy rdata
  rdatafiles <- paste0("./rdata/",  dir("./rdata/"))
  csv_files <-  data.table(filename= dir("./drop_download/"))[, file_end :=  substr(filename, nchar(filename)-3, nchar(filename))][file_end==".csv",
                                  paste0("./drop_download/",filename)]
  file.copy(from= c(rdatafiles, csv_files), to = "./drop_upload", overwrite = TRUE)
    tiedostot <- as.data.table(dir(path = "./drop_upload/"))

    setwd("./drop_upload")
    zip(zipfile = "../drop_upload/all_files.zip",
        files = tiedostot[,V1])
    setwd("..")
    test_mode <- "prod"
    upload_dir <- "mstat/all_data/"
    if (exists("GLOBAL_test_mode")) {
      if (GLOBAL_test_mode == "dev") {
        test_mode <- "dev"
      } else if (GLOBAL_test_mode == "test") {
        upload_dir <- "mstat/all_data_for_test/"
      }
    }
    if (test_mode == "prod") {
      drop_upload("./drop_upload/all_files.zip", upload_dir, mode = "overwrite", dtoken = token)
    }
  
}


luecsv <- function(tiedostonimi) {
  tulos <- as.data.table(read.csv(tiedostonimi,
                                  sep = ";",
                                  stringsAsFactors = FALSE,
                                  dec = ",",
                                  fileEncoding = "UTF-8-BOM"))
  return(tulos)
}


#pakkaa jsonit ja laheta
zipAndSend <- function(){
  tiedostot <- as.data.table(dir(path = "./decks_unzipped/"))
  
  tiedostot[, paate := substr(tiedostot[,V1], nchar(tiedostot[, V1]) - 5 + 1, nchar(tiedostot[, V1]))]
  json_files <- tiedostot[paate == ".json", paste0(V1)]
  if (length(json_files) > 0) {
    setwd("./decks_unzipped")
      zip(zipfile = "../drop_upload/json.zip",
          files = json_files)
    setwd("..")
    test_mode <- "prod"
    if(exists("GLOBAL_test_mode")) {
      if (GLOBAL_test_mode == "dev") {
        test_mode <- "dev"
      }
    }
    if (test_mode == "prod" | test_mode == "test") {
      zip_all_and_send()
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
