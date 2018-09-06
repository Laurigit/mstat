#options are prod, test, dev
GLOBAL_test_mode <- "dev"
dir.create("./external_files/", showWarnings = FALSE)
dir.create("./download_folder/", showWarnings = FALSE)
dir.create("./upload_folder/", showWarnings = FALSE)
dir.create("./all_data_test_upload/", showWarnings = FALSE)


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
library(curl)
library(stringr)
library(ggplot2)
library(ggthemes)
library(shinyalert)
library(anytime)
library(readxl)
#library(shinythemes)
#options(shiny.error=browser)
options(max.print=1000000)
options(DT.fillContainer = FALSE) 
options(DT.autoHideNavigation = FALSE)
Sys.setenv(TZ='EET')
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
load_data_from_DB <- function() {
  download_from_DropBox <- TRUE
  drop_box_folder <- "mstat/all_data/"
  download_folder <- "./download_folder/"
  if (exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      download_from_DropBox <- FALSE
    } else if (GLOBAL_test_mode == "test") {
      drop_box_folder <- "mstat/all_data_test_download/"
    }
  }
  
  #delete prev decks in case of testing
  do.call(file.remove, list(list.files("./external_files", full.names = TRUE)))
  if (download_from_DropBox == TRUE) {
   
    
    drop_download(path = paste0(drop_box_folder,"all_files.zip"),
                  local_path = download_folder,
                  overwrite = TRUE,
                  dtoken = token)
  } else {
    file.copy("./upload_folder/all_files.zip", to = "./download_folder", overwrite = TRUE)
  }
  unzip(zipfile = paste0(download_folder, "all_files.zip"),
        exdir = "./external_files")
}




kircsv <- function(datataulu, tiedostonimi, upload = TRUE) {

  write.table(x = datataulu,
              file = paste0("./external_files/", tiedostonimi),
              sep = ";",
              row.names = FALSE,

              dec = ",")
  
  if (upload == TRUE) {
       zip_all_and_send()
  }
}

zip_all_and_send <- function() {

    tiedostot <- as.data.table(dir(path = "./external_files/"))
    file.remove("./upload_folder/all_files.zip")
    setwd("./external_files")
    
    zip(zipfile = "../upload_folder/all_files.zip",
        files = tiedostot[,V1],  flags="-q")
    setwd("..")
    test_mode <- "prod"
    upload_dir <- "mstat/all_data/"
    if (exists("GLOBAL_test_mode")) {
      if (GLOBAL_test_mode == "dev") {
        test_mode <- "dev"
      } else if (GLOBAL_test_mode == "test") {
        upload_dir <- "mstat/all_data_test_upload/"
      }
    }
    if (test_mode == "prod") {
      drop_upload("./upload_folder/all_files.zip", upload_dir, mode = "overwrite", dtoken = token)
    }
  
}


luecsv <- function(tiedostonimi) {
 # tulos <- as.data.table(read_excel(path = paste0("./external_files/", tiedostonimi),
                #                    col_types = "text")
                         # )
  tulos <- as.data.table(read.csv(paste0("./external_files/", tiedostonimi),
                                  sep = ";",
                                  stringsAsFactors = FALSE,
                                  dec = ",",
                                  fileEncoding = "UTF-8-BOM"))
  return(tulos)
}


#pakkaa jsonit ja laheta
zipAndSend <- function(){
  # tiedostot <- as.data.table(dir(path = "./decks_unzipped/"))
  # 
  # tiedostot[, paate := substr(tiedostot[,V1], nchar(tiedostot[, V1]) - 5 + 1, nchar(tiedostot[, V1]))]
  # json_files <- tiedostot[paate == ".json", paste0(V1)]
  # if (length(json_files) > 0) {
  #   setwd("./decks_unzipped")
  #     zip(zipfile = "../drop_upload/json.zip",
  #         files = json_files)
  #   setwd("..")
  #   test_mode <- "prod"
  #   if(exists("GLOBAL_test_mode")) {
  #     if (GLOBAL_test_mode == "dev") {
  #       test_mode <- "dev"
  #     }
  #   }
  #   if (test_mode == "prod" | test_mode == "test") {
  #     zip_all_and_send()
  #   }
  # }
print("zipAndSend: THIS FUNC SHOULD NOT BE CALLED ANYMORE")
}

paivitaSliderit<-function(input_peli_ID,session) {
  kaikkipelit<-luecsv("pelit.csv")
  laurin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Laurin_pakka])
  martin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Martin_pakka])
  
  updateSelectInput(session,"select_laurin_pakka",selected=  laurin_pakka)
  updateSelectInput(session,"select_martin_pakka",selected=  martin_pakka)
  
}



saveR_and_send <- function(rdatasetti,RdataTallenna,RdataTiedostonimi){
  
  assign(RdataTallenna,rdatasetti)
  print(get(RdataTallenna))
  print("ladattu")
  save(list = RdataTallenna,file = paste0("./external_files/", RdataTiedostonimi))
  test_mode <- FALSE
  if (exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      test_mode <- TRUE
    }
  }
  if (test_mode == FALSE) {
    zip_all_and_send()
    print("tallennettu uus R-tiedosto jo lähetetty")
  }

  #load("tilastoAsetukset.R")
  print("ladattu taas ja nyt tulostetaan")
  print(get(RdataTallenna))
}



print( environment())
print("Global.R valmis")
