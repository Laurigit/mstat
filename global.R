#options are prod, test, dev
options(shiny.trace = FALSE)
GLOBAL_test_mode <- "test"
dir.create("./external_files/", showWarnings = FALSE)
dir.create("./download_folder/", showWarnings = FALSE)
dir.create("./upload_folder/", showWarnings = FALSE)
dir.create("./all_data_test_upload/", showWarnings = FALSE)
dir.create("./temporary_files/", showWarnings = FALSE)
dir.create("./www/", showWarnings = FALSE)
dir.create("./save_deck_here_from_mtg/", showWarnings = FALSE)
if (!dir.exists("./dmg_turn_files/")) {
  dir.create("./dmg_turn_files/")
}


library(shinyWidgets)
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
library(readtext)
library(qdapRegex)
library(httr)
library(V8)
library(shinydashboardPlus)
library(rhandsontable)
library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
#library(extendShinyjs)
#library(glob2rx)
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


saveR_and_send <- function(rdatasetti,RdataTallenna,RdataTiedostonimi){
  
  assign(RdataTallenna,rdatasetti)
  #print(get(RdataTallenna))
 # print("ladattu")
  save(list = RdataTallenna,file = paste0("./external_files/", RdataTiedostonimi))
  test_mode <- FALSE
  if (exists("GLOBAL_test_mode")) {
    if (GLOBAL_test_mode == "dev") {
      test_mode <- TRUE
    }
  }
  if (test_mode == FALSE) {
    zip_all_and_send()
    #print("tallennettu uus R-tiedosto jo lähetetty")
  }

  #load("tilastoAsetukset.R")
  #print("ladattu taas ja nyt tulostetaan")
 # print(get(RdataTallenna))
}
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
sourcelist[, rivi := seq_len(.N)]
suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[1], by = rivi])
sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

input_kansio_list <- c("utility",
                       "solution_functions",
                       "UID")
for(input_kansio in input_kansio_list) {
  dir_list <- sourcelist[kansio == input_kansio, polku]
  for(filename in dir_list) {
    result = tryCatch({
      print(paste0("sourced ", filename))
      source(paste0("./scripts/", filename), local = TRUE)
    }, error = function(e) {
      print(paste0("error in loading file: ", filename))
    })
  }
}



print("Global.R valmis")
