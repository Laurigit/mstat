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


source("sarjataulukko.R")
source("functio_bo_conversio.R")
source("process_uploaded_decks.R")
source("omaReadJson.R")
source("pysyvyys_pct.R")
source("turnausVoitot.R")
source("functio_lisakortit.R")
source("tilastoMurskain.R")
source("funcLiitaPelit_ja_Pysyvyys.R")

dirname <-  './omawd'
if (dir.exists(path=dirname)) {
  setwd(dirname) 
}



luecsvalku<-function() {
  print(getwd())
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "divari.csv"), dest = getwd(), sep=";",stringsAsFactors = FALSE,dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "pelit.csv"), dest = getwd(), sep=";",stringsAsFactors = FALSE,dtoken = token))
  tulos <- as.data.table(drop_read_csv(paste0("mstat/csv/", "turnaussaanto.csv"), dest = getwd(), sep=";",stringsAsFactors = FALSE,dtoken = token))
  #jsonit <- as.data.table(drop_dir("mstat/processed/", dtoken = token))
  #for(pakka in jsonit[,path]) {
  #  print(substring(pakka,2))
  drop_get("mstat/processed/json.zip",overwrite = TRUE,dtoken = token)
  unzip("json.zip")
  #}
  
  #tilastoasetukset
  drop_get("mstat/csv/tilastoAsetukset.R", overwrite = TRUE,dtoken = token)
  drop_get("mstat/csv/saavutusAsetukset.R", overwrite = TRUE,dtoken = token)
  
  
}
luecsvalku()

kircsv<-function(datataulu, tiedostonimi) {
  write.table(x=datataulu,file=tiedostonimi,sep=";",row.names = FALSE)
  drop_upload(tiedostonimi, "mstat/csv/", overwrite = TRUE,dtoken = token)
}



luecsv<-function(tiedostonimi) {
  tulos <-as.data.table(read.csv(tiedostonimi,sep=";",stringsAsFactors = FALSE))
  return(tulos)
}
kircsv2<-function(datataulu,tiedostonimi) {
  tulos <- write.table(x=datataulu,file=tiedostonimi,sep=";",row.names = FALSE)
}

#pakkaa jsonit ja laheta
zipAndSend<-function(){
  
  tiedostot<- as.data.table(dir())
  
  tiedostot[,paate:= substr(tiedostot[,V1], nchar(tiedostot[,V1])-5+1, nchar(tiedostot[,V1]))]
  json_files<-tiedostot[paate==".json",V1]
  if (length(json_files)>0){
    zip("json.zip",files=json_files)
    drop_upload("json.zip", "mstat/processed/", overwrite = TRUE,dtoken = token)
  }
}

paivitaSliderit<-function(input_peli_ID,session) {
  kaikkipelit<-luecsv("pelit.csv")
  laurin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Laurin_pakka])
  martin_pakka<-(kaikkipelit[peli_ID==  input_peli_ID ,Martin_pakka])
  
  updateSelectInput(session,"select_laurin_pakka",selected=  laurin_pakka)
  updateSelectInput(session,"select_martin_pakka",selected=  martin_pakka)
  
}

aikaero<-function(aika,loppuaika,pvm,loppupvm){
  return((loppupvm-pvm)*60*60*24+loppuaika-aika)
  
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
  
  drop_upload(RdataTiedostonimi, "mstat/csv/", overwrite = TRUE,dtoken = token)
  
  print("tallennettu")
  #drop_get("mstat/csv/tilastoAsetukset.R",overwrite = TRUE,dtoken = token)
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
load("tilastoAsetukset.R")
load("saavutusAsetukset.R")

print("Ladattu")