library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(lubridate)
library(DT)
library(reshape2)
library(jsonlite)
#library(shinythemes)

options(max.print=1000000)
options(DT.fillContainer = FALSE) 
options(DT.autoHideNavigation = FALSE) 
#setwd("C:/Users/laurilepisto/Documents/R/shiny/r2")
#setwd("C:/Users/Lauri/Documents/R/mstat2/code")
#setwd("E:/Pikkuohjelmat/mstat/mstat/code")



source("sarjataulukko.R")
source("functio_bo_conversio.R")
source("process_uploaded_decks.R")
source("omaReadJson.R")
source("pysyvyys_pct.R")

luecsv<-function(tiedostonimi) {
  tulos <-as.data.table(read.csv(tiedostonimi,sep=";",stringsAsFactors = FALSE))
  return(tulos)
}
kircsv<-function(datataulu,tiedostonimi) {
  tulos <- write.table(x=datataulu,file=tiedostonimi,sep=";",row.names = FALSE)
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