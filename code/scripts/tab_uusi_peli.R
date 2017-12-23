#seuraa valintalistoja seka tallennusta ja paivita UI + tiedot sen mukaan.
observe({
  print("seuraa valintalistoja alku")
  #seuraa tallenna buttonia myös 
  print(paste("tallennatulosarvo",input$tallenna_tulos))
  
  kaikkipelit<-luecsv("./drop_download/pelit.csv")
  #print(paste("Laurin pakka: ",input$select_laurin_pakka))
  #print(paste("maxvarotus:: ",max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])))
  if(!is.null(input$select_laurin_pakka) & !is.null(input$select_martin_pakka)) {
    maxottelu<-max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])
    r_valittu_peli$ottelutilanne_text <- kaikkipelit[Ottelu_ID==maxottelu,paste("Tilanne: ",sum(Lauri_voitti,na.rm=TRUE),"-",sum(Martti_voitti,na.rm=TRUE))]
    
    #kato onko peleja jaljella
    peleja_jaljella<-kaikkipelit[Ottelu_ID==maxottelu & is.na(Voittaja) ,peli_ID]
    if(length(peleja_jaljella)>0){
      temp_peli<-min(kaikkipelit[Ottelu_ID==maxottelu & is.na(Voittaja) ,peli_ID])
    } else {
      temp_peli<-Inf
    }
    
    r_valittu_peli$peliID<-temp_peli
    
    #kato onko peli pelattu
    if(is.infinite(temp_peli))
    {
      pelipelattu<-TRUE
      
    }else {
      pelipelattu<-FALSE
    }
    
    if(pelipelattu==TRUE){
      print("observeluettu TRUE")
      shinyjs::disable("lauri_voitti")
      shinyjs::disable("martti_voitti")
      shinyjs::disable("tallenna_tulos")
      
      #jos maxottelun turnausNo ei ole nykyinen turnaus, niin peli ei ole ohjelmassa, muuten peli on pelattu
      maxturnaus<-max(kaikkipelit[,TurnausNo])
      maxottelun_turnaus<-max(kaikkipelit[maxottelu==Ottelu_ID,TurnausNo])
      if (maxturnaus==maxottelun_turnaus) {
        r_valittu_peli$aloittaja_text<-"Pelattu"
      } else {
        r_valittu_peli$aloittaja_text<-"Ei ohjelmassa"
      }
      
    } else {
      
      #  updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
      # updateRadioButtons(session,"radio_voittaja",selected=0)
      shinyjs::enable("lauri_voitti")
      shinyjs::enable("martti_voitti")
      shinyjs::enable("tallenna_tulos")
      aloittajaNo<- kaikkipelit[peli_ID==r_valittu_peli$peliID, Aloittaja]
      r_valittu_peli$aloittaja_text<-ifelse(aloittajaNo==0,"Aloittaja: Lauri","Aloittaja: Martti")
    }
    #seuraa valintalistoja ja paivita r_valittu_peli sen mukaan
    
    
    
    
    #tarkista onko jatko-otteluita jäljellä
    if (is.finite(r_valittu_peli$jatkopeli)) {
      
      
      shinyjs::enable("jatka_ottelua") 
    }else {
      
      shinyjs::disable("jatka_ottelua") 
      #print("Ei ole peliä kesken")
      
    }
    
  }
  print("seuraa valintalistoja loppu")
})

#jatka ottelua

observeEvent(input$jatka_ottelua,{
  print("jatka ottelua alku")
  
  if (!is.na(r_valittu_peli$jatkopeli)) {
    #  
    paivitaSliderit(r_valittu_peli$jatkopeli,session) 
    
  }else {
    print("Ei ole peliä kesken")
  }
  print("jatka ottelua loppu")
})

output$peliKesto <- renderText({
  
  invalidateLater(1000, session)
  tempData<-luecsv("./drop_download/temp_data_storage.csv")
  
  if (nrow(tempData)>4) {
    pelialkuAika<-as.integer(tempData[muuttuja=="Aloitusaika",arvo])
    pelialkuPVM<-as.integer(tempData[muuttuja=="Aloituspvm",arvo])
    sekunnit_yht<-aikaero(pelialkuAika,as.integer(as.ITime(now(tz="Europe/Helsinki"))),pelialkuPVM,as.integer(as.IDate(now(tz="Europe/Helsinki"))))
    minuutit<-floor(sekunnit_yht/60)
    sekunnit<-sekunnit_yht-60*minuutit
    #print(paste("sekunnit",sekunnit,"lahetetty:",lahetaTempData$lahetetty,"laheta:",lahetaTempData$laheta))
    if(is.na(tempData[muuttuja=="laheta",arvo])) {
      print("muuttuja oli NA")
      tempData[,arvo:=as.character(arvo)]
      tempData[muuttuja=="laheta",arvo:="TRUE"]
      tempData[muuttuja=="kesken",arvo:="FALSE"]
    }
    
    if(sekunnit>10 & tempData[muuttuja=="laheta",arvo]==TRUE) {
      tempData[muuttuja=="laheta",arvo:="FALSE"]
      tempData[muuttuja=="kesken",arvo:="TRUE"]
      kircsv(tempData,"./drop_download/temp_data_storage.csv")
      print("lähetetty")
      tempData[muuttuja=="kesken",arvo:="FALSE"]
      kircsv2(tempData,"./drop_download/temp_data_storage.csv")
      
    }
    paste(minuutit,":",sekunnit)
  }
})

observe({
  print(paste(input$select_laurin_pakka,input$select_martin_pakka,input$slider_laurin_mulligan,input$slider_martin_mulligan,input$tallenna_tulos,input$nollaa_aika))
  print(!is.null(input$select_laurin_pakka ))
  if(!is.null(input$select_laurin_pakka )) {
    #req(input$select_laurin_pakka,input$select_martin_pakka,input$slider_laurin_mulligan,input$slider_martin_mulligan,input$tallenna_tulos)
    print(paste("Observe altotusaika!!!!!!!!!!!"))
    tempData<-luecsv("./drop_download/temp_data_storage.csv")
    print(tempData)
    if(tempData[muuttuja=="kesken",arvo]!=TRUE) {
      print("peli ei ollut kesken")
      alotusaika<-as.ITime(now(tz="Europe/Helsinki"))
      alotuspvm<-as.IDate(now(tz="Europe/Helsinki"))
      laurin_pakka<-input$select_laurin_pakka
      martin_pakka<-input$select_martin_pakka
      laurin_mull<-input$slider_laurin_mulligan
      martin_mull<-input$slider_martin_mulligan
      laheta<-TRUE
      kesken<-FALSE
      muuttujat<-c("Laurin_pakka","Martin_pakka","Aloitusaika","Aloituspvm","Laurin_mulligan","Martin_mulligan","laheta","kesken")
      arvot<-c(laurin_pakka,martin_pakka,alotusaika,alotuspvm,laurin_mull,martin_mull,laheta,kesken)
      tempData<-data.table(muuttuja=muuttujat,arvo=arvot)
      kircsv2(tempData,"./drop_download/temp_data_storage.csv")
    } else {
      print("peli ei ollut kesken")
      tempData[muuttuja=="kesken",arvo:=FALSE]
      kircsv2(tempData,"./drop_download/temp_data_storage.csv")
    }
  }
})


#arvopeli
observeEvent(input$arvo_peli,{
  print("arvo peli alku")
  kaikkipelit<-peliDataReact()
  #kato onko divarifiltteri päällä
  if(input$divariRadio!="Ei väliä") {
    pelaamattomat <- unique(kaikkipelit[is.na(Voittaja) & Divari==input$divariRadio,Ottelu_ID])  
  } else {
    pelaamattomat <- unique(kaikkipelit[is.na(Voittaja),Ottelu_ID])
  }
  
  arpa<-ceiling(runif(1,0,length(pelaamattomat)))
  arvottu_ottelu_ID<-pelaamattomat[arpa]
  #eti ottelun pienin pelaamaton peli
  
  
  arvottu_peli_id <- kaikkipelit[Ottelu_ID==arvottu_ottelu_ID & is.na(Voittaja) , .SD[which.min(Ottelu_no)],.SDcols=c("peli_ID")][,peli_ID]
  paivitaSliderit(arvottu_peli_id,session)
  
  #print(pfi_data())
  print("arvo peli loppu")
  
})


output$divariRadio_out <- renderUI({
  divarit_ilman_peleja <- peliDataReact()[is.na(Voittaja),.N,by=Divari]
  radioButtons("divariRadio", "Divari",
               c("Ei väliä",divarit_ilman_peleja[,Divari]),inline=TRUE)
})




#tee laurin pakka selectinput
output$selectInputLauri <- renderUI({
  pakat<-divaridata()
  keskenPeliData<-luecsv("./drop_download/temp_data_storage.csv")
  #tarkista, onko peli kesken
  print(keskenPeliData)
  laurin_pakkanimet<-pakat[Omistaja==1,Nimi]
  laurin_idt<-pakat[Omistaja==1,Pakka]
  selectinputListLauri<-setNames(as.list(laurin_idt), c(laurin_pakkanimet))
  if(nrow(keskenPeliData)>1) {
    preSelect <- keskenPeliData[muuttuja=="Laurin_pakka",arvo]
  } else {
    preSelect <- 1
  }
  selectInput("select_laurin_pakka","Laurin pakka",choices = selectinputListLauri,selected=preSelect)
  
})
#tee martin pakka selectinput
output$selectInputMartti <- renderUI({
  pakat<-divaridata()
  keskenPeliData<-luecsv("./drop_download/temp_data_storage.csv")
  pakkanimet<-pakat[Omistaja==2,Nimi]
  martin_idt<-pakat[Omistaja==2,Pakka]
  selectinputList<-setNames(as.list(martin_idt), c(pakkanimet))
  if(nrow(keskenPeliData)>1) {
    preSelect <- keskenPeliData[muuttuja=="Martin_pakka",arvo]
  } else {
    preSelect <- 1
  }
  selectInput("select_martin_pakka","Martin pakka",choices = selectinputList,selected=preSelect)
  
})

output$data_vs_taulukko<-renderDataTable({
  req(input$radio_bo_mode,input$select_laurin_pakka,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode)
  vs_statsit_MA<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  
  vs_statsit_all<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())
  
  pakka_stats_all_lauri<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
  laurin_pakkanimi<-colnames(pakka_stats_all_lauri)[3]
  pakka_stats_all_martti<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
  martin_pakkanimi<-colnames(pakka_stats_all_martti)[3]
  setkeyv(pakka_stats_all_lauri,c("Tilasto","selite"))
  setkeyv(pakka_stats_all_martti,c("Tilasto","selite"))   
  join_pakka_stats_all<-pakka_stats_all_lauri[pakka_stats_all_martti]
  
  
  #MA_pakak
  pakka_stats_MA_lauri<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  pakka_stats_MA_martti<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  
  
  pfistats<-sarjataulukkoKaikki(divaridata(),peliDataReact(),FALSE,1,TRUE,NA,NA,NA,NA,FALSE,pfi_data())$pfi_trans
  
  #ota vaan sarakkeet, mitä on muuallakkin käytetty
  pfi_subsetcols<-pfistats[,names(vs_statsit_all$transposed),with=FALSE]
  
  
  setkeyv(pakka_stats_MA_lauri,c("Tilasto","selite"))
  setkeyv(pakka_stats_MA_martti,c("Tilasto","selite"))   
  join_pakka_stats_MA<-pakka_stats_MA_lauri[pakka_stats_MA_martti]
  
  lisakortit<-funcLisakortit(peliDataReact(),divaridata(),turnausSaantoReact(),TRUE,pfi_data())$current_lisakortit
  
  #filtteröi mukaan vaan pelin pakat
  lisakortit_pelipakat<-lisakortit[(Omistaja=="Lauri" & Pakka==input$select_laurin_pakka)|(Omistaja=="Martti" & Pakka==input$select_martin_pakka),.(Nimi,Lisakortit,Tilasto="Pakan koko",selite="")]
  lisakortit_pelipakat[,':=' (Kortti_lkm=(floor(Lisakortit)+37),Lisakortit=NULL)]
  #transponoi
  
  lisakortit_trans<-data.table(dcast(lisakortit_pelipakat,Tilasto+selite~Nimi,value.var="Kortti_lkm"))
  lisakortit_final<-lisakortit_trans[,c(laurin_pakkanimi,"Tilasto","selite",martin_pakkanimi),with=FALSE]
  
  append<-rbind(vs_statsit_all$transposed,join_pakka_stats_all,vs_statsit_MA,join_pakka_stats_MA,pfi_subsetcols)#,laurin_MA$transposed)
  #vaihda sarakejärjestys
  result_table<-append[,c(laurin_pakkanimi,"Tilasto","selite",martin_pakkanimi),with=FALSE]
  #lisää vielä lisäkorttitilasto
  result_table<-rbind(result_table,lisakortit_final)
  
  return(result_table)  
  
},    options = list(
  paging = FALSE,
  
  searching = FALSE,
  info=FALSE,
  columnDefs = list(list(className = 'dt-center', targets = 1:2),
                    list(className = 'dt-left', targets = 3)),
  rowCallback = DT::JS(
    'function(row, data) {
    if ((data[2]) == "VS")
    $("td", row).css("background", "PaleTurquoise");
    else if (data[2] == "Deck" )
    $("td", row).css("background", "PapayaWhip");
    else if (data[2] == "pfi" )
    $("td", row).css("background", "PowderBlue");
    
    }')
    
  
  
  ),rownames=FALSE)