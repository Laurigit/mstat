#seuraa valintalistoja seka tallennusta ja paivita UI + tiedot sen mukaan.
observe({

  #seuraa tallenna buttonia myös 

  
  kaikkipelit<-luecsv("pelit.csv")
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
      r_valittu_peli$aloittaja_text <- ifelse(aloittajaNo==0,"Aloittaja: Lauri","Aloittaja: Martti")
      r_valittu_peli$aloittaja <- aloittajaNo
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

})

#jatka ottelua

observeEvent(input$jatka_ottelua,{

  jatkopeli <- find_jatka_ottelua(ADM_PELIT)
  if (!is.na(jatkopeli)) {
    #  
    paivitaSliderit(jatkopeli,session) 
    
  }else {

  }
  print("jatka ottelua loppu")
})



output$mulliganiSlideriLauri<-renderUI({
  pelitiedot<-luecsv("temp_data_storage.csv")
  if(nrow(pelitiedot)==0) {
    laurin_pre_mulligan<-0

  } else {
    laurin_pre_mulligan<-pelitiedot[muuttuja=="Laurin_mulligan",arvo]
   
  }
  
sliderInput("slider_laurin_mulligan", label = h4("Laurin mulliganit"), min = 0, 
                                 max = 6, value =laurin_pre_mulligan)

})

output$mulliganiSlideriMartti<-renderUI({
  pelitiedot<-luecsv("temp_data_storage.csv")
  if(nrow(pelitiedot)==0) {
    martin_pre_mulligan<-0
  } else {
    martin_pre_mulligan<-pelitiedot[muuttuja=="Martin_mulligan",arvo]
  }
sliderInput("slider_martin_mulligan", label = h4("Martin mulliganit"), min = 0, 
                                           max = 6, value = martin_pre_mulligan)
  
})


output$peliKesto <- renderText({
  
  invalidateLater(1000, session)
  tempData<-luecsv("temp_data_storage.csv")
  
  if (nrow(tempData)>4) {
    pelialkuAika <- as.integer(tempData[muuttuja == "Aloitusaika", arvo])
    pelialkuPVM <- as.integer(tempData[muuttuja == "Aloituspvm", arvo])
    sekunnit_yht<-aikaero(pelialkuAika, 
                          as.integer(as.ITime(now(tz = "Europe/Helsinki"))),
                          pelialkuPVM, as.integer(as.IDate(now(tz = "Europe/Helsinki"))))
    minuutit<-floor(sekunnit_yht/60)
    sekunnit<-sekunnit_yht-60*minuutit
    #print(paste("sekunnit",sekunnit,"lahetetty:",lahetaTempData$lahetetty,"laheta:",lahetaTempData$laheta))
    if(is.na(tempData[muuttuja=="laheta",arvo])) {
    #  print("muuttuja oli NA")
      tempData[,arvo:=as.character(arvo)]
      tempData[muuttuja=="laheta",arvo:="TRUE"]
      tempData[muuttuja=="kesken",arvo:="FALSE"]
    }
    
    if(sekunnit>10 & tempData[muuttuja=="laheta",arvo]=="TRUE") {
      tempData[muuttuja=="laheta",arvo:="FALSE"]
      tempData[muuttuja=="kesken",arvo:="TRUE"]
      #print("tallennetaan seuraava pilveen. Tähän muutettu, että kesken = TRUE ja laheta = FALSE. Tässä kohtaa oleteteaan, että yli 10 sec on menny ja käsky on laheta")
     # print(tempData)
      kircsv(tempData,"temp_data_storage.csv", upload = TRUE)
      tempData[muuttuja=="kesken",arvo:="FALSE"]
     # print("tallennetaan seuraava vaan levylle. Tähän muutettu, että kesken = FALSE. Tässä kohtaa oleteteaan, että yli 10 sec on menny ja käsky on laheta")
      #print(tempData)
      kircsv(tempData,"temp_data_storage.csv", upload = FALSE)
      
    }
    sekunnit_fix <- str_pad(sekunnit, 2, pad = "0")
    paste(minuutit,":",sekunnit_fix)
  } else {
   # print("temp_data_storagessa ei ollu neljää riviä")
  }
})

observeEvent(c(input$select_laurin_pakka,
               input$select_martin_pakka,
               input$slider_laurin_mulligan,
               input$slider_martin_mulligan,
               input$tallenna_tulos), {
 
      req(input$select_laurin_pakka)
     
      req(input$select_martin_pakka)
   
      req(input$slider_laurin_mulligan)
     
      req(input$slider_martin_mulligan)

      #actionbutton alkutilassa on NULL ja sen painallusten jälkeen vasta saa arvoja.

      # ,
      # ,
      # ,
      # ,
      # , cancelOutput = FALSE)
 # print("check req redi")
 # print("inputvektori laurinpakka, martinpakka, laurinmull, martinmull, tallennatulos, nollaa_aika")
 # print(paste(input$select_laurin_pakka,
 #              input$select_martin_pakka,
 #              input$slider_laurin_mulligan,
 #              input$slider_martin_mulligan,
 #              input$tallenna_tulos,
 #              input$nollaa_aika))
 #  print(!is.null(input$select_laurin_pakka ))
  if(!is.null(input$select_laurin_pakka )) {
    
   # print(paste("Tässä kohtaa luettiin koneelta tempData ja printataan se"))
    tempData<-luecsv("temp_data_storage.csv")
    #print(tempData)
    if (tempData[muuttuja == "kesken",arvo] != "TRUE") {
      #print("kesken == FALSE")
      alotusaika<-as.ITime(now(tz="Europe/Helsinki"))
      alotuspvm<-as.IDate(now(tz="Europe/Helsinki"))
      laurin_pakka<-input$select_laurin_pakka
      martin_pakka<-input$select_martin_pakka
      laurin_mull<-input$slider_laurin_mulligan
      martin_mull<-input$slider_martin_mulligan
      laheta<-"TRUE"
      kesken<-"FALSE"
      muuttujat<-c("Laurin_pakka","Martin_pakka","Aloitusaika","Aloituspvm","Laurin_mulligan","Martin_mulligan","laheta","kesken")
      arvot<-c(laurin_pakka,martin_pakka,alotusaika,alotuspvm,laurin_mull,martin_mull,laheta,kesken)
      tempData<-data.table(muuttuja=muuttujat,arvo=arvot)
      #print("lähetetään seuraava file pilveen. Tässä kohtaa otettiin uudet arvot nykytilasta")
      #print(tempData)
      kircsv(tempData,"temp_data_storage.csv", upload = FALSE)
    } else {
     # print("kesken == TRUE")
      tempData[muuttuja=="kesken",arvo:="FALSE"]
     #  print("Tallennetaan seuraava arvo lokaalisti. Tässä kohtaa vaan korjattiin, että kesken = FALSE")
     # print(tempData)

      kircsv(tempData,"temp_data_storage.csv", upload = FALSE)
    }
  }
})


#arvopeli
observeEvent(input$arvo_peli,{
#input$divariRadio <- 1
  required_data("ADM_PELIT")
  arvottu_peli_id <- getRandomPeli_ID(ADM_PELIT, input$divariRadio)
  paivitaSliderit(arvottu_peli_id,session)
})


output$divariRadio_out <- renderUI({
  divarit_ilman_peleja <- peliDataReact()[is.na(Voittaja),.N,by=Divari]
  radioButtons("divariRadio", "Division",
               c("All",divarit_ilman_peleja[,Divari]),inline=TRUE)
})




#tee laurin pakka selectinput
output$selectInputLauri <- renderUI({
  required_data("STG_PAKAT")
  pakat<-STG_PAKAT[Omistaja_ID == "L"]
  keskenPeliData<-luecsv("temp_data_storage.csv")
  #tarkista, onko peli kesken
#  print(keskenPeliData)
  laurin_pakkanimet<-pakat[,Pakka_NM]
  laurin_idt<-pakat[,Pakka_ID]
  selectinputListLauri<-setNames(as.list(laurin_idt), c(laurin_pakkanimet))
  if(nrow(keskenPeliData)>2) {
    preSelect <- keskenPeliData[muuttuja=="Laurin_pakka",arvo]
  } else {
    preSelect <- 1
  }
  selectInput("select_laurin_pakka","Laurin pakka",choices = selectinputListLauri,selected=preSelect)
  
})
#tee martin pakka selectinput
output$selectInputMartti <- renderUI({
  required_data("STG_PAKAT")
  pakat<-STG_PAKAT[Omistaja_ID == "M"]
  keskenPeliData<-luecsv("temp_data_storage.csv")
  pakkanimet<-pakat[,Pakka_NM]
  martin_idt<-pakat[,Pakka_ID]
  selectinputList<-setNames(as.list(martin_idt), c(pakkanimet))
  if(nrow(keskenPeliData)>2) {
    preSelect <- keskenPeliData[muuttuja=="Martin_pakka",arvo]
  } else {
    preSelect <- 1
  }
  selectInput("select_martin_pakka","Martin pakka",choices = selectinputList,selected=preSelect)

})

#seuraavaks uusi_peli statseja muistiin etukäteen.
vs_statsit_MA_react <- reactive(
  sarjataulukkoKaikki(divaridata(),
                                   peliDataReact(),
                                   input$radio_bo_mode,
                                   1,
                                   TRUE,
                                   NA,
                                   input$select_laurin_pakka,
                                   input$select_martin_pakka,
                                   input$numeric_MA_valinta,
                                   input$radio_pfi_mode,
                                   pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  )

vs_statsit_all_react <- reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())
  )

pakka_stats_all_lauri_react<-reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
)

pakka_stats_all_martti_react<-reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
)

pakka_stats_MA_lauri_react<-reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
)

pakka_stats_MA_martti_react <- reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
)

pfistats_react <- reactive(
  sarjataulukkoKaikki(divaridata(),peliDataReact(),FALSE,1,TRUE,NA,NA,NA,NA,FALSE,pfi_data())$pfi_trans
)

lisakortit_react <- reactive({
  lisakortit <- funcLisakortit(peliDataReact(),divaridata(),turnausSaantoReact(),TRUE,pfi_data())$current_lisakortit
  #filtteröi mukaan vaan pelin pakat
  lisakortit_pelipakat<-lisakortit[(Omistaja=="Lauri" & Pakka==input$select_laurin_pakka) |
                                     (Omistaja=="Martti" & Pakka==input$select_martin_pakka),
                                   .(Nimi,Lisakortit,Tilasto="Pakan koko",selite="ATK")]
  lisakortit_pelipakat[,':=' (Kortti_lkm=(floor(Lisakortit)),Lisakortit=NULL)]
  lisakortit_trans<-data.table(dcast(lisakortit_pelipakat,Tilasto+selite~Nimi,value.var="Kortti_lkm"))

}
)

output$PakkaLeftBox <- renderUI({
 # eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
result <- getDeckStats("Lauri", eR_UID_UUSI_PELI())
 # result(eR_UID_UUSI_PELI())
  box(HTML(result), background = "purple", width = NULL)
  
})
output$PakkaRightBox <- renderUI({

 result <- getDeckStats("Martti", eR_UID_UUSI_PELI())
 box(HTML(result), background = "yellow", width = NULL)
  
})
eR_Peli_ID <- eventReactive(c(input$select_laurin_pakka,
                              input$select_martin_pakka),{
                                
  # input$select_laurin_pakka <- 1
  # input$select_martin_pakka <-9                              
  required_data("ADM_PELIT")        
                               
  uusi_pelii <-getUusi_Peli_ID(ADM_PELIT,
                  input$select_laurin_pakka,
                   input$select_martin_pakka)
 
  return(uusi_pelii)
}, ignoreInit = TRUE, ignoreNULL = TRUE)
eR_UID_UUSI_PELI <- eventReactive(eR_Peli_ID(), {
  # input$numeric_MA_valinta <- 7
  # input$radio_bo_mode<- FALSE
  # input$radio_pfi_mode <- FALSE

  required_data(c("ADM_PELIT", "INT_PFI", "STG_PAKAT", "STG_OMISTAJA", "STAT_VOITTOENNUSTE"))

  tulos <- UID_UUSI_PELI(eR_Peli_ID(),
                         eR_UID_PAKKA(),
                         eR_UID_PAKKA_VS(),
                         STG_PAKAT,
                         STG_OMISTAJA,
                         ADM_PELIT,
                         STAT_VOITTOENNUSTE,
                         input$slider_laurin_mulligan,
                         input$slider_martin_mulligan
                         
                        )
  return(tulos)
}, ignoreInit = FALSE, ignoreNULL = FALSE)

eR_UID_PAKKA <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
required_functions("UID_PAKKA")
result <-  UID_PAKKA(ADM_PELIT,
                                                        INT_PFI,
                                                        input_MA_length = input$numeric_MA_valinta,
                                                        input_BO_mode  = input$radio_bo_mode,
                                                        input_pfi_mode = input$radio_pfi_mode)
return(result)
})

eR_UID_PAKKA_VS <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
                                  required_functions("UID_PAKKA_VS")
                                  result <-  UID_PAKKA_VS(ADM_PELIT,
                                                       INT_PFI,
                                                       input_MA_length = input$numeric_MA_valinta,
                                                       input_BO_mode  = input$radio_bo_mode,
                                                       input_pfi_mode = input$radio_pfi_mode)
                                  return(result)
 })

output$PakkaVSBox <- renderUI({
  #required_data("UID_UUSI_PELI", TRUE)
  #rm(eR_UID_UUSI_PELI)
  print(eR_UID_UUSI_PELI())
  eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
  result <- getVSStatsHtml(eR_UID_UUSI_PELI(), "Lauri")
  box(HTML(result), background = "aqua", width = NULL, align = "middle")
  
})


output$data_vs_taulukko<-renderDataTable({
  required_data("UID_UUSI_PELI")
  return(UID_UUSI_PELI)  
  
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


mallinnusDataReact <- reactiveValues(mallit = NULL)

observeEvent(input$luo_peleja,{

  voittoEnnusteMallit(peliData_ja_pfi_react())
  
})

observeEvent(input$tasuriPeli, {

#  divaridata <-luecsv("divarit.csv")
  # sarjadata <- sarjataulukkoKaikki(divaridata,
  #                                  peliData,
  #                                  0,
  #                                  17,
  #                                  FALSE,NA,NA,NA,NA,0,pfi_data)
    #tarkista, että pelattuja pelejä
  #eti uusimman turnauksen numero
  vikaturnausNo <- max(peliDataReact()[, TurnausNo])
  
  pelattuja_peleja <- nrow(peliDataReact()[!is.na(Voittaja) & TurnausNo == vikaturnausNo])
  if (pelattuja_peleja > 0) {
  sarjadata <- sarjataulukkoKaikki(divaridata(),
                                 peliDataReact(),
                                 input$radio_bo_mode,
                                 vikaturnausNo,
                                 FALSE,NA,NA,NA,NA,FALSE,pfi_data())

  pelaajat <- sarjadata$pelaajastats


  kokonaistilanne <- pelaajat[,.(Voitot_Lauri=sum(Voitot_Lauri),Voitot_Martti=sum(Voitot_Martti))]

  print(kokonaistilanne)
  erotus <- kokonaistilanne[,Voitot_Lauri] - kokonaistilanne[,Voitot_Martti]

    turnausTilanneInput <- ifelse(erotus > 0, "Lauri", ifelse(erotus < 0, "Martti", "Tasan"))
  } else {
    #jos ei pelattu pelejä vielä turnauksessa, niin tasan

    turnausTilanneInput <- "Tasan"
  }

  uusPeliID <- tasuripeli_ID(turnausTilanneInput, pfi_data(), peliDataReact(), ennusteMallitReact())

  paivitaSliderit(uusPeliID,session) 
})

