#tallennapeli
#input_Peli_ID <-909
# 
# Aloitusaika <-1
# Aloituspvm<-1
# Lopetus_DT = now(tz = "EET")
# Voittaja<-1
# Laurin_mulligan<-1
# Martin_mulligan<-1
# Laurin_arvosana<-1
# Martin_arvosana<-1
# Laurin_landit <-1
# Martin_landit<-1
# Laurin_lifet<-1
# Martin_lifet<-1
# Vuoroarvio<-1
# Laurin_kasikortit<-1
# Martin_kasikortit<-1
# 

#required_data("ADM_TEMP_DATA_STORAGE")
#tempData <- ADM_TEMP_DATA_STORAGE

# eR_Peli_Aloittaja <- NULL
# eR_Peli_Aloittaja$a <- 1
#vuoroarviolasku <- 9
observeEvent(input$tallenna_tulos, {
 shinyjs::disable("tallenna_tulos")
  input_Peli_ID <- eR_Peli_ID()
  #vuoroarviolasku
required_data(c("ADM_PELIT", "ADM_TEMP_DATA_STORAGE", "ADM_CURRENT_TURN", "ADM_CURRENT_DMG" ))
tempData <- ADM_TEMP_DATA_STORAGE 
aloittajaNo <- eR_Peli_Aloittaja$a
  if(aloittajaNo == 0) {
    vuoroarviolasku <- input$slider_vuoroarvio + input$slider_laurin_mulligan - 6
    # print(paste0(input$slider_vuoroarvio, " + ", input$slider_laurin_mulligan, " - 6 = ", vuoroarviolasku))

  } else {
    vuoroarviolasku <- input$slider_vuoroarvio + input$slider_martin_mulligan - 6
    # print(paste0(input$slider_vuoroarvio, " + ", input$slider_martin_mulligan, " - 6 = ", vuoroarviolasku))
  }
  # print("VUOROARVIOLASKU")
  # print(vuoroarviolasku)
#browser()
  uusrivi<- c(
    Aloitus_DT = as.character(tempData[muuttuja=="Aloitus_DT",arvo]),
    Lopetus_DT = as.character(now(tz = "EET")),
    Voittaja=as.character(input$radio_voittaja),
    Lauri_voitti=as.character(1-as.numeric(input$radio_voittaja)),
    Martti_voitti=as.character(input$radio_voittaja),
    Laurin_mulligan=input$slider_laurin_mulligan,
    Martin_mulligan=input$slider_martin_mulligan,
    Laurin_arvosana=input$slider_laurin_virhe,
    Martin_arvosana=input$slider_martin_virhe,
    # Laurin_humala=input$slider_laurin_humala,
    # Martin_humala=input$slider_martin_humala,
    Laurin_landit=input$slider_laurin_landit,
    Martin_landit=input$slider_martin_landit,
    Laurin_lifet=input$slider_laurin_lifet,
    Martin_lifet=input$slider_martin_lifet,
    Vuoroarvio=vuoroarviolasku,
    Laurin_kasikortit=input$slider_laurin_kasikortit,
    Martin_kasikortit=input$slider_martin_kasikorit
  )

  #tyhjennä tempdata


  kaikkipelit<-data.table(luecsv("pelit.csv"))
  cols<-names(kaikkipelit) 

  kaikkipelit[, (cols):= lapply(.SD, as.character), .SDcols=cols]

  kaikkipelit[peli_ID==input_Peli_ID, names(uusrivi) := as.list(uusrivi)]

   #jos bo_mode on päällä, niin tuhoa ylijäämäpelit
  #laske otteluiden voittoprosentti
  colsBackToNum <- c("Lauri_voitti", "Martti_voitti", "BO_mode", "Voittaja")
  kaikkipelit[, (colsBackToNum):= lapply(.SD, as.numeric), .SDcols=colsBackToNum]
  kaikkipelit[,':=' (MaxVP=pmax(sum(Lauri_voitti,na.rm=TRUE)/.N,sum(Martti_voitti,na.rm=TRUE)/.N)),by=Ottelu_ID]
  kaikkipelit[,MaxVP:=ifelse(is.na(MaxVP),0,MaxVP)]
  
  #jätä rivit, joiden MaxVP<0.5 tai rivillä on voittaja tai BO_mode on pois päältä
  pelit_jaljella <- kaikkipelit[(!is.na(Voittaja) | MaxVP <= 0.5) | BO_mode == 0]
  pelit_jaljella[,':='(MaxVP = NULL)]
  
  kircsv(pelit_jaljella,"pelit.csv", TRUE)
  
  
  
  # updateSliderInput(session, "slider_laurin_mulligan",  value = 0) 
  slider_laurin_mulligan$value <- 0
  # updateSliderInput(session, "slider_martin_mulligan",  value = 0) 
  slider_martin_mulligan$value <- 0
  # updateSliderInput(session, "slider_laurin_virhe",  value = 1) 
  slider_laurin_virhe$value <- 1
  #  updateSliderInput(session, "slider_martin_virhe",  value = 1) 
  slider_martin_virhe$value <- 1
  # updateSliderInput(session, "slider_laurin_landit",  value = 0) 
  slider_laurin_landit$value <- 0
  #  updateSliderInput(session, "slider_martin_landit",  value = 0) 
  slider_martin_landit$value <- 0
  #  updateSliderInput(session, "slider_laurin_lifet",  value = 0) 
  slider_laurin_lifet$value <- 0
  #  updateSliderInput(session, "slider_martin_lifet",  value = 0)
  slider_martin_lifet$value <- 0
  # updateSliderInput(session, "slider_vuoroarvio",  value = 0) 
  print("tallenna peli slidervuoroarvio")
  slider_vuoroarvio$value <- 4
  print(slider_vuoroarvio$value)
  #  updateSliderInput(session, "slider_laurin_kasikortit",  value = -1) 
  slider_laurin_kasikortit$value <- -1
  # updateSliderInput(session, "slider_martin_kasikorit",  value = -1) 
  slider_martin_kasikorit$value <- -1
# 
#   life_totals$data <-  calc_life_totals(ADM_CURRENT_DMG)
#   damage_data$data <- ADM_CURRENT_DMG
#   turnData$turn <- 1
#   


  

  #lifecoutnteri-nollaukset ja tallennukset
  #ota talteen vuorotiedosto ja 
  new_name <- paste0("./external_files/dmg_", eR_Peli_ID(), ".csv")
  file.copy(from = "./dmg_turn_files/current_dmg.csv",
            to = new_name)
  new_name2 <- paste0("./external_files/turn_", eR_Peli_ID(), ".csv")
  file.copy(from = "./dmg_turn_files/current_turn.csv",
            to = new_name2)
  #tun once for each players.
  tallenna_tulos_ui_update$value <-  2

}, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  print("tallenna_tulos_ui_update$value")
  print(tallenna_tulos_ui_update$value)
 if( tallenna_tulos_ui_update$value > 0 ) {
  
  
  
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_PELIT", ADM_DI_HIERARKIA, input_env = globalenv())
  
  required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_TURN"))
  write.table(x = ADM_CURRENT_DMG[1 == 0],
              file = paste0("./dmg_turn_files/", "current_dmg.csv"),
              sep = ";",
              row.names = FALSE,
              dec = ",")
  write.table(x = ADM_CURRENT_TURN[1 == 0],
              file = paste0("./dmg_turn_files/", "current_turn.csv"),
              sep = ";",
              row.names = FALSE,
              dec = ",")
  
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
  updateData("SRC_CURRENT_TURN", ADM_DI_HIERARKIA, globalenv())

  
  
  updateTabItems(session,"sidebarmenu","tab_uusi_peli") 
 # js$collapse("uusipeli_box")
  updatedTempData$a <- isolate(updatedTempData$a + 1)
  updateNumericInput(session,"sarjataulukkokierros",value = 0)
  tallenna_tulos_ui_update$value <- isolate( tallenna_tulos_ui_update$value - 1)
  shinyjs::enable("tallenna_tulos")
 }
 
})


#slider_laurin_lifet
observeEvent(input$slider_laurin_lifet,{
  slider_laurin_lifet$value <- input$slider_laurin_lifet

}, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 2)


observe({
  updateSliderInput(session,
                    inputId = "slider_laurin_lifet", value = slider_laurin_lifet$value)
}, priority = 1)
#slider_martin_lifet
observeEvent(input$slider_martin_lifet,{
  slider_martin_lifet$value <- input$slider_martin_lifet
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_martin_lifet", value = (slider_martin_lifet$value))
},priority = 1)

#slider_laurin_kasikortit
observeEvent(input$slider_laurin_kasikortit,{
  slider_laurin_kasikortit$value <- input$slider_laurin_kasikortit
  
}, ignoreNULL = TRUE,priority = 2, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_laurin_kasikortit", value = (slider_laurin_kasikortit$value))
},priority = 1)

#slider_martin_kasikorit
observeEvent(input$slider_martin_kasikorit,{
  slider_martin_kasikorit$value <- input$slider_martin_kasikorit

},priority = 2, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_martin_kasikorit", value = (slider_martin_kasikorit$value))
},priority = 1)

#slider_laurin_landit
observeEvent(input$slider_laurin_landit,{
  slider_laurin_landit$value <- input$slider_laurin_landit
  
}, priority = 2, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_laurin_landit", value = (slider_laurin_landit$value))
}, priority = 1)

#slider_martin_landit
observeEvent(input$slider_martin_landit,{
  slider_martin_landit$value <- input$slider_martin_landit
  
}, ignoreNULL = TRUE, priority = 2, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_martin_landit", value = (slider_martin_landit$value))
}, priority = 1)

#slider_laurin_mulligan
observeEvent(input$slider_laurin_mulligan,{
  slider_laurin_mulligan$value <- input$slider_laurin_mulligan
  
}, ignoreNULL = TRUE, priority = 2, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_laurin_mulligan", value = (slider_laurin_mulligan$value))
}, priority = 1)

#slider_martin_mulligan
observeEvent(input$slider_martin_mulligan,{
  slider_martin_mulligan$value <- input$slider_martin_mulligan
  
}, ignoreNULL = TRUE, priority = 2, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_martin_mulligan", value = (slider_martin_mulligan$value))
}, priority = 1)

#slider_laurin_virhe
observeEvent(input$slider_laurin_virhe,{
  slider_laurin_virhe$value <- input$slider_laurin_virhe
  
}, ignoreNULL = TRUE, priority = 2, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_laurin_virhe", value = (slider_laurin_virhe$value))
}, priority = 1)


#slider_martin_virhe
observeEvent(input$slider_martin_virhe,{
  slider_martin_virhe$value <- input$slider_martin_virhe
  
}, priority = 2, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  updateSliderInput(session,
                    inputId = "slider_martin_virhe", value = (slider_martin_virhe$value))
}, priority = 1)




observe({
  print("slider_vuoroarvio")
  print(slider_vuoroarvio$value)
  updateSliderInput(session,
                    inputId = "slider_vuoroarvio", value = slider_vuoroarvio$value)
}, priority = 1)


#observe if tallenna is enabled
observe({

  if(!is.null(slider_martin_kasikorit$value) & !is.null(slider_laurin_kasikortit$value)) {
  if ( slider_martin_kasikorit$value >= 0 &  slider_laurin_kasikortit$value >= 0) {
    shinyjs::enable("tallenna_tulos")
  } else {
    
    shinyjs::disable("tallenna_tulos")
  }
  } else {
    shinyjs::disable("tallenna_tulos")
  }
})


observeEvent(input$laurin_mulligan,{
      updateSliderInput(session, "slider_laurin_mulligan", value = input$slider_laurin_mulligan + 1)
})
observeEvent(input$martin_mulligan,{
  updateSliderInput(session, "slider_martin_mulligan", value = input$slider_martin_mulligan + 1)
})
observeEvent(input$laurin_virhe,{
   updateSliderInput(session, "slider_laurin_virhe", value = input$slider_laurin_virhe-1)
})
observeEvent(input$laurin_virhe_uusipeli,{
    updateSliderInput(session, "slider_laurin_virhe", value = input$slider_laurin_virhe-1)
})

observeEvent(input$martin_virhe,{
    updateSliderInput(session, "slider_martin_virhe", value = input$slider_martin_virhe-1)
})
observeEvent(input$martin_virhe_uusipeli,{
   updateSliderInput(session, "slider_martin_virhe", value = input$slider_martin_virhe-1)
})

#lauri voitto globaali
observeEvent(input$lauri_voitti,{

  react_lauri_voitti$value <- input$lauri_voitti
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
observe({
  if (react_lauri_voitti$value > 0 ) {
  #print("lauri voitti value updated")
  updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
  updateRadioButtons(session,"radio_voittaja",selected = 0)
  }
})

#martti voitti globaali
observeEvent(input$martti_voitti,{
  
 
  react_martti_voitti$value <- input$martti_voitti
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observe({
  
  if (react_martti_voitti$value > 0 ) {
 updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
  updateRadioButtons(session,"radio_voittaja",selected = 1)
  }
})


observeEvent(input$slider_vuoroarvio,{
#  print("rab_tallenna slider voroarvio")
  uusi_arvo <- round(input$slider_vuoroarvio * 0.42)

  print("observe_event input$slider_vuoroarvio")
  print(input$slider_vuoroarvio)
  slider_vuoroarvio$value <- input$slider_vuoroarvio
  
  #print(uusi_arvo)
   updateSliderInput(session, inputId = "slider_martin_landit", value = uusi_arvo)
   updateSliderInput(session, "slider_laurin_landit", value = uusi_arvo)
}, priority = 1)

observeEvent(input$action_add,{
  if(values$lastUpdated=="slider_laurin_humala" | values$lastUpdated == "slider_martin_humala") {
    steppi <- 0.1
  } else {
    steppi <- 1
  }
  updateSliderInput(session,values$lastUpdated,value=input[[values$lastUpdated]]+steppi)
})

observeEvent(input$action_reduce,{
  if(values$lastUpdated=="slider_laurin_humala" | values$lastUpdated == "slider_martin_humala") {
    steppi <- 0.1
  } else {
    steppi <- 1
  }
  updateSliderInput(session,values$lastUpdated,value=input[[values$lastUpdated]]-steppi)
})

#osuus, joka katsoo mitä UI-palikkaa on viimeksi muokattu. Liittyen sliderehein tallenna peli sivulla

values <- reactiveValues(
  lastUpdated = NULL
)

observe({
  
  lapply(names(input), function(x) {
    observe({
      input[[x]]
      values$lastUpdated <- x
    })
  })
})

#ruutu mikä näyttää muokattavaa numeroa
output$last_changed_value_text <- renderText({
  
  arvo <- input[[values$lastUpdated]]
  if(is.numeric(arvo)) {
    tulos <- arvo 
  }else {
    tulos <- ""
  }

  tulos
})

#Vuoroarvaus, kumman korttimäärä
output$vuoroArvausPelaaja <- renderUI({
req(eR_Peli_Aloittaja$a)
    aloittajaNo <- eR_Peli_Aloittaja$a
  if(aloittajaNo == 0) {
    aloittaja_vuoro_teksti <- "Laurin kortti_lkm"
  } else if (aloittajaNo == 1){
    aloittaja_vuoro_teksti <- "Martin kortti_lkm"
  } else {
    aloittaja_vuoro_teksti <- "RIKKI"
  }
  sliderInput("slider_vuoroarvio",
              label = h4(aloittaja_vuoro_teksti),
              min = 4,
              max = 16,
              value = 4)
  
 
})
  


output$validateWinnerText <- renderText({
  if(input$radio_voittaja == 0 & input$slider_laurin_lifet == 0) {
    validate <- FALSE
  } else if (input$radio_voittaja == 1 & input$slider_martin_lifet == 0) {
    validate <- FALSE
  } else {
    validate <- TRUE
  }
  
  if (validate == TRUE) {
    result_text <- ""
  } else {
    result_text <- "Invalid life"
  }
})
  
  
