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
 input_Peli_ID <- eR_Peli_ID()

  #vuoroarviolasku

required_data(c("ADM_PELIT", "ADM_TEMP_DATA_STORAGE"))
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
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_PELIT", ADM_DI_HIERARKIA, input_env = globalenv())
  updatedTempData$a <- isolate(updatedTempData$a + 1)

  updateTabItems(session,"sidebarmenu","tab_uusi_peli") 
  js$collapse("uusipeli_box")
 
  
  updateSliderInput(session, "slider_laurin_mulligan",  value = 0) 
  updateSliderInput(session, "slider_martin_mulligan",  value = 0) 
  updateSliderInput(session, "slider_laurin_virhe",  value = 1) 
  updateSliderInput(session, "slider_martin_virhe",  value = 1) 
  updateSliderInput(session, "slider_laurin_landit",  value = 0) 
  updateSliderInput(session, "slider_martin_landit",  value = 0) 
  updateSliderInput(session, "slider_laurin_lifet",  value = 0) 
  updateSliderInput(session, "slider_martin_lifet",  value = 0)
  updateSliderInput(session, "slider_vuoroarvio",  value = 0) 
  updateSliderInput(session, "slider_laurin_kasikortit",  value = -1) 
  updateSliderInput(session, "slider_martin_kasikorit",  value = -1) 
  updateNumericInput(session,"sarjataulukkokierros",value=0)

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

observeEvent(input$lauri_voitti,{
    updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
    updateRadioButtons(session,"radio_voittaja",selected=0)
  })

observeEvent(input$martti_voitti,{
  updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
  updateRadioButtons(session,"radio_voittaja",selected=1)
})


observeEvent(input$slider_vuoroarvio,{
#  print("rab_tallenna slider voroarvio")
  uusi_arvo <- round(input$slider_vuoroarvio * 0.42)

  #print(uusi_arvo)
   updateSliderInput(session, inputId = "slider_martin_landit", value = uusi_arvo)
   updateSliderInput(session, "slider_laurin_landit", value = uusi_arvo)
})

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
  
  
