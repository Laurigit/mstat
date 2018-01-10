#tallennapeli
observeEvent(input$tallenna_tulos,{
  print("tallenna tulos alku")
  tempData<-luecsv("temp_data_storage.csv")
  uusrivi<- c(
    Aloitusaika=tempData[muuttuja=="Aloitusaika",arvo],
    Aloituspvm=tempData[muuttuja=="Aloituspvm",arvo],
    Lopetusaika=as.ITime(now(tz="Europe/Helsinki")),
    Lopetuspvm=as.IDate(now(tz="Europe/Helsinki")),
    Voittaja=as.numeric(input$radio_voittaja),
    Lauri_voitti=(1-as.numeric(input$radio_voittaja)),
    Martti_voitti=as.numeric(input$radio_voittaja),
    Laurin_mulligan=input$slider_laurin_mulligan,
    Martin_mulligan=input$slider_martin_mulligan,
    Laurin_arvosana=input$slider_laurin_virhe,
    Martin_arvosana=input$slider_martin_virhe,
    Laurin_humala=input$slider_laurin_humala,
    Martin_humala=input$slider_martin_humala,
    Laurin_landit=input$slider_laurin_landit,
    Martin_landit=input$slider_martin_landit,
    Laurin_lifet=input$slider_laurin_lifet,
    Martin_lifet=input$slider_martin_lifet,
    Vuoroarvio=input$slider_vuoroarvio,
    Laurin_kasikortit=input$slider_laurin_kasikortit,
    Martin_kasikortit=input$slider_martin_kasikorit
  )
  #tyhjennä tempdata
  tyhjataulu<-data.table(muuttuja=c("kesken","laheta"),arvo=c(FALSE,FALSE))
  
  kircsv(tyhjataulu,"temp_data_storage.csv")
  
  
  kaikkipelit<-data.table(luecsv("pelit.csv"))
  
  cols<-names(kaikkipelit)
  kaikkipelit[, (cols):= lapply(.SD, as.numeric), .SDcols=cols]
  
  kaikkipelit[peli_ID==r_valittu_peli$peliID, names(uusrivi) := as.list(uusrivi)][]
  
  
  #laske valmiiksi mahdollinen jatkopeli
  
  
  kaikkipelit[,otteluLKM:=as.double(.N),by=Ottelu_ID]
  
  kaikkipelit[,pelatut:=as.double(sum(ifelse(!is.na(Voittaja),1,0))),by=Ottelu_ID]
  
  #prosentti sitten
  kaikkipelit[,peliprosentti:=pelatut/otteluLKM]
  #palauta pienin keskeneräinen ottelu
  keskeneraiset_pelit<-kaikkipelit[is.na(Voittaja) & peliprosentti>0 & peliprosentti <1,peli_ID]
  if (length(keskeneraiset_pelit)>0) {
    keskenpeli<-min(kaikkipelit[is.na(Voittaja) & peliprosentti>0 & peliprosentti <1,peli_ID])
  } else {
    keskenpeli<-Inf
  }
  if (is.finite(keskenpeli)) {
    
    r_valittu_peli$jatkopeli<-keskenpeli 
    shinyjs::enable("jatka_ottelua") 
  }else {
    r_valittu_peli$jatkopeli<-NA
    shinyjs::disable("jatka_ottelua") 
    #print("Ei ole peliä kesken")
    
  }
  
  
  
  #jos bo_mode on päällä, niin tuhoa ylijäämäpelit
  #laske otteluiden voittoprosentti
  kaikkipelit[,':=' (MaxVP=pmax(sum(Lauri_voitti,na.rm=TRUE)/.N,sum(Martti_voitti,na.rm=TRUE)/.N)),by=Ottelu_ID]
  kaikkipelit[,MaxVP:=ifelse(is.na(MaxVP),0,MaxVP)]
  
  #jätä rivit, joiden MaxVP<0.5 tai rivillä on voittaja tai BO_mode on pois päältä
  pelit_jaljella <- kaikkipelit[(!is.na(Voittaja)|MaxVP<=0.5)|BO_mode==0]
  pelit_jaljella[,':='(MaxVP=NULL,otteluLKM=NULL,pelatut=NULL,peliprosentti=NULL)]
  
  kircsv(pelit_jaljella,"pelit.csv")
  updateTabItems(session,"sidebarmenu","tab_uusi_peli")
  
  
  #jos pelejä jäljellä disabloi uusien pelien luominen
  peleja_jaljella <- pelit_jaljella[is.na(Voittaja),.N]
  if (peleja_jaljella>0) {
    shinyjs::disable("luo_peleja")
    shinyjs::enable("tasuri_peli")
    shinyjs::enable("arvo_peli")
  } else {
    shinyjs::enable("luo_peleja")
    shinyjs::disable("arvo_peli")
    shinyjs::disable("tasuri_peli")
    
  }
  #nollaa inputit
  
  updateSliderInput(session, "slider_laurin_mulligan",  value = 0) 
  updateSliderInput(session, "slider_martin_mulligan",  value = 0) 
  updateSliderInput(session, "slider_laurin_virhe",  value = 1) 
  updateSliderInput(session, "slider_martin_virhe",  value = 1) 
  updateSliderInput(session, "slider_laurin_humala",  value = -0.1) 
  updateSliderInput(session, "slider_martin_humala",  value = -0.1) 
  updateSliderInput(session, "slider_laurin_landit",  value = 0) 
  updateSliderInput(session, "slider_martin_landit",  value = 0) 
  updateSliderInput(session, "slider_laurin_lifet",  value = 0) 
  updateSliderInput(session, "slider_martin_lifet",  value = 0)
  updateSliderInput(session, "slider_vuoroarvio",  value = 0) 
  updateSliderInput(session, "slider_laurin_kasikortit",  value = -1) 
  updateSliderInput(session, "slider_martin_kasikorit",  value = -1) 
  updateNumericInput(session,"sarjataulukkokierros",value=0)
  print("tallenna tulos loppu")
})


output$mulliganiSliderit<-renderUI({
  pelitiedot<-luecsv("temp_data_storage.csv")
  if(nrow(pelitiedot)==0) {
    laurin_pre_mulligan<-0
    martin_pre_mulligan<-0
  } else {
    laurin_pre_mulligan<-pelitiedot[muuttuja=="Laurin_mulligan",arvo]
    martin_pre_mulligan<-pelitiedot[muuttuja=="Martin_mulligan",arvo]
  }
  
  fluidRow(column(3, sliderInput("slider_laurin_mulligan", label = h4("Laurin mulliganit"), min = 0, 
                                 max = 6, value =laurin_pre_mulligan)),
           
           
           
           column(3, offset=3, sliderInput("slider_martin_mulligan", label = h4("Martin mulliganit"), min = 0, 
                                           max = 6, value = martin_pre_mulligan))
  )
})


observeEvent(input$laurin_mulligan,{
  print("laurin mulligan alku")
  updateSliderInput(session, "slider_laurin_mulligan", value = input$slider_laurin_mulligan+1)
  print("laurin mulligan loppu")
})
observeEvent(input$martin_mulligan,{
  print("martin mulligan alku")
  updateSliderInput(session, "slider_martin_mulligan", value = input$slider_martin_mulligan+1)
  print("martin mulligan loppu")
})
observeEvent(input$laurin_virhe,{
  print("laurin virhe alku")
  updateSliderInput(session, "slider_laurin_virhe", value = input$slider_laurin_virhe-1)
  print("laurin virhe loppu")
})
observeEvent(input$laurin_virhe_uusipeli,{
  print("laurin virhe alku")
  updateSliderInput(session, "slider_laurin_virhe", value = input$slider_laurin_virhe-1)
  print("laurin virhe loppu")
})

observeEvent(input$martin_virhe,{
  print("martin virhe alku")
  updateSliderInput(session, "slider_martin_virhe", value = input$slider_martin_virhe-1)
  print("martin virhe loppu")
})
observeEvent(input$martin_virhe_uusipeli,{
  print("martin virhe alku")
  updateSliderInput(session, "slider_martin_virhe", value = input$slider_martin_virhe-1)
  print("martin virhe loppu")
})

observeEvent(input$lauri_voitti,{
  print("lauri voitti alku")
  kaikkipelit<-data.table(luecsv("pelit.csv"))
  #tarkista onko peli pelattu
  if(!is.na(kaikkipelit[peli_ID==  r_valittu_peli$peliID,Voittaja])){
    print("peli on jo pelattu")
  } else {
    
    updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
    updateRadioButtons(session,"radio_voittaja",selected=0)
  }
  print("lauri voitti loppu")
})

observeEvent(input$martti_voitti,{
  print("martti voitti alku")
  updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
  updateRadioButtons(session,"radio_voittaja",selected=1)
  print("martti voitti loppu")
})


observeEvent(input$slider_vuoroarvio,{
  print("slider vuoroarvio alku")
  updateSliderInput(session, "slider_martin_landit", value = input$slider_vuoroarvio)
  updateSliderInput(session, "slider_laurin_landit", value = input$slider_vuoroarvio)
  print("slider vuoroarvio loppu")
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