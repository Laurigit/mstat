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