#simulate_tallenna_peli_click <- reactiveValues(valueButton = 0) #


observeEvent(input$automated_tests, {

  updateRadioButtons(session, inputId = "radio_voittaja", selected = 0)
  updateSliderInput(session, inputId = "slider_laurin_mulligan", value = 1)
  updateSliderInput(session, inputId = "slider_martin_mulligan", value = 2)
  updateSliderInput(session, inputId = "slider_laurin_virhe", value = 1)
  updateSliderInput(session, inputId = "slider_martin_virhe", value = -1)
  
  updateSliderInput(session, inputId = "slider_laurin_humala", value = 1)
  updateSliderInput(session, inputId = "slider_martin_humala", value = 1.9)
  
  updateSliderInput(session, inputId = "slider_laurin_landit", value = 1)
  updateSliderInput(session, inputId = "slider_martin_landit", value = 2)
  
  updateSliderInput(session, inputId = "slider_laurin_lifet", value = 0)
  updateSliderInput(session, inputId = "slider_martin_lifet", value = 2)
  
  updateSliderInput(session, inputId = "slider_vuoroarvio", value = 7)
  
  updateSliderInput(session, inputId = "slider_martin_landit", value = 2)
  
  
  updateSliderInput(session, inputId = "slider_laurin_kasikortit", value = 1)
  updateSliderInput(session, inputId = "slider_martin_kasikorit", value = 2)
  
#  simulate_tallenna_peli_click$valueButton <- 1 +  simulate_tallenna_peli_click$valueButton 

  
# input$radio_voittaja,

#   
#   
#   tempData<-luecsv("./drop_download/temp_data_storage.csv")
#   uusrivi<- c(
#     Aloitusaika=tempData[muuttuja=="Aloitusaika",arvo],
#     Aloituspvm=tempData[muuttuja=="Aloituspvm",arvo],
#     Lopetusaika=as.ITime(now(tz="Europe/Helsinki")),
#     Lopetuspvm=as.IDate(now(tz="Europe/Helsinki")),
#     Voittaja=as.numeric(input$radio_voittaja),
#     Lauri_voitti=(1-as.numeric(input$radio_voittaja)),
#     Martti_voitti=as.numeric(input$radio_voittaja),
#     Laurin_mulligan=input$slider_laurin_mulligan,
#     Martin_mulligan=input$slider_martin_mulligan,
#     Laurin_arvosana=input$slider_laurin_virhe,
#     Martin_arvosana=input$slider_martin_virhe,
#     Laurin_humala=input$slider_laurin_humala,
#     Martin_humala=input$slider_martin_humala,
#     Laurin_landit=input$slider_laurin_landit,
#     Martin_landit=input$slider_martin_landit,
#     Laurin_lifet=input$slider_laurin_lifet,
#     Martin_lifet=input$slider_martin_lifet,
#     Vuoroarvio=input$slider_vuoroarvio,
#     Laurin_kasikortit=input$slider_laurin_kasikortit,
#     Martin_kasikortit=input$slider_martin_kasikorit
#   )
  
})
