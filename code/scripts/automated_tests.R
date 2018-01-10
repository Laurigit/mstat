observeEvent(input$automated_tests, {

  updateRadioButtons(session, inputId = "radio_voittaja", selected = 0)
  
# input$radio_voittaja,
#   Martti_voitti=as.numeric(input$radio_voittaja),
#   Laurin_mulligan=input$slider_laurin_mulligan,
#   Martin_mulligan=input$slider_martin_mulligan,
#   Laurin_arvosana=input$slider_laurin_virhe,
#   Martin_arvosana=input$slider_martin_virhe,
#   Laurin_humala=input$slider_laurin_humala,
#   Martin_humala=input$slider_martin_humala,
#   Laurin_landit=input$slider_laurin_landit,
#   Martin_landit=input$slider_martin_landit,
#   Laurin_lifet=input$slider_laurin_lifet,
#   Martin_lifet=input$slider_martin_lifet,
#   Vuoroarvio=input$slider_vuoroarvio,
#   Laurin_kasikortit=input$slider_laurin_kasikortit,
#   Martin_kasikortit=input$slider_martin_kasikorit
#   
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