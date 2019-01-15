

required_data("ADM_DI_HIERARKIA")
updateData("SRC_TEMP_DATA_STORAGE", ADM_DI_HIERARKIA, input_env = globalenv())

#seuraa valintalistoja seka tallennusta ja paivita UI + tiedot sen mukaan.
output$mulliganiSlideriLauri<-renderUI({
 
  
  required_data(c("ADM_TEMP_DATA_STORAGE"))

  laurin_pre_mulligan<-ADM_TEMP_DATA_STORAGE[muuttuja == "Laurin_mulligan",arvo]

  sliderInput("slider_laurin_mulligan",
              label = h4("Laurin mulliganit"),
              min = 0,
              max = 6,
              value = laurin_pre_mulligan)
})

output$mulliganiSlideriMartti<-renderUI({

  required_data(c("ADM_TEMP_DATA_STORAGE"))
  
  martin_pre_mulligan<-ADM_TEMP_DATA_STORAGE[muuttuja=="Martin_mulligan",arvo]
  
  sliderInput("slider_martin_mulligan", label = h4("Martin mulliganit"), min = 0, 
              max = 6, value = martin_pre_mulligan)
  
})

output$divariRadio_out <- renderUI({
  required_data("ADM_PELIT")
  divarit_ilman_peleja <- ADM_PELIT[is.na(Voittaja),.N,by=Divari]
  radioButtons("divariRadio", "Division",
               c("All", sort(divarit_ilman_peleja[,Divari])), inline = TRUE)
})




#tee laurin pakka selectinput
output$selectInputLauri <- renderUI({
 # print("output$selectInputLauri ")


  required_data(c("STG_PAKAT", "ADM_TEMP_DATA_STORAGE"))

 
  pakat<-STG_PAKAT[Omistaja_ID == "L"]

  keskenPeliData <- ADM_TEMP_DATA_STORAGE

  #tarkista, onko peli kesken
  #  print(keskenPeliData)
  laurin_pakkanimet<-pakat[,Pakka_NM]
  laurin_idt<-pakat[,Pakka_ID]
  selectinputListLauri<-setNames(as.list(laurin_idt), c(laurin_pakkanimet))
  
  preSelect <- keskenPeliData[muuttuja=="Laurin_pakka",arvo]

  
  selectInput("select_laurin_pakka","Laurin pakka",choices = selectinputListLauri,selected=preSelect)
  
  
})
#tee martin pakka selectinput
output$selectInputMartti <- renderUI({
  
  required_data(c("STG_PAKAT", "ADM_TEMP_DATA_STORAGE"))
  pakat<-STG_PAKAT[Omistaja_ID == "M"]
  keskenPeliData<- ADM_TEMP_DATA_STORAGE
  pakkanimet<-pakat[,Pakka_NM]
  martin_idt<-pakat[,Pakka_ID]
  selectinputList<-setNames(as.list(martin_idt), c(pakkanimet))
  
  preSelect <- keskenPeliData[muuttuja=="Martin_pakka",arvo]
  
  selectInput("select_martin_pakka","Martin pakka",choices = selectinputList,selected=preSelect)
  
})





