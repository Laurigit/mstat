output$pivot_cross <- renderRpivotTable({
  pivotData<-tilastoMurskain(divaridata(),peliDataReact(),pfi_data(),input_bo_mode=FALSE,input_moving_average=input$numeric_MA_valinta,input_pfiMA=NA)
  
  
  
  #1 jos tallennettu asetus valittu, käytä sitä
  #2 jos edellinen asetus tallennettu, käytä sitä
  #3 käytä tallennettua asetusta 1, jos sellanen olemassa
  #4 tyhjä taulu
  
  if(!is.null(input$tallennetut_tilastoasetukset_rows_selected)) {
    #lataa asetukset
    asetukset<- tilastoAsetuksetReact$data[input$tallennetut_tilastoasetukset_rows_selected,asetukset][[1]]
    dataLahto<- tilastoAsetuksetReact$data[input$tallennetut_tilastoasetukset_rows_selected,datataulu]
    #sorttaus<-tilastoAsetuksetReact$data[input$tallennetut_tilastoasetukset_rows_selected,sorttaus]
    
    #paivita valinta
    updateRadioButtons(session,"radio_tilastoData",selected=dataLahto)
    cols_use<-asetukset[[1]]
    rows_use<-asetukset[[2]]
    vals_use<-asetukset[[3]]
    exclusions_use<-asetukset[[4]]
    aggregator_use<-asetukset[[5]]
    renderName_use<-asetukset[[6]]
    #tallenna edelliset asetukset
    defaultStatValue$asetukset<-asetukset
  } else {
    cols_use<-defaultStatValue$asetukset[[1]]
    rows_use<-defaultStatValue$asetukset[[2]]
    vals_use<-defaultStatValue$asetukset[[3]]
    exclusions_use<-defaultStatValue$asetukset[[4]]
    aggregator_use<-defaultStatValue$asetukset[[5]]
    renderName_use<-defaultStatValue$asetukset[[6]]
    #kato miten sortataan
    #sorttaus<-input$radio_minMax
  }
  
  
  #konvertoi sorttaus oikeeseen muotoon (EI TOIMINUT, pivottiin ei vaikuttanut mitenkaan)
  # if(sorttaus=="max") {
  #   sortAsetus<-"value_z_to_a"
  # } else if (sorttaus =="min") {
  #   sortAsetus<-"value_a_to_z"
  # } else {
  #   sortAsetus<-"key_a_to_z"
  # }
  # print(sortAsetus)
  # 
  #lataa oikea data
  if(input$radio_tilastoData=="Aikasarja") {
    outputData<-pivotData$aikasarja
  } else if (input$radio_tilastoData=="Ristidata"){
    outputData<-pivotData$cross
  } else {
    outputData<-pivotData$turnaus
  }

  
  rpivotTable(outputData, 
              col=unlist(cols_use),
              rows=unlist(rows_use), 
              vals=unlist(vals_use), 
              exclusions=exclusions_use, 
              aggregatorName=aggregator_use,
              rowOrder= "value_z_to_a",
              rendererName=renderName_use, width="100%", height = "100%",
              onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
})


#nayta tallennettut asetukset
output$tallennetut_tilastoasetukset<- renderDataTable({
  naytaData<-tilastoAsetuksetReact$data[,.(Tallennettu_asetus=kuvaus)]
  input$radio_tilastoData
  return(naytaData)
},selection = 'single',options = list(
  searching = FALSE,
  info=FALSE,
  paging=FALSE,
  scrollY =105
),rownames=FALSE)#,colnames=NULL)


#poista tilastoasetus
observeEvent(input$poista_tilastoAsetus,{
  #lue data
  print(input$tallennetut_tilastoasetukset_rows_selected)
  print( tilastoAsetuksetReact$data[input$tallennetut_tilastoasetukset_rows_selected])
  tilastoAsetukset<- tilastoAsetuksetReact$data[-input$tallennetut_tilastoasetukset_rows_selected]
  tilastoAsetuksetReact$data<-tilastoAsetukset
  print( tilastoAsetuksetReact$data)
  saveR_and_send(tilastoAsetukset,"tilastoAsetukset","tilastoAsetukset.R")
  
})

observeEvent(input$tallennaSaavutusAsetus,{
  #kato onko siellä dataa
  if(is.null(saavutusAsetuksetReact$data)){
    saavutusAsetukset<-data.table(
      datataulu=character(),
      kuvaus=character(),
      asetukset=list(),
      minVaiMax=character(),
      minVaiMax_rivi=character(),
      Palkintonimi=character()
    )
  }
  cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
  # Apply a function to all keys, to get corresponding values
  allvalues <- lapply(cnames, function(name) {
    item <- input$myPivotData[[name]]
  })
  storeList<-NULL
  storeList[[1]]<-allvalues
  
  uusrivi<-data.table(
    datataulu=input$radio_tilastoData,
    kuvaus=input$text_tilastoKuvaus,
    asetukset=(storeList)
  )
  print(uusrivi)
  #tarkista onko asetusnimi jo olemassa
  if(nrow(saavutusAsetuksetReact$data[kuvaus==input$text_tilastoKuvaus])>0){
    print("TÄTKTEÄ")
    print(saavutusAsetuksetReact$data)
    vanhat_asetukset<-saavutusAsetuksetReact$data[kuvaus==input$text_tilastoKuvaus,.(Palkintonimi,Esitysmuoto,minVaiMax,minVaiMax_rivi)]
    #liita uudet ja vanhat
    uus_ja_vanha_rivi<-cbind(uusrivi,vanhat_asetukset)
    print(uus_ja_vanha_rivi)
    saavutusAsetuksetReact$data<-saavutusAsetuksetReact$data[kuvaus!=input$text_tilastoKuvaus]
    print(saavutusAsetuksetReact$data)
    saavutusAsetukset<-rbind(saavutusAsetuksetReact$data,uus_ja_vanha_rivi)
  }else{
    #lisätään tyhjat sarakkeet puuttuviin tietoihin
    uusrivi[,':=' (Palkintonimi="",Esitysmuoto="Decimal",minVaiMax="max",minVaiMax_rivi="max")]
    saavutusAsetukset<-rbind(saavutusAsetuksetReact$data,uusrivi)
  }
  
  #tallenna rdata
  print("TALLENNA SAAVUTUS")
  print(saavutusAsetukset)
  saveR_and_send(saavutusAsetukset,"saavutusAsetukset","saavutusAsetukset.R")
  saavutusAsetuksetReact$data<-saavutusAsetukset
  #tyhjennä tekstikenttä
  updateTextInput(session,"text_tilastoKuvaus",value="")
})