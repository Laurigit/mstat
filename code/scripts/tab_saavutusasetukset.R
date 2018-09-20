required_data("STG_SAAVUTUSASETUKSET")
saavutusAsetuksetReact<-reactiveValues(
  
  data = STG_SAAVUTUSASETUKSET
)

#poista saavutusAsetus
observeEvent(input$poista_saavutusAsetus,{

  saavutusAsetuksetReact$data<- saavutusAsetuksetReact$data[-input$tallennetut_saavutusAsetukset_rows_selected]

  saavutusAsetukset<-saavutusAsetuksetReact$data
  print(saavutusAsetukset)
  saveR_and_send(saavutusAsetukset,"saavutusAsetukset","saavutusAsetukset.RData")
  print(saavutusAsetukset)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_SAAVUTUSASETUKSET", ADM_DI_HIERARKIA, input_env = globalenv(), FALSE)
})

#paivita saavutusAsetus
observeEvent(input$paivita_saavutus,{
  #vanha rivi talteen
  
  vanhat_asetukset<-saavutusAsetuksetReact$data[input$tallennetut_saavutusAsetukset_rows_selected,.(datataulu,asetukset)]
  
  uusrivi<-data.table(
    minVaiMax=input$radio_minMax_saavutus,
    minVaiMax_rivi=input$radio_minMax_saavutus_rivi,
    Esitysmuoto=input$radio_muotoilu,
    Palkintonimi=input$txt_palkinto,
    kuvaus=input$txt_palkinto_kuvaus
  )
  #liita uudet ja vanhat
  print(uusrivi)
  uus_ja_vanha_rivi<-cbind(uusrivi,vanhat_asetukset)
  print(uus_ja_vanha_rivi)
  
  #tarkista onko asetusrivi olemassa
  if(nrow(saavutusAsetuksetReact$data[input$tallennetut_saavutusAsetukset_rows_selected])>0){
    #poista vanha rivi
    saavutusAsetuksetReact$data<-saavutusAsetuksetReact$data[-input$tallennetut_saavutusAsetukset_rows_selected]
    print(saavutusAsetuksetReact)
    saavutusAsetuksetReact$data<-rbind(saavutusAsetuksetReact$data,uus_ja_vanha_rivi)
    saavutusAsetukset<-saavutusAsetuksetReact$data
    saveR_and_send(saavutusAsetukset,"saavutusAsetukset","saavutusAsetukset.RData")
    print(saavutusAsetuksetReact)
  }else{
    print("ei riviä valittuna, mitaan ei muutettu")
  }
  
  
  
})

#seuraa saavutusasetusten rivivalintaa
observeEvent(input$tallennetut_saavutusAsetukset_rows_selected,{
  #lueData
  riviData<-saavutusAsetuksetReact$data[input$tallennetut_saavutusAsetukset_rows_selected]
  updateRadioButtons(session,"radio_minMax_saavutus",selected=riviData[,minVaiMax])
  updateRadioButtons(session,"radio_minMax_saavutus_rivi",selected=riviData[,minVaiMax])
  updateRadioButtons(session,"radio_muotoilu",selected=riviData[,Esitysmuoto])
  updateTextInput(session,"txt_palkinto",value=riviData[,Palkintonimi])
  updateTextInput(session,"txt_palkinto_kuvaus",value=riviData[,kuvaus])
})

output$tallennetut_saavutusAsetukset<- renderDataTable({
  input$paivita_saavutus
  input$poista_Saavutus
  input$tallennaSaavutusAsetus
  naytaData<-saavutusAsetuksetReact$data[,.(Kuvaus=kuvaus,Esitysmuoto,Palkintonimi,datataulu,minVaiMax,minVaiMax_rivi)]
  return(naytaData)
},selection = 'single',options = list(
  info=FALSE
),rownames=FALSE)#,colnames=NULL)
