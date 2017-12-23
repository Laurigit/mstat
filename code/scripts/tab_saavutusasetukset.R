#poista saavutusAsetus
observeEvent(input$poista_saavutusAsetus,{
  print(saavutusAsetuksetReact$data)
  saavutusAsetuksetReact$data<- saavutusAsetuksetReact$data[-input$tallennetut_saavutusAsetukset_rows_selected]
  print(saavutusAsetuksetReact$data)
  saavutusAsetukset<-saavutusAsetuksetReact$data
  saveR_and_send(saavutusAsetukset,"saavutusAsetukset","saavutusAsetukset.R")
  
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
    saveR_and_send(saavutusAsetukset,"saavutusAsetukset","saavutusAsetukset.R")
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
