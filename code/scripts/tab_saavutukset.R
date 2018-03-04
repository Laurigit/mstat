saavutusTaulu<-reactive({
  saavutusTaulu<-data.table(Omistaja=character(),saavutusNimi=character(),result=numeric(),Nimi=character())
  for(kierros in 1:nrow(saavutusAsetuksetReact$data)) {
    kierrosData<-saavutusAsetuksetReact$data[kierros]
    
    kierrosTulos<-laskeSaavtusAsetuksista(kierrosData,peliDataReact(),divaridata(),pfi_data(),ennusteDataReact$Data)
    
    
    saavutusTaulu<-rbind(saavutusTaulu,kierrosTulos,fill=TRUE)
    
  }
  
  
  
  print("saavutustaulu ajettu")
  print(saavutusTaulu)
  print("saavutustaulun tulos ylla")
  return(saavutusTaulu)
}
)


output$saavutus_UI<-renderUI({
  
  tekstiData<-saavutusTaulu()[source=="Paras"]
  # print("render UI ssavutus_UI")
  # print(infoBoxData)
  # looppi_kerrat<-nrow(infoBoxData)-1
  # 
  
  looppi<-1:nrow(tekstiData)-1
  fluidPage(
    lapply(looppi, function(i) {
      rivi<-i+1
      looppiData<-tekstiData[rivi]
      
      box(HTML(looppiData[,teksti]),background = looppiData[,color])
      
      
    })
  )
})
