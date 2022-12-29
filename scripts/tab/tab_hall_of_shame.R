#tab_hall_of_shame

output$shame_UI <- renderUI({
print("KÄYTÄ SHAMESSA")
  tekstiData<-saavutusTaulu()[source == "Huonoin"]
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
