# Define server logic required to draw a histogram 
shinyServer(function(input, output,session) {

  sourcelist <- dir("./scripts/")
  for(filename in sourcelist) {
    source(paste0("./scripts/", filename), local = TRUE)
  }


  #r_valittu_peli on valittu peli millä tahansa menetelmällä
  r_valittu_peli <-reactiveValues(peliID=1,jatkopeli=NA, aloittajatext="Ladataan")
  
    
  #obserEventit
  

    #nollaa temp data
    observeEvent(input$nollaa_temp_data, {
      tyhjataulu<-data.table(muuttuja=c("kesken","laheta"),arvo=c(FALSE,FALSE))
      print("tässä lähetetään tyhjataulu pilveen.")
      tyhjataulu
      kircsv(tyhjataulu,"./temp_data_storage.csv", upload = TRUE)
      
    })

  #päivitä divarit
  observeEvent(input$tallenna_divarit,{
    print("päivitä divarit alku")
    
    divarit<-divaridata()
    
    
    divarit[,syntax:=(text=paste0(Pakka,Omistaja))]
    
    lapply(divarit[,syntax],function(i) {
      divarit[syntax==i,Divari:=input[[i]]]
      
    })
    divarit[,syntax:=NULL]
    print(divarit)
    kircsv(divarit,"./divari.csv")
    #divaridata<-divarit
    print("päivitä divarit loppu")
  })
  
 

  #Serveripuolella tehdyt UI-palikat.
  output$text_aloittaja <- renderText(({paste(r_valittu_peli$aloittaja_text)}))
  output$text_tilanne <- renderText(({paste(r_valittu_peli$ottelutilanne_text)}))
  
 
  #divaricheckbox
  output$checkboxPakat<-renderUI({
    divarit<-divaridata()
    lapply(divarit[,rivi_id], function(i) {
      
      checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])
   
    })
  })


  observeEvent(input$sidebarmenu,{
    maxturnaus <-max(peliDataReact()[,TurnausNo])
    updateNumericInput(session,"sarjataulukkokierros",value=maxturnaus)})


  # divaridata <- reactiveFileReader(2000, session, "divari.csv",luecsv)
  divaridata <- reactive({
    print("divaritada alku")
    tulos <- luecsv("./divari.csv")
    print(paste(input$tallenna_bannit))
    print("divaritada loppu")
    return (tulos)
  })


  defaultStatValue<-reactiveValues(
   
  asetukset=list("Nimi","Vastustajan Nimi","Voitti",list(),"Average","Table")

  
  )
  
    
   #luo tilasto-asetus-objekti
   
   
   tilastoAsetuksetReact<-reactiveValues(
    
     data=tilastoAsetukset

   )

   observeEvent(input$myPivotData,{
       #ota edelliset asetukset talteen
       cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
       # Apply a function to all keys, to get corresponding values
       allvalues <- lapply(cnames, function(name) {
         item <- input$myPivotData[[name]]
         
       })
       defaultStatValue$asetukset<-allvalues
     })

   
   react_omaReadJson <- reactive({
     pakat<-omaReadJson("./external_files/",input$file1)
     pakat
   })
 
pfi_data<-reactive({
  pakat<-react_omaReadJson()
  print("TPALAT PAKAT")
 # print(pakat)
  tulos<-pakkaUutuusProsentti(pakat)
  tulos
})
    
anyFileUpload<-observe({
  req(input$anyfile)
  print(input$anyfile)
  drop_upload(input$anyfile$name, "mstat/csv/", mode = "overwrite", dtoken = token)
  
})

observe({
  req(input$file1)
  print(paste("ifile"))
  ifile <-input$file1
 # omistaja <- substr(1,1,ifile$name)
  if (!is.null(ifile)) {
    validointiteksti$teksti<-process_uploaded_decks(ifile,".//")}
  zip_all_and_send()
})

peliDataReact<-reactive({
  print("Luettu ./pelit.csv")
print(paste(input$tallenna_tulos),input$luo_peleja)
  if(input$radio_debug_mode==FALSE) {
    kaikkipelit<-luecsv("./pelit.csv")   
  } else {
    kaikkipelit<-luecsv("pelit_debug.csv")  
  }
   
  
})

saavutusAsetuksetReact<-reactiveValues(
  data=saavutusAsetukset
)


turnausSaantoReact<-reactive({
  print("luettu ./turnaussaanto.csv")
  turnaussaanto <- luecsv("turnaussaanto.csv")
  return(turnaussaanto)
})

validointiteksti <-reactiveValues(teksti="Ei ladattu pakkoja")
output$text_validointi <- renderText(({
  paste(validointiteksti$teksti)
  }))
  


})
