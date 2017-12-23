


# Define server logic required to draw a histogram 
shinyServer(function(input, output,session) {

  sourcelist <- dir("./scripts/")
  for(filename in sourcelist) {
    print(filename)
    source(paste0("./scripts/", filename), local = TRUE)
  }

  
  
  #r_valittu_peli on valittu peli millä tahansa menetelmällä
  r_valittu_peli <-reactiveValues(peliID=1,jatkopeli=NA, aloittajatext="Ladataan")
  
    
  #obserEventit
  
  #luo uusi turnaus
  observeEvent(input$luo_peleja,{
    print("luo pejelä alku")

    #divarit_dt<-luecsv("./drop_download/divari.csv")
    divarit_dt<-divaridata()
    kaikkiDivarit<-divarit_dt[order(Divari)][Picked==1,.N,by=Divari][,N:=NULL]
    #tarvitaan vain, kun ajetaan manuaalisesti eka kerta
    #kierroksia<-1
    #BO_mode<-FALSE
    #otteluita<-2
    #montako peliä on yhdessä ottelussa
    #TurnausNo<-1
    vanhatpelit <-luecsv("./drop_download/pelit.csv")
    turnaus_no<-max(vanhatpelit[,TurnausNo])+1
    for(divariKierros in kaikkiDivarit[,Divari]) {
    otteluita<-input[[paste0("numeric_ottelut",divariKierros)]]
    #otteluita<-5-divariKierros
     # print(paste("otteluita",otteluita))

    #montako ottelua on turnauksessa pakkojen välillä
    kierroksia <- input[[paste0("numeric_rounds",divariKierros)]]
    #kierroksia<-divariKierros
    BO_mode<-as.numeric(input[[paste0("checkbox_BO_mode",divariKierros)]])
    #BO_mode<-TRUE
    # otteluita <-1
    # kierroksia <-2
    # BO_mode<-FALSE
    divariKierrosData<-divarit_dt[Divari==divariKierros]

    pelit_list <- divariKierrosData[Picked==1,.(pakkalista=list(Pakka)),by=.(Divari,Omistaja)]
    pelit<-pelit_list[,expand.grid(pakkalista),by=Divari]
    setnames(pelit,c("Var1","Var2"),c("Laurin_pakka","Martin_pakka"))

    #lue edellinen turnausnumero
    vanhatpelit <-luecsv("./drop_download/pelit.csv")
    
    #eti edellinen max ottelu_id
    ed_ottelu_id_max<-max(vanhatpelit[,Ottelu_ID])
    if(!is.finite(ed_ottelu_id_max)) {ed_ottelu_id_max<-0}


    pelit[,Ottelu_ID:=.I+ed_ottelu_id_max]
    kaikki_ottelut<-NULL

    #lisää ottelut
    for (i in 1:otteluita){
      pelikierros <- pelit[,.(Divari,Laurin_pakka,Martin_pakka,Ottelu_no=i,Ottelu_ID,BO_mode)]
      kaikki_ottelut<-rbind(kaikki_ottelut,pelikierros)
    }
    #motanko ottelua per kierros?
    ottelua_per_kierros<-nrow(kaikki_ottelut)/otteluita

    #lisää kierrokset
    kaikkipelit<-NULL
    for (i in 1:kierroksia){
      ottelukierros <- kaikki_ottelut[,.(Divari,Laurin_pakka,Martin_pakka,Kierros=i,Ottelu_ID=(Ottelu_ID+(i-1)*ottelua_per_kierros),Ottelu_no,BO_mode)]
      kaikkipelit<-rbind(kaikkipelit,ottelukierros)
    }

      
      if(!is.finite(turnaus_no)) {turnaus_no<-1}
      kaikkipelit[,TurnausNo:=turnaus_no]

      #edellinen max peli_iD
      ed_peli_id<-max(vanhatpelit[,peli_ID])
      if(!is.finite(ed_peli_id)) {ed_peli_id<-0}

      #aloittaja

      kaikkipelit[, idl := 1:.N, by = Laurin_pakka]
      kaikkipelit[, idm := 1:.N, by = Martin_pakka]
      kaikkipelit[,Aloittaja:=(idl+idm+TurnausNo+Kierros+Divari+Ottelu_no)%%2]

      kaikkipelit[,':='(peli_ID=.I+ed_peli_id,idl=NULL,idm=NULL,Voittaja=NA,Aloitusaika=NA,Aloituspvm=NA,Lopetusaika=NA,Lopetuspvm=NA,Laurin_mulligan=NA,Martin_mulligan=NA,Laurin_arvosana=NA,Martin_arvosana=NA,Laurin_humala=NA,Martin_humala=NA,Laurin_landit=NA,Martin_landit=NA,Vuoroarvio=NA,Laurin_kasikortit=NA,Martin_kasikortit=NA,Lauri_voitti=NA,Martti_voitti=NA,Laurin_lifet=NA,Martin_lifet=NA)]
      #arvosana: 1= pelasin hyvin, en keksi parannettavaa. 0= Hieman löysäilyä. -1= merkittävää hölmöilyä.
      #str(kaikkipelit)

      #tee tyhja taulu
      empty_dt<-data.table(kaikkipelit[1==0])

      vanhatpelit<-rbind(empty_dt,vanhatpelit)
      #kircsv(vanhatpelit,"./drop_download/pelit.csv")
      #print(vanhatpelit)
      #lisää uudet
      kaikkipelit<-rbind(vanhatpelit,kaikkipelit)
     
      kircsv(kaikkipelit,"./drop_download/pelit.csv")
    }
      #päivitä nappulastatukset

        shinyjs::disable("luo_peleja")
        shinyjs::enable("arvo_peli")

        print("luo pejelä loppu")
   
  })
      

  
 
  
    

    #jatka ottelua
    
    observeEvent(input$jatka_ottelua,{
      print("jatka ottelua alku")

      if (!is.na(r_valittu_peli$jatkopeli)) {
    #  
        paivitaSliderit(r_valittu_peli$jatkopeli,session) 
  
      }else {
        print("Ei ole peliä kesken")
      }
      print("jatka ottelua loppu")
    })
   
    #nollaa temp data
    observeEvent(input$nollaa_temp_data, {
      tyhjataulu<-data.table(muuttuja=c("kesken","laheta"),arvo=c(FALSE,FALSE))
      
      kircsv(tyhjataulu,"./drop_download/temp_data_storage.csv")
      
    })
    
    
      

    

  
observe({
  print(paste(input$select_laurin_pakka,input$select_martin_pakka,input$slider_laurin_mulligan,input$slider_martin_mulligan,input$tallenna_tulos,input$nollaa_aika))
  print(!is.null(input$select_laurin_pakka ))
  if(!is.null(input$select_laurin_pakka )) {
  #req(input$select_laurin_pakka,input$select_martin_pakka,input$slider_laurin_mulligan,input$slider_martin_mulligan,input$tallenna_tulos)
  print(paste("Observe altotusaika!!!!!!!!!!!"))
  tempData<-luecsv("./drop_download/temp_data_storage.csv")
  print(tempData)
  if(tempData[muuttuja=="kesken",arvo]!=TRUE) {
    print("peli ei ollut kesken")
  alotusaika<-as.ITime(now(tz="Europe/Helsinki"))
    alotuspvm<-as.IDate(now(tz="Europe/Helsinki"))
    laurin_pakka<-input$select_laurin_pakka
    martin_pakka<-input$select_martin_pakka
    laurin_mull<-input$slider_laurin_mulligan
    martin_mull<-input$slider_martin_mulligan
    laheta<-TRUE
    kesken<-FALSE
    muuttujat<-c("Laurin_pakka","Martin_pakka","Aloitusaika","Aloituspvm","Laurin_mulligan","Martin_mulligan","laheta","kesken")
    arvot<-c(laurin_pakka,martin_pakka,alotusaika,alotuspvm,laurin_mull,martin_mull,laheta,kesken)
    tempData<-data.table(muuttuja=muuttujat,arvo=arvot)
    kircsv2(tempData,"./drop_download/temp_data_storage.csv")
  } else {
    print("peli ei ollut kesken")
    tempData[muuttuja=="kesken",arvo:=FALSE]
    kircsv2(tempData,"./drop_download/temp_data_storage.csv")
  }
  }
})

output$peliKesto <- renderText({

   invalidateLater(1000, session)
  tempData<-luecsv("./drop_download/temp_data_storage.csv")

  if (nrow(tempData)>4) {
  pelialkuAika<-as.integer(tempData[muuttuja=="Aloitusaika",arvo])
  pelialkuPVM<-as.integer(tempData[muuttuja=="Aloituspvm",arvo])
  sekunnit_yht<-aikaero(pelialkuAika,as.integer(as.ITime(now(tz="Europe/Helsinki"))),pelialkuPVM,as.integer(as.IDate(now(tz="Europe/Helsinki"))))
  minuutit<-floor(sekunnit_yht/60)
  sekunnit<-sekunnit_yht-60*minuutit
  #print(paste("sekunnit",sekunnit,"lahetetty:",lahetaTempData$lahetetty,"laheta:",lahetaTempData$laheta))
  if(is.na(tempData[muuttuja=="laheta",arvo])) {
    print("muuttuja oli NA")
    tempData[,arvo:=as.character(arvo)]
    tempData[muuttuja=="laheta",arvo:="TRUE"]
    tempData[muuttuja=="kesken",arvo:="FALSE"]
  }
  
  if(sekunnit>10 & tempData[muuttuja=="laheta",arvo]==TRUE) {
    tempData[muuttuja=="laheta",arvo:="FALSE"]
    tempData[muuttuja=="kesken",arvo:="TRUE"]
    kircsv(tempData,"./drop_download/temp_data_storage.csv")
    print("lähetetty")
    tempData[muuttuja=="kesken",arvo:="FALSE"]
    kircsv2(tempData,"./drop_download/temp_data_storage.csv")

  }
  paste(minuutit,":",sekunnit)
  }
})






#  observeEvent(input$button_aloitusaika,{
#    print(paste("Observe altotusaika"))
#     kaikkipelit<-data.table(luecsv("./drop_download/pelit.csv"))
#     kaikkipelit[peli_ID==r_valittu_peli$peliID, ':=' (Aloitusaika=as.ITime(now(tz="Europe/Helsinki")),Aloituspvm=as.IDate(now(tz="Europe/Helsinki")))]
#     print(kaikkipelit)
#     kircsv(kaikkipelit,"./drop_download/pelit.csv")
#     print("observe aloitusaika loppu")
# })



 

  #seuraa valintalistoja seka tallennusta ja paivita UI + tiedot sen mukaan.
  observe({
    print("seuraa valintalistoja alku")
    #seuraa tallenna buttonia myös 
    print(paste("tallennatulosarvo",input$tallenna_tulos))
    
    kaikkipelit<-luecsv("./drop_download/pelit.csv")
    #print(paste("Laurin pakka: ",input$select_laurin_pakka))
    #print(paste("maxvarotus:: ",max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])))
    if(!is.null(input$select_laurin_pakka) & !is.null(input$select_martin_pakka)) {
    maxottelu<-max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])
    r_valittu_peli$ottelutilanne_text <- kaikkipelit[Ottelu_ID==maxottelu,paste("Tilanne: ",sum(Lauri_voitti,na.rm=TRUE),"-",sum(Martti_voitti,na.rm=TRUE))]
    
    #kato onko peleja jaljella
    peleja_jaljella<-kaikkipelit[Ottelu_ID==maxottelu & is.na(Voittaja) ,peli_ID]
    if(length(peleja_jaljella)>0){
    temp_peli<-min(kaikkipelit[Ottelu_ID==maxottelu & is.na(Voittaja) ,peli_ID])
    } else {
      temp_peli<-Inf
    }
    
    r_valittu_peli$peliID<-temp_peli
    
    #kato onko peli pelattu
    if(is.infinite(temp_peli))
      {
      pelipelattu<-TRUE
      
    }else {
      pelipelattu<-FALSE
    }
    
    if(pelipelattu==TRUE){
      print("observeluettu TRUE")
      shinyjs::disable("lauri_voitti")
      shinyjs::disable("martti_voitti")
      shinyjs::disable("tallenna_tulos")
      
      #jos maxottelun turnausNo ei ole nykyinen turnaus, niin peli ei ole ohjelmassa, muuten peli on pelattu
      maxturnaus<-max(kaikkipelit[,TurnausNo])
      maxottelun_turnaus<-max(kaikkipelit[maxottelu==Ottelu_ID,TurnausNo])
      if (maxturnaus==maxottelun_turnaus) {
        r_valittu_peli$aloittaja_text<-"Pelattu"
      } else {
        r_valittu_peli$aloittaja_text<-"Ei ohjelmassa"
      }
   
    } else {
 
    #  updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
     # updateRadioButtons(session,"radio_voittaja",selected=0)
      shinyjs::enable("lauri_voitti")
      shinyjs::enable("martti_voitti")
      shinyjs::enable("tallenna_tulos")
      aloittajaNo<- kaikkipelit[peli_ID==r_valittu_peli$peliID, Aloittaja]
      r_valittu_peli$aloittaja_text<-ifelse(aloittajaNo==0,"Aloittaja: Lauri","Aloittaja: Martti")
      }
    #seuraa valintalistoja ja paivita r_valittu_peli sen mukaan

    
    
    
    #tarkista onko jatko-otteluita jäljellä
    if (is.finite(r_valittu_peli$jatkopeli)) {
      
   
      shinyjs::enable("jatka_ottelua") 
    }else {
  
      shinyjs::disable("jatka_ottelua") 
      #print("Ei ole peliä kesken")
      
    }
    
    }
    print("seuraa valintalistoja loppu")
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
    kircsv(divarit,"./drop_download/divari.csv")
    #divaridata<-divarit
    print("päivitä divarit loppu")
  })
  
  #paivitä bannit
  observeEvent(input$tallenna_bannit,{
    print("tallenna bannit alku")
    
    divarit<-divaridata()
    divarit[,syntax_cb:=(text=paste0("checkbox",Pakka,Omistaja))]
    divarit[,syntax:=(text=paste0(Pakka,Omistaja))]
    
    lapply(divarit[,syntax_cb],function(i) {
      
      divarit[syntax_cb==i,Picked:=as.numeric((input[[i]]))]
      
    })
    lapply(divarit[,syntax],function(i) {
      
      divarit[syntax==i,Divari:=input[[i]]]
      
      
    })
    
    divarit[,syntax:=NULL]
    divarit[,syntax_cb:=NULL]
    
    #validoi divari
    #laske montako pakkaa per divari
    pakkavalidoi<-divarit[,.N,by=.(Omistaja,Divari)]
    #laske montako omistajaa per divari
    omistajavalidoi<-pakkavalidoi[,.N,by=.(Divari)]
    if(min(omistajavalidoi[,N]==2)){
      shinyjs::enable("luo_peleja")
    } else {
      shinyjs::disable("luo_peleja")
    }
  kircsv(divarit,"./drop_download/divari.csv")
    
  print("tallenna bannit loppu")
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
  

  
  #divariNumericinput
  output$combUI<-renderUI({
    divarit<-divaridata()
    
    lapply(divarit[,rivi_id], function(i) {
      fluidRow(
        column(3,  numericInput(paste0( divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]), label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Divari])),
        column(3,h4("Divari/picked")),
        column(3, checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])))
      
    
    })
    
  })

  #output$testiteksti<-renderText({input$sidebarmenu})


  
  
  output$blob <- renderUI({
    fluidPage(
    fluidRow(
   box( dataTableOutput("plot1"))
    
    ),
    fluidRow(
      box(dataTableOutput("plot2"))
    )
    )
  })

  observeEvent(input$sidebarmenu,{
    maxturnaus <-max(peliDataReact()[,TurnausNo])
    updateNumericInput(session,"sarjataulukkokierros",value=maxturnaus)})


  
  

  # divaridata <- reactiveFileReader(2000, session, "./drop_download/divari.csv",luecsv)
  divaridata <- reactive({
    print("divaritada alku")
    tulos <- luecsv("./drop_download/divari.csv")
    print(paste(input$tallenna_bannit))
    print("divaritada loppu")
    return (tulos)
  })
  output$table_divari2<- renderUI({
    #montakodivaria
    print("montako divaria alku")
    divarit<-divaridata()
    divarit<-divarit[order(Divari)]
    eri_divarit<-unique(divarit[,Divari])

    fluidPage(
      lapply(eri_divarit,function(i)  {
        plotname2 <- paste0("plotdyndivari", i, sep="")

        fluidRow(box( dataTableOutput(plotname2),width=12,title=paste0("Divari: ",i),solidHeader = TRUE,status="primary" ))

      })
    )

  })

  
  output$divariRadio_out <- renderUI({
    divarit_ilman_peleja <- peliDataReact()[is.na(Voittaja),.N,by=Divari]
    radioButtons("divariRadio", "Divari",
                 c("Ei väliä",divarit_ilman_peleja[,Divari]),inline=TRUE)
  })

  

  
# saavutusUI<-
#   for(kierros in 0:20) {
#     local({
#       kierrosData<-saavutusTaulu()[kierros]
#       infoBoxName <- paste0("infoDynamic", kierros, sep="")
#       output[[infoBoxName]] <- renderInfoBox({
#         infoBox(
#           kierroData[,teksti], icon = icon("list"),
#           color = kierroData[,color], fill = TRUE
#         )
#       })
# 
#     })
#   }





  for (i in 0:10) {

    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste0("plotdyn", my_i, sep="")
      output[[plotname]] <- renderDataTable({
        Data_all<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,my_i,NA,NA,NA,input$radio_pfi_mode,pfi_data())$sarjataulukko
        Data<-Data_all
        return(Data)
        #print(Data)
      },    options = list(
        paging = FALSE,
        searching = FALSE,
        info=FALSE

        )
      )
      plotname_divari <- paste0("plotdyndivari", my_i, sep="")
      output[[plotname_divari]] <- renderDataTable({
        divarit<-divaridata()
        tempdata<-divarit[Picked==1,.(Omistaja=Omistaja_nimi,Nimi,Divari,Picked)]
        # Data<-tempdata[Divari==my_i]
        Data<-tempdata[Divari==my_i]
        return(Data)
        #print(Data)
      },    options = list(
        paging = FALSE,
        searching = FALSE,
        info=FALSE

      )
      )
    })
  }
  


  

  defaultStatValue<-reactiveValues(
   
  asetukset=list("Nimi","Vastustajan Nimi","Voitti",list(),"Average","Table")

  
  )
  
    
   #luo tilasto-asetus-objekti
   
   
   tilastoAsetuksetReact<-reactiveValues(
    
     data=tilastoAsetukset

   )
     observeEvent( input$tallennaTilastoAsetus,{

       #kato onko siellä dataa
       if(is.null(tilastoAsetuksetReact$data)){
         tilastoAsetukset<-data.table(
                                      datataulu=character(),
                                      kuvaus=character(),
                                      asetukset=list()
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
      #tarkista onko asetusnimi jo olemassa
      if(length(tilastoAsetuksetReact$data[kuvaus==input$text_tilastoKuvaus])>0){
        tilastoAsetuksetReact$data<-tilastoAsetuksetReact$data[kuvaus!=input$text_tilastoKuvaus]
      }
      tilastoAsetukset<-rbind(tilastoAsetuksetReact$data,uusrivi)

    #tallenna rdata

      saveR_and_send(tilastoAsetukset,"tilastoAsetukset","tilastoAsetukset.R")

tilastoAsetuksetReact$data<-tilastoAsetukset

   })
     



     
     observeEvent( input$myPivotData,{
       #ota edelliset asetukset talteen
       cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
       # Apply a function to all keys, to get corresponding values
       allvalues <- lapply(cnames, function(name) {
         item <- input$myPivotData[[name]]
         
       })
       defaultStatValue$asetukset<-allvalues
     })
     
     #voi käyttää debugissa, jos pistää UIsta päälle
   output$pivotRefresh <- renderText({
     
     cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
     # Apply a function to all keys, to get corresponding values
     allvalues <- lapply(cnames, function(name) {
       item <- input$myPivotData[[name]]
       if (is.list(item)) {
         list_to_string(item, name)
       } else {
         paste(name, item, sep=" = ")
       }
     })
     paste(allvalues, collapse = "\n")
   })

   
 
pfi_data<-reactive({
  print("TPALAT PAKAT")
  print("TPALAT PAKAT")
  pakat<-omaReadJson("./decks_unzipped/",input$file1)
  print("TPALAT PAKAT")
 # print(pakat)
  tulos<-pakkaUutuusProsentti(pakat)
  print(tulos)
  tulos
})  
    
anyFileUpload<-observe({
  req(input$anyfile)
  print(input$anyfile)
  drop_upload(input$anyfile$name, "mstat/csv/", mode = "overwrite", dtoken = token)
  
})

observe({
  print(paste("ifile"))
  ifile <-input$file1
 # omistaja <- substr(1,1,ifile$name)
  if (!is.null(ifile)) {
    validointiteksti$teksti<-process_uploaded_decks(ifile,".//")}
  zipAndSend()
  
  #varmaa vähän purkkaa, mutta päivitetään peliDataReact näin()

})
peliDataReact<-reactive({
  print("Luettu ./drop_download/pelit.csv")
print(paste(input$tallenna_tulos),input$luo_peleja)
  if(input$radio_debug_mode==FALSE) {
    kaikkipelit<-luecsv("./drop_download/pelit.csv")   
  } else {
    kaikkipelit<-luecsv("pelit_debug.csv")  
  }
   
  
})


saavutusAsetuksetReact<-reactiveValues(
  data=saavutusAsetukset
)


turnausSaantoReact<-reactive({
  print("luettu turnaussaanto.csv")
  turnaussaanto<-data.table(read.csv("turnaussaanto.csv",sep=";",fileEncoding="UTF-8-BOM"))
  return(turnaussaanto)
})

validointiteksti <-reactiveValues(teksti="Ei ladattu pakkoja")
output$text_validointi <- renderText(({
  paste(validointiteksti$teksti)
  }))
  

#osuus, joka katsoo mitä UI-palikkaa on viimeksi muokattu


values <- reactiveValues(
  lastUpdated = NULL
)

observe({
  
  lapply(names(input), function(x) {
    observe({
      input[[x]]
      values$lastUpdated <- x
    })
  })
})

observeEvent(input$action_add,{
  if(values$lastUpdated=="slider_laurin_humala" | values$lastUpdated == "slider_martin_humala") {
    steppi <- 0.1
  } else {
    steppi <- 1
  }
  updateSliderInput(session,values$lastUpdated,value=input[[values$lastUpdated]]+steppi)
})

observeEvent(input$action_reduce,{
  if(values$lastUpdated=="slider_laurin_humala" | values$lastUpdated == "slider_martin_humala") {
    steppi <- 0.1
  } else {
    steppi <- 1
  }
  updateSliderInput(session,values$lastUpdated,value=input[[values$lastUpdated]]-steppi)
})


})
