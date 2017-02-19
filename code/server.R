source("init.R")





# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  
  
  #r_valittu_peli on valittu peli millä tahansa menetelmällä
  r_valittu_peli <-reactiveValues(peliID=1,jatkopeli=NA, aloittajatext="Ladataan")
  
    
  #obserEventit
  
  #luo uusi turnaus
  observeEvent(input$luo_peleja,{
   
    divarit_dt<-luecsv("divari.csv")
    
    pelit_list <- divarit_dt[Picked==1,.(pakkalista=list(Pakka)),by=.(Divari,Omistaja)]
    pelit<-pelit_list[,expand.grid(pakkalista),by=Divari]
    setnames(pelit,c("Var1","Var2"),c("Laurin_pakka","Martin_pakka"))
    
    
    #lue edellinen turnausnumero
    vanhatpelit <-luecsv("pelit.csv")
    #eti edellinen max ottelu_id
    ed_ottelu_id_max<-max(vanhatpelit[,Ottelu_ID])
    
    #tarvitaan vain, kun ajetaan manuaalisesti eka kerta
    #kierroksia<-2
    #BO_mode<-FALSE
    #montako peliä on yhdessä ottelussa
    otteluita<-input$numeric_ottelut
    #montako ottelua on turnauksessa pakkojen välillä
    kierroksia <- input$numeric_rounds
    BO_mode<-as.numeric(input$checkbox_BO_mode)
    pelit[,Ottelu_ID:=.I+ed_ottelu_id_max]
    kaikki_ottelut<-NULL
    
    #lisää ottelut
    for (i in 1:otteluita){
      pelikierros <- pelit[,.(Divari,Laurin_pakka,Martin_pakka,Ottelu_No=i,Ottelu_ID,BO_mode)]
      kaikki_ottelut<-rbind(kaikki_ottelut,pelikierros)
    }
    #motanko ottelua per kierros?
    ottelua_per_kierros<-nrow(kaikki_ottelut)/otteluita
    
    #lisää kierrokset
    kaikkipelit<-NULL
    for (i in 1:kierroksia){
      ottelukierros <- kaikki_ottelut[,.(Divari,Laurin_pakka,Martin_pakka,Kierros=i,Ottelu_ID=(Ottelu_ID+(i-1)*ottelua_per_kierros),Ottelu_No,BO_mode)]
      kaikkipelit<-rbind(kaikkipelit,ottelukierros)
    }
      
      #TurnausNo<-1
      turnaus_no<-max(vanhatpelit[,TurnausNo])+1
      kaikkipelit[,TurnausNo:=turnaus_no]
      
      #edellinen max peli_iD
      ed_peli_id<-max(vanhatpelit[,peli_ID])
  
      
      #aloittaja
      
      kaikkipelit[, idl := 1:.N, by = Laurin_pakka]
      kaikkipelit[, idm := 1:.N, by = Martin_pakka]
      kaikkipelit[,Aloittaja:=(idl+idm+TurnausNo+Kierros+Divari+Ottelu_No)%%2]
      
      kaikkipelit[,':='(peli_ID=.I+ed_peli_id,idl=NULL,idm=NULL,Voittaja=NA,Aloitusaika=NA,Aloituspvm=NA,Lopetusaika=NA,Lopetuspvm=NA,Laurin_mulligan=NA,Martin_mulligan=NA,Laurin_arvosana=NA,Martin_arvosana=NA,Laurin_humala=NA,Martin_humala=NA,Laurin_landit=NA,Martin_landit=NA,Vuoroarvio=NA,Laurin_kasikortit=NA,Martin_kasikortit=NA,Lauri_voitti=NA,Martti_voitti=NA,Laurin_lifet=NA,Martin_lifet=NA)]
      #arvosana: 1= pelasin hyvin, en keksi parannettavaa. 0= Hieman löysäilyä. -1= merkittävää hölmöilyä.
      #str(kaikkipelit)
      #kircsv(kaikkipelit,"kierroksen_pelit.csv")
      #tee tyhja taulu
      empty_dt<-data.table(kaikkipelit[1==0])
      
      vanhatpelit<-rbind(empty_dt,vanhatpelit)
  
      #print(vanhatpelit)
      #lisää uudet
      kaikkipelit<-rbind(vanhatpelit,kaikkipelit)
      print(kaikkipelit)
      kircsv(kaikkipelit,"pelit.csv")
      
      #päivitä nappulastatukset

        shinyjs::disable("luo_peleja")
        shinyjs::enable("arvo_peli")
   
  })
      

  
  #arvopeli
  observeEvent(input$arvo_peli,{
    kaikkipelit<-luecsv("pelit.csv")
    pelaamattomat <- unique(kaikkipelit[is.na(Voittaja),Ottelu_ID])
    arpa<-ceiling(runif(1,0,length(pelaamattomat)))
    arvottu_ottelu_ID<-pelaamattomat[arpa]
    #eti ottelun pienin pelaamaton peli
   
    arvottu_peli_id <- kaikkipelit[Ottelu_ID==arvottu_ottelu_ID & is.na(Voittaja) , .SD[which.min(Kierros)],.SDcols=c("peli_ID")][,peli_ID]
    paivitaSliderit(arvottu_peli_id,session)

  
  })
  
    

    #jatka ottelua
    
    observeEvent(input$jatka_ottelua,{

      if (!is.na(r_valittu_peli$jatkopeli)) {
    #  
        paivitaSliderit(r_valittu_peli$jatkopeli,session) 
  
      }else {
        print("Ei ole peliä kesken")
      }
    })
   #seuraa selectinputlistoja
    
        #tallennapeli
  observeEvent(input$tallenna_tulos,{
     uusrivi<- c(
       Aloitusaika=alotusaika$alotusaika,
       Aloituspvm=alotusaika$alotuspvm,
       Lopetusaika=as.ITime(now()),
       Lopetuspvm=as.IDate(now()),
       Voittaja=as.numeric(input$radio_voittaja),
       Lauri_voitti=(1-as.numeric(input$radio_voittaja)),
       Martti_voitti=as.numeric(input$radio_voittaja),
     
       Laurin_mulligan=input$laurin_mulligan,
       Martin_mulligan=input$martin_mulligan,
       Laurin_arvosana=input$slider_laurin_virhe,
       Martin_arvosana=input$slider_martin_virhe,
       Laurin_humala=input$slider_laurin_humala,
       Martin_humala=input$slider_martin_humala,
       Laurin_landit=input$slider_laurin_landit,
       Martin_landit=input$slider_martin_landit,
       Laurin_lifet=input$slider_laurin_lifet,
       Martin_lifet=input$slider_martin_lifet,
       Vuoroarvio=input$slider_vuoroarvio,
       Laurin_kasikortit=input$slider_laurin_kasikortit,
       Martin_kasikortit=input$slider_martin_kasikorit
       )
     
   
     
     kaikkipelit<-data.table(luecsv("pelit.csv"))
    
     cols<-names(kaikkipelit)
     kaikkipelit[, (cols):= lapply(.SD, as.numeric), .SDcols=cols]
    
     kaikkipelit[peli_ID==r_valittu_peli$peliID, names(uusrivi) := as.list(uusrivi)][]
    
     
    #laske valmiiksi mahdollinen jatkopeli

     
     kaikkipelit[,otteluLKM:=as.double(.N),by=Ottelu_ID]

     kaikkipelit[,pelatut:=as.double(sum(ifelse(!is.na(Voittaja),1,0))),by=Ottelu_ID]

     #prosentti sitten
     kaikkipelit[,peliprosentti:=pelatut/otteluLKM]
     #palauta pienin keskeneräinen ottelu
     keskenpeli<-min(kaikkipelit[is.na(Voittaja) & peliprosentti>0 & peliprosentti <1,peli_ID])
     if (is.finite(keskenpeli)) {
       
       r_valittu_peli$jatkopeli<-keskenpeli 
       shinyjs::enable("jatka_ottelua") 
     }else {
       r_valittu_peli$jatkopeli<-NA
       shinyjs::disable("jatka_ottelua") 
       #print("Ei ole peliä kesken")
       
     }
  
     
     
    #jos bo_mode on päällä, niin tuhoa ylijäämäpelit
       #laske otteluiden voittoprosentti
       kaikkipelit[,':=' (MaxVP=pmax(sum(Lauri_voitti,na.rm=TRUE)/.N,sum(Martti_voitti,na.rm=TRUE)/.N)),by=Ottelu_ID]
       kaikkipelit[,MaxVP:=ifelse(is.na(MaxVP),0,MaxVP)]
       #jätä rivit, joiden MaxVP<0.5 tai rivillä on voittaja tai BO_mode on pois päältä
       pelit_jaljella <- kaikkipelit[(!is.na(Voittaja)|MaxVP<0.5)|BO_mode==0]
       pelit_jaljella[,':='(MaxVP=NULL,otteluLKM=NULL,pelatut=NULL,peliprosentti=NULL)]
      
     kircsv(pelit_jaljella,"pelit.csv")
     updateTabItems(session,"sidebarmenu","tab_uusi_peli")
     
  
   #jos pelejä jäljellä disabloi uusien pelien luominen
     peleja_jaljella <- pelit_jaljella[is.na(Voittaja),.N]
     if (peleja_jaljella>0) {
       shinyjs::disable("luo_peleja")
       shinyjs::enable("tasuri_peli")
       shinyjs::enable("arvo_peli")
     } else {
       shinyjs::enable("luo_peleja")
       shinyjs::disable("arvo_peli")
       shinyjs::disable("tasuri_peli")
       
     }
     
     updateNumericInput(session,"sarjataulukkokierros",value=0)
    })  
    
    
    
  
    
   
     
#observe- seuraa muuttujien arvoja
  #pelin aloitusaika ja lopetus
alotusaika<-reactiveValues()
 observe({
    test<-r_valittu_peli$peliID+input$tasuri_peli+input$arvo_peli+input$jatka_ottelua #kun joku näistä päivttyy, niin nollaa aika

    alotusaika$alotusaika<-as.ITime(now())
    alotusaika$alotuspvm<-as.IDate(now())
})

 

  #seuraa valintalistoja seka tallennusta ja paivita UI + tiedot sen mukaan.
  observe({
    #seuraa tallenna buttonia myös 
    print(paste("tallennatulosarvo",input$tallenna_tulos))
    
    kaikkipelit<-luecsv("pelit.csv")
    print(paste("Laurin pakka: ",input$select_laurin_pakka))
    print(paste("maxvarotus:: ",max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])))
    maxottelu<-max(kaikkipelit[Laurin_pakka==input$select_laurin_pakka & Martin_pakka==input$select_martin_pakka,Ottelu_ID])

    
    
    r_valittu_peli$ottelutilanne_text <- kaikkipelit[Ottelu_ID==maxottelu,paste("Tilanne: ",sum(Lauri_voitti,na.rm=TRUE),"-",sum(Martti_voitti,na.rm=TRUE))]
    
    #kato onko peleja jaljella
    temp_peli<-min(kaikkipelit[Ottelu_ID==maxottelu & is.na(Voittaja) ,peli_ID])
    
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
      print("observeluettu FALSE") 
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
    
    
  })
  observeEvent(input$laurin_mulligan,{
    updateSliderInput(session, "slider_laurin_mulligan", value = input$slider_laurin_mulligan+1)
  })
  observeEvent(input$martin_mulligan,{
    updateSliderInput(session, "slider_martin_mulligan", value = input$slider_martin_mulligan+1)
  })
  observeEvent(input$laurin_virhe,{
    updateSliderInput(session, "slider_laurin_virhe", value = input$slider_laurin_virhe-1)
  })
  
  observeEvent(input$martin_virhe,{
    updateSliderInput(session, "slider_martin_virhe", value = input$slider_martin_virhe-1)
  })
  
  observeEvent(input$lauri_voitti,{
    kaikkipelit<-data.table(luecsv("pelit.csv"))
    #tarkista onko peli pelattu
    if(!is.na(kaikkipelit[peli_ID==  r_valittu_peli$peliID,Voittaja])){
      print("peli on jo pelattu")
    } else {
      
      updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
      updateRadioButtons(session,"radio_voittaja",selected=0)
    }
  })
  
  observeEvent(input$martti_voitti,{
    updateTabItems(session,"sidebarmenu","tab_tallenna_peli")
    updateRadioButtons(session,"radio_voittaja",selected=1)
  })
  
  
  observeEvent(input$slider_vuoroarvio,{
    updateSliderInput(session, "slider_martin_landit", value = input$slider_vuoroarvio)
    updateSliderInput(session, "slider_laurin_landit", value = input$slider_vuoroarvio)
  })

  #päivitä divarit
  observeEvent(input$ tallenna_divarit,{
    
    divarit<-luecsv("divari.csv")
    
    
    divarit[,syntax:=(text=paste0(Pakka,Omistaja))]
    
    lapply(divarit[,syntax],function(i) {
      divarit[syntax==i,Divari:=input[[i]]]
      
    })
    divarit[,syntax:=NULL]
    print(divarit)
    kircsv(divarit,"divari.csv")
    #divaridata<-divarit
  })
  
  #paivitä bannit
  observeEvent(input$ tallenna_bannit,{
    
    divarit<-luecsv("divari.csv")
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
  kircsv(divarit,"divari.csv")
    
    
  })

  #Serveripuolella tehdyt UI-palikat.
  output$text_aloittaja <- renderText(({paste(r_valittu_peli$aloittaja_text)}))
  output$text_tilanne <- renderText(({paste(r_valittu_peli$ottelutilanne_text)}))
  
  #tee laurin pakka selectinput
  output$selectInputLauri <- renderUI({
    pakat<-luecsv("divari.csv")
    laurin_pakkanimet<-pakat[Omistaja==1,Nimi]
    laurin_idt<-pakat[Omistaja==1,Pakka]
    selectinputListLauri<-setNames(as.list(laurin_idt), c(laurin_pakkanimet))
    selectInput("select_laurin_pakka","Laurin pakka",choices = selectinputListLauri)
    
  })
  #tee martin pakka selectinput
  output$selectInputMartti <- renderUI({
    pakat<-luecsv("divari.csv")
    pakkanimet<-pakat[Omistaja==2,Nimi]
    martin_idt<-pakat[Omistaja==2,Pakka]
    selectinputList<-setNames(as.list(martin_idt), c(pakkanimet))
    selectInput("select_martin_pakka","Martin pakka",choices = selectinputList)
    
  })
  #divaricheckbox
  output$checkboxPakat<-renderUI({
    divarit<-luecsv("divari.csv")
    lapply(divarit[,rivi_id], function(i) {
      
      checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])
   
    })
  })
  
  #divariNumericinput
  output$combUI<-renderUI({
    divarit<-luecsv("divari.csv")
    
    lapply(divarit[,rivi_id], function(i) {
      fluidRow(
        column(3,  numericInput(paste0( divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]), label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Divari])),
        column(3,h4("Divari/picked")),
        column(3, checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])))
      
    
    })
    
  })

  #output$testiteksti<-renderText({input$sidebarmenu})

output$sarjataulukkovalitsin <- renderUI({
  kaikkipelit<-luecsv("pelit.csv")
  maxturnaus<-max(kaikkipelit[,TurnausNo])
  fluidRow(numericInput("sarjataulukkokierros","Valitse turnauksen numero",value=maxturnaus))
})
  
  
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
    kaikkipelit<-luecsv("pelit.csv")
    maxturnaus <-max(kaikkipelit[,TurnausNo])
    updateNumericInput(session,"sarjataulukkokierros",value=maxturnaus)})
  
  pelidata <- reactiveFileReader(1000, session, "pelit.csv",luecsv)
  output$sarjataulukot <-renderUI({
    #montakodivaria
    divarit<-sarjataulukkoKaikki(input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode)$divarit
    fluidPage(
      lapply(divarit,function(i)  {
        plotname <- paste0("plotdyn", i, sep="")

        fluidRow(box( dataTableOutput(plotname),width=12,title=paste0("Divari: ",i),solidHeader = TRUE,status="primary" ))
       
      })
    )
  })
  

  divaridata <- reactiveFileReader(1000, session, "divari.csv",luecsv)
  output$table_divari2<- renderUI({
    #montakodivaria

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


  
    
  for (i in 0:10) {

    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste0("plotdyn", my_i, sep="")

      output[[plotname]] <- renderDataTable({

        
        Data_all<-sarjataulukkoKaikki(input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,my_i,NA,NA,NA)$sarjataulukko
       
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

  output$data_vs_taulukko<-renderDataTable({
    
    vs_statsit_MA<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,input$numeric_MA_valinta)$transposed[(Tilasto %in% ("Voitot"))]

    vs_statsit_all<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,NA)

    pakka_stats_all_lauri<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,input$select_laurin_pakka,NA,NA)$transposed
    pakka_stats_all_martti<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,NA,input$select_martin_pakka,NA)$transposed

    setkeyv(pakka_stats_all_lauri,c("Tilasto","selite"))
    setkeyv(pakka_stats_all_martti,c("Tilasto","selite"))   
    join_pakka_stats_all<-pakka_stats_all_lauri[pakka_stats_all_martti]
    
    #MA_pakak
    pakka_stats_MA_lauri<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,input$select_laurin_pakka,NA,input$numeric_MA_valinta)$transposed[(Tilasto %in% ("Voitot"))]
    pakka_stats_MA_martti<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,NA,input$select_martin_pakka,input$numeric_MA_valinta)$transposed[(Tilasto %in% ("Voitot"))]
    
    
    pfistats<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,NA,NA,NA)$pfi_trans
    
    #ota vaan sarakkeet, mitä on muuallakkin käytetty
    pfi_subsetcols<-pfistats[,names(vs_statsit_all$transposed),with=FALSE]
    

    setkeyv(pakka_stats_MA_lauri,c("Tilasto","selite"))
    setkeyv(pakka_stats_MA_martti,c("Tilasto","selite"))   
    join_pakka_stats_MA<-pakka_stats_MA_lauri[pakka_stats_MA_martti]
    
    
    append<-rbind(vs_statsit_all$transposed,vs_statsit_MA,join_pakka_stats_all,join_pakka_stats_MA,pfi_subsetcols)#,laurin_MA$transposed)
    #vaihda sarakejärjestys
    result_table<-append[,c(3,1,2,4),with=FALSE]
    
    return(result_table)  
     
  },    options = list(
    paging = FALSE,
    searching = FALSE,
    info=FALSE,
    columnDefs = list(list(className = 'dt-center', targets = 1:2),
                      list(className = 'dt-left', targets = 3))
    
  ),rownames=FALSE)
  
#valueboksit
 


  output$paras_countteri<-renderValueBox({
    
    pelatut_parit<-luecsv("Pelit.csv")[!is.na(Voittaja),.N,by=.(Laurin_pakka,Martin_pakka)]
    #looppaa parit läpi ja eti paras voitto%
    pelatut_parit[,Voitto_pct:=sarjataulukkoKaikki(FALSE,1,TRUE,NA,Laurin_pakka,Martin_pakka)$laurin_voitto_pct,by=.(Laurin_pakka,Martin_pakka)]
    pelatut_parit[,vertailu:=abs(Voitto_pct-1)]
    #laurin paras countteri
    laurin_counter<-pelatut_parit[, .SD[which.max(Voitto_pct)]]
    pakat<-luecsv("Divari.csv")
    martin_counter <- pelatut_parit[, .SD[which.max(vertailu)]]
    
    if (laurin_counter[,Voitto_pct]>martin_counter[,vertailu]) {
      boksiteksti<-paste0(pakat[Omistaja==1 & Pakka == laurin_counter[,Laurin_pakka],Nimi], " voitto% VS ",pakat[Omistaja==2 & Pakka == laurin_counter[,Martin_pakka],Nimi])
      boksiarvo <-paste0(laurin_counter[,Voitto_pct*100],"%")
      boksivari="purple"
    } else {
      boksiteksti<-paste0(pakat[Omistaja==2 & Pakka == martin_counter[,Martin_pakka],Nimi], " voitto% VS ",pakat[Omistaja==1 & Pakka == martin_counter[,Laurin_pakka],Nimi])
      boksiarvo <-paste0(martin_counter[,vertailu*100],"%")
      boksivari<-"orange"    
      
      }
    valueBox(boksiarvo, boksiteksti, icon = icon("list"),
             color = boksivari)
    
  })
  
  output$vaikein_counteroitava<-renderValueBox({
    
    pelatut_parit<-luecsv("Pelit.csv")[!is.na(Voittaja),.N,by=.(Laurin_pakka,Martin_pakka)]
    #looppaa parit läpi ja eti paras voitto%
    pelatut_parit[,Voitto_pct:=sarjataulukkoKaikki(FALSE,1,TRUE,NA,Laurin_pakka,Martin_pakka)$laurin_voitto_pct,by=.(Laurin_pakka,Martin_pakka)]
    pelatut_parit[,vertailu:=abs(Voitto_pct-1)]
    #laurin paras countteri
    laurin_counter<-pelatut_parit[, .(maxvertailu=max(vertailu)),by=Laurin_pakka]
    laurin_countteroimaton_pakka<-laurin_counter[,.SD[which.min(maxvertailu)]]
    
    pakat<-luecsv("Divari.csv")
    martin_counter <- pelatut_parit[, .(maxvertailu=max(Voitto_pct)),by=Martin_pakka]
    martin_countteroimaton_pakka<-martin_counter[,.SD[which.min(maxvertailu)]]
    
    
    
    if (laurin_countteroimaton_pakka[,maxvertailu]>martin_countteroimaton_pakka[,maxvertailu]) {
      boksiteksti<-paste0(pakat[Omistaja==1 & Pakka == laurin_countteroimaton_pakka[,Laurin_pakka],Nimi], " voittaa pahimman counterpakan %")
      boksiarvo <-paste0(round(laurin_countteroimaton_pakka[,maxvertailu*100],0),"%")
      boksivari="purple"
    } else {
      boksiteksti<-paste0(pakat[Omistaja==2 & Pakka == martin_countteroimaton_pakka[,Martin_pakka],Nimi], " voittaa pahimman counterpakan %")
      boksiarvo <-paste0(round(martin_countteroimaton_pakka[,maxvertailu*100],0),"%")
      boksivari<-"orange"    
      
    }
    valueBox(boksiarvo, boksiteksti, icon = icon("list"),
             color = boksivari)
    
  })
  
  
  output$vb_voittoputki<-renderValueBox({
    putki<-sarjataulukkoKaikki(FALSE,1,TRUE,NA,NA,NA,NA)$ison_putki
    
    valueBox(paste0(putki[,Putki]), paste0("Pisin voittoputki: ",putki[,Nimi]), icon = icon("list"),
             color = "purple")
    
  })
 
observe({
  ifile <-input$file1

 # omistaja <- substr(1,1,ifile$name)
  if (!is.null(ifile)) {
    validointiteksti$teksti<-process_uploaded_decks(ifile,"C:/Users/Lauri/Documents/R/mstat2/pakat/processed/")}

})

validointiteksti <-reactiveValues(teksti="Ei ladattu pakkoja")
output$text_validointi <- renderText(({
  paste(validointiteksti$teksti)
  }))
  
})


