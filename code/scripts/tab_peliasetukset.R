output$peliAsetuksetUI<-renderUI({
  divarit<-divaridata()
  divarit<-divarit[Picked==1,.N,by=.(Divari)][order(Divari)]
  
  lapply(divarit[,Divari], function(i) {
    fluidRow(
      #column(3,textOutput(paste0("textPeliasetusDivari",i),paste0("Divari: ",i)))
      column(3,checkboxInput(paste0("checkbox_BO_mode",i),paste0("Best-of-mode päällä. Divari: ",i))),
      column(5,numericInput(paste0("numeric_rounds",i),paste0("Montako runkosarjakierrosta. Divari: ",i), value = 1)),
      column(4,numericInput(paste0("numeric_ottelut",i),paste0("Montako pelia per ottelu. Divari: ",i), value = 1))
    )
  })
  
})

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
      ottelukierros <- kaikki_ottelut[,.(Divari,
                                         Laurin_pakka,
                                         Martin_pakka,
                                         Kierros = i,
                                         Ottelu_ID = (Ottelu_ID + (i - 1) * ottelua_per_kierros),
                                         Ottelu_no,
                                         BO_mode)]
      kaikkipelit <- rbind(kaikkipelit, ottelukierros)
    }
    
    
    if(!is.finite(turnaus_no)) {turnaus_no <- 1}
    kaikkipelit[, TurnausNo := turnaus_no]
    
    #edellinen max peli_iD
    ed_peli_id <- max(vanhatpelit[, peli_ID])
    if(!is.finite(ed_peli_id)) {ed_peli_id <- 0}
    
    #aloittaja
    
    kaikkipelit[, idl := 1:.N, by = Laurin_pakka]
    kaikkipelit[, idm := 1:.N, by = Martin_pakka]
    kaikkipelit[, Aloittaja := (idl + idm + TurnausNo + Kierros + Divari + Ottelu_no) %% 2]
    
    kaikkipelit[, ':=' (peli_ID = .I + ed_peli_id,
                        idl = NULL,
                        idm = NULL, 
                        Voittaja = NA,
                        Aloitusaika = NA,
                        Aloituspvm = NA,
                        Lopetusaika = NA,
                        Lopetuspvm = NA,
                        Laurin_mulligan = NA,
                        Martin_mulligan = NA,
                        Laurin_arvosana = NA,
                        Martin_arvosana = NA,
                        Laurin_humala = NA,
                        Martin_humala = NA,
                        Laurin_landit = NA,
                        Martin_landit = NA,Vuoroarvio=NA,Laurin_kasikortit=NA,Martin_kasikortit=NA,Lauri_voitti=NA,Martti_voitti=NA,Laurin_lifet=NA,Martin_lifet=NA)]
    #arvosana: 1= pelasin hyvin, en keksi parannettavaa. 0= Hieman löysäilyä. -1= merkittävää hölmöilyä.
    #str(kaikkipelit)
    
    #tee tyhja taulu
    empty_dt<-data.table(kaikkipelit[1==0])
    
    vanhatpelit<-rbind(empty_dt,vanhatpelit)
    kaikkipelit<-rbind(vanhatpelit,kaikkipelit)
    
    kircsv(kaikkipelit,"./drop_download/pelit.csv", upload = TRUE)
  }
  #päivitä nappulastatukset
  
  shinyjs::disable("luo_peleja")
  shinyjs::enable("arvo_peli")
  
  print("luo pejelä loppu")
  
})