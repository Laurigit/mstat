output$peliAsetuksetUI<-renderUI({
  required_data("STG_DIVARI")
  divarit <- STG_DIVARI
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
  #print("luo pejelä alku")
  
  #divarit_dt<-luecsv("./drop_download/divari.csv")
  required_data(c("STG_DIVARI", "ADM_PELIT", "STG_PAKAT"))
  #tehään formaatti so+pivaksi pelit.csv:lle

  #loop jokasta divaria kohti
  #peliparit divarissa
  #loop jokasta kierrosta kohti
  #loop jokasta ottelua kohti
  divarit <- STG_DIVARI
  kaikkiDivarit <- divarit[order(Divari)][Picked == 1,
                                          .N,
                                          by = .(Pakka_ID, Omistaja_ID, Divari)][, N := NULL]
  kaikkiDivarit_vihu <- kaikkiDivarit[, .(Vastustajan_Pakka_ID = Pakka_ID,
                                          Vastustajan_Omistaja_ID = Omistaja_ID,
                                          Divari)]
  peliparit <- kaikkiDivarit[kaikkiDivarit_vihu, on = "Divari", allow.cartesian = TRUE]
  #ei pelata omia vastaan
  #tuloksena ois tuplakierros, joten otetaan vaan yks omistaja, niin sopii pelit.csv muotoon
  peliparit_vihua <- peliparit[Omistaja_ID != Vastustajan_Omistaja_ID & Omistaja_ID == "L"]
  peliparit_vihua[, dummy_key := "KEY"]
  divarit_loop <- peliparit_vihua[, .N, by = Divari][, Divari]
  output_pelit <- NULL
  for(divariKierros in divarit_loop) {
  #print(divariKierros)
    message("input$luo_peleja", divariKierros)
  
  kierroksia <- as.numeric(input[[paste0("numeric_rounds",divariKierros)]])
  peleja_per_ottelu <-as.numeric(input[[paste0("numeric_ottelut",divariKierros)]])
  input_BO_mode <- as.numeric(input[[paste0("checkbox_BO_mode",divariKierros)]])
  
  
  # kierroksia <-1
  # peleja_per_ottelu <-2
  # input_BO_mode <-1
  divaripeliParit <- peliparit_vihua[Divari == divariKierros]
  kierros_dt <- data.table(Kierros = 1:kierroksia, dummy_key = "KEY")
  peleja_per_ottelu_dt <- data.table(Ottelu_NO = 1:peleja_per_ottelu, dummy_key = "KEY")
  joini <- kierros_dt[peleja_per_ottelu_dt, on = "dummy_key", allow.cartesian = TRUE]
  joinipelit <- joini[divaripeliParit,  on = "dummy_key", allow.cartesian = TRUE][, dummy_key := NULL]

    output_Pelit_kierros <- joinipelit[, .(Divari,
                                 Laurin_pakka = Pakka_ID,
                                 Martin_pakka = Vastustajan_Pakka_ID,
                                 Kierros,
                             
                                 Ottelu_no = Ottelu_NO,
                                 BO_mode = input_BO_mode,
                                
                                
                               
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
                                Martin_landit = NA,
                                Vuoroarvio = NA,
                                Laurin_kasikortit = NA,
                                Martin_kasikortit = NA,
                                Lauri_voitti = NA,
                                Martti_voitti = NA,
                                Laurin_lifet = NA,
                                Martin_lifet = NA,
                                Aloitus_DT = NA,
                                Lopetus_DT = NA)]
  output_pelit <- rbind(output_pelit, output_Pelit_kierros)
  }
  vanha_max_peli_ID <- ADM_PELIT[, max(Peli_ID)]
  if(!is.finite(vanha_max_peli_ID)) {vanha_max_peli_ID<-0}
  
  #eti edellinen max ottelu_id
  vanha_max_Ottelu_ID <- max(ADM_PELIT[,Ottelu_ID])
  if(!is.finite(vanha_max_Ottelu_ID)) {vanha_max_Ottelu_ID<-0}
  setorder(output_pelit, Kierros,  Laurin_pakka, Martin_pakka, Ottelu_no)
  #tee Ottelu_ID
  output_pelit[, ':=' (Ottelu_ID = vanha_max_Ottelu_ID + rleid(Kierros, Laurin_pakka, Martin_pakka))]
  
  
  #tee Peli_ID ja TUrnaus_NO
  uus_turnaus <- max(ADM_PELIT[,Turnaus_NO]) + 1
  output_pelit[, ':=' (peli_ID = vanha_max_peli_ID + .I,
                     TurnausNo = uus_turnaus)]
  
  #aloittaja
  
  output_pelit[, idl := 1:.N, by = Laurin_pakka]
  output_pelit[, idm := 1:.N, by = Martin_pakka]
  output_pelit[, Aloittaja := (idl + idm + TurnausNo + Kierros + Divari + Ottelu_no) %% 2]
  output_pelit[, ':=' (idl = NULL, idm = NULL)]
  #convert Pakka_ID to Pakka_NO
  sscols_lauri <- STG_PAKAT[, .(Laurin_pakka = Pakka_ID,
                         Laurin_pakka_old_format = Pakka_NO)]
  sscols_martti <- STG_PAKAT[, .(Martin_pakka = Pakka_ID,
                                Martin_pakka_old_format = Pakka_NO)]
  joinL <- sscols_lauri[output_pelit, on = .(Laurin_pakka)]
  joinM <- sscols_martti[joinL, on = .(Martin_pakka)]
  joinM[, ':=' (Laurin_pakka = NULL,
                Martin_pakka = NULL)]
  setnames(joinM,
           c("Laurin_pakka_old_format", "Martin_pakka_old_format"),
           c("Laurin_pakka", "Martin_pakka")
           )
  
  vanahCSV <- luecsv("pelit.csv")

  kaikkipelit <- rbind(vanahCSV, joinM)

  kircsv(kaikkipelit,"pelit.csv", upload = TRUE)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_PELIT", ADM_DI_HIERARKIA, input_env = globalenv(), rewriteSaveR = TRUE)
  refresh_counter$a <-   refresh_counter$a + 1
  shinyjs::disable("luo_peleja")
  shinyjs::enable("arvo_peli")
  
  #print("luo pejelä loppu")
  
})
