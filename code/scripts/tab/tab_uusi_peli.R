
eR_UID_temp_data_storage <- reactive({

  eR_Peli_ID()
  input$select_laurin_pakka
  input$select_martin_pakka
  input$slider_laurin_mulligan
  input$slider_martin_mulligan
  required_data("ADM_TEMP_DATA_STORAGE")
  return(ADM_TEMP_DATA_STORAGE)
})

observeEvent(c(input$select_laurin_pakka,
               input$select_martin_pakka,
               input$slider_laurin_mulligan,
               input$slider_martin_mulligan), {
                 
     req(input$select_laurin_pakka,
           input$select_martin_pakka,
           input$slider_laurin_mulligan,
           input$slider_martin_mulligan)
#1. päivitä uudet arvot
                 # laurin_pakka<-1
                 # martin_pakka<-10
                 # laurin_mull<-0
                 # martin_mull<-1
      Aloitus_DT <- now(tz = "EET")
      laurin_pakka<-input$select_laurin_pakka
      martin_pakka<-input$select_martin_pakka
      laurin_mull<-input$slider_laurin_mulligan
      martin_mull<-input$slider_martin_mulligan

      muuttujat<-c("Laurin_pakka",
                   "Martin_pakka",
                   "Aloitus_DT",
                   "Laurin_mulligan",
                   "Martin_mulligan")
      arvot<-c(laurin_pakka,
               martin_pakka,
               as.character(Aloitus_DT),
               laurin_mull,
               martin_mull)
      tempData <- data.table(muuttuja = muuttujat, arvo = arvot)
      #jos mikään ei muutu, niin älä lähetä
      required_data("ADM_TEMP_DATA_STORAGE")
      ssColsVanhat <- ADM_TEMP_DATA_STORAGE[!muuttuja == "Aloitus_DT"]


      ssColsUudet <- tempData[!muuttuja == "Aloitus_DT"]

      comparison <- all.equal(ssColsVanhat, ssColsUudet)
    
      if(!(comparison == TRUE)) {

      kircsv(tempData,"temp_data_storage.csv", upload = FALSE)
   
      required_data("ADM_DI_HIERARKIA")

      updateData("SRC_TEMP_DATA_STORAGE", ADM_DI_HIERARKIA, input_env = globalenv())

      print("observeEvent(c(input$select_laurin_pakka,
               input$select_martin_pakka,
               input$slider_laurin_mulligan,
               input$slider_martin_mulligan), {")
      #nollaa tempdatalaskuri
      tempDataLehtysLaskuri$a <- 0
      }
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


#arvopeli
observeEvent(input$arvo_peli,{
#input$divariRadio <- 1
  required_data("ADM_PELIT")
  arvottu_peli_id <- getRandomPeli_ID(ADM_PELIT, input$divariRadio)
  paivitaSliderit(arvottu_peli_id,session)
})


eR_Peli_ID <- eventReactive(c(input$select_laurin_pakka,
                              input$select_martin_pakka,
                              input$tallenna_tulos,
                              input$luo_peleja),{

if (!is.null(input$select_laurin_pakka) & !is.null(input$select_martin_pakka)) {

  # input$select_laurin_pakka <- 1
  # input$select_martin_pakka <-9    
  required_functions("getUusi_Peli_ID")
  required_data(c("ADM_PELIT"))
                               
  normiToiminto <-getUusi_Peli_ID(ADM_PELIT,
                  input$select_laurin_pakka,
                   input$select_martin_pakka)
  

  return(normiToiminto)
} else {
 
  required_data(c("ADM_PELIT", "ADM_TEMP_DATA_STORAGE", "STG_PAKAT"))
  pakat<-STG_PAKAT[Omistaja_ID == "M"]
  keskenPeliData<- ADM_TEMP_DATA_STORAGE
  P1 <- keskenPeliData[muuttuja == "Laurin_pakka", arvo]
  P2 <- keskenPeliData[muuttuja == "Martin_pakka", arvo]
  alkuLataus <- getPeli_ID_from_pakat(P1, P2, ADM_PELIT)

  return(alkuLataus)
}
}, ignoreInit = FALSE, ignoreNULL = FALSE)

eR_Peli_Aloittaja <- reactiveValues(a = -1, b = -4)
observe({
required_data("ADM_PELIT")
  eR_Peli_Aloittaja$a <- ADM_PELIT[Peli_ID == eR_Peli_ID() & Omistaja_ID =="M", Aloittaja]
  message("  eR_Peli_Aloittaja$a ",  eR_Peli_Aloittaja$a )
  #0 = Lauri, 1 = martti
} )


eR_UID_UUSI_PELI <- reactive({
  
  # input$numeric_MA_valinta <- 7
  # input$radio_bo_mode<- FALSE
  # input$radio_pfi_mode <- FALSE
  #create dependency 
  print("eR_UID_UUSI_PELI")
  print(eR_Peli_ID())#dont del me
  required_data(c("ADM_PELIT", "INT_PFI", "STG_PAKAT", "STG_OMISTAJA", "STAT_VOITTOENNUSTE"))
required_functions("UID_UUSI_PELI")
  tulos <- isolate(UID_UUSI_PELI(eR_Peli_ID(),
                         eR_UID_PAKKA(),
                         eR_UID_PAKKA_VS(),
                         STG_PAKAT,
                         STG_OMISTAJA,
                         ADM_PELIT,
                         STAT_VOITTOENNUSTE,
                         input$slider_laurin_mulligan,
                         input$slider_martin_mulligan
                        ))
  return(tulos)
})

eR_UID_PAKKA <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
required_functions("UID_PAKKA")
required_data(c("ADM_PELIT", "INT_PFI"))                                  
result <-  UID_PAKKA(ADM_PELIT,
                                                        INT_PFI,
                                                        input_MA_length = input$numeric_MA_valinta,
                                                        input_BO_mode  = input$radio_bo_mode,
                                                        input_pfi_mode = input$radio_pfi_mode)
return(result)
})

eR_UID_PAKKA_VS <- eventReactive(c(input$numeric_MA_valinta,
                                input$radio_bo_mode,
                                input$radio_pfi_mode),{
                                  # input$numeric_MA_valinta <- 7
                                  # input$radio_bo_mode<- FALSE
                                  # input$radio_pfi_mode <- FALSE
                                  required_functions("UID_PAKKA_VS")
                                  result <-  UID_PAKKA_VS(ADM_PELIT,
                                                       INT_PFI,
                                                       input_MA_length = input$numeric_MA_valinta,
                                                       input_BO_mode  = input$radio_bo_mode,
                                                       input_pfi_mode = input$radio_pfi_mode)
                                  return(result)
 })

eR_UID_TURNAUS_EV <- eventReactive(input$tallenna_tulos, {
  required_data(c("STAT_VOITTOENNUSTE", "ADM_PELIT"))
  results <- UID_TURNAUS_EV(ADM_PELIT, STAT_VOITTOENNUSTE)
  return(results)
}, ignoreNULL = FALSE, ignoreInit = FALSE)

eV_UID_MALLI_KOMPONENTIT <- reactive( {
  required_data("STAT_VOITTOENNUSTE")
  tulos <- UID_MALLI_KOMPONENTIT(STAT_VOITTOENNUSTE,
                                 eR_Peli_ID())
  return(tulos)
})



observeEvent(input$tasuriPeli, {
required_data(c("ADM_PELIT", "STAT_VOITTOENNUSTE"))
  uusPeliID <- getTasuriPeli(ADM_PELIT, STAT_VOITTOENNUSTE)
  paivitaSliderit(uusPeliID, session) 
})




tempDataLehtysLaskuri <- reactiveValues(a = 0)



output$PakkaLeftBox <- renderUI({
  # eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
  
  result <- getDeckStats("Lauri", eR_UID_UUSI_PELI())
  # result(eR_UID_UUSI_PELI())
  box(HTML(result), background = "purple", width = NULL)
  
})
output$PakkaRightBox <- renderUI({
  
  result <- getDeckStats("Martti", eR_UID_UUSI_PELI())
  box(HTML(result), background = "yellow", width = NULL)
  
})


output$peliKesto <- renderText({
  # required_data("ADM_TEMP_DATA_STORAGE")
  #tempData <- ADM_TEMP_DATA_STORAGE
  tempData <- eR_UID_temp_data_storage()
  invalidateLater(1000, session)
  pelialkuAika <- tempData[muuttuja == "Aloitus_DT", as.POSIXct(arvo, tz = "EET")]
  aikaNyt <- now(tz = "EET")
  sekunnit_yht<- as.integer(difftime(aikaNyt, pelialkuAika, units = c("secs")))
  minuutit_yht<-floor(sekunnit_yht/60)
  sekunnit<-sekunnit_yht-60*minuutit_yht
  tunnit <- floor(minuutit_yht / 60)
  minuutit <- minuutit_yht - 60 * tunnit
  sekunnit_fix <- str_pad(sekunnit, 2, pad = "0")
  minuutit_fix <- str_pad(minuutit, 2, pad = "0")
  tunnit_text <- ifelse(tunnit > 0,
                        paste0(str_pad(tunnit, 2, pad = "0"),":"),
                        "")
  
  isolate(tempDataLehtysLaskuri$a <- tempDataLehtysLaskuri$a + 1)
  if ( tempDataLehtysLaskuri$a == 10) {
    zip_all_and_send()
  }
  
  paste0(tunnit_text, minuutit_fix,":",sekunnit_fix)
  
  
})


output$EV_plot <- renderPlot({
  
  melttaa_aggr <-  eR_UID_TURNAUS_EV()
  plot <-ggplot(melttaa_aggr, aes(x = ottelu_id, y = Martin_johto, colour = variable)) + geom_line(size = 1.5) +
    theme_calc() + scale_color_calc() 
  plot+ theme(legend.title=element_blank(),
              legend.position = c(0.12, 0.1),
              legend.background = element_rect(color = "black",
                                               fill = "transparent", size = 1, linetype = "solid")) 
  
})

output$win_distribution <- renderPlot({
  melttaa <- eV_UID_MALLI_KOMPONENTIT()
  graphs_breaks <- melttaa[, Turnaus_NO]
  plot <- ggplot(melttaa, aes(x = (Turnaus_NO), y = Martin_etu, colour = variable)) + geom_line(size = 1.5) +
    theme_calc() + scale_color_calc() 
  
  plot+ theme(legend.title=element_blank(),
              legend.position = c(0.12, 0.1),
              legend.background = element_rect(color = "black",
                                               fill = "transparent", size = 1, linetype = "solid")) +
    scale_x_continuous(name = "Turnaus_NO",
                       breaks = graphs_breaks) +
    ylim(-0.5,0.50)
  
})

output$PakkaVSBox <- renderUI({
  #required_data("UID_UUSI_PELI", TRUE)
  #rm(eR_UID_UUSI_PELI)


    #eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
    result <- getVSStatsHtml(eR_UID_UUSI_PELI(), "Lauri")
    box(HTML(result), background = "aqua", width = NULL, align = "middle")
  
})

