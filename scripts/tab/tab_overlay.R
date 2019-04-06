output$overlay_sarjataulukko <- renderDataTable({
  required_data(c("ADM_PELIT", "STG_PAKAT"))
  # input <- NULL
  # input$sarjataulukkokierros <- 27
  # input$radio_bo_mode <- TRUE
  # Data <- UID_SARJATAULUKKO(input$sarjataulukkokierros, input$radio_bo_mode, ADM_PELIT, STG_PAKAT)
  # print("LOOP AT TAB_SARJATAULUKKO")

  Data <-  eR_UID_SARJATAULUKKO()
  # print(eR_UID_SARJATAULUKKO())

  peli_ID_temp <- eR_Peli_ID()
  peliDivari <- ADM_PELIT[Peli_ID == peli_ID_temp, max(Divari)]
  
  DivariData <- Data[[peliDivari]]
  #print(DivariData)
  if(!is.null(DivariData)) {
    outputData <- DivariData[, .(Pakka_NM,
                                 Matches,
                                 Wins)]
  } else {
    outputData <- data.table(not_played = "-")
  }
    
 # data.table(outputData, colnames = NULL)
  #mtcars <- data.table(mtcars, rownames = FALSE, colnames = FALSE)
  noCol <- datatable(outputData, rownames = FALSE,colnames=NULL, selection = 'none',
                     options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                                      dom = 't',
                                    ordering = F))
  print(noCol)
  return(noCol)
  #print(Data)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info = FALSE
  ),  rownames = FALSE)

output$turnaustilanne_overlay <- renderValueBox({
  required_data("STAT_TURNAUS") 
  sarjataulukkoData <- eR_UID_SARJATAULUKKO()
  total <- do.call(rbind, sarjataulukkoData)
  current_turnaus <- STAT_TURNAUS[Turnaus_valmis == FALSE]
  Lvoitot <- total[Omistaja_ID == "L", sum(Score)]
  Mvoitot <- total[Omistaja_ID == "M" , sum(Score)]
  tilanneteksti <- paste0(Lvoitot,
                          "-",
                          Mvoitot)
  box(valueBox(tilanneteksti,"Score" ,icon = icon("trophy",lib = "font-awesome"), width = 12, color = "blue"))
})


output$overlay_left_col <- renderUI({
  required_data("ADM_TURN_SEQ")
 # if (input$vasen == "Lauri") {

    lifetVasen <- life_totals$data$Lifetotal[Omistaja_NM == "Lauri", Life_total]
  

    if ( turnData$turn > 0) {
      vuorotekstiAlku <- ADM_TURN_SEQ[TSID == turnData$turn, Turn_text]
      if (isolate(eR_Peli_Aloittaja$a) == 0) {
        Aloittaja <- "L"
        Nostaja <- "M"
      } else {
        Aloittaja <- "M"
        Nostaja <- "L"
      }
      
      if (ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn] == TRUE) {
        pelaaja_vuorossa <- Aloittaja
      } else {
        pelaaja_vuorossa <- Nostaja
      }
      
      
      vuoroTeksti <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
    } else {
      vuoroTeksti <- "Not started"
    }
    
    
    
 # }
    fluidPage(
    fluidRow(
    
      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                          lifetVasen,
                                          '</b></font></div>')),
                              background = "blue",
                              width = NULL)
      ),
      fluidRow(
        uiOutput("PakkaLeftBox_overlay")) ,
      
   
    fluidRow(

             box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                   vuoroTeksti,
                                              '</b></font></div>')),
                                  background = "blue",
                                  width = NULL)
    ),
    fluidRow(uiOutput("PakkaVSBox_overlay")))

    
  #  column(2,
  #         valueBox(input$laurin_mulligan, subtitle = "Mulls", color = "maroon", width = NULL))
    
    #  plotOutput("EV_plot_ovelary"),
    #  uiOutput("PakkaRightBox_overlay")
      # ,
      # box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
      #                 lifetOikea,
      #                 '</b></font></div>')),
      #     background = "blue",
      #     width = "100%")

      


               # column(3, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
               #                           pakkaVasen,
               #                           '</b></font></div>')),
               #               background = "maroon",
               #               width = "100%")),

  })


output$valueBoxRows <- renderUI({
  print("VALUBOKSIT PÄIVITTYYs")
  required_data("ADM_TURN_SEQ")
  accpetd_dmg_row_all <- calc_life_totals(damage_data$data)$aggr_accepted
  accpetd_dmg_row <- accpetd_dmg_row_all[nrow(accpetd_dmg_row_all)]
  colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "maroon", "green")
  ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "fist-raised", "bolt")
  targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
  soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
  maara <- abs(accpetd_dmg_row[, Amount])
  vuoro <- accpetd_dmg_row[, TSID]
  #suunta riippuen damagen vastaanottajasta
  if(targetti == "Lauri") {
    teksti <- paste0(targetti, " <- ", soursa)
  } else {
    teksti <-  paste0( targetti, " -> ", soursa)
  }
  
  
  
  if ( vuoro > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == vuoro, Turn_text]
    if (isolate(eR_Peli_Aloittaja$a) == 0) {
      Aloittaja <- "L"
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }
    
    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }
    
    
    subTitle <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  }
  

 box(valueBox(value = tags$p(paste0("   ", maara, "  ", teksti), style = "font-size: 125%;"),
          subtitle = tags$p(subTitle, style = "font-size: 125%;"),
          icon = icon(ikoni),
                      color = colori,
                      width = NULL), width = NULL)
  
})


output$valueBoxRows_prev <- renderUI({

  required_data("ADM_TURN_SEQ")
  accpetd_dmg_row_all <- calc_life_totals(damage_data$data)$aggr_accepted
  accpetd_dmg_row <- accpetd_dmg_row_all[nrow(accpetd_dmg_row_all) - 1]
  colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "maroon", "green")
  ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "fist-raised", "bolt")
  targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
  soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
  maara <- abs(accpetd_dmg_row[, Amount])
  vuoro <- accpetd_dmg_row[, TSID]
  #suunta riippuen damagen vastaanottajasta
  if(targetti == "Lauri") {
    teksti <- paste0(targetti, " <- ", soursa)
  } else {
    teksti <-  paste0( targetti, " -> ", soursa)
  }
  
  
  
  if ( vuoro > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == vuoro, Turn_text]
    if (isolate(eR_Peli_Aloittaja$a) == 0) {
      Aloittaja <- "L"
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }
    
    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }
    
    
    subTitle <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  }
  
  
  box(valueBox(value = tags$p(paste0("   ", maara, "  ", teksti), style = "font-size: 125%;"),
               subtitle = tags$p(subTitle, style = "font-size: 125%;"),
               icon = icon(ikoni),
               color = colori,
               width = NULL), width = NULL)
  
})


output$overlay_right_col <- renderUI({
  # if (input$vasen == "Lauri") {
  

  lifetOikea <-   life_totals$data$Lifetotal[Omistaja_NM == "Martti", Life_total]


  # }
  fluidPage(
    fluidRow(

             box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                             lifetOikea,
                             '</b></font></div>')),
                 background = "blue",
                 width = NULL)
    ),
    fluidRow(
 uiOutput("PakkaRightBox_overlay"))
    
    ,
    fluidRow(
      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                      aika_text_reactive$aika,
                      '</b></font></div>')),
          background = "blue",
          width = NULL)),
 
    uiOutput("valueBoxRows"),
    uiOutput("valueBoxRows_prev")
    )
  
  #  column(2,
  #         valueBox(input$laurin_mulligan, subtitle = "Mulls", color = "maroon", width = NULL))
  
  #  plotOutput("EV_plot_ovelary"),
  #  uiOutput("PakkaRightBox_overlay")
  # ,
  # box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
  #                 lifetOikea,
  #                 '</b></font></div>')),
  #     background = "blue",
  #     width = "100%")
  
  
  
  
  # column(3, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
  #                           pakkaVasen,
  #                           '</b></font></div>')),
  #               background = "maroon",
  #               width = "100%")),
  
})

