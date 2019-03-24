output$overlay_row <- renderUI({
  required_data("ADM_TURN_SEQ")
 # if (input$vasen == "Lauri") {
  
    result <- getDeckStats("Lauri", eR_UID_UUSI_PELI())
    result_data <- result$data
    
    resultM <- getDeckStats("Martti", eR_UID_UUSI_PELI())
    result_dataM <- resultM$data
    
    lifetVasen <- life_totals$data$Lifetotal[Omistaja_NM == "Lauri", Life_total]
    lifetOikea <-   life_totals$data$Lifetotal[Omistaja_NM == "Martti", Life_total]
    pakkaVasen <- result_data$Deck
    pakkaOikea <- result_dataM$Deck
    vuoro  <- ADM_TURN_SEQ[TSID == turnData$turn, Turn_text]
 # }

      fluidRow(column(2,  box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                          lifetVasen,
                                          '</b></font></div>')),
                              background = "blue",
                              width = "100%")),
               column(3, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                         pakkaVasen,
                                         '</b></font></div>')),
                             background = "maroon",
                             width = "100%")),
               column(2, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                         vuoro,
                                         '</b></font></div>')),
                             background = "blue",
                             width = "100%")),
               column(3, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                         pakkaOikea,
                                         '</b></font></div>')),
                             background = "maroon",
                             width = "100%")),
               column(2, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                         lifetOikea,
                                         '</b></font></div>')),
                             background = "blue",
                             width = "100%")))
  })
