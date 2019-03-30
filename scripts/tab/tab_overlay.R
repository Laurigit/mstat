output$overlay_left_col <- renderUI({
  required_data("ADM_TURN_SEQ")
 # if (input$vasen == "Lauri") {

    lifetVasen <- life_totals$data$Lifetotal[Omistaja_NM == "Lauri", Life_total]
    vuoro  <- ADM_TURN_SEQ[TSID == turnData$turn, Turn_text]
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
                                              vuoro,
                                              '</b></font></div>')),
                                  background = "blue",
                                  width = NULL)
    ))
    
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
                      "12:24",
                      '</b></font></div>')),
          background = "blue",
          width = NULL)),
    fluidRow(
  
             uiOutput("PakkaVSBox_overlay"))
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

