
  tabItem(tabName="tab_uusi_peli",
          fluidPage(
            useShinyalert(),
            #theme = shinytheme("yeti"),
            shinyjs::useShinyjs(),
            box(id = "uusipeli_box", fluidRow(column(4, uiOutput("selectInputLauri")),
                                              column(4, actionButton("arvo_peli","Random match")),
                                              column(4, uiOutput("selectInputMartti"))),
                fluidRow(column(4, actionButton("laurin_mulligan",
                                                "Laurin Mulligan",
                                                icon = icon("undo"),
                                                style = "color: #fff; background-color: #b73338; border-color: #2e6da4")),
                         column(4, actionButton("tasuriPeli", "Equalizer")),
                         column(4, actionButton("martin_mulligan",
                                                "Martin Mulligan",
                                                icon = icon("undo"),
                                                style = "color: #fff; background-color: #b73338; border-color: #2e6da4"))),
                fluidRow(column(4, uiOutput("mulliganiSlideriLauri")),
                         column(4, uiOutput("divariRadio_out")),
                         column(4,  sliderInput("slider_martin_mulligan", label = h4("Martin mulliganit"), min = 0, 
                                                max = 6, value = 0))),
                width = NULL,
                collapsible = TRUE),
            fluidRow(column(2, actionButton("lauri_voitti","Lauri voitti")),
                     column(2, actionButton("laurin_virhe_uusipeli","Laurin virhe", icon = icon("exclamation-circle"))
                     ),
                     column(2, textOutput("peliKesto"),    tags$head(tags$style("#peliKesto{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                     )
                     )
                     ),
                     column(2, actionButton("martin_virhe_uusipeli", "Martin virhe", icon = icon("exclamation-circle"))
                     ),
                     
                     
                     column(1, actionButton("martti_voitti","Martti voitti"))),
            
            fluidRow(column(3,(textOutput("peli_id")))),
            
            fluidRow(column(4, uiOutput("PakkaLeftBox")),
                     column(4,  uiOutput("PakkaVSBox")),
                     column(4, uiOutput("PakkaRightBox"))),
            fluidRow(column(6,plotOutput("EV_plot")),
                     column(6, plotOutput("win_distribution")))
          )          
  )
