
  tabItem(tabName = "tab_uusi_peli",
          fluidPage(
            #fluidRow(dataTableOutput("debug_keymap")),

            
            fluidRow(  
                       tags$script('
                                   $(document).on("keypress", function (e) {
                                   Shiny.onInputChange("mydata", [e.which,e.timeStamp,Math.random()]);
                                   });
                                   ') ),
         
            
            useShinyalert(),
            #theme = shinytheme("yeti"),
            shinyjs::useShinyjs(),
           
            box(id = "uusipeli_box", fluidRow(column(4, uiOutput("selectInputLauri")),
                                              column(2, actionButton("arvo_peli","Random match")),
                                              column(2, actionButton("tasuriPeli", "Equalizer")),
                                              column(4, uiOutput("selectInputMartti"))),
                fluidRow(column(4, actionButton("laurin_mulligan",
                                                "Laurin Mulligan",
                                                icon = icon("undo"),
                                                style = "color: #fff; background-color: #b73338; border-color: #2e6da4")),
                         column(4, uiOutput("divariRadio_out")),
                         column(4, actionButton("martin_mulligan",
                                                "Martin Mulligan",
                                                icon = icon("undo"),
                                                style = "color: #fff; background-color: #b73338; border-color: #2e6da4"))),
                fluidRow(column(4, uiOutput("mulliganiSlideriLauri")),
                         column(4, actionButton("start_life_counter", "Start new lifecounter game")),
                         column(4, uiOutput("mulliganiSlideriMartti"))),
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
            #fluidRow(box(HTML(       '<font size="7" color="red">  This is some t1ext!</font></body>')),
           # fluidRow(box(HTML('<div align="center"><font size="40" color="white"> <b>20</b></font></div'),
           #          background = "black")),
           
            
            fluidRow(column(3,(textOutput("peli_id")))),
            
            fluidRow(column(4, uiOutput("PakkaLeftBox")),
                     column(4,  uiOutput("PakkaVSBox")),
                     column(4, uiOutput("PakkaRightBox"))),
            fluidRow(column(6,plotOutput("EV_plot")),
                     column(6, plotOutput("win_distribution")))
          )          
  )
