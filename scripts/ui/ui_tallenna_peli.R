tabItem(tabName="tab_tallenna_peli",
        fluidPage(
          fluidRow(column(4,sliderInput("slider_laurin_virhe",label=h4("Laurin arvosana"),
                                        min = -10,
                                        max = 1,
                                        value = 1),
                          actionButton("laurin_virhe","Laurin virhe")),
                   column(2,      radioButtons("radio_voittaja", label = h3("Voittaja"),
                                               choices = list("Lauri voitti" = 0, "Martti voitti" = 1), 
                                               selected = 1)  
                          
                   ),
                   column(2,textOutput("last_changed_value_text"),
                          
                          tags$head(tags$style("#last_changed_value_text{color: green;
                                               font-size: 100px;
                                               font-style: bold;
                                               }"
                              )
                          )
                          
                          
                          ),
                   column(4,
                          sliderInput("slider_martin_virhe",
                                      label = h4("Martin arvosana"),
                                      min = -10,
                                      max = 1,
                                      value = 1),
                          actionButton("martin_virhe",
                                       "Martin virhe"))
                          ),
          fluidRow(column(4,
                          sliderInput("slider_laurin_landit",
                                      label = h4("Lauri landit"),
                                      min = 0,
                                      max = 10, 
                                      value = 0)),
                   #column(4,sliderInput("slider_vuoroarvio",label=h4("Vuoroarvaus"),min=0,max=10,value=0)),
                   column(4, uiOutput("vuoroArvausPelaaja")),
                   column(4, sliderInput("slider_martin_landit",
                                         label = h4("Martin landit"),
                                         min = 0,
                                         max = 10,
                                         value = 0))
          ),
          fluidRow(column(6,
                          sliderInput("slider_laurin_kasikortit",
                                      label = h4("Laurin kasikortit"),
                                      min=-1,max=7,value=-1)),
                   
                   column(6, sliderInput("slider_martin_kasikorit",label=h4("Martin kasikortit"),min=-1,max=7,value=-1))),
          fluidRow(column(6,sliderInput("slider_laurin_lifet",label=h4("Laurin lifet"),min=0,max=21,value=0)),
                   
                   column(6, sliderInput("slider_martin_lifet",label=h4("Martin lifet"),min=0,max=21,value=0))),
          
          fluidRow(column(3,actionButton("tallenna_tulos","Tallenna tulos")),
                   column(3, textOutput("validateWinnerText"),
                          
                          tags$head(tags$style("#validateWinnerText{color: red;
                                 font-size: 20px;
                                                   font-style: bold;
                                                   }"
                          )
                          )
                          
                   ),
                   column(3,actionButton("action_reduce",label="",icon("arrow-left"),width='100%')),
                   column(3,actionButton("action_add",label="",icon("arrow-right"),width='100%')))
          
          )
        )
