#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram

useShinyalert()
uusi_peli<-dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jscode),
  tags$head(
    tags$style(
      HTML("
           #myScrollBox{ 
           overflow-y: scroll; 
           overflow-x: hidden; 
           height:740px;
           }
           ")
      ),
    
    tags$style(type = "text/css", "
      .irs-slider {width: 30px; height: 30px; top: 22px;}
    ")
    
    
      ),
  
  tabItems(
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
    ,
    tabItem(tabName="tab_tallenna_peli",
            fluidPage(
              fluidRow(column(4,sliderInput("slider_laurin_virhe",label=h4("Laurin arvosana"),min=-1,max=1,value=1),actionButton("laurin_virhe","Laurin virhe")),
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
                                          min = -1,
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
    ),
    tabItem(tabName="tab_blow",
            fluidPage(
              fluidRow(column(6,sliderInput("slider_laurin_humala_new",label=h4("Laurin humala"),min=-0.1,max=2.5,value=-0.1,step=0.1)),
                       
                       column(6,sliderInput("slider_martin_humala_new",
                                            label=h4("Martin humala"),
                                            min=-0.1,
                                            max=2.5,
                                            value=-0.1,
                                            step=0.1))),
              fluidRow(actionButton("tallenna_humala", "Tallenna"))
            )),
    tabItem(tabName="tab_combined",
            fluidPage(
              fluidRow(uiOutput("table_divari2")),
              fluidRow(uiOutput("combUI")),
              fluidRow(actionButton("tallenna_bannit","Tallenna"))
            )
    ),
    tabItem(tabName="tab_peliasetukest",
            fluidPage(
            #  fluidRow(
                # column(3,checkboxInput("checkbox_BO_mode","Best-of-mode päällä")),
                # column(3,numericInput("numeric_rounds","Montako runkosarjakierrosta",value=1)),
                # column(3,numericInput("numeric_ottelut","Montako pelia per ottelu",value=1))),
              fluidRow(column(3,actionButton("luo_peleja","Luo uudet pelit"))
                       
              ),
              
              uiOutput("peliAsetuksetUI")
              
            )
    ),
    tabItem(tabName="tab_sarjataulukko",
            uiOutput("sarjataulukkovalitsin"),
            #fluidRow(numericInput("sarjataulukkokierros","Valitse turnauksen numero",value=1)),
            fluidRow(dataTableOutput("sarjataulukko")),
            fluidRow(uiOutput("sarjataulukot"))
            #fluidRow(box(DT::dataTableOutput("sarjataulukot_all"),width=12,title="Kaikki pelit", solidHeader = TRUE,status="primary"))
    ),
    tabItem(tabName="tab_tilastomurskain",
            fluidRow(column(4, uiOutput("radio_data_type")), 
                     column(4, uiOutput("radio_data_selected")),
                     column(4, uiOutput("validateSaavutusText"))),
            fluidRow(
                     #column(2,radioButtons("radio_minMax","Sorttaa",choices=c("Kategoria", "min", "max"),selected = "Kategoria")),
                     
                     #  column(2, verbatimTextOutput("pivotRefresh")),
                     
                     column(4, textInput("text_tilastoKuvaus",label="Tilaston/Saavutuksen nimi"),
                            actionButton("tallennaTilastoAsetus","Tallenna tilasto"),
                            
                            actionButton("tallennaSaavutusAsetus", "Tallenna saavutukset"),
                            actionButton("validateSaavutusAsetus", "Testaa toimiiko saavutus"),
                            
                            
                            
                            actionButton("poista_tilastoAsetus","Poista tilasto")),
                     column(4,(DT::dataTableOutput("tallennetut_tilastoasetukset")))),
            fluidRow(
              div(id = "myScrollBox",
                  rpivotTableOutput("pivot_cross")
              ))),
    tabItem(tabName = "tab_saavutukset",
            #fluidRow(valueBoxOutput("vb_voittoputki"),valueBoxOutput("paras_countteri"),valueBoxOutput("vaikein_counteroitava"))
            # fluidRow( actionButton("laskeSaavutukset", "Laske saavutukset"))
            uiOutput("saavutus_UI")
            
            # fluidPage(
            #   DT::dataTableOutput('aSummaryTable'),
            #   rpivotTableOutput('RESULTS')
            # )
            
            
    ),
    tabItem(tabName = "pakkaupload",
            fluidPage(
              
                  fluidRow(column(3,
                                  fileInput("file1",
                                            "Valitse pakkoja .json muodossa",
                                            multiple = TRUE,
                                            accept = c(".json"))),
                           column(3,
                                  offset = 3,
                                  fileInput("anyfile",
                                            "lähetä mikä tahansa tiedosto",
                                            multiple = TRUE)),
                           column(3,
                                  actionButton("input_lataa_valitut_pakat", "Update selected decks"))),
                  
                fluidRow(tableOutput("contents")),
                fluidRow(verbatimTextOutput("text_validointi")),
                fluidRow(box(DT::dataTableOutput("pfi_taulukko"),
                             title = ("Nykypakkastatsit"),
                             solidHeader = TRUE,
                             status = "primary",
                             width = 12))
            
    )),
    tabItem(tabName = "tab_saavutusasetukset",
            fluidPage(
              fluidRow(column(2, radioButtons("radio_minMax_saavutus",
                                             "Voittajan valinta",
                                             choices = c("min", "max"), selected = "max")),
                       column(2, radioButtons("radio_minMax_saavutus_rivi",
                                              "Rivitavoite",
                                              choices = c("min", "max"), selected = "max")),
                       column(3, radioButtons("radio_muotoilu",
                                              "Numeron muotoilu",
                                              choices = c("Decimal",
                                                          "Integer",
                                                          "%",
                                                          "e"),
                                              selected = "%")),
                       column(3, actionButton("paivita_saavutus",
                                              "Paivita saavutus"),
                              actionButton("poista_saavutusAsetus",
                                           "Poista saavutus")),
                       column(3,textInput("txt_palkinto",
                                          "Palkinnon nimi"),
                              textInput("txt_palkinto_kuvaus",
                                        "Palkinnon kuvaus"))),
              fluidRow(column(6, dataTableOutput("tallennetut_saavutusAsetukset")))
            )
    ),
    tabItem(tabName = "tab_boosterit",
            fluidPage(
              fluidRow(
                       column(3, numericInput("numeric_count_boosters", "How many boosters to draft?", 4, 1, 16, step = 1)),
                       column(3, textOutput("txt_vect_boosters")),
                       column(3, actionButton("action_add_boosters", "Draft these boosters")),
                       column(3, textOutput("txt_confirm_drafted")))
            )),
    tabItem(tabName = "tab_decks",
            fluidPage(   
              fluidRow(
                column(12, rHandsontableOutput("hot_decks"))),
              fluidRow(actionButton(inputId = "Save_decks", label = "Save changes and upload"))
              ))
    
    
    
    
    
  ))



#SIDEBAR  
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              menuItem("Uusi peli", tabName = "tab_uusi_peli", icon = icon("gamepad")),
              menuItem("Tallenna peli", icon = icon("hdd"), tabName = "tab_tallenna_peli"),
              menuItem("Blow", icon = icon("beer"), tabName = "tab_blow"),
              menuItem("Sarjataulukko", icon = icon("trophy"), tabName = "tab_sarjataulukko"),
              menuItem("Tilastomurskain",icon = icon("bar-chart"),tabName = "tab_tilastomurskain"),
              menuItem("Saavutukset",icon = icon("bullseye"),tabName = "tab_saavutukset"),
              
              #menuItem("Turnausasetukset",tabName="nimeton",
              menuItem('Divarit ja pickit', icon = icon("tasks") ,tabName = 'tab_combined'),
              #menuSubItem(icon = NULL,actionButton("tallenna_bannit","Tallenna")),
              menuItem('Peliasetukset',  icon = icon("sliders-h"),tabName = 'tab_peliasetukest'),
              menuItem("Download decks", icon = icon("cloud-download-alt") ,tabName = "pakkaupload"),
              menuItem("Saavutusasetukset", icon = icon("cog"), tabName = "tab_saavutusasetukset"),
              menuItem("Boosterit", icon = icon("envelope"), tabName = "tab_boosterit"),
              menuItem("Decks", icon = icon("server"), tabName = "tab_decks"),
              radioButtons("radio_pfi_mode",
                           label = h5("PFI mode"),
                           choices = list("Pois" = FALSE, "Paalla" = TRUE),
                           selected = TRUE, 
                           inline = T),
              # actionButton("automated_tests", label = h5("Run tests")),
             actionButton("blow_timer", label = h5("Blow timer")),
             actionButton("refresh", label = "Update data"),
             radioButtons("radio_bo_mode", label = h5("BO mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
             #radioButtons("radio_debug_mode", label = h5("Debug"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
               #div(style="display:inline-block;width:90%;text-align: center;",uiOutput("sarjataulukkovalitsin")),
              numericInput("numeric_MA_valinta","Valitse Ed X pelia",value=7)
              #menuSubItem(icon = NULL,actionButton("luo_peleja","Luo uudet pelit"))
  )
  
  
)

#RUNKO  
dashboardPage(
  
  #dashboardHeader(title = paste0("run_mode = ", GLOBAL_test_mode, " ", textOutput('blow_timer')),
  dashboardHeader(title = textOutput('blow_timer'),
                  titleWidth = 450),
  sidebar,
  uusi_peli
)





