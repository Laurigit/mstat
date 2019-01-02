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
  source("./scripts/ui/ui_uusi_peli.R",local = TRUE)$value,
  source("./scripts/ui/ui_tallenna_peli.R",local = TRUE)$value,
  source("./scripts/ui/ui_blow.R",local = TRUE)$value,
  source("./scripts/ui/ui_combined.R",local = TRUE)$value,
  source("./scripts/ui/ui_peliasetukset.R",local = TRUE)$value,
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
              )),
    source("./scripts/tab/ui_life_counter.R",local = TRUE)$value

    
    
  ))



#SIDEBAR  
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              menuItem("Uusi peli", tabName = "tab_uusi_peli", icon = icon("gamepad")),
              menuItem("Tallenna peli", icon = icon("hdd"), tabName = "tab_tallenna_peli"),
              menuItem("LifeCounter", tabName = "tab_LifeCounter", icon = icon("gamepad")),
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
              numericInput("numeric_MA_valinta","Valitse Ed X pelia",value=7),
             actionButton("loginbutton", "Login")
              #menuSubItem(icon = NULL,actionButton("luo_peleja","Luo uudet pelit"))
  )
  
  
)

#RUNKO  
dashboardPage(
  
  #dashboardHeader(title = paste0("run_mode = ", GLOBAL_test_mode, " ", textOutput('blow_timer')),
#  dashboardHeader(title = textOutput('blow_timer'),
#                 titleWidth = 450),
  dashboardHeader(title = textOutput('Username')),
 
  sidebar,
  uusi_peli
)





