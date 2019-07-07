#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram


uusi_peli <- dashboardBody(

  useShinyalert(),
  useShinyjs(),
  extendShinyjs(text = jscode),
  extendShinyjs(script = "beep.js"),
  extendShinyjs(script = "no.js"),
  extendShinyjs(script = "toot.js"),
  extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }"),
 
  tags$head(
    tags$style(
      HTML("
           #myScrollBox{ 
           overflow-y: scroll; 
           overflow-x: hidden; 
           height:740px;
           }
           ")
      )
    ,
    
    tags$style(type = "text/css", "
      .irs-slider {width: 30px; height: 30px; top: 22px;}
    ")
    
    
      ),
  tabItems(
 #  source("./scripts/ui/ui_uusi_peli.R",local = TRUE)$value,
 #  source("./scripts/ui/ui_tallenna_peli.R",local = TRUE)$value,
  source("./scripts/ui/ui_blow.R",local = TRUE)$value,
  source("./scripts/ui/ui_combined.R",local = TRUE)$value,
  source("./scripts/ui/ui_peliasetukset.R",local = TRUE)$value,
  source("./scripts/ui/ui_sarjataulukko.R",local = TRUE)$value,
  source("./scripts/ui/ui_tilastomurskain.R",local = TRUE)$value,
  source("./scripts/ui/ui_saavutukset.R",local = TRUE)$value,
  source("./scripts/ui/ui_pakkaupload.R",local = TRUE)$value,
  source("./scripts/ui/ui_saavutusasetukset.R",local = TRUE)$value,
  source("./scripts/ui/ui_boosterit.R",local = TRUE)$value,
  source("./scripts/ui/ui_decks.R",local = TRUE)$value,
#  source("./scripts/ui/ui_life_counter.R",local = TRUE)$value,
  source("./scripts/ui/ui_overlay.R",local = TRUE)$value
  
    
    
  ))



#SIDEBAR  
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
            #  menuItem("Uusi peli", tabName = "tab_uusi_peli", icon = icon("gamepad")),
            #  menuItem("Tallenna peli", icon = icon("hdd"), tabName = "tab_tallenna_peli"),
              #menuItem("LifeCounter", tabName = "tab_LifeCounter", icon = icon("gamepad")),
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
              menuItem("Overlay", icon = icon("server"), tabName = "tab_overlay"),
              radioButtons("radio_pfi_mode",
                           label = h5("PFI mode"),
                           choices = list("Pois" = FALSE, "Paalla" = TRUE),
                           selected = TRUE, 
                           inline = T),
              # actionButton("automated_tests", label = h5("Run tests")),
             actionButton("blow_timer", label = h5("Blow timer")),
             actionButton("refresh", label = "Update data"),
             radioButtons("radio_bo_mode", label = h5("BO mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
             #radioButtons("radio_total_mode", label = h5("Total mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
             
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





