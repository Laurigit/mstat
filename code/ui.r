#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram


  uusi_peli<-dashboardBody(

    tabItems(
      tabItem(tabName="tab_uusi_peli",
    fluidPage(
      #theme = shinytheme("yeti"),
      shinyjs::useShinyjs(),
 
      fluidRow(column(3,actionButton("arvo_peli","Arvo peli")),
               column(3,actionButton("tasuri_peli","Tasuripeli")),
               column(3,actionButton("jatka_ottelua","Jatka ottelua"))
                ),
      fluidRow(column(3,uiOutput("selectInputLauri")),column(3,h3(textOutput("text_aloittaja"))),column(3,uiOutput("selectInputMartti"))),
      fluidRow(column(3,actionButton("laurin_mulligan","Laurin Mulligan")),column(3,h3(textOutput("text_tilanne"))),column(3,actionButton("martin_mulligan","Martin Mulligan"))),
      
      fluidRow(column(3, sliderInput("slider_laurin_mulligan", label = h4("Laurin mulliganit"), min = 0, 
                                     max = 6, value = 0)),

               column(3,offset=3, sliderInput("slider_martin_mulligan", label = h4("Martin mulliganit"), min = 0, 
                                     max = 6, value = 0))
        ),
      fluidRow(column(3,actionButton("lauri_voitti","Lauri voitti")),column(3,offset=3,actionButton("martti_voitti","Martti voitti"))),

      fluidRow(column(3,textOutput("peli_id")) ),
      
      fluidRow(column(12,box(dataTableOutput("data_vs_taulukko"),width=12)))
      )          
      )
    ,
    tabItem(tabName="tab_tallenna_peli",
            fluidPage(
              fluidRow(column(3,sliderInput("slider_laurin_virhe",label=h4("Laurin arvosana"),min=-1,max=1,value=1),actionButton("laurin_virhe","Laurin virhe")),
                       column(3,      radioButtons("radio_voittaja", label = h3("Voittaja"),
                                                   choices = list("Lauri voitti" = 0, "Martti voitti" = 1), 
                                                   selected = 1)  
                              
                       ),
                       column(3,sliderInput("slider_martin_virhe",label=h4("Martin arvosana"),min=-1,max=1,value=1),actionButton("martin_virhe","Martin virhe"))
              ),
              fluidRow(column(3,sliderInput("slider_laurin_landit",label=h4("Lauri landit"),min=0,max=10,value=0)),
                       column(3,sliderInput("slider_vuoroarvio",label=h4("Vuoroarvaus"),min=0,max=10,value=0)),
                       column(3,sliderInput("slider_martin_landit",label=h4("Martin landit"),min=0,max=10,value=0))
              ),
              fluidRow(column(3,sliderInput("slider_laurin_kasikortit",label=h4("Laurin kasikortit"),min=-1,max=7,value=-1)),
                       column(3),
                       column(3, sliderInput("slider_martin_kasikorit",label=h4("Martin kasikortit"),min=-1,max=7,value=-1))),
              fluidRow(column(3,sliderInput("slider_laurin_lifet",label=h4("Laurin lifet"),min=0,max=21,value=0)),
                       column(3),
                       column(3, sliderInput("slider_martin_lifet",label=h4("Martin lifet"),min=0,max=21,value=0))),
              fluidRow(column(3,sliderInput("slider_laurin_humala",label=h4("Laurin humala"),min=-0.1,max=2.5,value=-0.1,step=0.1)),
                       column(3),
                       column(3,sliderInput("slider_martin_humala",label=h4("Martin humala"),min=-0.1,max=2.5,value=-0.1,step=0.1))),
              fluidRow(actionButton("tallenna_tulos","Tallenna tulos"))
     
              
            )
            ),
    
            tabItem(tabName="tab_combined",
                    fluidPage(
                      #fluidRow(box(DT::dataTableOutput("table_divari2"))),
                      fluidRow(uiOutput("table_divari2")),
                      fluidRow(uiOutput("combUI")),
                      fluidRow(actionButton("tallenna_bannit","Tallenna"))
                    )
            ),
            tabItem(tabName="tab_peliasetukest",
            fluidPage(
              fluidRow(
                column(3,checkboxInput("checkbox_BO_mode","Best-of-mode päällä")),
                column(3,numericInput("numeric_rounds","Montako runkosarjakierrosta",value=1)),
                column(3,numericInput("numeric_ottelut","Montako pelia per ottelu",value=1))),
              fluidRow(column(3,actionButton("luo_peleja","Luo uudet pelit"))
                
              )
              

            )
            ),
          tabItem(tabName="tab_sarjataulukko",
                    fluidRow(column(3,offset=1,uiOutput("sarjataulukkovalitsin"))
                             ),
                    #fluidRow(numericInput("sarjataulukkokierros","Valitse turnauksen numero",value=1)),
                    fluidRow(dataTableOutput("sarjataulukko")),
                    fluidRow(uiOutput("sarjataulukot"))
                  #fluidRow(box(DT::dataTableOutput("sarjataulukot_all"),width=12,title="Kaikki pelit", solidHeader = TRUE,status="primary"))
          ),
    tabItem(tabName="tab_saavutukset",
            fluidRow(valueBoxOutput("vb_voittoputki"),valueBoxOutput("paras_countteri"),valueBoxOutput("vaikein_counteroitava")
                     
            )
    ),
    tabItem(tabName="pakkaupload",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                fileInput("file1", "Valitse pakkoja .json muodossa",multiple=TRUE,accept = c(".json"))),
                mainPanel(
                  tableOutput("contents")
                )
              ),verbatimTextOutput("text_validointi")
              )
    )
  )
)

    
   
#SIDEBAR  
sidebar <- dashboardSidebar(
 
  sidebarMenu(id="sidebarmenu",
    menuItem("Uusi peli", tabName = "tab_uusi_peli", icon = icon("dashboard")),
    
    menuItem("Tallenna peli", icon = icon("th"), tabName = "tab_tallenna_peli",badgeLabel = "new", badgeColor = "green"),
    menuItem("Sarjataulukko", icon = icon("th"), tabName = "tab_sarjataulukko",badgeLabel = "omg", badgeColor = "red"),
    menuItem("Saavutukset",icon=icon("th"),tabName="tab_saavutukset"),

    #menuItem("Turnausasetukset",tabName="nimeton",
     menuItem('Divarit ja pickit', tabName = 'tab_combined'),
     #menuSubItem(icon = NULL,actionButton("tallenna_bannit","Tallenna")),
     menuItem('Peliasetukset', tabName = 'tab_peliasetukest'),
    menuItem("Lataa pakkoja", tabName = "pakkaupload"),
    radioButtons("radio_total_mode",label=h5("Total mode"),choices = list("Pois"=FALSE,"Paalla"=TRUE),selected=FALSE,inline=T),
    radioButtons("radio_bo_mode", label = h5("BO mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = TRUE,inline=T),
    radioButtons("radio_pfi_mode", label = h5("PFI mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
    #div(style="display:inline-block;width:90%;text-align: center;",uiOutput("sarjataulukkovalitsin")),
    numericInput("numeric_MA_valinta","Valitse Ed X pelia",value=5)
     #menuSubItem(icon = NULL,actionButton("luo_peleja","Luo uudet pelit"))
    )

  
)
  
#RUNKO  
dashboardPage(

  dashboardHeader(title = "Mätkysofta"),
  sidebar,
  uusi_peli
)

  



