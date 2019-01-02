#ui_peliasetukset
tabItem(tabName = "tab_peliasetukest",
        fluidPage(
          #  fluidRow(
          # column(3,checkboxInput("checkbox_BO_mode","Best-of-mode päällä")),
          # column(3,numericInput("numeric_rounds","Montako runkosarjakierrosta",value=1)),
          # column(3,numericInput("numeric_ottelut","Montako pelia per ottelu",value=1))),
          fluidRow(column(3,actionButton("luo_peleja","Luo uudet pelit"))
                   
          ),
          
          uiOutput("peliAsetuksetUI")
          
        )
)
