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
          )))
