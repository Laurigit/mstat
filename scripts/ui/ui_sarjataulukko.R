tabItem(tabName="tab_sarjataulukko",
        uiOutput("sarjataulukkovalitsin"),
        #fluidRow(numericInput("sarjataulukkokierros","Valitse turnauksen numero",value=1)),
        fluidRow(dataTableOutput("sarjataulukko")),
        fluidRow(uiOutput("sarjataulukot"))
        #fluidRow(box(DT::dataTableOutput("sarjataulukot_all"),width=12,title="Kaikki pelit", solidHeader = TRUE,status="primary"))
)
