tabItem(tabName="tab_combined",
        fluidPage(
          fluidRow(uiOutput("table_divari2")),
          fluidRow(uiOutput("combUI")),
          fluidRow(actionButton("tallenna_bannit","Tallenna"))
        )
)
