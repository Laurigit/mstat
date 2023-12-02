tabItem(tabName="tab_combined",
        fluidPage(
          fluidRow(uiOutput("table_divari2")),
          fluidRow(radioButtons("randomize_divisions", "Randomize divisions", choiceNames = c("No", "Yes"), choiceValues = c(FALSE, TRUE)), selected = FALSE),
          fluidRow(uiOutput("combUI")),
          fluidRow(actionButton("tallenna_bannit","Tallenna"))
        )
)
