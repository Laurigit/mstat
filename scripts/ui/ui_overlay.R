
tabItem(tabName = "tab_overlay",
        fluidPage(
          fluidRow(
          column(2,
            fluidRow(uiOutput("overlay_left_col"),
            fluidRow(dataTableOutput("overlay_sarjataulukko")
        ))),
        column(2, offset = 8,
               fluidRow(   uiOutput("overlay_right_col")),
               fluidRow(uiOutput("turnaustilanne_overlay"))
             
              )))
)
