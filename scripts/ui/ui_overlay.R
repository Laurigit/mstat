
tabItem(tabName = "tab_overlay",
        fluidPage(
          fluidRow(
          column(2,
            uiOutput("overlay_left_col")
        ),
        column(2, offset = 8,
               uiOutput("overlay_right_col"))))
)
