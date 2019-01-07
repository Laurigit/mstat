tabItem(tabName = "tab_boosterit",
        fluidPage(
          fluidRow(
            column(3, numericInput("numeric_count_boosters", "How many boosters to draft?", 4, 1, 16, step = 1)),
            column(3, textOutput("txt_vect_boosters")),
            column(3, actionButton("action_add_boosters", "Draft these boosters")),
            column(3, textOutput("txt_confirm_drafted")))
        ))
