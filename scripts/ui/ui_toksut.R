tabItem(tabName = "tab_toksut",
        selectInput("toksut_vai_pakat", label = "Sort by", choices = c("Decks", "Tokens"), selected = "Decks"),
        tableOutput("toksulista")
)
