tabItem(tabName = "pakkaupload",
        fluidPage(
          
          fluidRow(column(3,
                          fileInput("file1",
                                    "Valitse pakkoja .json muodossa",
                                    multiple = TRUE,
                                    accept = c(".json"))),
                   column(3,
                          offset = 3,
                          fileInput("anyfile",
                                    "lähetä mikä tahansa tiedosto",
                                    multiple = TRUE)),
                   column(3,
                          actionButton("input_lataa_valitut_pakat", "Update selected decks"))),
          
          fluidRow(tableOutput("contents")),
          fluidRow(verbatimTextOutput("text_validointi")),
          fluidRow(box(DT::dataTableOutput("pfi_taulukko"),
                       title = ("Nykypakkastatsit"),
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12))
          
        ))
