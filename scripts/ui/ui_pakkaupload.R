tabItem(tabName = "pakkaupload",
        fluidPage(
          
          fluidRow(
                   column(3,
                          offset = 3,
                          fileInput("anyfile",
                                    "lähetä mikä tahansa tiedosto",
                                    multiple = TRUE))),
          fluidRow(box(DT::dataTableOutput("pfi_taulukko"),
                       title = ("Nykypakkastatsit"),
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12))
          
        ))
