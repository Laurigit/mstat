tabItem(tabName="tab_blow",
        fluidPage(
          fluidRow(column(6,sliderInput("slider_laurin_humala_new",label=h4("Laurin humala"),min=-0.1,max=2.5,value=-0.1,step=0.1)),
                   
                   column(6,sliderInput("slider_martin_humala_new",
                                        label=h4("Martin humala"),
                                        min=-0.1,
                                        max=2.5,
                                        value=-0.1,
                                        step=0.1))),
          fluidRow(actionButton("tallenna_humala", "Tallenna"))
        ))
