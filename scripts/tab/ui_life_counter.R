tabItem(tabName = "tab_LifeCounter",
        fluidPage(
          fluidRow(
            box(
              fluidRow(column(width = 4,
                          actionButton(inputId = "Lose_1",
                                       label = "Lose 1",
                                       width = '100%',
                                       style = "height: 126px;")),
                   column(width = 4,
                          actionButton(inputId = "Lose_2",
                                       label = "2",
                                       width = '100%',
                                       style = "height: 126px;")),
                   column(width = 4,
                          actionButton(inputId = "Lose_3",
                                       label = "3",
                                       width = '100%',
                                       style = "height: 126px;"))),
            fluidRow(column(width = 4,
                            actionButton(inputId = "Lose_4",
                                         label = "4",
                                         width = '100%',
                                         style = "height: 126px;")),
                     column(width = 4,
                            actionButton(inputId = "Lose_5",
                                         label = "5",
                                         width = '100%',
                                         style = "height: 126px;")),
                     column(width = 4,
                            actionButton(inputId = "Lose_6",
                                         label = "6",
                                         width = '100%',
                                         style = "height: 126px;"))),
            fluidRow(column(width = 4,
                            actionButton(inputId = "Lose_7",
                                         label = "7",
                                         width = '100%',
                                         style = "height: 126px;")),
                     column(width = 4,
                            actionButton(inputId = "Lose_8",
                                         label = "8",
                                         width = '100%',
                                         style = "height: 126px;")),
                     column(width = 4,
                            actionButton(inputId = "Lose_9",
                                         label = "9",
                                         width = '100%',
                                         style = "height: 126px;"))
              
            )
            ),
            box(
              fluidRow(
              
            
                   column(width = 4,
                          actionButton(inputId = "Deal_1",
                                       label = "1",
                                        width = '100%', style = "height: 126px;")),
                   column(width = 4,
                          actionButton(inputId = "Deal_2",
                                       label = "2",
                                       width = '100%',
                                       style = "height: 126px;")),
                   column(width = 4,
                          actionButton(inputId = "Deal_3",
                                       label = "3",
                                       width = '100%',
                                       style = "height: 126px;"))
                   ),
                fluidRow(
                  column(width = 4,
                         actionButton(inputId = "Deal_4",
                                      label = "4",
                                      width = '100%',
                                      style = "height: 126px;")),
                  column(width = 4,
                         actionButton(inputId = "Deal_5",
                                      label = "5",
                                      width = '100%',
                                      style = "height: 126px;")),
                  column(width = 4,
                         actionButton(inputId = "Deal_6",
                                      label = "6",
                                      width = '100%',
                                      style = "height: 126px;"))
                ),
                fluidRow(
                  column(width = 4,
                         actionButton(inputId = "Deal_7",
                                      label = "7",
                                      width = '100%',
                                      style = "height: 126px;")),
                  column(width = 4,
                         actionButton(inputId = "Deal_8",
                                      label = "8",
                                      width = '100%',
                                      style = "height: 126px;")),
                  column(width = 4,
                         actionButton(inputId = "Deal_9",
                                      label = "9",
                                      width = '100%',
                                      style = "height: 126px;"))
                )
                   
          ),
          fluidRow(
                   column(width = 3,
                          actionButton(inputId = "Lose_other",
                                       label = "Other")),
                   column(width = 6,
                          actionButton(inputId = "Deal_Non_combat",
                                       label = "Non-combat damage")),
                   column(width = 3,
                          actionButton(inputId = "Deal_other",
                                       label = "Other")))
        ),
        fluidRow(actionButton(inputId = "CHARTPLACEHOLDER",
                              label = "CHARTPLACEHOLDER",
                              style = "height: 420px")),
        fluidRow(column(width = 6,
                        textOutput("Omat_lifet")),
                 column(width = 6,
                        textOutput("Vihun lifet"))),
        fluidRow(
          column(6,
          box(
            
            actionButton(inputId = "ab_Vaihda_vuoro",
                         label = "End turn")
          )),
          column(3,
          box(
              actionButton(inputId = "ab_Vaihda_vuoro_virhe",
                           label = "End turn, add mistake")
              )),
          column(3,
          box(
            fluidRow(
              actionButton(inputId = "ab_Undo",
                           label = "Undo")
            ),
            fluidRow(
              actionButton(inputId = "ab_fix_lifes",
                           label = "Fix lifes")
            )
            )
          )
        )
)
)
