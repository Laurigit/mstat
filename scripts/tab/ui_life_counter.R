tabItem(tabName = "tab_LifeCounter",
        fluidPage(
            tabBox(id = "lifeBox", 
                title = NULL,
                width = 12,
                height = "560px",
                tabPanel(value = "life_input",
                         title = "Input life",
                         fluidRow(
                            checkboxGroupButtons(inputId = "dmg_settings",
                                                 label  = NULL,
                                                 choices = c("Non-combat damage",
                                                             "Lifegain",
                                                             "Reverse Source"),
                                                 status = "success",
                                                 size = "lg",
                                                 direction = "horizontal",
                                                 justified = TRUE,
                                                 individual = TRUE,
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))),
            
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
                  ))))),
            tabPanel(value = "waiting_panel",
                     title = "Waiting",
                
                   
          fluidRow(
                   column(width = 3,
                          actionBttn(inputId = "Lifegain",
                                       label = "Lifeloss selected",
                                     style = "material-flat",
                                     size = "md",
                                     color = "primary",
                                     block = TRUE)),
                   box(column(width = 6,
                          actionBttn(inputId = "Deal_Non_combat",
                                       label = "Non-combat damage",
                                     style = "material-flat",
                                     size = "md",
                                     color = "warning",
                                     block = TRUE,
                                     no_outline = TRUE))),
                   column(width = 3,
                          actionBttn(inputId = "Reverse_source",
                                       label = "Reverse source",
                                       style = "material-flat",
                                       size = "lg",
                                     color = "danger",
                                       block = TRUE)))
            )),
                
          
        fluidRow(actionButton(inputId = "CHARTPLACEHOLDER",
                              label = "CHARTPLACEHOLDER",
                              style = "height: 420px")),
        uiOutput(outputId = "life_total_row"),
       
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
