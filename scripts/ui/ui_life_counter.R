tabItem(tabName = "tab_LifeCounter",
        fluidPage(
         #fluidRow(    box(id = "hideBox", title = "SWITH", background = "maroon")),
        #  fluidRow(  verbatimTextOutput("results")),
          fluidRow(plotOutput(outputId = "lifeChart")),
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
          
          tabBox(id = "lifeBox", 
                 title = NULL,
                 width = 12,
                 height = "365px",
                 tabPanel(value = "life_input",
                          title = "Input life",
                          
                          
                          fluidRow(
                            box(
                              fluidRow(column(width = 4,
                                              actionButton(inputId = "Lose_1",
                                                           label = "1",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_2",
                                                           label = "2",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_3",
                                                           label = "3",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;"))),
                              fluidRow(column(width = 4,
                                              actionButton(inputId = "Lose_4",
                                                           label = "4",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_5",
                                                           label = "5",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_6",
                                                           label = "6",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;"))),
                              fluidRow(column(width = 4,
                                              actionButton(inputId = "Lose_7",
                                                           label = "7",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_8",
                                                           label = "8",
                                                           width = '100%',
                                                           style = "font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px;")),
                                       column(width = 4,
                                              actionButton(inputId = "Lose_9",
                                                           label = HTML("9+"),
                                                           width = '100%',
                                                           style = " font-size:250%; color: #fff; background-color: #b73338; border-color: #2e6da4; height: 100px"))
                                       
                              )
                            ),
                            box(
                              fluidRow(
                                
                                
                                column(width = 4,
                                       actionButton(inputId = "Deal_1",
                                                    label = "1",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_2",
                                                    label = "2",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_3",
                                                    label = "3",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%; "))
                              ),
                              fluidRow(
                                column(width = 4,
                                       actionButton(inputId = "Deal_4",
                                                    label = "4",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_5",
                                                    label = "5",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_6",
                                                    label = "6",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;"))
                              ),
                              fluidRow(
                                column(width = 4,
                                       actionButton(inputId = "Deal_7",
                                                    label = "7",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_8",
                                                    label = "8",
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;")),
                                column(width = 4,
                                       actionButton(inputId = "Deal_9",
                                                    label = HTML("9+"),
                                                    width = '100%',
                                                    style = "height: 100px; font-size:250%;"))
                              )))),
                 tabPanel(value = "NinePlusPanel",
                          title = "9+ Life",
                          fluidRow(
                            box(width = 8,
                                fluidRow(column(width = 3,
                                                actionButton(inputId = "Edit_1",
                                                             label = "1",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_2",
                                                             label = "2",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_3",
                                                             label = "3",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "backSpace",
                                                             label = "Delete",
                                                             width = '100%',
                                                             style = "height: 100px;"))),
                                fluidRow(column(width = 3,
                                                actionButton(inputId = "Edit_4",
                                                             label = "4",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_5",
                                                             label = "5",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_6",
                                                             label = "6",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "ab_fix_lifes",
                                                             label = "Edit turn",
                                                             width = '100%',
                                                             style = "height: 100px;"#tästä jäi ehkä sulku pois
                                                ))),
                                fluidRow(column(width = 3,
                                                actionButton(inputId = "Edit_7",
                                                             label = "7",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_8",
                                                             label = "8",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_9",
                                                             label = "9",
                                                             width = '100%',
                                                             style = "height: 100px;")),
                                         column(width = 3,
                                                actionButton(inputId = "Edit_0",
                                                             label = "0",
                                                             width = '100%',
                                                             style = "height: 100px;"))
                                         
                                )
                            ),
                            box(width = 4,
                                fluidRow(uiOutput(outputId = "value_type_life")),
                                fluidRow(radioGroupButtons(inputId = "editTurnOrLife",
                                                           direction = "horizontal",
                                                           justified = TRUE,
                                                           individual = TRUE,
                                                           label = "Input type",
                                                           choices = c("Life", 
                                                                       "Turn"), 
                                                           status = "primary",
                                                           checkIcon = list(
                                                             yes = icon("ok", 
                                                                        lib = "glyphicon"),
                                                             no = icon("remove",
                                                                       lib = "glyphicon")))),
                                fluidRow(
                                  
                                  radioGroupButtons(inputId = "isMyTurn", 
                                                    justified = TRUE,
                                                    individual = TRUE,
                                                    choiceNames = c("My turn", 
                                                              "Opponent turn"), 
                                                    choiceValues = c(TRUE, FALSE),
                                                    status = "primary",
                                                    checkIcon = list(
                                                      yes = icon("ok", 
                                                                 lib = "glyphicon"),
                                                      no = icon("remove",
                                                                lib = "glyphicon")))
                                  
                                ),
                                fluidRow(
                                  radioGroupButtons(
                                    inputId = "isEndStep",
                                    justified = TRUE,
                                    individual = TRUE,
                                    checkIcon = list(
                                      yes = icon("ok", 
                                                 lib = "glyphicon"),
                                      no = icon("remove",
                                                lib = "glyphicon")),
                                    
                                    choiceNames = c("Main phase", 
                                                              "End step"), 
                                    choiceValues = c(FALSE, TRUE),
                                      status = "primary")),
                                fluidRow(actionButton(inputId = "save_9_damage",
                                                      label = "Save damage",
                                                      width = '100%',
                                                      style = "height: 40px;" )))
                          )),
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
                 ),
                 tabPanel(value = "dmg_rows_panel",
                          title = "damage rows",
                          fluidPage(
                            fluidRow(
                            actionButton(inputId = "Delete_dmg_row",
                                         label = "Delete selected row"),
                          fluidRow(
                            dataTableOutput("damage_rows_dt")
                          )
                          
                          )
                 ))),
          
        #  fluidRow(h3(textOutput("debug_text"))),
          
          uiOutput(outputId = "life_total_row"),
        fluidRow(
          
          column(4,
                 
                 actionButton(inputId = "ab_Vaihda_vuoro",
                              label =   "End turn",
                              style = "font-size:250%; color: #fff; background-color: #FF007F; border-color: #2e6da4; height: 87px;",
                              width = '100%')),
          column(2,
                 
                 actionButton(inputId = "ab_Vaihda_vuoro_virhe",
                              label = HTML("End turn <br> add mistake"),
                              style = "font-size:150%; color: #fff; background-color: #000080; border-color: #2e6da4; height: 87px;",
                              width = '100%')
          ),
          
          
          
          column(2,
              
                   actionButton(inputId = "ab_Undo",
                                label = HTML("Undo input <br> (not delete)"),
                                style = "font-size:150%; color: #fff; background-color: #000080; border-color: #2e6da4; height: 87px;",
                                width = '100%')
                   
                 
          ),
          column(4,
                 #tähä ui output
                 uiOutput("dynamic_turn_box")
                 
          )
        )
          
          
         
        )
)
