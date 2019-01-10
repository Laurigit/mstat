#tab_lifecounter

combat_DMG_reactive <- reactiveValues()
combat_DMG_reactive$combat_dmg <- TRUE
lifegain_DMG_reactive <- reactiveValues("Lifegain" = FALSE)
reverse_DMG_reacive <- reactiveValues("Reverse_DMG" = FALSE)
amount_DMG_reactive <- reactiveValues("dmg" = NULL, "opp" = TRUE)
waiting_opponent_input <- reactiveValues(waiting = FALSE)

observeEvent(input$dmg_settings,{
listz <- input$dmg_settings
#print(listz)
if ("Lifegain" %in% listz) {
  lifegain_DMG_reactive$Lifegain <- TRUE
} else {
  lifegain_DMG_reactive$Lifegain <- FALSE
}

if ("Non-combat damage" %in% listz) {
  combat_DMG_reactive$combat_dmg <- FALSE
} else {
  combat_DMG_reactive$combat_dmg <- TRUE
}


if ("Reverse Source" %in% listz) {
  reverse_DMG_reacive$Reverse_DMG <- TRUE
} else {
  reverse_DMG_reacive$Reverse_DMG <- FALSE
}
  
})
#session <- NULL
#session$user <- "Lauri"
#message(amount_DMG_reactive$dmg, tablifecounter)
observeEvent(input$Lose_1, {
 amount_DMG_reactive$dmg <- 1
 amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Lose_9, {
  amount_DMG_reactive$dmg <- 9
  amount_DMG_reactive$opp <- FALSE
})


observeEvent(input$Deal_1, {
  amount_DMG_reactive$dmg <- 1
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Deal_9, {
  amount_DMG_reactive$dmg <- 9
  amount_DMG_reactive$opp <- TRUE
})


observe({
  #Amount damage should be the only thing triggering this
  req(amount_DMG_reactive$dmg)
  required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_DMG"))
  input_TSID <- ADM_CURRENT_TURN[, max(TSID)] + 1
  isolate(
    if(lifegain_DMG_reactive$Lifegain == TRUE) {
      life_kerroin <- -1
    } else {
      life_kerroin <- 1
    }
  )
  #uuspeli <- data.table(Omistaja_NM = c("Lauri", "Martti"), Peli_ID_input = 1033)
  #testitulos <- mark_damage(3, "Lauri", 1, TRUE, "Lauri", 1, ADM_CURRENT_DMG, uuspeli)
  tulos <- mark_damage(Amount = amount_DMG_reactive$dmg * life_kerroin,
                       Opponent_target = isolate(amount_DMG_reactive$opp),
                       Combat_dmg =  isolate(combat_DMG_reactive$combat_dmg),
                       Reverse_source = isolate(reverse_DMG_reacive$Reverse_DMG),
                       input_session_user = session$user,
                       input_TSID = isolate(turnData$turn),
                       current_dmg = damage_data$data,
                       input_UID_UUSI_PELI = isolate(eR_UID_UUSI_PELI())
  )
 # print(tulos)
 
 isolate(amount_DMG_reactive$dmg <- NULL)
 updateCheckboxGroupButtons(session,
                            "dmg_settings",
                            selected = c(""))
  damage_data$data <- tulos
 

 #waiting_opponent_input$waiting <- !waiting_opponent_input$waiting 
 
})


#observeEvent damage_data$data 
#handles changed in damagedate
observe({
  #jos muuttuu, niin validoi ja kirjota, jos validi
 # print(damage_data$data)
  templife <- calc_life_totals(damage_data$data)
  # print(templife)
  #validate input
  if(templife$count_missing_rows == 0){
    print("kirjotetaan csv")
    print(damage_data$data)
    #write to csv
    write.table(x = damage_data$data,
                file = paste0("./dmg_turn_files/", "current_dmg.csv"),
                sep = ";",
                row.names = FALSE,
                dec = ",")
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
    
   
    life_totals$data <- templife
    input_error$error <- FALSE
    waiting_opponent_input$waiting <- FALSE
  } else if (templife$count_missing_rows == 2) {
    #error
    #show both players input and let them choose the correct.
    input_error$error <- TRUE
    message("input error",  input_error$error)
    waiting_opponent_input$waiting <- FALSE
    
  } else if (tail(damage_data$data, 1)[, Input_Omistaja_NM] == session$user ) {
    #if I did the input, then I am waiting opponent
    waiting_opponent_input$waiting <- TRUE
  }
  
  if ( waiting_opponent_input$waiting == TRUE) {
    updateTabsetPanel(session, "lifeBox", selected = "waiting_panel") 
  }
})

#obseveEVent input_error$error
observe({
  if (input_error$error == TRUE) {
    #calc error
    #debug
    #damage_data <- NULL
    #damage_data$data <- ADM_CURRENT_DMG
   # print("dmg data")
 #  print(damage_data$data)
    choose_input <- calc_life_totals(isolate(damage_data$data))$input_error
  #  print("choose_input")
  # print(choose_input)

    #print(damage_data_for_observe)
   # print(input_error_response$response)
    shinyalert(callbackR = function(x) {
      new_row <- data.table(user = session$user, response = x, done = FALSE)
      input_error_response$response <- (rbind(input_error_response$response , new_row))
    },
              title = "Difference in damage input",
               text = paste0(choose_input[, text], collapse = "\n"),
               type = "warning",
               closeOnClickOutside = FALSE,
               closeOnEsc = FALSE,
               showCancelButton = TRUE,
               showConfirmButton = TRUE,
               confirmButtonText = "Accept opponent input",
               cancelButtonText = "My input is correct")
  }
 # print( input_error_response$response)
})
               
 
    #tää jäi loooppiin, pitäs ehkä siirtää niin, että input$shiny alert on omassa observessa ja se muuttaa input_errorresponsen arvoja
    #print( input_error_response$response )
  observe({
    req(input_error_response$response)
    damage_data_for_observe <- isolate(damage_data$data)[DID > 0]
                 if (nrow(input_error_response$response) == 2) {
                   if (input_error_response$response[1, response] != input_error_response$response[2, response]
                       ) {
                     print("TRUEN puoelella")
                     #case inputit meni oikein eli oltiin samaa mieltä virheestä
                     ##tallenna valittu inputti
                     ###tuhoa oma viimeisin ja lisää sinne vihun asetukset omalla inputilla
                     
                     my_response <- input_error_response$response[user  == session$user, response]
                     if (my_response == TRUE) {
                     #  "Accept opponent input"
                       # damage_data_for_observe <- ADM_CURRENT_DMG
                       # session <- NULL
                       # session$user <- "Lauri"
                       damage_undone <- isolate(undo_damage(damage_data_for_observe, session$user))

                       row_to_copy <- damage_undone[which.max(DID)]
                       row_to_copy[, ':=' (Input_Omistaja_NM = session$user,
                                           DID = max(DID) + 1)]
                       damage_data$data <- rbind(damage_undone, row_to_copy)
                       
                     }
                     #debug
                     #input_error_response <- NULL
                     #input_error_response$response <- data.table(user = c("Lauri", "Martti"), response = c(TRUE, FALSE))
                    
                     # tuhottava <- damage_data_for_observe[Input_Omistaja_NM ==
                     #                    input_error_response$response[response ==
                     #                                                    FALSE,
                     #                                                  user], .(DID = max(DID)), by = .(Input_Omistaja_NM)]
                     # message("Tuhottava rivi", tuhottava)

                     
                     # print(data_mihin_lisatty)
                     # life_totals$data <- calc_life_totals(data_mihin_lisatty)
                     
                   } else {
                   
                     #case inputit meni väärin
                     #tuhoa oma viimeisin input ja ilmoita pelaajalle
                   #  print("elsen puolella")
                     tuhottava <- damage_data_for_observe[Input_Omistaja_NM == session$user, .(DID = max(DID))]
                #   print(tuhottava)
                    damage_data$data <- damage_data_for_observe[!DID %in% tuhottava[, DID]]
                   # print(damage_data$data)
                   #  life_totals$data <- isolate(calc_life_totals(damage_data$data))
                     #print( life_totals$data)
                     shinyalert(title = "Damage disagreement",
                                text = "Both inputs have been deleted. Please input damage again",
                                type = "error",
                                closeOnClickOutside = TRUE,
                                closeOnEsc = TRUE,
                                showCancelButton = TRUE,
                                showConfirmButton = TRUE,
                                confirmButtonText = "Sorry, my bad",
                                cancelButtonText = "He's a drunk idiot")
                   }
                   #when second user confirms, delete global var row
                 
                   if (input_error_response$response[1, done ] == FALSE) {
                      isolate(input_error_response$response[1, done := TRUE ])
                   } else {
                     input_error_response$response <- NULL
                   }
                 }
  
  
})

output$debug_text <- renderText({
 resp <-  paste0("input_error_response: ", input_error$response, "\n",
         "waiting_opponent_input: ", waiting_opponent_input$waiting, "\n",
         "input_error: ", input_error$error, "\n",
         "turnData: ", turnData$turn
       #  "damage_data: ", damage_data$data, "\n"
         )
 print(resp)
 return(resp)

})



  
  
  
observe({
  if (waiting_opponent_input$waiting  == FALSE) {
    updateTabsetPanel(session, "lifeBox", selected = "life_input") 
  }
})


output$life_total_row <- renderUI({
  req(life_totals$data)
 # print(session$user)
# print(life_totals$data)
  lifedata <- life_totals$data$Lifetotal
  lifetext <- life_totals$data$dmg_text
  fluidRow(column(5,
                  valueBox( lifedata[Omistaja_NM == session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)),
           column(5, offset = 2,
                  valueBox( lifedata[Omistaja_NM != session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)))
})

observeEvent(input$ab_Vaihda_vuoro, {
  
  turnData$turn <- ADM_TURN_SEQ[TSID == turnData$turn, Next_turn_TSID]
})

observeEvent(input$ab_pakita_endille, {
  turnData$turn <- turnData$turn - 1
})

observe({
  Aloittaja <- isolate(eR_UID_UUSI_PELI()[Aloittaja == 1, Omistaja_NM])
 if (Aloittaja == session$user) {
   I_start <- TRUE
 } else {
   I_start <- FALSE
 }
  print(session$user)
  message("I_START", I_start)
  required_data("ADM_TURN_SEQ")
  my_turn <- I_start == ADM_TURN_SEQ[TSID == turnData$turn, Starters_turn]
  end_phase <-  ADM_TURN_SEQ[TSID == turnData$turn, End_phase]
  message("my_turn", my_turn)
  if (my_turn == 1 & end_phase == FALSE) {
    shinyjs::enable("ab_Vaihda_vuoro")
    shinyjs::enable("ab_pakita_endille")
  } else if  (my_turn == 1 & end_phase == TRUE) {
    shinyjs::enable("ab_Vaihda_vuoro")
    shinyjs::disable("ab_pakita_endille")
  } else {
    shinyjs::disable("ab_Vaihda_vuoro")
    shinyjs::disable("ab_pakita_endille") 
  }
  
  
  #my_turn <- ifelse((turnData$turn) - I_start)
 # if( )
})

# 
# observeEvent(input$Deal_Non_combat, {
#   
#   if (combat_DMG_reactive$combat_dmg == FALSE) {
#     updateActionButton(session,
#                        inputId = "Deal_Non_combat",
#                        label = "Deal combat dmg")
#     combat_DMG_reactive$combat_dmg <- TRUE
#   } else {
#     updateActionButton(session,
#                        inputId = "Deal_Non_combat",
#                        label = "Non-combat damage")
#     combat_DMG_reactive$combat_dmg <- FALSE
#   }
# })
# 
# 
# observeEvent(input$Lifegain, {
#   if (lifegain_DMG_reactive$Lifegain == FALSE) {
#     updateActionButton(session,
#                        inputId = "Lifegain",
#                        label = "Lifeloss selected")
#     lifegain_DMG_reactive$Lifegain <- TRUE
#   } else {
#     updateActionButton(session,
#                        inputId = "Lifegain",
#                        label = "Lifegain selected")
#     lifegain_DMG_reactive$Lifegain <- FALSE
#   }
#     
# })
# observeEvent(input$Reverse_source, {
#   if (reverse_DMG_reacive$Reverse_DMG == FALSE) {
#     updateActionButton(session,
#                        inputId = "Reverse_source",
#                        label = "Normal source selected")
#     reverse_DMG_reacive$Reverse_DMG <- TRUE
#   } else {
#     updateActionButton(session,
#                        inputId = "Reverse_source",
#                        label = "Reverse source selected")
#     reverse_DMG_reacive$Reverse_DMG <- FALSE
#   }
#   
# })
