#tab_lifecounter

combat_DMG_reactive <- reactiveValues()
combat_DMG_reactive$combat_dmg <- TRUE
lifegain_DMG_reactive <- reactiveValues("Lifegain" = FALSE)
reverse_DMG_reacive <- reactiveValues("Reverse_DMG" = FALSE)
amount_DMG_reactive <- reactiveValues("dmg" = NULL, "opp" = TRUE)


observeEvent(input$dmg_settings,{
listz <- input$dmg_settings
print(listz)
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
                       input_TSID = input_TSID,
                       current_dmg = damage_data$data,
                       input_UID_UUSI_PELI = isolate(eR_UID_UUSI_PELI())
  )
  print(tulos)
 
 isolate(amount_DMG_reactive$dmg <- NULL)
 updateCheckboxGroupButtons(session,
                            "dmg_settings",
                            selected = c(""))
  damage_data$data <- tulos
  templife <- calc_life_totals(tulos)
  #validate input
  if(templife$count_missing_rows == 0){
    #write to csv
    write.table(x = tulos,
                file = paste0("./dmg_turn_files/", "current_dmg.csv"),
                sep = ";",
                row.names = FALSE,
                dec = ",")
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
    
    life_totals$data <- templife
  } else if (templife$count_missing_rows == 2) {
    #error
    #show both players input and let them choose the correct.
    input_error$error <- TRUE
    
  }

 waiting_opponent_input$waiting <- !waiting_opponent_input$waiting 
 if( waiting_opponent_input$waiting == TRUE) {
 updateTabsetPanel(session, "lifeBox", selected = "waiting_panel") 
 }
})

observe({
  if(input_error$error == TRUE) {
    #calc error
    choose_input <- calc_life_totals(isolate(damage_data$data))$input_error
    
    shinyalert(title = "Difference in damage input",
               text = paste0(choose_input[, text], collapse = "\n"),
               type = "warning",
               closeOnClickOutside = FALSE,
               closeOnEsc = FALSE,
               showCancelButton = TRUE,
               showConfirmButton = TRUE,
               confirmButtonText = "Accept opponent input",
               cancelButtonText = "My input is correct",
               callbackR = function(x) {
                 new_row <- data.table(user = session$user,
                                       response = x)
                 input_error_response$response <- rbind(input_error_response$response , new_row)
                 print(input_error_response$response)
               }
               )
  } 
})

observe({
  if (waiting_opponent_input$waiting  == FALSE) {
    updateTabsetPanel(session, "lifeBox", selected = "life_input") 
  }
})


output$life_total_row <- renderUI({
  req(life_totals$data)
  print(session$user)
 print(life_totals$data)
  lifedata <- life_totals$data$Lifetotal
  lifetext <- life_totals$data$dmg_text
  fluidRow(column(5,
                  valueBox( lifedata[Omistaja_NM == session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)),
           column(5, offset = 2,
                  valueBox( lifedata[Omistaja_NM != session$user, Life_total], lifetext, icon = NULL, color = "aqua", width = 12)))
})

output$offer_turn <- renderUI({
  turnData$turn
  button_text <- 
  actionButton(inputId = "act_offer_turn",
               label = button_text)
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
