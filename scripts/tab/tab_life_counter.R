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


if ("Reverse Sourcee" %in% listz) {
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
                       current_dmg = ADM_CURRENT_DMG,
                       input_UID_UUSI_PELI = isolate(eR_UID_UUSI_PELI())
  )
  print(tulos)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
 isolate(amount_DMG_reactive$dmg <- NULL)
 
 life_totals$data <- calc_life_totals(ADM_CURRENT_DMG)
 
 #disabloi kaikki napit
 # shinyjs::disable("Lose_1")
 # shinyjs::disable("Lose_2")
 # shinyjs::disable("Lose_3")
 # shinyjs::disable("Lose_4")
 # shinyjs::disable("Lose_5")
 # shinyjs::disable("Lose_6")
 # shinyjs::disable("Lose_7")
 # shinyjs::disable("Lose_8")
 # shinyjs::disable("Lose_9")
 # shinyjs::disable("Deal_1")
 # shinyjs::disable("Deal_2")
 # shinyjs::disable("Deal_3")
 # shinyjs::disable("Deal_4")
 # shinyjs::disable("Deal_5")
 # shinyjs::disable("Deal_6")
 # shinyjs::disable("Deal_7")
 # shinyjs::disable("Deal_8")
 # shinyjs::disable("Deal_9")

 # updateActionButton(session, "Deal_1", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_2", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_3", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_4", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_5", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_6", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_7", icon = icon("times-circle")) 
 # updateActionButton(session, "Deal_8", icon = icon("times-circle"))     
 # updateActionButton(session, "Deal_9", icon = icon("times-circle"))
          
 updateTabsetPanel(session, "lifeBox", selected = "waiting_panel") 
 
})




output$life_total_row <- renderUI({
  req(life_totals$data)
 print(life_totals$data)
  
  fluidRow(column(5,
                  valueBox( life_totals$data[Omistaja_NM == "Lauri", Life_total], -2, icon = NULL, color = "aqua", width = 12)),
           column(5, offset = 2,
                  valueBox( life_totals$data[Omistaja_NM == "Martti", Life_total], -2, icon = NULL, color = "aqua", width = 12)))
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
