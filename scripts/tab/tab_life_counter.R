#tab_lifecounter

combat_DMG_reactive <- reactiveValues()
combat_DMG_reactive$combat_dmg <- TRUE

amount_DMG_reactive <- reactiveValues("dmg" = 0, "opp" = TRUE)
#session <- NULL
#session$user <- "Lauri"
#message(amount_DMG_reactive$dmg, tablifecounter)
observeEvent(input$Lose_1, {
 amount_DMG_reactive$dmg <- 1
 amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- TRUE
})

observeEvent(input$Lose_9, {
  amount_DMG_reactive$dmg <- 9
  amount_DMG_reactive$opp <- TRUE
})


observeEvent(input$Deal_1, {
  amount_DMG_reactive$dmg <- 1
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_2, {
  amount_DMG_reactive$dmg <- 2
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_3, {
  amount_DMG_reactive$dmg <- 3
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_4, {
  amount_DMG_reactive$dmg <- 4
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_5, {
  amount_DMG_reactive$dmg <- 5
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_6, {
  amount_DMG_reactive$dmg <- 6
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_7, {
  amount_DMG_reactive$dmg <- 7
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_8, {
  amount_DMG_reactive$dmg <- 8
  amount_DMG_reactive$opp <- FALSE
})

observeEvent(input$Deal_9, {
  amount_DMG_reactive$dmg <- 9
  amount_DMG_reactive$opp <- FALSE
})


observe({
  print(combat_DMG_reactive$combat_dmg)
  required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_DMG"))
  input_TSID <- ADM_CURRENT_TURN[, max(TSID)] + 1
  tulos <- mark_damage(Amount = amount_DMG_reactive$dmg,
                       Opponent_target = FALSE,
                       Combat_dmg =  isolate(combat_DMG_reactive$combat_dmg),
                       Opponent_source = amount_DMG_reactive$opp,
                       input_session_user = session$user,
                       input_TSID = input_TSID,
                       current_dmg = ADM_CURRENT_DMG,
                       input_UID_UUSI_PELI = isolate(eR_UID_UUSI_PELI())
  )
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
 isolate(amount_DMG_reactive$dmg <- 0)
 #isolate(amount_DMG_reactive$opp <- TRUE)
})


observe({
  print(combat_DMG_reactive$combat_dmg)
  required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_DMG"))
  input_TSID <- ADM_CURRENT_TURN[, max(TSID)] + 1
  tulos <- mark_damage(Amount = 2,
                       Opponent_target = FALSE,
                       Combat_dmg =  combat_DMG_reactive$combat_dmg,
                       Opponent_source = TRUE,
                       input_session_user = session$user,
                       input_TSID = input_TSID,
                       current_dmg = ADM_CURRENT_DMG,
                       input_UID_UUSI_PELI = eR_UID_UUSI_PELI()
  )
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
})

observeEvent(input$Deal_Non_combat, {
  
  if (combat_DMG_reactive$combat_dmg == FALSE) {
    updateActionButton(inputId = Deal_Non_combat,
                       label = "Deal combat dmg")
    combat_DMG_reactive$combat_dmg <- TRUE
  } else {
    updateActionButton(inputId = Deal_Non_combat,
                       label = "Non-combat damage")
    combat_DMG_reactive$combat_dmg <- FALSE
  }
  
  
})
