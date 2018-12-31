#tab_lifecounter

combat_DMG_reactive <- reactiveValues()
combat_DMG_reactive$combat_dmg <- TRUE

#session <- NULL
#session$user <- "Lauri"
observeEvent(input$Lose_1, {
  print(combat_DMG_reactive$combat_dmg)
  required_data(c("ADM_CURRENT_TURN", "ADM_CURRENT_DMG"))
  input_TSID <- ADM_CURRENT_TURN[, max(TSID)] + 1
  tulos <- mark_damage(Amount = 1,
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
