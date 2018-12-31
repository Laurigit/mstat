mark_damage <- function(Amount,
                        Opponent_target,
                        Combat_dmg,
                        Opponent_source,
                        input_session_user,
                        input_TSID,
                        current_dmg,
                        input_UID_UUSI_PELI
                        ){

  Opponent_NM <- input_UID_UUSI_PELI[Omistaja_NM != input_session_user, Omistaja_NM]
  if (Opponent_target == TRUE) {
    Target_player <- Opponent_target
  } else {
    Target_player <- input_session_user
  }
  
  if (Opponent_source == TRUE) {
    Dmg_source <- Opponent_NM
  } else {
    Dmg_source <- input_session_user
  }
  
  Peli_ID <- input_UID_UUSI_PELI[, max(Peli_ID_input)]
  max_DID <- current_dmg[, max(DID)]
  new_row <- data.table(DID = max_DID + 1,
                        Amount,
                        Target_player,
                        Dmg_source,
                        Combat_dmg,
                        Input_Omistaja_NM = input_session_user,
                        TSID = input_TSID,
                        Peli_ID)
  appendaa <- rbind(current_dmg, new_row)
  write.table(x = appendaa,
             file = paste0("./dmg_turn_files/", "current_dmg.csv"),
             sep = ";",
             row.names = FALSE,
             dec = ",")
  return(appendaa)
}
