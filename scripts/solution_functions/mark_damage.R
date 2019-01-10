mark_damage <- function(Amount,
                        Opponent_target,
                        Combat_dmg,
                        Reverse_source,
                        input_session_user,
                        input_TSID,
                        current_dmg,
                        input_UID_UUSI_PELI
                        ){

  Opponent_NM <- input_UID_UUSI_PELI[Omistaja_NM != input_session_user, Omistaja_NM]

  if (Opponent_target == TRUE) {
    Target_player <- Opponent_NM
  } else {
    Target_player <- input_session_user
  }
#  print(paste0("target_player=", Target_player ))
  
  #defaul_dmg_source != Target
  Dmg_source <- input_UID_UUSI_PELI[Omistaja_NM != Target_player, Omistaja_NM]
  
  if (Reverse_source == TRUE) {
    Dmg_source <- input_UID_UUSI_PELI[Omistaja_NM != Dmg_source, Omistaja_NM]
  }
  
  #if lifegain, then normal source = reverse. So reverse again
  if (Amount < 0) {
    Dmg_source <- input_UID_UUSI_PELI[Omistaja_NM != Dmg_source, Omistaja_NM]
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
  
  return(appendaa)
}
