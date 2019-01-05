#calc_life_totals

#uuspeli <- data.table(Omistaja_NM = c("Lauri", "Martti"), Peli_ID_input = 1033)
#testitulos <- mark_damage(3, "Lauri", 1, TRUE, "Lauri", 1, ADM_CURRENT_DMG, uuspeli) 
required_data("ADM_CURRENT_DMG")
calc_life_totals <- function(input_current_dmg, initial_life = 20) {
  #input_current_dmg <- ADM_CURRENT_DMG
  # aggr_to_turn <- input_current_dmg[TSID > 0, .(Amount = sum(Amount)), by = .(
  #                                                 Dmg_source,
  #                                                 Target_player,
  #                                                 Combat_dmg,
  #                                                 TSID,
  #                                                 Peli_ID,
  #                                                 Input_Omistaja_NM
  #                                                )]
  row_count <-  input_current_dmg[, .(count_rows = .N), by = .(Amount,
                                      Dmg_source,
                                      Target_player,
                                      Combat_dmg,
                                      TSID,
                                      Peli_ID)]
  accepted_rows <- row_count[count_rows %% 2 == 0]
  
  Tot_dmg <- accepted_rows[, .(Total_damage = sum(Amount) ), by = Target_player]
  Lifetotal <- Tot_dmg[, .(Life_total = initial_life - Total_damage), by = .(Omistaja_NM = Target_player)]
  return(Lifetotal)
}
