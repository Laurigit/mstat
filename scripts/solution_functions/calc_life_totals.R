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
  #input_current_dmg[, rivi := seq_len(.N)]
#  message("missing rows")

  if (nrow(input_current_dmg) > 0) {
  dmg_table_for_calc <- input_current_dmg[DID >0]
  dmg_table_for_calc[, dmg_pari := ceiling(seq_len(.N) / 2)]
  row_count <-  dmg_table_for_calc[TSID > 0, .(count_rows = .N,
                                      Omistaja_NM = paste0(Input_Omistaja_NM, collapse = ""))
                                     # rows = paste0(rivi, collapse = " ")
                                     , by = .(Amount,
                                      Dmg_source,
                                      Target_player,
                                      Combat_dmg,
                                      TSID,
                                      Peli_ID,
                                      dmg_pari)]
  accepted_rows <- row_count[count_rows %% 2 == 0]
  missing_rows <- row_count[count_rows %% 2 == 1]
  #there are 0, 1 or 2 missing rows.
  row_texts <- NULL

 
  if(nrow(missing_rows) > 0) {
  for (loop_rows in 1:nrow(missing_rows)) {
    missing_rows_own_input <- missing_rows[loop_rows]
    loop_text <-paste0(missing_rows_own_input[, Omistaja_NM],
           ": ",
           missing_rows_own_input[, Dmg_source], " -> ",
           missing_rows_own_input[,Target_player],
           ifelse(missing_rows_own_input[, Combat_dmg] == 1, " @ Cmbt: (", ": ("),
           missing_rows_own_input[,Amount],
           ")")
 #   message("loopissa")
  #  print(missing_rows_own_input)
    loop_dt <- data.table(Omistaja_NM = missing_rows_own_input[, Omistaja_NM], text = loop_text)
    row_texts <- rbind(row_texts, loop_dt)
  }
  }
 count_missing_rows <- nrow(missing_rows)

  
  Tot_dmg <- accepted_rows[, .(Total_damage = sum(Amount) ), by = Target_player]
  Last_dmg <- accepted_rows[nrow(accepted_rows)]
  Last_dmg_text <- paste0(Last_dmg[, Dmg_source], " -> ", Last_dmg[,Target_player],
                          ifelse(Last_dmg[, Combat_dmg] == 1, " @ Cmbt: (", ": ("), Last_dmg[,Amount],
                          ")")
  
  Lifetotal <- Tot_dmg[, .(Life_total = initial_life - Total_damage), by = .(Omistaja_NM = Target_player)]
  starting_lifes<- data.table(Omistaja_NM = c("Lauri", "Martti"), Life_total_start = c(initial_life, initial_life))
  join_lives <- Lifetotal[starting_lifes, on = c("Omistaja_NM")]
  life_result <- join_lives[, Life_total := ifelse(is.na(Life_total), Life_total_start, Life_total)][, Life_total_start := NULL]
  
  res <- NULL
  res$count_missing_rows <- count_missing_rows
  res$input_error <- row_texts
  res$Lifetotal <- life_result
  res$dmg_text <- Last_dmg_text
  } else {
    res <- NULL
    res$Lifetotal <- data.table(Omistaja_NM = c("Lauri", "Martti"), Life_total = c(initial_life, initial_life))
    res$count_missing_rows <- 0
    res$input_error <- ""
    res$dmg_text <- ""
  }
  return(res)
}
