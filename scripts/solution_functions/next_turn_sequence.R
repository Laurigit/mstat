next_turn_sequence <- function(current_turn_table) {
  TSID <- current_turn_table[, max(TSID)] + 1
  Peli_ID <- current_turn_table[, max(Peli_ID)]
  time_stamp <- as.character(now(tz = "EET"))
  new_row <- data.table(TSID, Peli_ID, time_stamp)
  appendaa <- cbind(current_turn_table, new_row)
  write.table(x = appendaa,
              file = paste0("./dmg_turn_files/", "current_turn.csv"),
              sep = ";",
              row.names = FALSE,
              dec = ",")
  return(appendaa)
}
