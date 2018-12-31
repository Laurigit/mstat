#SRC_CURRENT_TURN
if(!file.exists("./dmg_turn_files/current_turn.csv")) {
 
  res_table <- data.table(TSID = 0,
                               Peli_ID =0,
                               time_stamp = now())
  
  write.table(x = res_table,
              file = paste0("./dmg_turn_files/", "current_turn.csv"),
              sep = ";",
              row.names = FALSE,
              dec = ",")
} else {
  res_table <- as.data.table(read.csv(paste0("./dmg_turn_files/", "current_turn.csv"),
                                  sep = ";",
                                  stringsAsFactors = FALSE,
                                  dec = ",",
                                  colClasses = "character",
                                  fileEncoding = "UTF-8"))
}
SRC_CURRENT_TURN <- res_table
