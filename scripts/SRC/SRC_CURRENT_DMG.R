#SRC_CURRENT_DMG

if(!file.exists("./dmg_turn_files/current_dmg.csv")) {
  
  new_row <- data.table(DID = 0,
                        Amount = 0,
                        Target_player = "",
                        Dmg_source = "",
                        Combat_dmg = "",
                        Input_Omistaja_NM = "",
                        TSID = 0,
                        Peli_ID = 0)
  
  write.table(x = new_row,
              file = paste0("./dmg_turn_files/", "current_dmg.csv"),
              sep = ";",
              row.names = FALSE,
              dec = ",")
} else {
  new_row <- as.data.table(read.csv(paste0("./dmg_turn_files/", "current_dmg.csv"),
                                      sep = ";",
                                      stringsAsFactors = FALSE,
                                      dec = ",",
                                      colClasses = "character",
                                      fileEncoding = "UTF-8"))
}
SRC_CURRENT_DMG <- new_row
