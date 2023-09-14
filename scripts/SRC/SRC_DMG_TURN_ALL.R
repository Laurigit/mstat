#SRC_DMG_TURN_ALL
required_functions("luecsv")
files <- data.table(file = dir("./external_files/"))
dmg_files <- files[substr(file, 1, 3) == "dmg"]
total <- NULL
for(looppi in 1:nrow(dmg_files)) {
  
  loop_data <- luecsv(dmg_files[looppi, file])
  #lisätty dmg_ttimestamp proxyprelluja varten. csv-filuit ennen sitä ei sisällä tuota timestamp sarkaetta. Sen jälkeen sisältää. Nullataan se pois.
  loop_data[, dmg_timestamp := NULL]
  total <- rbind(total, loop_data)
}
SRC_DMG_TURN_ALL <- total
