#SRC_DMG_TURN_ALL
required_functions("luecsv")
files <- data.table(file = dir("./external_files/"))
dmg_files <- files[substr(file, 1, 3) == "dmg"]
total <- NULL
for(looppi in 1:nrow(dmg_files)) {
  total <- rbind(total, luecsv(dmg_files[looppi, file]))
}
SRC_DMG_TURN_ALL <- total
