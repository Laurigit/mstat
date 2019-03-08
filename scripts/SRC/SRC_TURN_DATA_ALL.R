#SRC_TURN_DATA_ALL
required_functions("luecsv")
files <- data.table(file = dir("./external_files/"))
dmg_files <- files[substr(file, 1, 5) == "turn_"]
total <- NULL
for(looppi in 1:nrow(dmg_files)) {
  total <- rbind(total, luecsv(dmg_files[looppi, file]))
}
SRC_TURN_DATA_ALL <- total
