required_functions("luecsv")
luettu <- luecsv("pelit.csv")
colnimet <- names(luettu)
colnimet_num <- colnimet[!colnimet %in% c("Aloitus_DT", "Lopetus_DT")]
luettu[, (colnimet_num) := lapply(.SD, as.numeric), .SDcols = colnimet_num]
SRC_PELIT <- luettu



