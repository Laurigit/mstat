required_functions("luecsv")
luettu <- luecsv("pelit.csv")
colnimet <- names(luettu)
colnimet_num <- colnimet[!colnimet %in% c("Aloitus_DT", "Lopetus_DT")]
luettu[, (colnimet_num) := lapply(.SD, as.numeric), .SDcols = colnimet_num]
SRC_PELIT <- luettu
# if(GLOBAL_test_mode == "dev") {
#   riveja <- nrow(SRC_PELIT)
#   SRC_PELIT <- SRC_PELIT[(riveja - 100):riveja]
# 
# }



