#SRC_KEY_MAP

tulos <- as.data.table(read.csv(paste0("./external_files/", "keymap.csv"),
                                sep = ";",
                                stringsAsFactors = FALSE,
                                dec = ",",
                                colClasses = "character",
                                fileEncoding = "latin1"))
tulos[, Nappain := (gsub("Ó", "\"", Nappain))]


SRC_KEY_MAP <- tulos
