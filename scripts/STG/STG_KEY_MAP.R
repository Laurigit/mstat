#SRC_KEY_MAP
required_data("SRC_KEY_MAP")
temp <- SRC_KEY_MAP
temp[, uft_nappi := utf8ToInt(Nappain), by = Nappain]
STG_KEY_MAP <- temp
