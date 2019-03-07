#STG_DMG_TURN_ALL
required_data("SRC_DMG_TURN_ALL")
temp <- SRC_DMG_TURN_ALL
temp[, Peli_ID := as.numeric(Peli_ID)]
temp[, TSID := as.integer(TSID)]
temp[, Amount := as.integer(Amount)]
STG_DMG_TURN_ALL <- temp
