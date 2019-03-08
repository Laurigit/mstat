#STG_TURN_DATA_ALL
required_data("SRC_TURN_DATA_ALL")
temp <- SRC_TURN_DATA_ALL
temp[, ':=' (TSID = as.numeric(TSID),
             Peli_ID = as.numeric(Peli_ID),
             posix_aika = as.POSIXct(time_stamp, tz = "EET"))]
STG_TURN_DATA_ALL <- temp
#str(STG_TURN_DATA_ALL)
