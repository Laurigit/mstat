#STG_CURRENT_TURN
required_data("SRC_CURRENT_TURN")
STG_CURRENT_TURN <- SRC_CURRENT_TURN[1 != 0,
                                     .(TSID = as.numeric(TSID),
                                     Peli_ID = as.numeric(Peli_ID),
                                     time_stamp)]
