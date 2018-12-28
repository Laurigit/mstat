#STG_TURNAUSSAANTO
required_data("SRC_TURNAUSSAANTO")
STG_TURNAUSSAANTO <- SRC_TURNAUSSAANTO[,. (Turnaus_NO = TurnausNo, Divari, lisakortit_per_voitto)]
