#STG_VAHENNYSKORTIT
required_data("SRC_VAHENNYSKORTIT")
STG_VAHENNYSKORTIT <- SRC_VAHENNYSKORTIT[,. (Turnaus_NO = TurnausNo, vahennyskortit_ykkospick, jakaja)]
