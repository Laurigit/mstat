#SRC_TURNAUSSAANTO.R
required_functions("luecsv")
SRC_VAHENNYSKORTIT <- luecsv("vahennyskortit.csv")

SRC_VAHENNYSKORTIT[, ':=' (TurnausNo = as.numeric(TurnausNo),
                          vahennyskortit_ykkospick = as.numeric(vahennyskortit_ykkospick),
                          jakaja = as.numeric(jakaja))]
