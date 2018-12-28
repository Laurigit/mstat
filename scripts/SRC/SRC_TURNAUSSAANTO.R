#SRC_TURNAUSSAANTO.R
required_functions("luecsv")
SRC_TURNAUSSAANTO <- luecsv("turnaussaanto.csv")
SRC_TURNAUSSAANTO[, ':=' (TurnausNo = as.numeric(TurnausNo),
                          Divari = as.numeric(Divari),
                          lisakortit_per_voitto = as.numeric(lisakortit_per_voitto))]
