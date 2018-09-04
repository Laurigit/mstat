#SRC_HUMALA.R
required_functions(c("luecsv", "convSecsToTime"))
temp <- luecsv("humala.csv")
temp[, Puhallus_DT_fix := convSecsToTime(Puhallusaika, Puhalluspvm)]
SRC_HUMALA <- temp
