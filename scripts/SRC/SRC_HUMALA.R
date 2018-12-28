#SRC_HUMALA.R
required_functions(c("luecsv", "convSecsToTime"))
temp <- luecsv("humala.csv")
temp[, ':=' (Puhallisaika_num = as.numeric(Puhallusaika),  Puhalluspvm_num = as.numeric(Puhalluspvm))]
temp[, Puhallus_DT_fix := convSecsToTime(Puhallisaika_num, Puhalluspvm_num)]
temp[, ':=' (Laurin_humala = as.numeric(gsub(",",".", Laurin_humala)),
             Martin_humala = as.numeric(gsub(",", ".", Martin_humala)))]
SRC_HUMALA <- temp

