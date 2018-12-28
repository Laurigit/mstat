#SRC_BOOSTERS.
required_functions("luecsv")
temp <- luecsv("boosters.csv")
korjaa_vanha_aikamuoto <- temp[!is.na(muutos), .(setti,
                                       muutos,
                                       Aikaleima,
                                       Aikaleima_V1 = as.POSIXct(strptime(Aikaleima, format = "%Y-%m-%d"), tz = "EET"),
                                       draft_no)]
korjaa_vanha_aikamuoto[, Aikaleima := ifelse(is.na(Aikaleima_V1), as.character(as.POSIXct(strptime(Aikaleima, format = "%d.%m.%Y"))),Aikaleima)]
korjaa_vanha_aikamuoto[, Aikaleima_V1 := NULL]
korjaa_vanha_aikamuoto[, ':=' (muutos = as.numeric(muutos),
                              draft_no = as.numeric(draft_no))]
SRC_BOOSTERS <- korjaa_vanha_aikamuoto
