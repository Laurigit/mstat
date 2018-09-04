#STG_HUMALA
required_data("SRC_HUMALA")
temp <- SRC_HUMALA[, .(Laurin_humala,
                       Martin_humala,
                       Puhallus_DT = as.POSIXct(ifelse(is.na(Puhallus_DT), as.character(Puhallus_DT_fix), Puhallus_DT),
                                                tz = "EET",
                                                origin = "1970-01-01"))]
meltattu <- melt.data.table(temp, id.vars = "Puhallus_DT", measure.vars = c("Laurin_humala", "Martin_humala"))
STG_HUMALA <- meltattu[value >= 0, .(Puhallus_DT,
                      Omistaja_ID = ifelse(variable == "Laurin_humala", "L", "M"),
                      Humala = value)]
