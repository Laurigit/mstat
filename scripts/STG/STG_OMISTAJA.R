#STG_OMISTAJA.R
required_data("SRC_DIVARI")
temp <- SRC_DIVARI[, .(Omistaja_ID = substr(Omistaja_nimi, 1,1), Omistaja_NM = Omistaja_nimi)]
STG_OMISTAJA <- temp[, .N, by = .(Omistaja_ID, Omistaja_NM)][, N := NULL]
               
