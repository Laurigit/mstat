#STG_PFI
required_data(c("SRC_CARDS", "SRC_DIVARI"))
#datetime, Pakka_ID, kortti_lkm, hinnat,
pakkametataulu_uus <- SRC_CARDS[, .(Kortti_lkm_manastack = sum(Maindeck),
                                    Valid_from_DT = max(Valid_from_DT)), by = .(Pakka_form_ID, Pakka_ID)]
pakkametataulu_uus[, ':=' (Hinta = 1,
                           Hinta_low = 1,
                           Hinta_high = 1,
                           Valid_from_DT = as.POSIXct(Valid_from_DT, tz ="EET"))]



pakka_meta_sorted <- pakkametataulu_uus[order(Pakka_ID, Valid_from_DT)]
pakka_meta_sorted[,':=' (Valid_to_DT = shift(Valid_from_DT,1,type="lead")),
                  by = .(
                    Pakka_ID)]
pakka_meta_sorted[,':=' (Valid_to_DT = as.POSIXct((ifelse(is.na(Valid_to_DT),
                                                          as.POSIXct("2100-01-01", tz = "EET"),
                                                          Valid_to_DT)), tz = "EET", origin = "1970-01-01"))]
pakka_meta_sorted[, Current_Pakka_form_ID := max(Pakka_form_ID), by = Pakka_ID]
STG_PFI <- pakka_meta_sorted 
