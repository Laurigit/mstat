#STG_PAKAT
required_data("SRC_DIVARI", TRUE)
STG_PAKAT <- SRC_DIVARI[,. (Pakka_ID = rivi_id,
                            Omistaja_ID = substr(Omistaja_nimi,1,1),
                            Pakka_NO = as.numeric(Pakka),
                            Pakka_NM = Nimi,
                            Retired,
                            Side,
                            Picked,
                            Manastack_Deck_ID,
                            Manastack_name_url)]
save(list = "STG_PAKAT", file = "../common_data/STG_PAKAT.RData")
