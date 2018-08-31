#STG_PAKAT
required_data("SRC_DIVARI")
STG_PAKAT <- SRC_DIVARI[,. (Pakka_ID = rivi_id,
                            Omistaja_ID = Omistaja,
                            Pakka_NO = Pakka,
                            Pakka_NM = Nimi)]
