#STG_DIVARI
required_data("SRC_DIVARI")
STG_DIVARI <- SRC_DIVARI[ ,.(Pakka_ID = rivi_id,
                             Divari = (Divari),
                             Picked,
                             Manastack_Deck_ID,
                             Json_Prefix,
                             Deck_name = Nimi,
                             Side,
                             Retired)]
