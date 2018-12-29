#SRC_DIVARI
required_functions("luecsv")

luettu <- luecsv("divari.csv")
SRC_DIVARI <- luettu[, .(rivi_id,
               Omistaja = as.numeric(Omistaja),
               Pakka = as.numeric(Pakka),
               Divari = as.numeric(Divari),
               Nimi,
               Omistaja_nimi,
               Picked = as.numeric(Picked),
               Manastack_Deck_ID,
               Json_Prefix,
               Retired,
               Side)]
