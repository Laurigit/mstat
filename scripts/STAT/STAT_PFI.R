required_data(c("STAT_LISAKORTIT", "ADM_PELIT", "STG_PAKAT", "INT_PFI", "STG_PFI", "STAT_VAHENNYSKORTIT"))

current_manastack_cards <- INT_PFI[Pakka_form_ID == Current_Pakka_form_ID,. (Manastack_Cards = Kortti_lkm_manastack, Pakka_ID)]
maxturnaus_per_pakka <- STAT_LISAKORTIT[, .(Turnaus_NO = max(Turnaus_NO)), by = .(Pakka_ID)]

onlymax <- STAT_LISAKORTIT[maxturnaus_per_pakka, on = c("Turnaus_NO", "Pakka_ID")]

current_pfi_by_pakka <- STG_PFI[Pakka_form_ID == Current_Pakka_form_ID, .(Current_Pakka_form_ID, Pakka_ID)]

bo_conv_pelit <- BO_conversio(ADM_PELIT)
sscols_pelit <- bo_conv_pelit[,. (Pakka_ID, Voittaja, Tasapeli, Peli_LKM, Pakka_form_ID)]

nykypakan_tilastot <- sscols_pelit[current_pfi_by_pakka, on = .(Pakka_ID)][Pakka_form_ID >= Current_Pakka_form_ID]

aggr_to_pakka <- nykypakan_tilastot[ , .(Voitot = sum(Voittaja * Peli_LKM, na.rm = TRUE),
                                         Tasapelit = sum(Tasapeli * Peli_LKM, na.rm = TRUE),
                                         Peli_LKM = sum(Peli_LKM * !is.na(Voittaja), na.rm =TRUE) ),
                                     by = .(Pakka_ID)]

aggr_to_pakka[, Pfi_voitot := (Voitot + Tasapelit * 0.5), by = Pakka_ID]
aggr_to_pakka[, Pfi_tappiot := Peli_LKM - Pfi_voitot, by = Pakka_ID]

sspakat <- STG_PAKAT[, .(Pakka_ID, Omistaja_ID, Pakka_NM, Retired, Side)]

joinpakat <- aggr_to_pakka[sspakat, on = "Pakka_ID"][Retired == 0 & Side == 0]

#joinlisakortit
joinlisa <- onlymax[joinpakat, on = "Pakka_ID"]

#joinmanastackLisa
joinlisaManaStack <- current_manastack_cards[joinlisa, on = "Pakka_ID"]

#join vahennyskortit

joinvah <- STAT_VAHENNYSKORTIT[joinlisaManaStack, on = "Pakka_ID"]
joinvah[, Lisakortit_lkm := ifelse(is.na(Lisakortit_lkm), 0, Lisakortit_lkm)]
joinvah[, Card_cnt := Lisakortit_lkm - vahennyskortit]
res_table <- joinvah[Retired == 0 & Side == 0 & Card_cnt != 0, .(
                          Deck = Pakka_NM,
                          Own = Omistaja_ID,
                          Incr = round(Lisakortit_lkm, 2),
                          Decr = round(vahennyskortit, 2),
                          Min_cards = pmax(floor(Card_cnt) + 40, 40),
                          Act_cards = Manastack_Cards,
                          W = Pfi_voitot,
                          L = Pfi_tappiot,
                          OK = ifelse((Pfi_voitot + Pfi_tappiot == 0) & floor(Card_cnt + 40) <= Manastack_Cards, 0, 1))]
setorder(res_table, -Min_cards)

STAT_PFI <- res_table
todb <- STAT_PFI[, .(Deck,
                     Owner = Own,
                     Deck_size = Min_cards,
                     Deck_size_MS = Act_cards,
                     Wins = W,
                     Losses = L,
                     Valid = OK)]
con <- connDB(con)
dbWT(con, todb)
