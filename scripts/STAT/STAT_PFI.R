required_data(c("STAT_LISAKORTIT", "ADM_PELIT", "STG_PAKAT", "INT_PFI", "STG_PFI"))

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

res_table <- joinlisaManaStack[Retired == 0 | Side == 1, .(Deck = Pakka_NM,
                          Owner = Omistaja_ID,
                          Deck_size = Lisakortit_lkm,
                          Deck_size_MS = Manastack_Cards,
                          Wins = Pfi_voitot,
                          Losses = Pfi_tappiot,
                          Valid = ifelse((Pfi_voitot + Pfi_tappiot == 0) & floor(Lisakortit_lkm + 40) != Manastack_Cards, 0, 1))]
setorder(res_table, -Wins)

STAT_PFI <- res_table
con <- connDB(con)
dbWT(con, STAT_PFI)
