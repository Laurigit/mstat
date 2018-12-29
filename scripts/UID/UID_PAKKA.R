#UID_PAKKA
# required_data(c("ADM_PELIT"))
# required_data(c("INT_PFI"))
# 
# input_MA_length = 7
# input_BO_mode  = FALSE
# input_pfi_mode = TRUE

UID_PAKKA <- function(ADM_PELIT,
                      INT_PFI,
                      input_MA_length = 7,
                      input_BO_mode  = FALSE,
                      input_pfi_mode = FALSE) {
required_functions("Prepare_Pelit_for_stats")
#Voitto_PCT
Voitto_PCT_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                        MA = "NO",
                        BO = input_BO_mode,
                        PFI = input_pfi_mode)
Voitto_PCT_pakka <- Voitto_PCT_data[!is.na(Voittaja_Stat),
                               .(Voitto_PCT = sum(Voittaja_Stat) / sum(Peli_LKM_Stat)),
                               by = Pakka_ID]
Voitto_PCT_pakka[, Voitto_PCT_rank := rank(-Voitto_PCT, ties.method = "min")]


#Voitto_PCT_MA
Voitto_PCT_MA_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                              MA = "YES",
                                              input_MA = input_MA_length,
                                              BO = input_BO_mode,
                                              PFI = input_pfi_mode)
Voitto_PCT_MA_pakka <- Voitto_PCT_MA_data[!is.na(Voittaja_Stat),.(Voitto_PCT_MA = sum(Voittaja_Stat) / sum(Peli_LKM_Stat)),
                                          by = Pakka_ID]
Voitto_PCT_MA_pakka[, Voitto_PCT_MA_rank := rank(-Voitto_PCT_MA, ties.method = "min")]


Lisakortit <- INT_PFI[Pakka_form_ID == Current_Pakka_form_ID,. (Pakka_ID, Deck_size = Kortti_lkm_manastack,
                                                                Shuffle8 = Kortti_lkm_manastack %% 8)]
Lisakortit[, Deck_size_rank := rank(-Deck_size, ties.method = "min")]

#putki
Putki_pakka <- Voitto_PCT_data[Voitto_PCT_data[, .I[which.max(Lopetus_DT)], by=Pakka_ID]$V1][,.(Putki, Pakka_ID)]
Putki_pakka[, Putki_rank := rank(-Putki, ties.method = "min")]
#join all

STAT_PAKKA <- Lisakortit[Putki_pakka,
                                  on = "Pakka_ID"][Voitto_PCT_MA_pakka,
                                                   on = "Pakka_ID"][Voitto_PCT_pakka,
                                                                    on = "Pakka_ID"]
return(STAT_PAKKA)


}
