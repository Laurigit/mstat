#UID_PAKKA_VS

# required_data(c("ADM_PELIT"))
# required_data(c("INT_PFI"))
# #
# input_MA_length = 7
# input_BO_mode  = FALSE
# input_pfi_mode = TRUE
# 
# input_P1_mulligan <- 0
# input_P2_mulligan <- 0
# only_current_decks <- TRUE
# 
# res <-UID_PAKKA_VS(ADM_PELIT, INT_PFI,input_MA_length = 7,
#              input_BO_mode  = FALSE,
#              input_pfi_mode = FALSE)


UID_PAKKA_VS <- function(ADM_PELIT,
                         INT_PFI,
                         input_MA_length = 7,
                         input_BO_mode  = FALSE,
                         input_pfi_mode = FALSE,
                         STAT_VOITTOENNUSTE,
                         input_P1_mulligan,
                         input_P2_mulligan,
                         STG_PAKAT = NULL,
                         only_current_decks = FALSE
                         ){

required_functions("Prepare_Pelit_for_stats")
  
  #only active decks
  if (only_current_decks == TRUE) {
    currdecks <- STG_PAKAT[Side == 0 & Retired == 0, Pakka_ID]
    peliData_filtered <- ADM_PELIT[Pakka_ID %in% currdecks]
  } else {
    peliData_filtered <- ADM_PELIT
  }
  
  
  
#Voitto_PCT_VS_MA
Voitto_PCT_MA_VS_data <- Prepare_Pelit_for_stats(peliData_filtered,
                                                 MA = "VS",
                                                 input_MA = input_MA_length,
                                                 BO = input_BO_mode,
                                                 PFI = input_pfi_mode)
Voitto_PCT_MA_VS_pakka <- Voitto_PCT_MA_VS_data[!is.na(Voittaja_Stat),.(Voitto_PCT_MA_VS = sum(Voittaja_Stat) / sum(Peli_LKM_Stat)),
                                                by = .(Pakka_ID, Vastustajan_Pakka_ID)]
Voitto_PCT_MA_VS_pakka[, Voitto_PCT_MA_VS_rank := rank(-Voitto_PCT_MA_VS, ties.method = "min")]

#Voitto_PCT
Voitto_PCT_data <- Prepare_Pelit_for_stats(peliData_filtered,
                                           MA = "NO",
                                           BO = input_BO_mode,
                                           PFI = input_pfi_mode)

Voitto_PCT_VS_pakka <- Voitto_PCT_data[!is.na(Voittaja_Stat),
                                       .(Voitto_PCT_VS = sum(Voittaja_Stat) / sum(Peli_LKM_Stat),
                                         Pelit_ABS_VS = sum(Peli_LKM_Stat, na.rm = TRUE)),
                                       by = .(Pakka_ID, Vastustajan_Pakka_ID)]
Voitto_PCT_VS_pakka[, Voitto_PCT_VS_rank := rank(-Voitto_PCT_VS, ties.method = "min")]
Voitto_PCT_VS_pakka[, Pelit_ABS_VS_rank := rank(-Pelit_ABS_VS, ties.method = "min")]


#putki_VS
Putki_pakka_VS <-
  Voitto_PCT_data[Voitto_PCT_data[,
                                                  .I[which.max(Lopetus_DT)], by=.(Pakka_ID,
                                                                                    Vastustajan_Pakka_ID)]$V1][,.(Putki_VS,
                                                                                                                  Pakka_ID,
                                                                                                Vastustajan_Pakka_ID)]
Putki_pakka_VS[, Putki_VS_rank := rank(-Putki_VS, ties.method = "min")]

STAT_PAKKA_VS <-
  Voitto_PCT_MA_VS_pakka[Voitto_PCT_VS_pakka,
                           on = .(Pakka_ID, Vastustajan_Pakka_ID)][Putki_pakka_VS,
                                              on = .(Pakka_ID, Vastustajan_Pakka_ID)]


return(STAT_PAKKA_VS)
}
