#UID_PAKKA_VS

# required_data(c("ADM_PELIT"))
# required_data(c("INT_PFI"))
# 
# input_MA_length = 7,
# input_BO_mode  = FALSE
# input_pfi_mode = FALSE

# UID_PAKKA_VS(ADM_PELIT, INT_PFI,input_MA_length = 7,
#              input_BO_mode  = FALSE,
#              input_pfi_mode = FALSE)


UID_PAKKA_VS <- function(ADM_PELIT,
                         INT_PFI,
                         input_MA_length = 7,
                         input_BO_mode  = FALSE,
                         input_pfi_mode = FALSE
                         ){
  
required_functions("Prepare_Pelit_for_stats")
#Voitto_PCT_VS_MA
Voitto_PCT_MA_VS_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                                 MA = "VS",
                                                 input_MA = input_MA_length,
                                                 BO = input_BO_mode,
                                                 PFI = input_pfi_mode)
Voitto_PCT_MA_VS_pakka <- Voitto_PCT_MA_VS_data[,.(Voitto_PCT_MA_VS = mean(Voittaja_Stat, na.rm = TRUE)),
                                                by = .(Pakka_ID, Vastustajan_Pakka_ID)]

#Voitto_PCT
Voitto_PCT_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                           MA = "NO",
                                           BO = input_BO_mode,
                                           PFI = input_pfi_mode)

Voitto_PCT_VS_pakka <- Voitto_PCT_data[!is.na(Voittaja),
                                       .(Voitto_PCT_VS = mean(Voittaja_Stat, na.rm = TRUE),
                                         Pelit_ABS_VS = sum(Peli_LKM, na.rm = TRUE)),
                                       by = .(Pakka_ID, Vastustajan_Pakka_ID)]


#putki_VS
Putki_pakka_VS <-
  Voitto_PCT_data[Voitto_PCT_data[,
                                                  .I[which.max(Lopetus_DT)], by=.(Pakka_ID,
                                                                                    Vastustajan_Pakka_ID)]$V1][,.(Putki_VS,
                                                                                                                  Pakka_ID,
                                                                                                Vastustajan_Pakka_ID)]

STAT_PAKKA_VS <-
  Voitto_PCT_MA_VS_pakka[Voitto_PCT_VS_pakka,
                           on = .(Pakka_ID, Vastustajan_Pakka_ID)][Putki_pakka_VS,
                                              on = .(Pakka_ID, Vastustajan_Pakka_ID)]
return(STAT_PAKKA_VS)
}
