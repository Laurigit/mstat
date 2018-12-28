#STAT_VS
# required_functions("Prepare_Pelit_for_stats")
#required_data ("ADM_PELIT")
# prepared_Pelit <- Prepare_Pelit_for_stats(ADM_PELIT,
#                                           MA = "VS",
#                                           input_MA = 3,
#                                           BO = TRUE)
calculate_VS_stats <- function(prepared_Pelit) {
  dataset <- prepared_Pelit
  VS_pelit <- dataset[!is.na(Voittaja_Stat), .(Voitot = sum(Voittaja_Stat),
                                               Voitto_PCT = mean(Voittaja_Stat),
                                               Tasapeli_PCT = mean(Tasapeli_Stat)), by = .(Pakka_ID, Vastustajan_Pakka_ID)]

}
