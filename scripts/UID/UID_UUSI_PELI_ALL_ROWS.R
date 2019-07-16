# # UID_UUSI_PELI
# required_data(c("STG_PAKAT", "STG_OMISTAJA", "ADM_PELIT", "INT_PFI", "STAT_VOITTOENNUSTE", "STAT_CURRENT_PAKKA"))
# required_functions(c("UID_PAKKA", "UID_PAKKA_VS", "UID_UUSI_PELI"))
# input_MA_length = 7
# input_BO_mode  = FALSE
# input_pfi_mode = TRUE
# input_left_mulligan <- 0
# input_right_mulligan <- 0
# UID_PAKKA <- UID_PAKKA(ADM_PELIT,
#                        INT_PFI,
#                        input_MA_length,
#                        input_BO_mode,
#                        input_pfi_mode)
# 
# UID_PAKKA_VS <- UID_PAKKA_VS(ADM_PELIT, INT_PFI,input_MA_length,
#              input_BO_mode,
#              input_pfi_mode)
# 
# Peli_ID_input <- "1000"
# 
# res <-  UID_UUSI_PELI(Peli_ID_input, UID_PAKKA, UID_PAKKA_VS, STG_PAKAT, STG_OMISTAJA, ADM_PELIT,
#                       STAT_VOITTOENNUSTE,
#                       input_left_mulligan,
#                       input_right_mulligan,
#                        STAT_CURRENT_PAKKA)
# res
# res

UID_UUSI_PELI_ALL_ROWS <- function(
  UID_PAKKA,
  UID_PAKKA_VS,
  STG_PAKAT,
  STG_OMISTAJA,
  ADM_PELIT,
  STAT_VOITTOENNUSTE,
  input_left_mulligan,
  
  input_right_mulligan,
  STAT_CURRENT_PAKKA) {
  
  
  required_data(c("STAT_CURRENT_PAKKA", "STG_PAKAT", "STG_OMISTAJA", "ADM_PELIT", "STAT_VOITTOENNUSTE"))
  
  pelidata <- ADM_PELIT[1 != 0]
  
  
  #message(Peli_ID_input, 
  #          input_left_mulligan,
  #          input_right_mulligan)
  required_functions("predict_result")
  #Left_pakka <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="L", .(Pakka_ID)]
  #Right_pakka <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="M", .(Pakka_ID)]
  #Divari <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="M", .(Divari)]
  # browser
  Pakka <- UID_PAKKA
  PakkaVS_subset <- UID_PAKKA_VS
  
  max_turnaus <- pelidata[, max(Turnaus_NO)]
  turnausparit <- pelidata[Turnaus_NO == max_turnaus, .(Peli_ID, Pakka_ID, Vastustajan_Pakka_ID)]
  
  PakkaVS <-  PakkaVS_subset[turnausparit, on = .(Pakka_ID, Vastustajan_Pakka_ID)]
  
  #message("PakkaVS ", PakkaVS)
  # Tilanne <- getTilanne(pelidata, Peli_ID_input)
  # Aloittaja <- pelidata[Peli_ID == Peli_ID_input , .(Pakka_ID, Aloittaja)]
  
  #join dybamic color
  
  sscols_pakat <- STAT_CURRENT_PAKKA[, .(Pakka_NM, Pakka_ID, Colors, Most_same_card, Most_wins_sames_card,
                                         Pakka_NM_Dynamic, Omistaja_ID)]
  
  joini_ilman_omistajaa <- PakkaVS[Pakka,
                                   on = "Pakka_ID"][sscols_pakat,
                                                    on = "Pakka_ID"]
  joini <- STG_OMISTAJA[joini_ilman_omistajaa,   on = "Omistaja_ID"]
  
  
  
  joini_ssrows <- joini#[Pakka_ID %in%  c(Left_pakka,
  # Right_pakka)]
  joini_ssrows[, ':=' (#Pakka_ID = NULL,
    #Omistaja_ID = NULL,
    #Vastustajan_Pakka_ID = NULL,
    Colors = NULL)]
  
  #prediction
  #print("UID_UUSI_PELI <- function")
  #print(paste(Peli_ID_input, input_left_mulligan,
  #            input_right_mulligan
  #            ))
  
  # ennuste <- predict_result(Peli_ID_input, input_left_mulligan,
  #                           input_right_mulligan,
  #                           STAT_VOITTOENNUSTE)
  ennuste <- STAT_VOITTOENNUSTE[, .(Prediction = ennuste, Peli_ID, Prediction_mirror = 1 - ennuste, Pakka_ID)]
  
  max_turnaus <- pelidata[, max(Turnaus_NO)]
  curr_turn_pelit <- pelidata[Turnaus_NO == max_turnaus , .(Peli_ID,
                                                            Pakka_ID,
                                                            Vastustajan_Pakka_ID,
                                                            Aloittaja,
                                                            Divari,
                                                            Ottelu_ID,
                                                            Voittaja,
                                                            Aloitus_DT)]
  join_stats <- joini_ssrows[curr_turn_pelit, on = .(Pakka_ID, Vastustajan_Pakka_ID, Peli_ID)]
  
  # #join_omistaja-uusiks
  # omistajadata <- STG_PAKAT[, .(Pakka_ID, Omistaja_ID, Pakka_NM, Most_same_card,
  #                               Most_wins_sames_card,
  #                               Pakka_NM_Dynamic)][STG_OMISTAJA, on = "Omistaja_ID"]
  
  join_ennuste <- ennuste[join_stats, on = c("Peli_ID", "Pakka_ID")]
  # join_ennuste[, ':=' (Omistaja_ID = NULL, Omistaja_NM = NULL)]
  #join_omistaja <- omistajadata[join_ennuste, on = "Pakka_ID"]
  
  tulos <- join_ennuste
  return(tulos)
}
