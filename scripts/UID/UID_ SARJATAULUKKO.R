#UID_SARJATAULUKKO <- 
# input_Turnaus_NO <- 11
# input_BO_mode <- FALSE
# 
# testres <- UID_SARJATAULUKKO(27, TRUE, ADM_PELIT, STG_PAKAT)
# testres
UID_SARJATAULUKKO <- function(input_Turnaus_NO, input_BO_mode, ADM_PELIT, STG_PAKAT, input_total_mode = FALSE) {
  required_functions("BO_conversio")
  



  if (input_BO_mode == FALSE) {
    turnausData_temp <- ADM_PELIT[1 == 1]
    turnausData_temp[, Tasapeli := 0]
  } else {
    #convbo
    converted <- BO_conversio(ADM_PELIT[1 == 1])
    turnausData_temp <- converted
  }
  if(is.null(input_total_mode)) {
    input_total_mode <- FALSE
  }
  if (input_total_mode == FALSE) {
    if(is.null(input_Turnaus_NO)) {
      
      input_Turnaus_NO <- turnausData_temp[, max(Turnaus_NO)]
    }
    turnausData <- turnausData_temp[Turnaus_NO == input_Turnaus_NO]
  } else {
    turnausData <- turnausData_temp
  }
  
 # print(turnausData[, Voittaja])
  
  sspakat <- STG_PAKAT[ Side == 0, .(Pakka_NM, Pakka_ID)]
  joinPakat <- sspakat[turnausData, on = .(Pakka_ID)]
    divarit <- joinPakat[, .N, by = Divari][, N := NULL]
    divari_List <- NULL
    for (divari_loop in divarit[, Divari]) {
      listNM <- divari_loop
    divari_List[[listNM]] <-  joinPakat[Divari == divari_loop, .(
                                                                      Matches = sum(!is.na(Voittaja)),
                                                                      Wins = sum(Voittaja, na.rm = TRUE),
                                                                        Draws = sum(Tasapeli, na.rm = TRUE)
                                                                      
                                                                        ), by = .(Pakka_ID, Pakka_NM, Divari, Omistaja_ID)]
    divari_List[[listNM]][, ':=' (Losses = Matches - Wins - Draws,
                               Score = (Wins + Draws * 0.5),
                               Pakka_ID = NULL)]
    setorder(divari_List[[listNM]], -Score, Matches)

    }
   # print("UID_SARJATAULUKKO")
   # print(divari_List)
    return(divari_List)
  }
