#UID_SARJATAULUKKO <- 


UID_SARJATAULUKKO <- function(input_Turnaus_NO, input_BO_mode, ADM_PELIT, STG_PAKAT) {
  required_functions("BO_conversio")
  if (input_BO_mode == FALSE) {
    turnausData <- ADM_PELIT[Turnaus_NO == input_Turnaus_NO]
    turnausData[, Tasapeli := 0]
  } else {
    #convbo
    converted <- BO_conversio(ADM_PELIT)
    turnausData <- converted[Turnaus_NO == input_Turnaus_NO]
    sspakat <- STG_PAKAT[, .(Pakka_NM, Pakka_ID)]
    joinPakat <- sspakat[turnausData, on = .(Pakka_ID)]
  }
    divarit <- joinPakat[, .N, by = Divari][, N := NULL]
    divari_List <- NULL
    for (divari_loop in divarit[, Divari]) {
      listNM <- divari_loop
    divari_List[[listNM]] <-  joinPakat[Divari == divari_loop, .(
                                                                      Matches = sum(!is.na(Voittaja)),
                                                                      Wins = sum(Voittaja, na.rm = TRUE),
                                                                        Draws = sum(Tasapeli, na.rm = TRUE)
                                                                      
                                                                        ), by = .(Pakka_ID, Pakka_NM, Divari, Omistaja_ID)]
    divari_List[[listNM]][, ':=' (Losses = Matches - Wins,
                               Score = (Wins + Draws * 0.5),
                               Pakka_ID = NULL)]
    setorder(divari_List[[listNM]], -Score, Matches)

    }
    return(divari_List)
  }
