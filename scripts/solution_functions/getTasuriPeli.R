
#test <- getTasuriPeli(ADM_PELIT, STAT_VOITTOENNUSTE)
#STAT_VOITTOENNUSTE[Peli_ID == test]
getTasuriPeli <- function(ADM_PELIT, STAT_VOITTOENNUSTE) {
  maxTO <- ADM_PELIT[,max(Turnaus_NO)]
  nykyTurnaus <- ADM_PELIT[Turnaus_NO == maxTO & Omistaja_ID == "L"] 
  sscolsEnnuste <- STAT_VOITTOENNUSTE[Omistaja_ID == "L", .(Peli_ID, ennuste)]
  joinEnnuste <- sscolsEnnuste[nykyTurnaus, on = "Peli_ID"]
  turnausTilanne <- joinEnnuste[Omistaja_ID == "L", mean(Voittaja,na.rm = TRUE)]
  
  if(!is.nan(turnausTilanne)) {
    turnausTilanneInput <- ifelse(turnausTilanne > 0.5, "Lauri", ifelse(turnausTilanne < 0.5, "Martti", "Tasan"))
  } else {
    #jos ei pelattu pelejä vielä turnauksessa, niin tasan
    
    turnausTilanneInput <- "Tasan"
  }
  

  ennustePelit_aggr <- joinEnnuste[is.na(Voittaja) & Omistaja_ID == "L", .(sum_ennuste = mean(ennuste - 0.5), Peli_ID = min(Peli_ID)), by = .(Pakka_ID,
                                                                                                                                    Vastustajan_Pakka_ID)]
  
  Martti_johtaa <- ennustePelit_aggr[which.max(sum_ennuste), Peli_ID]
  Lauri_johtaa <- ennustePelit_aggr[which.min(sum_ennuste), Peli_ID]
  Tasan  <- ennustePelit_aggr[which.min(abs(sum_ennuste)), Peli_ID]
  lopputulos <-  switch(turnausTilanneInput,
                        Martti = Martti_johtaa,
                        Tasan = Tasan,
                        Lauri = Lauri_johtaa)

  
  uusPeliID <- lopputulos
  return(uusPeliID)
  }
