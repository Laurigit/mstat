turnausVoitot<-function(divariData,peliData) {
  
  #jos sekä laurin ja martin pakka valittu, tulee vs statsit. Jos vain toinen, niin tulee sen pakan omat statsit
  
  pelidata_temp_all<-bo_data_conv(TRUE,peliData)
  
  Voitot_perTurnaus<-pelidata_temp_all[,.(Laurin_voitot=sum(Lauri_voitti),Pelit=sum(Lauri_voitti+Martti_voitti)),by=.(TurnausNo)]
  Voitot_perTurnaus[,Turnausvoittaja:=ifelse(Laurin_voitot>(Pelit/2),0,ifelse(Laurin_voitot<(Pelit/2),1,NA))]

  #eti alin ja ylin divari
  minmaxdiv<-pelidata_temp_all[,.(maxdiv=max(Divari),mindiv=min(Divari)),by=.(TurnausNo)]
  #vähiten häviöitä alimmassa divarissa
  
  Laurinstats<-pelidata_temp_all[,.(Voitot=sum(Lauri_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=1),by=.(Pakka=Laurin_pakka,Divari,TurnausNo)]
  Martinstats <- pelidata_temp_all[,.(Voitot=sum(Martti_voitti,na.rm=TRUE),Pelit=sum(Lauri_voitti+Martti_voitti,na.rm=TRUE),Omistaja=2),by=.(Pakka=Martin_pakka,Divari,TurnausNo)]
  append<-rbind(Laurinstats,Martinstats)
  append[,Tappiot:=Pelit-Voitot]
  pakkatiedot<-divariData[,.(Omistaja,Pakka,Nimi)]
  #joinaa Nimi
  setkeyv(pakkatiedot,c("Omistaja","Pakka"))
  setkeyv(append,c("Omistaja","Pakka"))
  joinapakka <- pakkatiedot[append]
  joinapakka[,':='(Voitto_pct=Voitot/Pelit)]
  #ei huonoin pakka per divari

  most_losses<-joinapakka[ , .SD[Tappiot == max(Tappiot)],by=.(TurnausNo,Divari)]
  #eti turnaukset, missä vain yksi huonoin
  count_losers<-most_losses[,.N,by=TurnausNo]
  valid_losers<-count_losers[N==1]
  #joinaa validit häviäjät
  joinlosers<-most_losses[valid_losers,on="TurnausNo"]
  tb_losers<-joinlosers[,.(TurnausNo,Loser_omistaja=Omistaja)]
  #nämä on tiebreakerkandidaatteja priolla 1. Toisella priolla ylimmän divarin paras. Lasketaan nyt
  
  most_wins<-joinapakka[ , .SD[Voitot == max(Voitot)],by=.(TurnausNo,Divari)]
  #eti turnaukset, missä vain yksi paraas
  count_winners<-most_wins[,.N,by=TurnausNo]
  valid_winners<-count_winners[N==1]
  #joinaa validit häviäjät
  joinwinners<-most_wins[valid_winners,on="TurnausNo"]
  tb_winner<-joinwinners[,.(TurnausNo,Winner_omistaja=Omistaja)]
  #joinaa tiebrakerit
  tb_joined<-tb_losers[tb_winner,on="TurnausNo"][Voitot_perTurnaus,on="TurnausNo"]
  #ratkase turnausvoittaja
  tb_joined[,tbwinner:=ifelse(is.na(Loser_omistaja),ifelse(is.na(Winner_omistaja),0.5,Winner_omistaja-1),ifelse(Loser_omistaja==2,0,1))]
  after_tb<-tb_joined[,Turnausvoittaja_tb:=ifelse(is.na(Turnausvoittaja),tbwinner,Turnausvoittaja)]
  turnausvoitot<-NULL
  turnausvoitot$total<-after_tb[,.(Laurin_TV=.N-sum(Turnausvoittaja_tb),Martin_TV=sum(Turnausvoittaja_tb))]
  return(turnausvoitot)
}
