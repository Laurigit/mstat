

UID_TURNAUS_EV <- function(ADM_PELIT, STAT_VOITTOENNUSTE)  {

  sscols_ennuste <- STAT_VOITTOENNUSTE[Omistaja_ID == "M",.(Peli_ID, Martin_voitto_tn = ennuste)]
  pelikopio <- ADM_PELIT[1==1]
  sscols_pelit <- pelikopio[Omistaja_ID == "M",.(Aloitus_DT, Martti_Voitti =  Voittaja, Turnaus_NO, Laurin_Pakka = Vastustajan_Pakka_ID,
                                                 Martin_pakka = Pakka_ID, Peli_ID)]
 # ADM_PELIT[Turnaus_NO == 29 & !is.na(Voittaja) & Omistaja_ID =="L"]
peliData_ja_pfi <-  sscols_ennuste[sscols_pelit, on = "Peli_ID"]
#peliData_ja_pfi <- peliData_ja_pfi[Laurin_Pakka == 1 & Martin_pakka == 9]
# peliData_ja_pfi<- funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
# tulos <- voittoEnnusteMallit(peliData_ja_pfi)


maxTurnaus <- peliData_ja_pfi[,max(Turnaus_NO)]
#maxTurnaus <- maxTurnaus -1
turnausData <- peliData_ja_pfi[Turnaus_NO == maxTurnaus]
#turnausData[, ':=' (alkuaika = oma_timedate(Aloituspvm, Aloitusaika), rivi = seq_len(.N)) ]


turnaus_ja_voitto <- turnausData[order(Aloitus_DT)]
ssCols <- turnaus_ja_voitto[, .(Aloitus_DT, Martti_Voitti,
                                Voittaja_EV = (Martti_Voitti - 0.5) * 2, Martin_voitto_tn,
                                voittoEnnusteVar_EV = (Martin_voitto_tn - 0.5) * 2)]


#deviin arvotaan voittajia
# ssCols[sample.int(32)[1:8], ':=' (Voittaja_EV = 1, alkuaika = seq_len(.N))]
# ssCols[sample.int(32)[1:8], ':=' (Voittaja_EV = -1, alkuaika = seq_len(.N))]
ssCols <- ssCols[order(Aloitus_DT)]
alkuperainen_ennuste <- ssCols[, sum(voittoEnnusteVar_EV)]
ssCols[, ':=' (Tilanne = cumsum(Voittaja_EV), cEnnuste = cumsum(voittoEnnusteVar_EV))]
ssCols[, Ennuste := (alkuperainen_ennuste + Tilanne - cEnnuste)]

#lisää nollarivi
nollarivi <- data.table(Tilanne = 0, Ennuste = alkuperainen_ennuste, peliNo = 0)
kuvaajadata <- ssCols[,. (Tilanne, Ennuste, peliNo = seq_len(.N))]
bindaa <- rbind(nollarivi, kuvaajadata)


melttaa <- melt(bindaa, id.vars = "peliNo", measure.vars = c("Tilanne", "Ennuste") )
melttaa[, Martin_johto := value]
melttaa[, ottelu_id := (peliNo/2)]
melttaa_aggr <- melttaa[ottelu_id %% 1 == 0, .(ottelu_id, value, Martin_johto, variable)]

return(melttaa_aggr)
}
