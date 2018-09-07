#STAT_VOITTOENNUSTE
required_data("ADM_PELIT")
#p1, vastpak, turnaus, ennuste




#1. luo data
#2 luo mallit
#3 ennusta

required_data("ADM_PELIT")
#älä ota mukaan keskeneräistä turnausta
maxTurnaus <- ADM_PELIT[, max(Turnaus_NO)]
pelatut_turnaukset <-  ADM_PELIT[Turnaus_NO < maxTurnaus]

#dataan tarvitaan P1, Vpak, hinta, vhinta, mull, vmull, kortit, vkortit, pysyvyys, voittaja, aloittaja
#omadata
analyse_cols <- pelatut_turnaukset[, .(Pakka_ID,
                       Vastustajan_Pakka_ID,
                       Hinta,
                       Vastustajan_Hinta,
                       Mulligan,
                       Vastustajan_Mulligan,
                       Pakka_form_pct,
                       Vastustajan_Pakka_form_pct,
                       Kortti_lkm_manastack,
                       Vastustajan_Kortti_lkm_manastack,
                       Voittaja,
                       Aloittaja,
                       Turnaus_NO)]




#loopataan kaikki pelit
#  Laurin_pakat <- pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Pakka_ID)][,N:=NULL][, avain := 1]
# Martin_pakat <-  pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Martin_pakka)][,N:=NULL][, avain := 1]
pakkayhdistelmat <-pelatut_turnaukset[,.N, by = ,.(Pakka_ID, Vastustajan_Pakka_ID, Turnaus_NO)][,N:=NULL]

#turnausloop
mallitulokset <- NULL
for(tour_loop in pakkayhdistelmat[, .N, by = "Turnaus_NO"][order(Turnaus_NO)][,Turnaus_NO]){
  
  #paripeli_Loop

  rivi_lkm <-nrow(pakkayhdistelmat[Turnaus_NO == tour_loop])
  turnausData <- analyse_cols[Turnaus_NO< tour_loop]
  paripeli_Turnaus <- pakkayhdistelmat[Turnaus_NO == tour_loop]
  for(paripeli in 1:rivi_lkm) {
  P1 <- paripeli_Turnaus[paripeli, Pakka_ID]
  P2 <- paripeli_Turnaus[paripeli, Vastustajan_Pakka_ID]
  paridata <- turnausData[Pakka_ID == P1 | Vastustajan_Pakka_ID == P2]
  paridata[,  ':=' (VS_peli_bool = ifelse(Pakka_ID == P1 & Vastustajan_Pakka_ID == P2, 1, 0),
                    Mull_diff =  Vastustajan_Mulligan - Mulligan,
                    P1_keskihinta = Hinta / Kortti_lkm_manastack,
                    P2_keskihinta = Vastustajan_Hinta / Vastustajan_Kortti_lkm_manastack)]
  paridata[, ':=' (weight = Pakka_form_pct * Vastustajan_Pakka_form_pct,
                keskiHintaEro = log((P1_keskihinta / P2_keskihinta), base = exp(1)))]
  
  modeling_cols <- paridata[, .(Pakka_ID,
                                   Vastustajan_Pakka_ID,
                                   Voittaja,
                                   Mull_diff,
                                   VS_peli_bool,
                                   weight,
                                   Aloittaja,
                                   keskiHintaEro)]
  
  count_vs_peli <- modeling_cols[, sum(VS_peli_bool)]
  count_pelit <- modeling_cols[, .N]
  if (count_vs_peli > 6) {
    model <- suppressWarnings(glm(data = modeling_cols, 
                                  formula = Voittaja ~ Aloittaja + Mull_diff  + VS_peli_bool + keskiHintaEro, 
                                  family = binomial(link = "logit")
                                  ,weights = weight))
  } else if (count_pelit > 11) {
    model <- suppressWarnings(glm(data = modeling_cols, 
                                  formula = Voittaja ~ Aloittaja + Mull_diff  + keskiHintaEro, 
                                  family = binomial(link = "logit")
                                  ,weights = weight))
  } else {
    # dummydata <- data.table(Voittaja = c(1, 0, 1, 0), Aloittaja = c(1, 1, 0, 0))
    # model <- suppressWarnings(glm(data = dummydata, 
    #                               formula = Voittaja ~ Aloittaja,
    #                               family = binomial(link = "logit")
    model <- NA
    
  }
  model_list <- list()
  model_list[[1]]<-model
  
  mallirivi <- data.table(Pakka_ID = P1, Vastustajan_Pakka_ID = P2, Turnaus_NO = tour_loop, malli = model_list)
  mallitulokset <- rbind(mallitulokset, mallirivi)
  }
}

  
#predict

# aloittaja_for_dt <- Aloittaja

#  pred_data <- data.table(Mull_diff = 0 - 0, VS_peli_bool = 1, Aloittaja = 0,
# keskiHintaEro = log((1 / 1)/(1 / 1), base = exp(1)))
#print(pred_data)
# prediction <- predict(pakkayhdistelmat[1,model], pred_data, type = "response")
return(pakkayhdistelmat)
