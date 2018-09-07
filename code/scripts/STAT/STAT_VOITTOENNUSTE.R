#STAT_VOITTOENNUSTE
required_data("ADM_PELIT")
#p1, vastpak, turnaus, ennuste




#1. luo data
#2 luo mallit
#3 ennusta


#älä ota mukaan keskeneräistä turnausta
maxTurnaus <- ADM_PELIT[, max(Turnaus_NO)]
pelatut_turnaukset <-  ADM_PELIT[1==1]

#dataan tarvitaan P1, Vpak, hinta, vhinta, mull, vmull, kortit, vkortit, pysyvyys, voittaja, aloittaja
#omadata


pelatut_turnaukset[, ':=' (Mull_diff =  Vastustajan_Mulligan - Mulligan,
        P1_keskihinta = Hinta / Kortti_lkm_manastack,
        P2_keskihinta = Vastustajan_Hinta / Vastustajan_Kortti_lkm_manastack)]

pelatut_turnaukset[, ':=' (weight = Pakka_form_pct * Vastustajan_Pakka_form_pct,
                 keskiHintaEro = log((P1_keskihinta / P2_keskihinta), base = exp(1)))]

analyse_cols <- pelatut_turnaukset[, .(Pakka_ID,
                              Vastustajan_Pakka_ID,
                              Voittaja,
                              Mull_diff,
                              weight,
                              Aloittaja,
                              keskiHintaEro,
                              Turnaus_NO)]




#loopataan kaikki pelit
#  Laurin_pakat <- pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Pakka_ID)][,N:=NULL][, avain := 1]
# Martin_pakat <-  pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Martin_pakka)][,N:=NULL][, avain := 1]
pakkayhdistelmat <-pelatut_turnaukset[,.N, by = ,.(Pakka_ID, Vastustajan_Pakka_ID, Turnaus_NO)][,N:=NULL]

P1 <- pelatut_turnaukset[,.N, by = ,.(Pakka_ID, Turnaus_NO, Omistaja_ID)][,N:=NULL]
P2 <- pelatut_turnaukset[,.N, by = ,.(Vastustajan_Pakka_ID, Turnaus_NO, V_Omistaja_ID = Vastustajan_Omistaja_ID)][,N:=NULL]
crossjoin_all <- P1[P2, on ="Turnaus_NO", allow.cartesian = TRUE]
crossjoin <- crossjoin_all[Omistaja_ID != V_Omistaja_ID][, ':=' (Omistaja_ID = NULL, V_Omistaja_ID = NULL)]


#turnausloop
mallitulokset <- NULL
for(tour_loop in crossjoin[, .N, by = "Turnaus_NO"][order(Turnaus_NO)][,Turnaus_NO]){
  
  #paripeli_Loop

  rivi_lkm <-nrow(crossjoin[Turnaus_NO == tour_loop])
  turnausData <- analyse_cols[Turnaus_NO< tour_loop]
  paripeli_Turnaus <- crossjoin[Turnaus_NO == tour_loop]
  for(paripeli in 1:rivi_lkm) {
  P1 <- paripeli_Turnaus[paripeli, Pakka_ID]
  P2 <- paripeli_Turnaus[paripeli, Vastustajan_Pakka_ID]
  paridata <- turnausData[Pakka_ID == P1 | Vastustajan_Pakka_ID == P2]
  paridata[,  ':=' (VS_peli_bool = ifelse(Pakka_ID == P1 & Vastustajan_Pakka_ID == P2, 1, 0))]
                    
  
  modeling_cols <- paridata
  
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
  
  
  mallirivi <- data.table(Pakka_ID = P1, Vastustajan_Pakka_ID = P2, malli = model_list,
                          Turnaus_NO = tour_loop )
  mallitulokset <- rbind(mallitulokset, mallirivi)
  }
}

#jatketaan tekemällä pilkommalla mallinnuskomponentit
join_model <- mallitulokset[analyse_cols, on = .(Pakka_ID, Vastustajan_Pakka_ID, Turnaus_NO)]
# 
# 
# CJ(crossjoin, Aloittaja)
# Aloittaja <- 0:1
# Turnaukset <- 1:maxTurnaus
# expandi <- as.data.table(expand.grid(Aloittaja, Turnaukset))
# setnames(expandi, c("Var1", "Var2"), c("Aloittaja", "Turnaus_NO"))
# #joinaloittaja
# cj_aloittaja <- expandi[crossjoin, on = "Turnaus_NO", allow.cartesian = TRUE]
join_model[,  ':=' (rivi = seq_len(.N),
                     VS_peli_bool = 1,
                    Mull_diff = 0)]
  join_model[, ':=' (ennuste = ifelse(is.na(malli) | is.na(keskiHintaEro) ,
                                                    0.5,
                                                    predict.glm(object =malli[[1]], newdata = .SD, type = "response"))),
                               by = rivi]
  
  analysii <- join_model[!is.na(Voittaja ), .(Ennuste_KPI = abs(ennuste-Voittaja)-0.5)]
  summary(analysii)
  
  
  #joinback
  pelit_ja_mallit_ja_ennuste <- pelit_ja_mallit[ennusteet_fit, on = "rivi"]
  # pelit_ja_mallit_ja_ennuste[, virhe := weight * abs(Voittaja-ennuste)]
  # pelit_ja_mallit_ja_ennuste[!is.na(ennuste), avg :=sum(virhe, na.rm = TRUE)/sum(weight, na.rm = TRUE) ]
  # summary(pelit_ja_mallit_ja_ennuste[, virhe])
  
  #luo matriisi
  Laurin_pakka <- joinpelit[,.N, by = ,.(Laurin_pakka)][, Laurin_pakka]
  Martin_pakka  <- joinpelit[,.N, by = ,.(Martin_pakka)][, Martin_pakka]
  Aloittaja <- 0:1
  Pelit <- expand.grid(Laurin_pakka, Martin_pakka, Aloittaja)
  dt_pelimatriisi <- as.data.table(Pelit)
  colnames(dt_pelimatriisi) <- c("Laurin_pakka","Martin_pakka","Aloittaja") 
  
  
  #pakkahinnat
  max_turnee_ss <- joinpelit[,max(TurnausNo)]
  
  
  Laurin_pakka_hinta <- joinpelit[TurnausNo == max_turnee_ss, .SD[c(1)], by=Laurin_pakka][, .(Laurin_pakka,
                                                                                              Laurin_Hinta = hinta_lauri,
                                                                                              laurin_kortti_lkm)]
  
  
  Martin_pakka_hinta <- joinpelit[TurnausNo == max_turnee_ss, .SD[c(1)], by = Martin_pakka][,.(Martin_pakka,
                                                                                               Martin_hinta = hinta_martti,
                                                                                               martin_kortti_lkm)]
  #joinaa
  joinaa_hinnat <- dt_pelimatriisi[Laurin_pakka_hinta, on ="Laurin_pakka"][Martin_pakka_hinta, on = "Martin_pakka"]
  joinaa_hinnat[, ':=' (VS_peli_bool = 1,
                        Mull_diff = 0,
                        rivi = seq_len(.N),
                        keskiHintaEro = log((Laurin_Hinta / laurin_kortti_lkm)/(Martin_hinta / martin_kortti_lkm)))]
  #joinaa malli
  join_malli<- mallitKaikk[joinaa_hinnat, on = c("Laurin_pakka", "Martin_pakka")]
  
  ennusteet_matrix <- join_malli[, .(ennuste =  predict.glm(object =malli[[1]], newdata = .SD, type = "response")), by = rivi]
  
  ennusteet_matrix[,mean(ennuste)]
  ennusteet_matrix
  join_back <-ennusteet_matrix[join_malli, on = "rivi"]
  paraNames <- names(join_back[1,malli[[1]]$coefficients])
  
  #parametrien nimet
  malliNimet <- join_back[, .(malliNimiLista =list(names(malli[[1]]$coefficients))), by = rivi ]
  malliNimet[, nimi_string := paste0(malliNimiLista),by = rivi]
  join_Nimet <- malliNimet[join_back, on = "rivi"]
  #eri mallit
  eri_nimi_rivit <- join_Nimet[,.(rivi= min(rivi)), by = nimi_string]
  
  nimiLooppi<- join_Nimet[rivi %in% (eri_nimi_rivit[,rivi]), .(malliNimiLista, nimi_string)][,rivi := seq_len(.N)]
  nimiLooppi[,para_count := unlist(lapply(malliNimiLista,length))]
  nimiLooppi<-nimiLooppi[order(-para_count)]
  tot_result <- NULL
  for(kierros in nimiLooppi[,rivi]) {
    #tähän jäit kirjottaa filtteriä
    paraNames <- nimiLooppi[kierros, malliNimiLista]
    #lisää pääte
    paraNames_paate <- paste0(paraNames[[1]], "_par")
    #ota sulut pois
    paraNames_paate <- gsub("[()]", "", paraNames_paate) 
    kierrosData <- join_Nimet[nimi_string == nimiLooppi[kierros, nimi_string]]
    kierros_result <- kierrosData[, eval(paraNames_paate)  := as.list(malli[[1]]$coefficients), by = rivi]
    # , c( "Intercept",   "Aloittaja_par" , "Mull_diff_par" ,    "VS_peli_bool_par" , "keskiHintaEro_par") := as.list(malli[[1]]$coefficients), by = rivi]
    tot_result <- rbind(tot_result,kierros_result, fill = TRUE)
  }
  tot_result[, ':=' (malliNimiLista = NULL, malli = NULL, Mull_diff = NULL, VS_peli_bool = NULL)]
  
  #konvertoi parametrit niin, että ne voi visualisoida
  tot_result[, ':=' (Pakka_etu = Intercept_par + Aloittaja_par/2 + keskiHintaEro_par * keskiHintaEro,
                     Aloittaja_etu = Aloittaja_par/2)]
  ssCols <- tot_result[,.(Pakka_etu, Aloittaja, Aloittaja_col = ifelse(Aloittaja ==0, "Laurin_aloitus", "Martin_aloitus"),Laurin_pakka, Martin_pakka, Aloittaja_etu, VS_peli_bool_par, ennuste)]
  ssCols[is.na(ssCols)] <- 0
  transp<-data.table(dcast(ssCols, formula = Laurin_pakka + Martin_pakka + Aloittaja_etu + Pakka_etu + VS_peli_bool_par ~ Aloittaja_col, value.var = "ennuste"))
  #ratkaise yhtälöparin x ja y. Näiden avulla aloittajan vaikutus pysyy vakiona
  transp[, x := (Laurin_aloitus + Martin_aloitus -1) / (2 * VS_peli_bool_par + 2 * Pakka_etu)]
  transp[, y := (-Laurin_aloitus + 0.5 +x *( VS_peli_bool_par + Pakka_etu)) / Aloittaja_etu]
  transp[, ':=' (Aloittaja_vaikutus_temp = y * Aloittaja_etu, Pakan_vaikutus = Pakka_etu * x, VS_vaikutus = VS_peli_bool_par * x) ]
  result_ss_cols <- transp[, .(Laurin_pakka, Martin_pakka, Aloittaja_vaikutus_temp, Pakan_vaikutus, VS_vaikutus,
                               ennakkosuosikki = ifelse(Laurin_aloitus + Martin_aloitus < 1, 0, 1))]
  
  #joinaa taas tuloksiin
  joined_vaikutus <- tot_result[result_ss_cols, on =c("Laurin_pakka", "Martin_pakka")]
  
  joined_vaikutus[, Aloittajan_vaikutus := ifelse(Aloittaja == 0, -Aloittaja_vaikutus_temp, Aloittaja_vaikutus_temp)][,Aloittaja_vaikutus_temp:=NULL]
  
  #joinaa nimet
  
  laurin_nimet <-STG_PAKAT[Omistaja_ID=="L", .(Laurin_pakka = Pakka_ID, Vastustajan_nimi = Pakka_NM, Vastustajan_omistaja = "Lauri")]
  martin_nimet <-STG_PAKAT[Omistaja_ID=="M", .(Martin_pakka = Pakka_ID, Nimi = Pakka_NM, Omistaja = "Martti")]
  
  joined_vaikutus_laurin <- joined_vaikutus[laurin_nimet, on = "Laurin_pakka"]
  joined_vaikutus_martin <- joined_vaikutus_laurin[martin_nimet, on = "Martin_pakka"][,rivi := NULL]
  
  martin_Data <- joined_vaikutus_martin[,.(Pakka = Martin_pakka,
                                           Vastustajan_Pakka =Laurin_pakka ,
                                           Nimi,
                                           Vastustajan_nimi,
                                           Omistaja,
                                           Vastustajan_omistaja,
                                           ennuste ,
                                           ennakkosuosikki,
                                           nimi_string,
                                           Aloittaja,
                                           VS_vaikutus,
                                           Aloittajan_vaikutus,
                                           Pakan_vaikutus
  )]
  
  laurin_Data <- joined_vaikutus_martin[,.(Pakka = Laurin_pakka,
                                           Vastustajan_Pakka = Martin_pakka,
                                           Nimi = Vastustajan_nimi,
                                           Vastustajan_nimi = Nimi,
                                           Omistaja = "Lauri",
                                           Vastustajan_omistaja = "Martti",
                                           ennuste = 1 - ennuste,
                                           ennakkosuosikki = 1 - ennakkosuosikki,
                                           nimi_string,
                                           Aloittaja,
                                           VS_vaikutus = - VS_vaikutus,
                                           Aloittajan_vaikutus = - Aloittajan_vaikutus,
                                           Pakan_vaikutus = - Pakan_vaikutus
  )]
  
  result_data <- rbind(laurin_Data, martin_Data)
  
  #tehää vielä kopio niin, että käännetään luvut toisinpäin ja Lauri omistaa
  
  return(result_data)



#predict

# aloittaja_for_dt <- Aloittaja

#  pred_data <- data.table(Mull_diff = 0 - 0, VS_peli_bool = 1, Aloittaja = 0,
# keskiHintaEro = log((1 / 1)/(1 / 1), base = exp(1)))
#print(pred_data)
# prediction <- predict(pakkayhdistelmat[1,model], pred_data, type = "response")
return(pakkayhdistelmat)
