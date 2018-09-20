#STAT_VOITTOENNUSTE
required_data("ADM_PELIT")
#p1, vastpak, turnaus, ennuste
print("TÄMÄ KESTÄÄ")



#1. luo data
#2 luo mallit
#3 ennusta


#älä ota mukaan keskeneräistä turnausta
maxTurnaus <- ADM_PELIT[, max(Turnaus_NO)]
pelatut_turnaukset <-  ADM_PELIT[1==1]
#pelatut_turnaukset <- ADM_PELIT[Pakka_ID == 1 | Vastustajan_Pakka_ID == 9 | Pakka_ID == 9 | Vastustajan_Pakka_ID == 1]
#dataan tarvitaan P1, Vpak, hinta, vhinta, mull, vmull, kortit, vkortit, pysyvyys, voittaja, aloittaja
#omadata


pelatut_turnaukset[, ':=' (Mull_diff =  Vastustajan_Mulligan - Mulligan,
        P1_keskihinta = Hinta / Kortti_lkm_manastack,
        P2_keskihinta = Vastustajan_Hinta / Vastustajan_Kortti_lkm_manastack)]

pelatut_turnaukset[, ':=' (weight = Pakka_form_pct * Vastustajan_Pakka_form_pct,
                 keskiHintaEro = log((P1_keskihinta / P2_keskihinta), base = exp(1)),
                 MA_ranking_ero = Turnaus_Ranking_PFI- Vastustajan_Turnaus_Ranking_PFI)]

analyse_cols <- pelatut_turnaukset[, .(Pakka_ID,
                              Vastustajan_Pakka_ID,
                              Voittaja,
                              Mull_diff,
                              weight,
                              Aloittaja,
                              keskiHintaEro,
                              MA_ranking_ero,
                              Turnaus_NO,
                              Peli_ID)]




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
  
  
  if (count_vs_peli > 6 ) { #oli 6
    model <- suppressWarnings(glm(data = modeling_cols, 
                                  formula = Voittaja ~ Aloittaja + Mull_diff + VS_peli_bool + MA_ranking_ero,
                                  family = binomial(link = "logit")
                                  ,weights = weight))
  } else if (count_pelit > 11) {
    model <- suppressWarnings(glm(data = modeling_cols,
                                  formula = Voittaja ~ Aloittaja + Mull_diff + MA_ranking_ero,
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
                          Turnaus_NO = tour_loop, vs_peli_lkm = count_vs_peli)
  mallitulokset <- rbind(mallitulokset, mallirivi)
  }
}

#jatketaan tekemällä pilkommalla mallinnuskomponentit
# tee data mallinnusta varten niin, että käytetään edellisen. Tarvitaan vaan edellisen kierroksen MA_ranking_ero


ma_ranking_data <- pelatut_turnaukset[, . (mean_MA = mean(Turnaus_Ranking_PFI)),
                      by = .(Pakka_ID, Turnaus_NO)]

ma_ranking_data[, Turnaus_NO := Turnaus_NO + 1]
ma_ranking_data_vastustaja <- pelatut_turnaukset[, . (
  mean_MA_vastustaja = mean(Vastustajan_Turnaus_Ranking_PFI)), by = .(Vastustajan_Pakka_ID, Turnaus_NO)]
ma_ranking_data_vastustaja[, Turnaus_NO := Turnaus_NO + 1]

fit_data <- pelatut_turnaukset[, .(Peli_ID, Turnaus_NO, Pakka_ID, Vastustajan_Pakka_ID, Aloittaja, Voittaja, Omistaja_ID)]
join_MA <- ma_ranking_data[fit_data, on = .(Pakka_ID, Turnaus_NO)]
join_MA_Vihu <- ma_ranking_data_vastustaja[join_MA, on = .(Vastustajan_Pakka_ID, Turnaus_NO)]


join_MA_Vihu[, MA_ranking_ero := mean_MA - mean_MA_vastustaja]


join_model <- mallitulokset[join_MA_Vihu, on = .(Pakka_ID, Vastustajan_Pakka_ID, Turnaus_NO)]
# 
# mallitulokset
# CJ(crossjoin, Aloittaja)
# Aloittaja <- 0:1
# Turnaukset <- 1:maxTurnaus
# expandi <- as.data.table(expand.grid(Aloittaja, Turnaukset))
# setnames(expandi, c("Var1", "Var2"), c("Aloittaja", "Turnaus_NO"))
# #joinaloittaja
# cj_aloittaja <- expandi[crossjoin, on = "Turnaus_NO", allow.cartesian = TRUE]
join_model[,  ':=' (rivi = seq_len(.N),
                     VS_peli_bool = 1,
                    Mull_diff = 0,
                    mean_MA_vastustaja = NULL,
                    mean_MA = NULL)]
  join_model[, ':=' (ennuste = ifelse(is.na(malli) | is.null(malli[[1]]),
                                                    0.5,
                                                    predict.glm(object =malli[[1]], newdata = .SD, type = "response"))),
                               by = rivi]
  join_model[, ':=' (ennuste = ifelse(is.na(ennuste), 0.5, ennuste))]
  # analysii <- join_model[!is.na(Voittaja ) & Turnaus_NO > 10 & abs(ennuste - 0.5) < 0.25 & abs(ennuste - 0.5) > 0.05, .(Ennuste_KPI = abs(ennuste-Voittaja)-0.5, Turnaus_NO, Pakka_ID)]
  # summary(analysii)
  # analysii[, .meanKPI <- mean(Ennuste_KPI)]

  
 # paraNames <- names(join_model[1,malli[[1]]$coefficients])
  
  #parametrien nimet
  malliNimet <- join_model[ennuste != 0.5, .(malliNimiLista =list(names(malli[[1]]$coefficients))), by = rivi ]
  malliNimet[, nimi_string := paste0(malliNimiLista),by = rivi]
  join_Nimet <- malliNimet[join_model, on = "rivi"]
  join_Nimet[, nimi_string := ifelse(is.na(nimi_string), "Eimallia", nimi_string)]
  #eri mallit
  eri_nimi_rivit <- join_Nimet[,.(rivi= min(rivi)), by = nimi_string]
  
  nimiLooppi<- join_Nimet[rivi %in% (eri_nimi_rivit[,rivi]), .(malliNimiLista, nimi_string)][,rivi := seq_len(.N)]
  
  nimiLooppi[,para_count := unlist(lapply(malliNimiLista,length))]
  nimiLooppi<-nimiLooppi[order(-para_count)]
  tot_result <- NULL
  for(kierros in nimiLooppi[,rivi]) {
    #tähän jäit kirjottaa filtteriä
    paraNames <- nimiLooppi[kierros, malliNimiLista]
    if(!is.null(paraNames[[1]])) {
    #lisää pääte
    paraNames_paate <- paste0(paraNames[[1]], "_par")
    #ota sulut pois
    paraNames_paate <- gsub("[()]", "", paraNames_paate) 
    kierrosData <- join_Nimet[nimi_string == nimiLooppi[kierros, nimi_string]]
    
    
    kierros_result <- kierrosData[, eval(paraNames_paate)  := as.list(malli[[1]]$coefficients), by = rivi]
   
    } else {
      kierros_result <- join_Nimet[nimi_string == nimiLooppi[kierros, nimi_string]]
    }
    # , c( "Intercept",   "Aloittaja_par" , "Mull_diff_par" ,    "VS_peli_bool_par" , "keskiHintaEro_par") := as.list(malli[[1]]$coefficients), by = rivi]
    tot_result <- rbind(tot_result,kierros_result, fill = TRUE)
  }
  tot_result[, ':=' (malliNimiLista = NULL, Mull_diff = NULL, VS_peli_bool = NULL)]
  
  #konvertoi parametrit niin, että ne voi visualisoida
 
  ssCols <- tot_result[,.(
                          Voittaja, Pakka_ID, Vastustajan_Pakka_ID, Aloittaja, Aloittaja_par, VS_peli_bool_par, Intercept_par, MA_ranking_ero_par,MA_ranking_ero,
                          ennuste, Turnaus_NO, Peli_ID, malli, Omistaja_ID)]
  ssCols[is.na(ssCols)] <- 0


  ssCols[, VS_TN := 1 / (1 + exp(-VS_peli_bool_par)) - 0.5 ]
  ssCols[, Pakka_TN := 1 / (1 + exp ( - (MA_ranking_ero_par * MA_ranking_ero + Intercept_par + Aloittaja_par / 2))) - 0.5]
  ssCols[, Aloittaja_TN := (ennuste - 0.5) - Pakka_TN - VS_TN]
  
  # 
  # ssCols[, VS_TN := 1 / (1 + exp(-VS_peli_bool_par)) - 0.5 ]
  # ssCols[, Pakka_TN := 1 / (1 + exp ( - (MA_ranking_ero_par * MA_ranking_ero + Intercept_par))) - 0.5]
  # ssCols[, Aloittaja_TN := (ennuste - 0.5) - Pakka_TN - VS_TN]
  # 

STAT_VOITTOENNUSTE <- ssCols

