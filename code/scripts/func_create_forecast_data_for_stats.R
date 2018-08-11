#mallinnusmatriimi
# 
# divarit<-luecsv("divari.csv")
# peliData <- luecsv("pelit.csv")
# 
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
#include_tourNo_and_before<-14
# db<-create_forecast_data_for_stats(peliData_ja_pfi, divarit, 13)
# db[Pakka==1 & Vastustajan_Pakka == 8 ]

create_forecast_data_for_stats <- function(peliData_ja_pfi, divarit, include_tourNo_and_before = 99999) {

peliData_ja_pfi_SS <- peliData_ja_pfi[TurnausNo <= include_tourNo_and_before]
mallitKaikk <- voittoEnnusteMallit(peliData_ja_pfi_SS)

peliData_SS <- peliData_ja_pfi_SS[,. (weight = Laurin_pysyvyys_pct * Martin_pysyvyys_pct, Voittaja,hinta_lauri, laurin_kortti_lkm, hinta_martti, martin_kortti_lkm, Aloittaja, Laurin_pakka, Martin_pakka)]

#joinaa
pelit_ja_mallit <- peliData_SS[mallitKaikk, on = c("Laurin_pakka", "Martin_pakka")]
pelit_ja_mallit[, ':=' (rivi = seq_len(.N),
                        Mull_diff = 0,
                        VS_peli_bool = 1,
                        keskiHintaEro = log((hinta_lauri / laurin_kortti_lkm)/(hinta_martti / martin_kortti_lkm)))]
 


ennusteet_fit <-  pelit_ja_mallit[, .(ennuste = predict.glm(object =malli[[1]], newdata = .SD, type = "response")), by = rivi]
                              
#joinback
pelit_ja_mallit_ja_ennuste <- pelit_ja_mallit[ennusteet_fit, on = "rivi"]
# pelit_ja_mallit_ja_ennuste[, virhe := weight * abs(Voittaja-ennuste)]
# pelit_ja_mallit_ja_ennuste[!is.na(ennuste), avg :=sum(virhe, na.rm = TRUE)/sum(weight, na.rm = TRUE) ]
# summary(pelit_ja_mallit_ja_ennuste[, virhe])

#luo matriisi
Laurin_pakka <- 1:nrow(peliData_ja_pfi_SS[,.N, by = ,.(Laurin_pakka)])
Martin_pakka  <- 1:nrow(peliData_ja_pfi_SS[,.N, by = ,.(Martin_pakka)])
Aloittaja <- 0:1
Pelit <- expand.grid(Laurin_pakka, Martin_pakka, Aloittaja)
dt_pelimatriisi <- as.data.table(Pelit)
colnames(dt_pelimatriisi) <- c("Laurin_pakka","Martin_pakka","Aloittaja") 


#pakkahinnat
max_turnee_ss <- peliData_ja_pfi_SS[,max(TurnausNo)]


Laurin_pakka_hinta <- peliData_ja_pfi_SS[TurnausNo == max_turnee_ss, .SD[c(1)], by=Laurin_pakka][, .(Laurin_pakka,
                                                                               Laurin_Hinta = hinta_lauri,
                                                                               laurin_kortti_lkm)]


Martin_pakka_hinta <- peliData_ja_pfi_SS[TurnausNo == max_turnee_ss, .SD[c(1)], by = Martin_pakka][,.(Martin_pakka,
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
print("DIVARIT")
laurin_nimet <-divarit[Omistaja==1, .(Laurin_pakka = Pakka, Vastustajan_nimi = Nimi, Vastustajan_omistaja = "Lauri")]
martin_nimet <-divarit[Omistaja==2, .(Martin_pakka = Pakka, Nimi, Omistaja = "Martti")]

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
#dcast.data.table(join_back, Laurin_pakka ~ Martin_pakka, value.var = "ennuste_filled", fun.aggregate = mean)
}
