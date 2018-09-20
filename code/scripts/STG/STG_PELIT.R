required_data(c("SRC_PELIT", "SRC_DIVARI"))
temp <- SRC_PELIT
# temp[Aloitusaika > Lopetusaika]
# temp[Aloituspvm < Lopetuspvm]
# temp[Lopetusaika < 60 * 60 *3 & Aloituspvm == Lopetuspvm, .(Aloituspvm, Lopetuspvm, Aloitusaika, Lopetusaika, Aloitus_DT, Lopetus_DT)]
# #aiemmin ei oo toiminu vuorokauden ylitykset
temp[, ':=' (Lopetuspvm = ifelse(Lopetusaika < 60 * 60 *3, Lopetuspvm + 1, Lopetuspvm),
             Aloituspvm = ifelse(Aloitusaika < 60 * 60 * 3, Aloituspvm +1, Aloituspvm))]
#siirretään kelloja 3h taaksepäin, koska aiemmin tallennettiin UTC
temp[is.na(Aloitus_DT) | Aloitus_DT == "", ':=' (Aloitus_DT = as.character(convSecsToTime(Aloitusaika, Aloituspvm, 60 * 60 *3)),
             Lopetus_DT = as.character(convSecsToTime(Lopetusaika, Lopetuspvm, 60 * 60 *3)))]
#joinaa pakka_idt
sscols_divari <- SRC_DIVARI[, .(Pakka_ID = rivi_id,
                               Pakka_NO = Pakka,
                               Omistaja_nimi)]
join_pid_lauri <- sscols_divari[Omistaja_nimi == "Lauri"][temp, on = .(Pakka_NO = Laurin_pakka)][, Omistaja_nimi := NULL]
setnames(join_pid_lauri, c("Pakka_NO", "Pakka_ID"), c("Laurin_pakka", "Laurin_Pakka_ID"))
martin_pakat <- sscols_divari[Omistaja_nimi == "Martti"]
join_pid_martti <- martin_pakat[join_pid_lauri, on = .(Pakka_NO = Martin_pakka)][, Omistaja_nimi := NULL]
setnames(join_pid_martti, c("Pakka_NO", "Pakka_ID"), c("Martin_pakka", "Martin_Pakka_ID"))
temp_lauri <- join_pid_martti[, .(Divari,
                      Pakka_ID = Laurin_Pakka_ID,
                      Vastustajan_Pakka_ID = Martin_Pakka_ID,
                      Pakka_NO = Laurin_pakka,
                      Vastustajan_Pakka_NO = Martin_pakka,
                      Omistaja_ID = "L",
                      Vastustajan_Omistaja_ID = "M",
                      Kierros, Ottelu_ID,
                      Ottelu_NO = Ottelu_no,
                      BO_mode,
                      Turnaus_NO = TurnausNo,
                      Aloittaja = abs(Aloittaja - 1),
                      Peli_ID = peli_ID,
                      Voittaja = abs(Voittaja - 1),
                      Mulligan = Laurin_mulligan,
                      Vastustajan_Mulligan = Martin_mulligan,
                      Arvosana = Laurin_arvosana,
                      Vastustajan_Arvosana = Martin_arvosana,
                      Landit = Laurin_landit,
                      Vastustajan_Landit = Martin_landit,
                      Vuoroarvio,
                      Kasikortit = Laurin_kasikortit,
                      Vastustajan_Kasikortit = Martin_kasikortit,
                      Lifet = Laurin_lifet,
                      Vastustajan_Lifet = Martin_lifet,
                      Aloitus_DT,
                      Lopetus_DT)]

temp_martti <- join_pid_martti[, .(Divari,
                      Pakka_ID = Martin_Pakka_ID,
                      Vastustajan_Pakka_ID = Laurin_Pakka_ID,
                      Pakka_NO = Martin_pakka,
                      Vastustajan_Pakka_NO = Laurin_pakka,
                      Omistaja_ID = "M",
                      Vastustajan_Omistaja_ID = "L",
                      Kierros, Ottelu_ID,
                      Ottelu_NO = Ottelu_no,
                      BO_mode,
                      Turnaus_NO = TurnausNo,
                      Aloittaja = Aloittaja ,
                      Peli_ID = peli_ID,
                      Voittaja = Voittaja,
                      Mulligan = Martin_mulligan,
                      Vastustajan_Mulligan = Laurin_mulligan,
                      Arvosana = Martin_arvosana,
                      Vastustajan_Arvosana = Laurin_arvosana,
                      Landit = Martin_landit,
                      Vastustajan_Landit = Laurin_landit,
                      Vuoroarvio,
                      Kasikortit = Martin_kasikortit,
                      Vastustajan_Kasikortit = Laurin_kasikortit,
                      Lifet = Martin_lifet,
                      Vastustajan_Lifet = Laurin_lifet,
                      Aloitus_DT,
                      Lopetus_DT)]

temp_total <- rbind(temp_lauri, temp_martti)
#conv to posix
temp_total[, ':=' (Aloitus_DT = as.POSIXct(Aloitus_DT, tz = "EET"),
                   Lopetus_DT = as.POSIXct(Lopetus_DT, tz = "EET"))]
STG_PELIT <- temp_total
# IDt <-STG_PELIT[Lopetusaika < 60 * 60 *3, Ottelu_ID]
# ottelut <- STG_PELIT[Ottelu_ID %in% IDt]
# ottelut[Ottelu_ID == 253]
