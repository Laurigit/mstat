
library(data.table)
kansio <- "D:/Videos"
videot <- data.table(tiedostot = dir(kansio))
videot[, type := (word(tiedostot, sep = fixed("."), end = -1, start = -1))]
videot[, rivi := seq_len(.N)]
videot[, polku := paste0(kansio, "/", tiedostot)]
videot[, file_tieto := lapply(polku, file.info)]
videot[, alku_aika := file_tieto[[1]]$ctime, by = rivi]
videot[, loppu_aika := file_tieto[[1]]$mtime, by = rivi]
#videot[, kesto_sec_tot := as.integer(loppu_aika - alku_aika)]
#videot[, kesto_h := floor(kesto_sec_tot / 60 / 60)]
#videot[, kesto_min := floor((kesto_sec_tot - kesto_h * 60 * 60) / 60)]
#videot[, kesto_sec := floor((kesto_sec_tot - kesto_h * 60 * 60 - kesto_min * 60))]
required_data("ADM_TURN_DATA_ALL")
required_data("ADM_PELIT")
required_data("STG_PAKAT")
required_data("ADM_DMG_TURN_ALL")
vuoro_alku <- ADM_TURN_DATA_ALL[ADM_TURN_DATA_ALL[, .I[which.min(TSID)], by=Peli_ID]$V1][, .(max_posix_aika, Peli_ID)]
turnipelit <- ADM_PELIT[Peli_ID %in% vuoro_alku[, Peli_ID] &  Omistaja_ID == "L", .(Turnaus_NO, Pakka_ID, Vastustajan_Pakka_ID,
                                                              Voittaja, Aloittaja, Peli_ID)]
sspakat <- STG_PAKAT[, .(Pakka_ID, Pakka_NM)]
join_lauri <- sspakat[turnipelit, on = c("Pakka_ID")]
setnames(join_lauri, "Pakka_NM", "Laurin_pakka")
join_martti <- sspakat[join_lauri, on = .(Pakka_ID == Vastustajan_Pakka_ID)]
res <- join_martti[, .(Martin_pakka = Pakka_NM, Laurin_pakka, Turnaus_NO, Voittaja, Aloittaja, Peli_ID)]

join_alku <- res[vuoro_alku, on =  "Peli_ID"]

