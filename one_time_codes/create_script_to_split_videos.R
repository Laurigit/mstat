
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
                                                              Voittaja, Aloittaja, Peli_ID, Aloitus_DT, Lopetus_DT)]
sspakat <- STG_PAKAT[, .(Pakka_ID, Pakka_NM)]
join_lauri <- sspakat[turnipelit, on = c("Pakka_ID")]
setnames(join_lauri, "Pakka_NM", "Laurin_pakka")
join_martti <- sspakat[join_lauri, on = .(Pakka_ID == Vastustajan_Pakka_ID)]
res <- join_martti[, .(Martin_pakka = Pakka_NM, Laurin_pakka, Turnaus_NO, Voittaja, Aloittaja, Peli_ID, Aloitus_DT, Lopetus_DT)]
res[, kesto_sec_tot := as.integer(difftime(Lopetus_DT, Aloitus_DT, units = c("secs")))]
res[, kesto_text := convSecsToTimeFormat(kesto_sec_tot)]
res[, kesto_sec_tot := NULL]
videot[, ':=' (copy_alku_aika = alku_aika,
               copu_loppu_aika = loppu_aika)]

joini_ajat <- videot[res, on = .(alku_aika < Aloitus_DT, loppu_aika > Lopetus_DT), allow.cartesian = TRUE]
videolliset <- joini_ajat[!is.na(copy_alku_aika) ]
videolliset[, filename := paste0(kansio, "/cut/", "T", Turnaus_NO, "_", Laurin_pakka, "-", Martin_pakka, "_A", Aloittaja,  "_PID", Peli_ID)]
videolliset[, kesto_sec_tot := as.integer(difftime(alku_aika, copy_alku_aika, units = c("secs")))]
videolliset[, vakiominuutti := convSecsToTimeFormat(ifelse(kesto_sec_tot < 60, 0, 60))]
videolliset[, kelausAlkuAika := convSecsToTimeFormat(pmax(kesto_sec_tot - 60, 0))]
videolliset[, kesto_sec_tot := NULL]

videolliset[, syntaksi := paste0('ffmpeg -ss ', kelausAlkuAika, ' -i ', tiedostot,  ' -ss ',  vakiominuutti,  ' -t ',  kesto_text, ' ', filename, '.mp4')]
videolliset[, syntaksi_copy := paste0(kansio, '/ffmpeg -ss ', kelausAlkuAika, ' -i "', kansio, '/', tiedostot,  '" -ss ',  vakiominuutti,  ' -t ',  kesto_text, ' -acodec copy -vcodec copy ', filename, '.mp4')]

for(rivi in 1:nrow(videolliset)) {
 # print(videolliset[849, filename])
  system(videolliset[type == "mkv"][35][, syntaksi_copy], intern = FALSE)
 
}

videotiedostot <- videot[type == "mp4"]
# 
# for(rivi in 1:nrow(videotiedostot)) {
# 
#  res <- system(paste0("move ",  videotiedostot[rivi, polku], " ", "D:\\Videos\\raaka_leikattu\\", videotiedostot[rivi, tiedostot]))
# print(res)
#  }


