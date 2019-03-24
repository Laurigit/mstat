#STAT_BO_PELATUT_PELIT
required_data("ADM_PELIT")
temp <- ADM_PELIT
pelatut <- temp[!is.na(Voittaja)]
BO <- BO_conversio(pelatut)
BO[, ':=' (Pelattu_Peli =  1), by = Pakka_ID]
#BO[, ':=' (Voitto_PCT =  Voitta), by = Pakka_ID]
vahan_pelanneet <- BO[, .N, by = Pakka_ID]
STAT_BO_PELATUT_PELIT <- BO[Pakka_ID %in% vahan_pelanneet[N > 6 , Pakka_ID]]
