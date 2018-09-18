#UID_DIVARI
#required_data(c("STG_DIVARI", "STG_PAKAT", "STAT_LISAKORTIT", "ADM_PELIT"))
UID_DIVARI <- function(STG_DIVARI, STG_PAKAT, STAT_LISAKORTIT, ADM_PELIT) {
    maxturnaus <- STAT_LISAKORTIT[,.(Turnaus_NO = max(Turnaus_NO)), by = Pakka_ID]
    maxturnaus_tot <- maxturnaus[, max(Turnaus_NO)]
    sarjatilanne<- UID_SARJATAULUKKO(maxturnaus_tot, FALSE, ADM_PELIT, STG_PAKAT)
    total <- do.call(rbind, sarjatilanne)
    sarja_ss_cols <- total[,.(Pakka_NM, Score)]
    lisakortit <- STAT_LISAKORTIT[maxturnaus, on = .(Pakka_ID, Turnaus_NO)]
    join_lisakortit <- lisakortit[STG_DIVARI, on = "Pakka_ID"]
    sspakat <- STG_PAKAT[, .(Pakka_NM, Pakka_ID, Omistaja_ID)]
    join_nimi <- sspakat[join_lisakortit, on = "Pakka_ID"]
    join_score <- join_nimi[sarja_ss_cols, on = "Pakka_NM"]
    return(join_score)
}

