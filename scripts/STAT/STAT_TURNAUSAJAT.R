#STAT_TURNAUSAJAT
required_data("ADM_PELIT")
sscols <- ADM_PELIT[!is.na(Aloitus_DT), .(Turnaus_NO, Lopetus_DT, Aloitus_DT)]

lopetukset <- sscols[sscols[, .I[which.max(Lopetus_DT)], by=Turnaus_NO]$V1, .(Lopetus_DT, Turnaus_NO)]
alotukset <-  sscols[sscols[, .I[which.min(Aloitus_DT)], by=Turnaus_NO]$V1,. (Aloitus_DT, Turnaus_NO)]
joini <- alotukset[lopetukset, on = "Turnaus_NO"]
joini[, Turnauskesto := difftime(Lopetus_DT, Aloitus_DT, tz = "EET", units = "days")]
STAT_TURNAUSAJAT <- joini
#con <- connDB(con)
#dbWriteTable(con, "STAT_TURNAUSAJAT", STAT_TURNAUSAJAT, row.names = FALSE, overwrite = TRUE)
