required_data("ADM_PAKKA_COMPONENTS")

orderi <- ADM_PAKKA_COMPONENTS[, c("Eka", "Toka", "Kolmas", "Neljas", "Viides") := tstrsplit(Name, " ", fixed=TRUE)]

sanat <- orderi[Eka != tolower(Eka), strsplit(paste0(Eka, collapse = " "), " ")]
uniikit_eka <- sanat[, .N, by = V1][, key := 1]

sanat2 <- orderi[Toka != tolower(Toka), strsplit(paste0(Toka, collapse = " "), " ")]
uniikit_toka <- sanat2[, .N, by = V1][, key := 1]
joini <- uniikit_toka[uniikit_eka, on = "key", allow.cartesian = TRUE]
joini[, conc := paste(i.V1, V1)]
uniikittot <- joini[, .N, by = conc]
STAT_PW <- uniikittot[, .(Sana = conc)]


