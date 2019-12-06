#find too old cards in side

required_data("STG_PFI")
required_data("STG_PAKAT")
required_data("STG_PAKKA_COMPONENTS")

required_data("STAT_TURNAUSAJAT")
required_data("ADM_PELIT")
max_turnaus <- ADM_PELIT[, max(Turnaus_NO)]


sscolst_ajat <- STAT_TURNAUSAJAT[, .(Turnaus_NO, Aloitus_DT, Lopetus_DT,
                                                    # turnaus_Aloitus_DT = Aloitus_DT,
                                                     #turnaus_Lopetus_DT = Lopetus_DT,
                                                     key = 1)]
sscols <- STG_PFI[,. (Pakka_form_ID, Pakka_ID, Valid_from_DT, Valid_to_DT,
                      key = 1
                      #,
                     # copy_Valid_from_DT = Valid_from_DT,
                      #copy_Valid_to_DT = Valid_to_DT
                      )]

#eka kartesian, sitten filtterillä ulos
inequijoini <- sscolst_ajat[sscols, on = "key", allow.cartesian = TRUE]

#filtterii
filtter_vaarat <- inequijoini[!(Aloitus_DT < Valid_to_DT & Lopetus_DT < Valid_from_DT) &
                                !(Aloitus_DT > Valid_to_DT & Lopetus_DT > Valid_from_DT) ]
aggregoi <- filtter_vaarat[, .(Turnaus_NO_max = min(Turnaus_NO)), by = .(Pakka_form_ID)]

sscols_PAKKA_COMPS <- STG_PAKKA_COMPONENTS[, .(Count = sum(Count)), by = .(Pakka_form_ID, Name, Maindeck)]

joinaa <- aggregoi[sscols_PAKKA_COMPS, on = "Pakka_form_ID"]
#etitään pakka_Id takasin

sscols <- STG_PFI[, .(Pakka_ID, Pakka_form_ID)]
join_pid <- sscols[joinaa, on = "Pakka_form_ID"]#[Pakka_ID == 33 & Name == "Angelic Page"]

setorder(join_pid, Pakka_ID, Pakka_form_ID)
#paikataan uupuvat
join_pid[, Turnaus_NO_eka := na.locf(Turnaus_NO_max, fromLast = FALSE, na.rm = FALSE), by = Pakka_ID]
#ja toiseen suuntaan

join_pid[, Turnaus_NO := na.locf(Turnaus_NO_eka, fromLast = TRUE, na.rm = FALSE), by = Pakka_ID]

#sorttaa uusiks ja huomio vaan muutokset

setorder(join_pid, Pakka_ID, Turnaus_NO, Pakka_form_ID, Name)
#join_pid[Pakka_ID == 33 & Name == "Angelic Page"]
diff <- join_pid
diff[, Muutos := Count - lag(Count), by = .(Pakka_ID, Name)]
diff[, Muutos_NA := ifelse(is.na(Muutos), Count, Muutos)]
filter_out_nochange <- diff[Muutos_NA > 0]
# t1 <- filter_out_nochange[Pakka_ID == 33]#[, sum(Muutos_NA)]
# t2 <- STG_PAKKA_COMPONENTS[Pakka_form_ID ==  312][, sum(Count)]
# joinitesti <- t2[t1, on = "Name"]
# joinitesti[is.na(Count)]

#korjaa vielä kortit, jotka on ollut sidessä, mutta ei oo enää.
maxpfi_data <- join_pid[, .(max_pfi = max(Pakka_form_ID)), by = Pakka_ID]
#uusimpien pakkojen laput
kortit <- join_pid[Pakka_form_ID %in% maxpfi_data[, max_pfi],. (Pakka_ID, Name, aa = "aa")]
filter_out_poistetut <- filter_out_nochange[kortit, on = c("Pakka_ID", "Name")]
#filter_out_poistetut <- kortit[filter_out_nochange, on = c("Pakka_ID", "Name")]
#filter_out_poistetut[is.na(aa) & Maindeck == FALSE]
#joinaa omistaja
sscols <- STG_PAKAT[Side >= -1, .(Pakka_NM, Pakka_ID, Omistaja_ID, Side)]
join_omi <- filter_out_poistetut[sscols, on = "Pakka_ID"]
#join_omi[is.na(aa) & Maindeck == FALSE & Omistaja_ID == "M"]

STAT_SIDE_CARD_AGE <- join_omi[,. (Pakka_ID,
                                   Pakka_form_ID,
                                   Omistaja_ID,
                                   Name,
                                   Side,
                                   Maindeck,
                                   Count = Muutos_NA,
                                   Turnaus_NO_drafted = Turnaus_NO,
                                   Card_age = max_turnaus - Turnaus_NO)]

# con <- connDB(con)
# dbWriteTable(con, "STAT_SIDE_CARD_AGE", STAT_SIDE_CARD_AGE, row.names = FALSE, overwrite = TRUE)
# sidet <- STAT_SIDE_CARD_AGE[ Side == 1][, .N, by = .(Omistaja_ID, Card_age)][order(Card_age)]
# side_agg <- STAT_SIDE_CARD_AGE[Side == 1, sum(Count), by = .(Card_age, Omistaja_ID)][order(Card_age)]
# 
# vert <- dcast.data.table(side_agg, formula = Card_age ~ Omistaja_ID)
# vert[, .(l = sum(L), m = sum(M))]
