#find too old cards in side

required_data("STG_PFI")

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
aggregoi <- filtter_vaarat[, .(Turnaus_NO_max = max(Turnaus_NO)), by = .(Pakka_form_ID)]

sscols_PAKKA_COMPS <- STG_PAKKA_COMPONENTS[,. (Pakka_form_ID, Name, Count)]

joinaa <- aggregoi[sscols_PAKKA_COMPS, on = "Pakka_form_ID"]
#etitään pakka_Id takasin

sscols <- STG_PFI[, .(Pakka_ID, Pakka_form_ID)]
join_pid <- sscols[joinaa, on = "Pakka_form_ID"]

setorder(join_pid, Pakka_ID, Pakka_form_ID)
#paikataan uupuvat
join_pid[, Turnaus_NO_eka := na.locf(Turnaus_NO_max, fromLast = FALSE, na.rm = FALSE), by = Pakka_ID]
#ja toiseen suuntaan

join_pid[, Turnaus_NO := na.locf(Turnaus_NO_eka, fromLast = TRUE, na.rm = FALSE), by = Pakka_ID]
STAT_SIDE_CARD_AGE <- join_pid[,. (Pakka_ID,
                                   Pakka_form_ID,
                                   Name,
                                   Count,
                                   Turnaus_NO_drafted = Turnaus_NO,
                                   Card_age = max_turnaus - Turnaus_NO)]


