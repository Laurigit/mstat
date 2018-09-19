#STG_TEMP_DATA_STORAGE
required_data("SRC_TEMP_DATA_STORAGE")

#default arvot
defa <- data.table(
                   Aloitus_DT = as.character(now(tz = "EET") - 1000000000),
                   Laurin_mulligan = 0,
                   Martin_mulligan = 0,
                   Peli_ID = 1, 
                   melt ="edvar")
melt_defa <- suppressWarnings(melt.data.table(defa, id.vars = "melt")[, melt := NULL])
#join_defa
join_defa <- SRC_TEMP_DATA_STORAGE[melt_defa, on = .(muuttuja = variable)]
join_defa[, coal := coalesce2(arvo, value)]


STG_TEMP_DATA_STORAGE <- join_defa[, .(muuttuja, arvo = coal)]


