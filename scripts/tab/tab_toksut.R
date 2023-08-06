#tab toksut
output$toksulista  <- renderTable({

  required_data(c("STAT_CURRENT_PAKKA_COMPONENTS", "ADM_DIVARI"))
active <- ADM_DIVARI[, Pakka_ID]
cp <- copy(STAT_CURRENT_PAKKA_COMPONENTS)[Pakka_ID %in% active]
cp[, .N, by = Pakka_ID]
cp[, row_id := seq_len(.N)]
find_token <- cp[, res := grep("token", Text), by = row_id]

find_token[, subis := (str_extract_all(Text, "Create(.*?)token"))]
find_token[, subis]

regelist <- c("(?<=Create).*(?=token)", "(?<=create).*(?=token)" ,"(?<=Put).*(?=token)" ,"(?<=put).*(?=token)")
totres <- NULL
tot_dt <- NULL
for (regeloop in regelist) {
  find_token[, subis2 := (str_extract_all(Text, regeloop))]
  #find_token[, subis3 := (str_extract_all(Text, "(?<=create).*(?=token)"))]
  #find_token[, subis4 := (str_extract_all(Text, "(?<=put).*(?=token)"))]
  #find_token[, subis5 := (str_extract_all(Text, "(?<=Put).*(?=token)"))]
  #find_token[, all_list := list(list(subis2[[1]], subis3[[1]], subis4[[1]], subis5[[1]]))]
  find_token[, row_id := seq_len(.N)]
  #find_token[, leng := nchar(subis2[[1]]), by=row_id]
  #find_token[1, all_list]
  found <- copy(find_token)#[leng>0]
  
  #found[,  grep(pattern, subis2 , value = TRUE)]
  #matches <- unlist(str_extract_all(found[, subis2], "\\b[A-Z]\\w+"))
  
  found[, toksut := (str_extract_all(found[, subis2], "\\b[A-Z]\\w+"))]
  ss_found <- found[, .(Pakka_NM, toksut, Omistaja_ID)]
  dt_unlisted <- ss_found[,.(toksut = unlist(toksut)), by = setdiff(names(ss_found), 'toksut')]
  
  tot_dt <- rbind(dt_unlisted, tot_dt)

  #aggre2 <- found[, toksulist3 = list(toksut),), by = Pakka_ID]
  aggre3 <- found[, .(toksulist = (list(toksut)))]
  #aggre4<- found[, .(toksulist = (list(all_list)))]
  printtaa <- aggre3[, unique(unlist(toksulist))]
  totres <- c(totres, printtaa)
}
tot_dt_agg <- tot_dt[, .N, by = .(Pakka_NM, toksut, Omistaja_ID)]
analyss <- tot_dt_agg[, .(pakat = paste0(Pakka_NM, collapse = " "), count_pakat = .N, omistajat = paste0(unique(sort(Omistaja_ID)), collapse = "")), by = toksut]

analyss[order(omistajat, toksut)][, .(omistajat, toksut, pakat, count_pakat )]
res <- tot_dt_agg[order(Omistaja_ID, Pakka_NM, toksut)][, .(Omistaja_ID, Pakka_NM, toksut)]
res
})
# uniikit <- unique(totres)
# 
# sort(uniikit)
# #aggre <- found[, .(toksut = paste0( (str_extract_all(found[, subis2], "\\b[A-Z]\\w+")), collapse = " "))]
# #aggre[2]
# 
# 
# find_token[, subis2]
# find_token[, res2 := str_replace_all(res, "^.*?\\.(?=\\s|$)", "")]
# tokentext <- find_token[!is.na(res2), str_sub(Text, 2, -1)]
# tokentext[1]
# names <- data.table(name = unlist(str_extract_all(tokentext, "\\b[A-Z]\\w+")))
# coutnti <- names[, .N, by = name][order(-N)]
# coutnti[1:100]
# 
# STG_DECKS_DIM
# 
