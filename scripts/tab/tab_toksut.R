#tab toksut
output$toksulista  <- renderTable({

  required_data(c("STAT_TOKENS_IN_DECKS", "ADM_DIVARI"))
  active <- ADM_DIVARI[, Deck_name]
  tot_dt_agg <- STAT_TOKENS_IN_DECKS[Pakka_NM  %in% active]
  
  
  
  analyss <- tot_dt_agg[, .(pakat = paste0(Pakka_NM, collapse = " "), count_pakat = .N, omistajat = paste0(unique(sort(Omistaja_ID)), collapse = "")), by = .(Token, uniqueness)]
 # analyss[19]
analyss[order(omistajat, Token)][, .(omistajat, Token, pakat, count_pakat, uniqueness)]
res <- tot_dt_agg[order(Omistaja_ID, Pakka_NM, Token)][, .N, by = .(Omistaja_ID, Pakka_NM, Token, uniqueness)][, N := NULL]
res[, how_many_decks_uses_this := .N, by = .(Token, uniqueness)]
how_many_owners_a_token_has <- res[, .N, by = .(Token, Omistaja_ID, uniqueness)][, .(how_many_owners_has_this_token = .N), by = .(Token, uniqueness)]
join_to_res <- how_many_owners_a_token_has[res, on = .(Token, uniqueness)]

sorttaa_pakat <- join_to_res[,.(Pakka_NM, Token, uniqueness = str_sub(uniqueness, 1, 25), how_many_decks_uses_this, how_many_owners_has_this_token )][order(Pakka_NM, Token, how_many_decks_uses_this, how_many_owners_has_this_token)]
sorttaa_toksut <- join_to_res[, .(decks_using = paste0(Pakka_NM, collapse = " - ")), by = .(Token, str_sub(uniqueness, 1, 25), how_many_decks_uses_this, how_many_owners_has_this_token )][order(Token, how_many_decks_uses_this, how_many_owners_has_this_token)]
if (input$toksut_vai_pakat == "Decks") {
  sorttaa_pakat 
  } else {
    sorttaa_toksut
  }
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
