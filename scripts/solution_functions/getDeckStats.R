#getDeckStats
#Omistaja <- "Lauri"
#Omistaja_NM <- "Lauri"
required_functions("getCardImg")
required_data("UID_UUSI_PELI")
getDeckStats <- function(Omistaja, UID_UUSI_PELI) {
 # browser()
lauri_stats <- UID_UUSI_PELI[Omistaja_NM == Omistaja, .(Deck = Pakka_NM_Dynamic,
                                                       'Win%' = paste0(round(Voitto_PCT, 2) * 100,
                                                                       " (", Voitto_PCT_rank, ")"),
                                                       'Win%-MA' = paste0(round(Voitto_PCT_MA, 2) * 100,
                                                                          " (", Voitto_PCT_MA_rank, ")"),
                                                       Streak = paste0(Putki,
                                                                       " (",Putki_rank, ")"),
                                                       Cards = paste0(Deck_size,
                                                                      " (",Deck_size_rank, ")"),
                                                       Shuffle8,
                                                       Most_same_card_raw = Most_wins_sames_card)]

tulos <-suppressWarnings(melt.data.table(lauri_stats[, -"Most_same_card_raw", with = FALSE], id.vars = "Deck"))
Pakkanimi <- tulos[1, Deck]
tulos[, riviteksti := paste0("<h4>", variable, ":<b> ", value, "</b><br>")]
getCardImg(UID_UUSI_PELI[Omistaja_NM == Omistaja, Most_wins_sames_card])
hmtlout <- paste0("<h3>",Pakkanimi, "<br>", paste0(tulos[, riviteksti], collapse =""))
#kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
#                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]

#fix athersnipe

lauri_stats[, Most_same_card :=  stringi::stri_trans_general(Most_same_card_raw, "Latin-ASCII")][, Most_same_card_raw := NULL]

result <- NULL
result$html <- htmlOutput
result$data <- lauri_stats
return(result)
}
