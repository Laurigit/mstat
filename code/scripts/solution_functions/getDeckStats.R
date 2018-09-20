#getDeckStats
#Omistaja <- "Lauri"
getDeckStats <- function(Omistaja, UID_UUSI_PELI) {
lauri_stats <- UID_UUSI_PELI[Omistaja_NM == Omistaja, .(Deck = Pakka_NM,
                                                       'Win%' = paste0(round(Voitto_PCT, 2) * 100,
                                                                       " (", Voitto_PCT_rank, ")"),
                                                       'Win%-MA' = paste0(round(Voitto_PCT_MA, 2) * 100,
                                                                          " (", Voitto_PCT_MA_rank, ")"),
                                                       Streak = paste0(Putki,
                                                                       " (",Putki_rank, ")"),
                                                       Cards = paste0(Deck_size,
                                                                      " (",Deck_size_rank, ")"),
                                                       Shuffle8)]
tulos <-suppressWarnings(melt.data.table(lauri_stats, id.vars = "Deck"))
Pakkanimi <- tulos[1, Deck]
tulos[, riviteksti := paste0("<h4>", variable, ":<b> ", value, "</b><br>")]
hmtlout <- paste0("<h3>",Pakkanimi, "<br>", paste0(tulos[, riviteksti], collapse =""))
#kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
#                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]


return(hmtlout)
}
