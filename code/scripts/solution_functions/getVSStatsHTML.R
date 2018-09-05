#getDeckStats
#Omistaja1 <- "Lauri"
getVSStatsHtml <- function(Omistaja1) {
  required_data("UID_UUSI_PELI")
  lauri_stats <- UID_UUSI_PELI[Omistaja_NM == Omistaja1, .(Games = Pelit_ABS_VS,
                                  'Winpct' = round(Voitto_PCT_VS, 2) * 100,
                                  'WinpctMA' = round(Voitto_PCT_MA_VS, 2) * 100, 
                                  'Streak' = Putki_VS)]
  text_stats <- lauri_stats[, .(idvar ="idvari",
                                Games,
                                'Win%' = paste0(Winpct, "-", 100 - Winpct),
                                'Win%-MA' = paste0(WinpctMA, '-', 100 - WinpctMA),
                                Streak = ifelse(Streak > 0,
                                                paste0(Streak, "-0"),
                                                paste0("0-", -Streak)))]
 
  
  
  tulos <-suppressWarnings(melt.data.table(text_stats, id.vars = "idvar")[,idvar := NULL])
  # tulos[variable == "Win_pct", color :=  ifelse(value > 50, "purple", "yellow")]
  # tulos[variable == "Win_pct-MA", color :=  ifelse(value > 50, "purple", "yellow")]
  # tulos[variable == "Streak", color :=  ifelse(value > 0, "purple", "yellow")]
  # tulos[variable == "Games", color :=  "blue"]
  # 

  tulos[, riviteksti := paste0("<h4>", variable, ": <b>", value, "</b><br>")]
  #  "<h4>", variable, ":<b> ", value, "<b><br/>")]
  hmtlout <- paste0("<h3>", "VS", "<br>", paste0(tulos[, riviteksti], collapse =""), "<br>")

  #hmtlout <- paste0("<h2>",Pakkanimi, "<h2/><br/>", paste0(tulos[, riviteksti], collapse =""))
  #kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
  #                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]
  
  #kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
  #                       saavutusNimi,": <b>",txtResult,"</b><h4/>")]
  
  
  return(hmtlout)
}
