#render_deck_evolve_plot

  
  UID_MALLI_KOMPONENTIT <- function(STAT_VOITTOENNUSTE, input_Peli_ID) {
    
# input_Peli_ID  
  Martin_pakka <- STAT_VOITTOENNUSTE[Peli_ID == input_Peli_ID & Omistaja_ID == "M", Pakka_ID]
  Laurin_pakka <- STAT_VOITTOENNUSTE[Peli_ID == input_Peli_ID & Omistaja_ID == "L", Pakka_ID]
  Aloittaja_input <- STAT_VOITTOENNUSTE[Peli_ID == input_Peli_ID & Omistaja_ID == "M", Aloittaja]
ss_cols <- STAT_VOITTOENNUSTE[Omistaja_ID == "M" &
                      Pakka_ID == Martin_pakka &
                        Vastustajan_Pakka_ID == Laurin_pakka &
                      Aloittaja == Aloittaja_input,
                    .(                      
                      VS_TN ,
                      Aloittaja_TN,
                      Pakka_TN,
                      Pakka_ID,
                      
                      Vastustajan_Pakka_ID,
                      Ennuste_orig = ennuste - 0.5,
                                            Ennuste = VS_TN + Aloittaja_TN + Pakka_TN,
                                            Turnaus_NO)]
ss_cols_sort <- ss_cols[order(Turnaus_NO)]
graphs_breaks <- ss_cols_sort[, Turnaus_NO]

melttaa <- melt(ss_cols_sort, id.vars = "Turnaus_NO", measure.vars = c("VS_TN", "Aloittaja_TN",
                                                                 "Pakka_TN",
                                                                 "Ennuste"))
melttaa[, Martin_etu := value]

return(melttaa)

}
