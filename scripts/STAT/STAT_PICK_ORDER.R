required_data(c("ADM_PAKKA_COMPONENTS", "STG_PAKAT"))
comp <- copy(ADM_PAKKA_COMPONENTS)[is_current_form == TRUE & Maindeck == TRUE]

comp[, round_pick_order := round(avg_pick_order, 0)]
pick_data <- comp[Type_exact != "Basic Land", .(count_of_picks = sum(Count, na.rm = TRUE)) , by = .(round_pick_order, Omistaja_ID,
                                                                                                    Pakka_NM)]
                                                                                                    
pick_data[, share_of_picks := count_of_picks / sum(count_of_picks, na.rm = TRUE), by = .(Omistaja_ID, Pakka_NM)]
pick_data[, round_pick_order_per_two := ceiling(round_pick_order / 2)]
#pick_data[round_pick_order <= 2, sum(share_of_picks), by = .(Pakka_NM)][order(-V1)]
STAT_PICK_ORDER <- pick_data
