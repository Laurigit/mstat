#STAT_CURRENT_PAKKA
required_data(c("ADM_PAKKA_COMPONENTS", "STG_PAKAT", "ADM_PELIT"))
comp <- ADM_PAKKA_COMPONENTS[is_current_form == TRUE & Maindeck == TRUE]

comp_hist <- ADM_PAKKA_COMPONENTS[Maindeck == TRUE]
pfi_voitot <- ADM_PELIT[!is.na(Voittaja),.(sum_voitot = sum(Voittaja, na.rm = TRUE), sum_pelit = .N), by = .( Pakka_form_ID)]
#join voitot
comp_and_wins <- pfi_voitot[comp_hist, on = "Pakka_form_ID"]
#only include cards that are currently in the deck
curr_card_list <- comp_and_wins[is_current_form == TRUE, .N, by = Card_ID]

most_wins <- comp_and_wins[Type != "Lands" & Card_ID %in% curr_card_list[, Card_ID],
                           .(sum_voitot = sum(sum_voitot * Count,
                                              na.rm = TRUE),
                             avg_voitot = sum(sum_voitot * Count,
                                              na.rm = TRUE) / sum(sum_pelit * Count,
                                                                  na.rm = TRUE),
                             count_pelit = sum(sum_pelit * Count, na.rm = TRUE)),
                           by = .(Pakka_ID, Card_ID, Name)]
most_wins[, ':=' (Likelihood = -sum(dbinom(sum_voitot:count_pelit, count_pelit, 0.5))),  by = .(Pakka_ID, Card_ID, Name)]
most_wins_final <- most_wins[most_wins[, .I[which.max(Likelihood)], by = Pakka_ID]$V1][, .(Pakka_ID, Card_ID_most_wins = Card_ID, Most_wins_sames_card = Name)]


mode <- comp[comp[Type != "Lands", .I[which.max(Count)], by=Pakka_ID]$V1][, .(Pakka_ID, Card_ID, Most_same_card = Name)]
comp[Type == "Creatures" & substr(Stats, 1, 1) != "*" , ':=' (Power = as.numeric(word(Stats, 1,1, "/")),
             Toughness = as.numeric(word(Stats, 2,2, "/")))]
comp[Type == "Planeswalkers", Loyalty := str_sub(Stats,-2, -2)]
comp[, Cost_clean := gsub("}", "", Cost)]
comp[, Cost_clean := gsub("\\{", "", Cost_clean)]
#muutetaan tuplavarisymbolit varittomaksi
comp[, rivi := .I]
comp[, is_multimana_symbol := as.numeric(grepl("/", Cost_clean)), by = rivi]


all_chars <- paste0(comp[, Cost_clean], collapse = "")

uniqchars <- data.table(kirjaimet = strsplit(all_chars, "")[[1]])[, .N, by = kirjaimet]
suppressWarnings(uniqchars[, isnum := !is.na(as.numeric(kirjaimet))])
manacost_sarakkeet  <- uniqchars[isnum == FALSE  & !kirjaimet %in% c("/", "A", "N"), kirjaimet]

lapply(manacost_sarakkeet, function(inp) {
  comp[, (inp) := str_count(Cost,inp)]
  
})
suppressWarnings(comp[, AnyColor := as.numeric((gsub("([0-9]+).*$", "\\1", Cost_clean)))])
comp[, AnyColor := ifelse(is.na(AnyColor), 0, AnyColor)]

flatList <- c("Rarity", "Type")
totalCols <-STG_PAKAT[, .(Pakka_ID)]
for(inp in flatList) {
 
  comp_aggr_to_pakka <- comp[, .(.N), by = c(inp, "Pakka_ID")]
  comp_aggr_to_pakka[, pct := N / sum(N, na.rm = TRUE), by = c("Pakka_ID")]
#res <- melt.data.table(comp_aggr_to_pakka, id.vars = "dummy")
  form_input <- paste("Pakka_ID ~", eval(inp))
  res <- dcast.data.table(comp_aggr_to_pakka, formula = form_input, value.var = "pct")
  res <- fix_colnames(res)
  getnamesOrig <- names(res)[-1]
  newNames <- paste0(inp, "_", getnamesOrig)
  names(res) <- c("Pakka_ID", newNames)
  totalCols <-merge.data.frame(x = totalCols, y = res, by = "Pakka_ID", all = TRUE)
}
totalCols[is.na(totalCols)] <- 0

#commons, rares, uncs, mythics, count_colors, count_types, count_race, count_class, count_subclass, mean_power, mean tough, mean conv_cc, mean_Cost_W

aggr_comp <- comp[is_multimana_symbol==0,. (  
                        W = sum(W * Count, na.rm = TRUE),
                        B = sum(B * Count, na.rm = TRUE),
                        U = sum(U * Count, na.rm = TRUE),
                        R = sum(R * Count, na.rm = TRUE),
                        G = sum(G * Count, na.rm = TRUE),
                        X = sum(X * Count, na.rm = TRUE),
                        
                        C = as.integer(sum(P * Count, na.rm = TRUE) + sum(AnyColor * Count, na.rm = TRUE)))
                  , by = .(Omistaja_ID,
                                          Pakka_NM,
                                          Pakka_ID
                                          
                        )]

melt_aggr_comp <- melt.data.table(aggr_comp, id.vars = c("Omistaja_ID", "Pakka_NM", "Pakka_ID"))
melt_aggr_comp[variable != "C", maxVariLKM := max(value), by = .(Pakka_ID)]
melt_aggr_comp[value > 0, ':=' (Kirjain = ifelse(value / maxVariLKM > 0.3, toupper(variable), tolower(variable)))]
melt_aggr_comp[, Kirjain := ifelse(is.na(Kirjain), "", Kirjain)]
setorder(melt_aggr_comp, Pakka_ID, -value)
variNimi <- melt_aggr_comp[Kirjain != "x", .(Colors = sort_MTG_colors(paste0(Kirjain, collapse = ""))),
                           by = .(Pakka_ID)]

aggr_comp_pct <- aggr_comp[,. (  
  W = W / (W + B + U + R + G + X + C),
  B = B / (W + B + U + R + G + X + C),
  U = U / (W + B + U + R + G + X + C),
  R = R / (W + B + U + R + G + X + C),
  G = G / (W + B + U + R + G + X + C),
  X = X / (W + B + U + R + G + X + C),
  C = C / (W + B + U + R + G + X + C))
  , by = .(Omistaja_ID,
           Pakka_NM,
           Pakka_ID
           
  )]

joinVari <- aggr_comp_pct[variNimi, on = "Pakka_ID"]
joinmode <- mode[joinVari, on = "Pakka_ID"]
joinWins <- most_wins_final[joinmode, on = "Pakka_ID"]
joinWins[, ':=' (Pakka_NM_Dynamic = paste(word(Pakka_NM, 1, sep = "_"),
                                          (Colors),
                                          word(Pakka_NM, -1, sep = "_"),
                                          sep = "_"),
                 Pakka_NM_no_color = paste(word(Pakka_NM, 1, sep = "_"),
                                           word(Pakka_NM, -1, sep = "_"),
                                           sep = "_"))]

STAT_CURRENT_PAKKA <- joinWins[order(as.numeric(Pakka_ID))]

#muistaa joinaa mode
