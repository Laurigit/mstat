#STAT_CURRENT_PAKKA_COMPONENTS
required_data(c("ADM_PAKKA_COMPONENTS", "STG_PAKAT"))
comp <- ADM_PAKKA_COMPONENTS[is_current_form == TRUE & Maindeck == TRUE]

mode <- comp[comp[Type != "Lands", .I[which.max(Count)], by=Pakka_ID]$V1][, .(Pakka_ID, Card_ID, Most_same_card = Name)]

comp[, ':=' (Power = as.numeric(word(Stats, 1,1, "/")),
             Toughness = as.numeric(word(Stats, 2,2, "/")))]

comp[, Cost_clean := gsub("}", "", Cost)]
comp[, Cost_clean := gsub("\\{", "", Cost_clean)]
all_chars <- paste0(comp[, Cost_clean], collapse = "")
uniqchars <- data.table(kirjaimet = strsplit(all_chars, "")[[1]])[, .N, by = kirjaimet]
uniqchars[, isnum := !is.na(as.numeric(kirjaimet))]
manacost_sarakkeet  <- uniqchars[isnum == FALSE  & !kirjaimet %in% c("/", "A", "N"), kirjaimet]

lapply(manacost_sarakkeet, function(inp) {
  comp[, (inp) := str_count(Cost,inp)]
  
})
comp[, AnyColor := as.numeric((gsub("([0-9]+).*$", "\\1", Cost_clean)))]
comp[, AnyColor := ifelse(is.na(AnyColor), 0, AnyColor)]

flatList <- c("Rarity", "Type", "Subtype", "Race", "Class", "Subclass")
totalCols <-STG_PAKAT[, .(Pakka_ID)]
for(inp in flatList) {
 
  comp_aggr_to_pakka <- comp[, .(.N), by = c(inp, "Pakka_ID")]


  
  #res <- melt.data.table(comp_aggr_to_pakka, id.vars = "dummy")
  formula_input <- paste("Pakka_ID ~ ", eval(inp))
  res <- dcast.data.table(comp_aggr_to_pakka, formula = formula_input, value.var = "N")
  res <- fix_colnames(res)
  getnamesOrig <- names(res)[-1]
  newNames <- paste0(inp, "_", getnamesOrig)
  names(res) <- c("Pakka_ID", newNames)
  totalCols <-merge.data.frame(x = totalCols, y = res, by = "Pakka_ID", all = TRUE)
}
totalCols[is.na(totalCols)] <- 0
#commons, rares, uncs, mythics, count_colors, count_types, count_race, count_class, count_subclass, mean_power, mean tough, mean conv_cc, mean_Cost_W

ssCols_comp <- comp[,. (Omistaja_ID,
                        Pakka_NM,
                        Pakka_ID,
                        Name,
                        Converted_Cost,
                        Rarity,
                        Colors,
                        Count,
                        Type_and_Class,
                        Type,
                        Subtype,
                        Type_exact,
                        Tribe_total,
                        Race,
                        Class,
                        Subclass,
                        Power,
                        Toughness,
                        W,
                        B,
                        U,
                        R,
                        G,
                        X,
                        P,
                        AnyColor,
                        avg_pick_order,
                        Text)]

STAT_CURRENT_PAKKA_COMPONENTS <- ssCols_comp

#muistaa joinaa mode
