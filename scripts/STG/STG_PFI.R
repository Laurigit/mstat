#STG_PFI
required_data(c("SRC_PFI", "SRC_DIVARI"))
#datetime, Pakka_ID, kortti_lkm, hinnat,
pakkametataulu <- NULL
count_decks <- length(SRC_PFI)
for (pakka_no in 1:count_decks){ 
  
    omid <- SRC_PFI[[pakka_no]]$omistaja
    pakkano <- SRC_PFI[[pakka_no]]$pakkanumero
    valid_from <- as.POSIXct(SRC_PFI[[pakka_no]]$load_datetime$datetime, tz ="EET")
    price_high <- SRC_PFI[[pakka_no]]$price$high
    price_med <- SRC_PFI[[pakka_no]]$price$med
    price_low <- SRC_PFI[[pakka_no]]$price$low
    pakkakoko <- SRC_PFI[[pakka_no]]$list$total
    pakkacolors <- SRC_PFI[[pakka_no]]$info$colors
    pfi <- SRC_PFI[[pakka_no]]$pakka_form_id
    if(is.null(pakkacolors)) {pakkacolors <- NA}
    
    uusmetarivi <- data.table(Pakka_form_ID = pfi,
                             Omistaja_ID = omid,
                             Pakka_NO = pakkano,
                             Hinta=price_med,
                             Hinta_low = price_low,
                             Hinta_high = price_high,
                             Kortti_lkm_manastack = pakkakoko,
#                             Pakka_colors = pakkacolors, #not used as colors are calculated based on the deck
                             Valid_from_DT = valid_from
                             )
    
    pakkametataulu<-as.data.table(rbind(pakkametataulu,uusmetarivi))
    #laske voimassaolon päättyminen
    #pakkametataulu[,':=' (pvm_end=shift(pvm,1,type="lead"))]
  }

pakkametataulu_sorted <- pakkametataulu[order(Omistaja_ID, Pakka_NO, Valid_from_DT)]
pakkametataulu_sorted[,':=' (Valid_to_DT = shift(Valid_from_DT,1,type="lead")),
                      by = .(Omistaja_ID,
                             Pakka_NO)]
#fix na ending to future
pakkametataulu_sorted[,':=' (Valid_to_DT = as.POSIXct((ifelse(is.na(Valid_to_DT),
                                                  as.POSIXct("2100-01-01", tz = "EET"),
                                                  Valid_to_DT)), tz = "EET", origin = "1970-01-01"))]
sscols <- SRC_DIVARI[,. (Pakka_ID = rivi_id, Omistaja_ID = substr(Omistaja_nimi, 1, 1),
                         Pakka_NO = Pakka)]
#join_pakka_ID
#join_pid <- sscols[pakkametataulu_sorted, on = .(Omistaja_ID, Pakka_NO)]
 join_pid <- merge(sscols,pakkametataulu_sorted, by = c("Omistaja_ID", "Pakka_NO"), all = TRUE)
 missing_decks <- join_pid[is.na(Pakka_form_ID)]
puuttuvat_pakat <- nrow(missing_decks)
if(puuttuvat_pakat > 0) {
  for (looper in 1:puuttuvat_pakat) {
    new_file_name <- missing_decks[looper, paste0(Omistaja_ID, "_", Pakka_NO, ".json")]
    new_path <- paste0("./external_files/", new_file_name)
    pakka_input <- data.frame(name = new_file_name, size = 7790, type ="", datapath = new_path, stringsAsFactors = FALSE)
    
    file.copy("./external_files/default_deck_for_new_decks_without_decklist.template",
              new_path)
    process_uploaded_decks(pakka_input,
                           "./external_files/",input_default_decklist = TRUE)
    file.remove(new_path)
  }
}


join_pid[, Current_Pakka_form_ID := max(Pakka_form_ID), by = Pakka_ID]
join_pid[, ':=' (Omistaja_ID = NULL, Pakka_NO = NULL)]
STG_PFI <- join_pid

if(puuttuvat_pakat > 0) {
updateData("SRC_PFI", ADM_DI_HIERARKIA, input_env = globalenv())
}


# tulos <- omaReadJson("./external_files/")
# str(tulos[[1]][[100]])
# 
# #eka on taulutyyppi, esim json vai yhteenveto. Toka on pakan id
# [[1]]$cards[,. (name, count, type)] taulu, missä korttilkm, nimi, kortin tyyppi
# [[1]]$sideboard[,. (name, count, type)] taulu, missä korttilkm, nimi, kortin tyyppi
# 
# tulos[[1]][[100]][[2]]$cards (sama ku [[1]], mutta jaettuna korttiluokkiin)
# #[1] "Creatures"     "Lands"         "Instants"      "Enchantments"  "Sorceries"     "Artifacts"     "Planeswalkers"
# #[8] "Sideboard"
# 
# tulos[[1]][[100]][[4]] #hinnat
# 
# tulos[[1]][[120]][[5]]$created$date ja $created$timezone ja $colors
# 
# tulos[[1]][[200]]
