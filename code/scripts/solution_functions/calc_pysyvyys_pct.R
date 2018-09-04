#required_data("STG_COMPONENTS")
#component_data <- STG_PAKKA_COMPONENTS
#input_PFID <- 1
#input_PFI_newer <- 13

calc_pysyvyys_pct <- function(input_PFID, input_PFI_newer, component_data) {
  #pakan idt
  pakka <- component_data[Pakka_form_ID %in% c(input_PFID, input_PFI_newer) & Type != "Lands" & Maindeck == TRUE,
                          .(Name, Count, Pakka_form_ID)]
  joini <- pakka[Pakka_form_ID == input_PFID][pakka[Pakka_form_ID == input_PFI_newer], on = "Name"]
  joini[, Count := ifelse(is.na(Count), 0, Count)]
  #if old deck has more cards than new one, limit the old count to new
  joini[, max_card_vertailu := pmin(Count, i.Count,na.rm =TRUE)]
  aggr <- joini[, .(sum_vanha = sum(max_card_vertailu), sum_uus = sum(i.Count))]
  pysyvyys_pct <- aggr[, sum_vanha] / aggr[, sum_uus]
  return(pysyvyys_pct)
}
