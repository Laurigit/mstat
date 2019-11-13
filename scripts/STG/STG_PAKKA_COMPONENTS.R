#STG_PAKKA_COMPONENTS
required_data("STG_PFI")
required_data("SRC_CARDS_DIM")
required_data("SRC_CARDS")

sscols_cards_dim <- SRC_CARDS_DIM[, .(Type, Name)]
sscols_cards <- SRC_CARDS[, .(Name, Pakka_form_ID, Maindeck, Pakka_ID)]

join_ss <- sscols_cards_dim[sscols_cards, on = .(Name)]
join_ss_aggr <- join_ss[, .(count = .N), by = .(Type, Name, Pakka_form_ID, Maindeck)]

join_ss_aggr[, ':=' (
  Type_exact = word(Type, start = 1, sep = " — "),
  Tribe_total = word(Type, start = 2, sep =" — ")
)]
join_ss_aggr[, ':=' (Race = word(Tribe_total, start = 1, sep = " "),
                     Class = word(Tribe_total, start = 2, sep = " "),
                     Subclass = word(Tribe_total, start = 3, sep = " "),
                     Subtype = word(Type_exact, start = -2, end = -2, sep = " "))]



#get all different card types
# alldecks[, .N, by = Card_type_exact][, .N, by = Card_type_exact][, Card_type_exact]
#                    
#                    word(type, -2, sep = fixed(" ")) )]

STG_PAKKA_COMPONENTS <- join_ss_aggr[, .(Pakka_form_ID,
                                         Card_ID = "id_not_used",
                                         Count = count,
                                         Name = gsub("Æ", "Ae", Name),
                                         Maindeck,
                                         Type_and_Class = "",
                                         Type,
                                         Subtype,
                                         Type_exact,
                                         Tribe_total,
                                         Race,
                                         Class,
                                         Subclass
)]
