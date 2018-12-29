#laske nykyturnauksen ennusteet


# peliData <- luecsv("pelit.csv")
# LP<-5
# MP<-5
# LMull <- 0
# MMull <- 0
# LHinta<-100
# MHinta <-100
# LKortit <- 50
# MKortit  <- 50
# Aloittaja <- 0
# 
# peliData <- peliDataReact()
# pakat<-react_omaReadJson()
# pfi_data<-pakkaUutuusProsentti(pakat)
output$EV_plot <- renderPlot({
peliData_ja_pfi <-  peliData_ja_pfi_react()
# tulos <- voittoEnnusteMallit(peliData_ja_pfi)


maxTurnaus <- peliData_ja_pfi[,max(TurnausNo)]
#maxTurnaus <- maxTurnaus -1
turnausData <- peliData_ja_pfi[TurnausNo == maxTurnaus]
turnausData[, ':=' (alkuaika = oma_timedate(Aloituspvm, Aloitusaika), rivi = seq_len(.N)) ]

ennusteMallit <- voittoEnnusteMallit(peliData_ja_pfi, maxTurnaus -1)
voittoEnnusteData <- turnausData[, .(voittoEnnusteVar = voittoEnnuste(Laurin_pakka,
                                                                      Martin_pakka,
                                                                       ennusteMallit,
                                                                      0,
                                                                      0,
                                                                      Aloittaja,
                                                                      hinta_lauri,
                                                                       hinta_martti,
                                                                      laurin_kortti_lkm,
                                                                       martin_kortti_lkm)),
                                 by = rivi]
#joinbach
turnaus_ja_voitto <- turnausData[voittoEnnusteData, on = "rivi"][order(alkuaika)]
ssCols <- turnaus_ja_voitto[, .(alkuaika, Voittaja,
                                Voittaja_EV = (Voittaja - 0.5) * 2, voittoEnnusteVar,
                                voittoEnnusteVar_EV = (voittoEnnusteVar - 0.5) * 2)]
#deviin arvotaan voittajia
# ssCols[sample.int(32)[1:8], ':=' (Voittaja_EV = 1, alkuaika = seq_len(.N))]
# ssCols[sample.int(32)[1:8], ':=' (Voittaja_EV = -1, alkuaika = seq_len(.N))]
ssCols <- ssCols[order(alkuaika)]
alkuperainen_ennuste <- ssCols[,sum(voittoEnnusteVar_EV)]
ssCols[, ':=' (tilanne = cumsum(Voittaja_EV), cEnnuste = cumsum(voittoEnnusteVar_EV))]
ssCols[, loppuEnnuste := alkuperainen_ennuste + tilanne - cEnnuste]
#lisää nollarivi
nollarivi <- data.table(tilanne = 0, loppuEnnuste = alkuperainen_ennuste, peliNo = 0)
kuvaajadata <- ssCols[,. (tilanne, loppuEnnuste, peliNo = seq_len(.N))]
bindaa <- rbind(nollarivi, kuvaajadata)
melttaa <- melt(bindaa, id.vars = "peliNo", measure.vars = c("tilanne", "loppuEnnuste") )
melttaa[, Martin_johto := value]
melttaa[, ottelu_id := (peliNo/2)]
melttaa_aggr <- melttaa[ottelu_id %% 1 == 0, .(ottelu_id, value, Martin_johto, variable)]
ggplot(melttaa_aggr, aes(x = ottelu_id, y = Martin_johto, colour = variable)) + geom_line(size = 1.5) +
   theme_calc() + scale_color_calc()
})
  #pelijärjestys
#peliTilanne <- ssCols[!is.na(Voittaja),sum(Voittaja)]
# pelatut <- ssCols[!is.na(Voittaja)]
# pelaamattomat <- ssCols[is.na(Voittaja)]
# pelaamattomat[, ':=' (peli_jarj = as.numeric(NA), absEnnuset = abs(voittoEnnusteVar), copy_ennuste = voittoEnnusteVar)]
# for(loop in 1:nrow(pelaamattomat)) {
#   if (peliTilanne < 0) {
#     pelaamattomat[which.max(voittoEnnusteVar) , ':=' (peli_jarj = loop, voittoEnnusteVar = NA)]
#   } else if (peliTilanne > 0) {
#     pelaamattomat[which.min(voittoEnnusteVar), ':=' (peli_jarj = loop, voittoEnnusteVar = NA)]
#   } else {
#     pelaamattomat[which.min(absEnnuset) ,  ':=' (peli_jarj = loop, voittoEnnusteVar = NA)]
#   }
# 
#    peliTilanne <- peliTilanne + pelaamattomat[peli_jarj == loop, copy_ennuste]
#   # pelaamattomat_tulos <- rbind(pelaamattomat_tulos, pelaamattomat[])
# }
# result_pelaamattomat <- pelaamattomat[, .(alkuaika = as.numeric(NA), Voittaja = as.numeric(NA),
#                                           voittoEnnusteVar = copy_ennuste)]
# #liita platut ja pelaamattomat
# pelijarjestys <- rbind(pelatut,result_pelaamattomat)
# #
# 
# 
# pelijarjestys[, peli_no := seq_len(.N)]
# pelijarjestys[, ':=' (tilanne = cumsum(Voittaja), cEnnuste = cumsum(voittoEnnusteVar))]
# peliTilanne2 <- ssCols[!is.na(Voittaja),sum(Voittaja)]
# pelijarjestys[is.na(Voittaja), loppuEnnuste := cumsum(voittoEnnusteVar) +peliTilanne2 ]
# pelijarjestys[!is.na(Voittaja), loppuEnnuste := tilanne]


