# #laske nykyturnauksen ennusteet
# 
# 
# 
# LP<-5
# MP<-5
# LMull <- 0
# MMull <- 0
# LHinta<-100
# MHinta <-100
# LKortit <- 50
# MKortit  <- 50
# Aloittaja <- 0
# #
# 
#  peliData <- luecsv("pelit.csv")
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)

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
# pelijarjestys[, ':=' (Tilanne = cumsum(Voittaja), cEnnuste = cumsum(voittoEnnusteVar))]
# peliTilanne2 <- ssCols[!is.na(Voittaja),sum(Voittaja)]
# pelijarjestys[is.na(Voittaja), Ennuste := cumsum(voittoEnnusteVar) +peliTilanne2 ]
# pelijarjestys[!is.na(Voittaja), Ennuste := Tilanne]


