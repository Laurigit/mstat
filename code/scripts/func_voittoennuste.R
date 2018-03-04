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
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
#voittoEnnuste(1,1,tulos,0,0,0,100,100,50,50)
voittoEnnuste <- function(LP, MP, ennusteMallit, LMull, MMull, Aloittaja, LHinta, MHinta, LKortit, MKortit) {

model <- ennusteMallit[Laurin_pakka == LP & Martin_pakka == MP, malli[[1]]]

pred_data <- data.table(Mull_diff = MMull - LMull, VS_peli_bool = 1, Aloittaja ,
                        keskiHintaEro = log((LHinta / LKortit)/(MHinta / MKortit), base = exp(1)))
#print(pred_data)
prediction <- predict(model, pred_data, type = "response")
return(prediction)
#analyse_cols[,fix_voit := ifelse(Voittaja==0, -1, 1)]
#analyse_cols[, weight_fix := weight*fix_voit]
#analyse_cols[, ':=' (cum_Voit = cumsum(fix_voit), cum_Voit_weight = cumsum(weight_fix))]
#analyse_cols[, tulos := (predict(model, analyse_cols, type = "response"))]
#analyse_cols[,.N,by=.(tulos, Mull_diff, Aloittaja, VS_peli_bool)]
}
