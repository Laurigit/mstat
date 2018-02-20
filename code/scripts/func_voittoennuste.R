# peliData <- luecsv("pelit.csv")
# LP<-5
# MP<-5
# LMull <- 0
# MMull <- 0
# Aloittaja <- 0
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
voittoEnnuste <- function(LP, MP, peliData_ja_pfi, LMull, MMull, Aloittaja, LHinta, MHinta, LKortit, MKortit) {

  pelidata_joined_pakkatiedot <-  peliData_ja_pfi
  
  #filtteröi data
  subset_pelit <- pelidata_joined_pakkatiedot[!is.na(Voittaja) & (Laurin_pakka == LP | Martin_pakka == MP),
                           .(Laurin_pakka, Martin_pakka, Voittaja, Laurin_mulligan,
                              Martin_mulligan, Laurin_pysyvyys_pct, Martin_pysyvyys_pct, Aloittaja, hinta_lauri, hinta_martti, laurin_kortti_lkm, martin_kortti_lkm)]
  #luo muuttujat. Kertoimet sen takia, että otetaan yhteiskerroin vaan VS_peliin.
  subset_pelit[, ':=' (Mull_diff =  Martin_mulligan - Laurin_mulligan,
    VS_peli_bool = ifelse(Laurin_pakka == LP & Martin_pakka == MP, 1, 0),
    L_kerroin = ifelse(Laurin_pakka == LP, Laurin_pysyvyys_pct,1),
    M_kerroin = ifelse(Martin_pakka == MP, Martin_pysyvyys_pct, 1),
    laurin_keskihinta = hinta_lauri / laurin_kortti_lkm,
    martin_keskihinta = hinta_martti / martin_kortti_lkm)]
  
  
  subset_pelit[, ':=' (weight = L_kerroin * M_kerroin,
                       keskiHintaEro = log((laurin_keskihinta / martin_keskihinta), base = exp(1)))]


#analyysisarakkeet
analyse_cols <- subset_pelit[, .(Voittaja,
                                 Mull_diff,
                                 VS_peli_bool,
                                 weight,
                                 Aloittaja,
                                 keskiHintaEro)]

model <- glm(data = analyse_cols, 
             formula = Voittaja ~ Aloittaja + Mull_diff  + VS_peli_bool + keskiHintaEro, 
             family = binomial(link = "logit")
             ,weights = weight
             )

#predict

aloittaja_for_dt <- Aloittaja

pred_data <- data.table(Mull_diff = MMull - LMull, VS_peli_bool = 1, Aloittaja = aloittaja_for_dt,
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
