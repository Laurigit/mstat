# peliData <- luecsv("pelit.csv")
# LP<-3
# MP<-6
# LMull <- 0
# MMull <- 0
# Aloittaja <- 1
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
voittoEnnuste <- function(LP, MP, peliData_ja_pfi, LMull, MMull, Aloittaja) {

  pelidata_joined_pakkatiedot <-  peliData_ja_pfi
  
  #filtteröi data
  subset_pelit <- pelidata_joined_pakkatiedot[!is.na(Voittaja) & (Laurin_pakka == LP | Martin_pakka == MP),
                           .(Laurin_pakka, Martin_pakka, Voittaja, Laurin_mulligan,
                              Martin_mulligan, Laurin_pysyvyys_pct, Martin_pysyvyys_pct, Aloittaja)]
  #luo muuttujat
  subset_pelit[, ':=' (Mull_diff =  Martin_mulligan - Laurin_mulligan,
    VS_peli_bool = ifelse(Laurin_pakka == LP & Martin_pakka == MP, 1, 0),
    weight = Laurin_pysyvyys_pct * Martin_pysyvyys_pct)]


#analyysisarakkeet
analyse_cols <- subset_pelit[, .(Voittaja,
                                 Mull_diff,
                                 VS_peli_bool,
                                 weight,
                                 Aloittaja)]

model <- glm(data = analyse_cols, 
             formula = Voittaja ~ Aloittaja + Mull_diff  + VS_peli_bool, 
             family = binomial(link = "logit")
             ,weights = weight
             )

#predict
print(paste0(MMull, " ", LMull, " "))
pred_data <- data.table(Mull_diff = MMull - LMull, VS_peli_bool = 1, Aloittaja = Aloittaja)
print(pred_data)
prediction <- predict(model, pred_data, type = "response")
return(prediction)
#analyse_cols[,fix_voit := ifelse(Voittaja==0, -1, 1)]
#analyse_cols[, weight_fix := weight*fix_voit]
#analyse_cols[, ':=' (cum_Voit = cumsum(fix_voit), cum_Voit_weight = cumsum(weight_fix))]
#analyse_cols[, tulos := (predict(model, analyse_cols, type = "response"))]
#analyse_cols[,.N,by=.(tulos, Mull_diff, Aloittaja, VS_peli_bool)]
}
