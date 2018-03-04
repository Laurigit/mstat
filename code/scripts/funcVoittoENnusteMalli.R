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
# peliData <- luecsv("pelit.csv")
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
# tulos <- voittoEnnusteMallit(peliData_ja_pfi)
voittoEnnusteMallit <- function(peliData_ja_pfi) {
  
  pelidata_joined_pakkatiedot <-  peliData_ja_pfi
  
  #loopataan kaikki pelit
  Laurin_pakat <- pelidata_joined_pakkatiedot[,.N, by = ,.(Laurin_pakka)][,N:=NULL][, avain := 1]
  Martin_pakat <-  pelidata_joined_pakkatiedot[,.N, by = ,.(Martin_pakka)][,N:=NULL][, avain := 1]
  pakkayhdistelmat <-merge(Laurin_pakat, Martin_pakat, all = TRUE, by = "avain", allow.cartesian = TRUE)
    #luo muuttujat. Kertoimet sen takia, että otetaan yhteiskerroin vaan VS_peliin.
  #looppaa_kombot
  pakkayhdistelmat[, avain := NULL]
  for(kierros in 1:nrow(pakkayhdistelmat)) {
    LP <- pakkayhdistelmat[kierros, Laurin_pakka]
    MP <- pakkayhdistelmat[kierros, Martin_pakka]
  
  
  
  #filtteröi data
  subset_pelit <- pelidata_joined_pakkatiedot[!is.na(Voittaja) & (Laurin_pakka == LP | Martin_pakka == MP),
                                              .(Laurin_pakka, Martin_pakka, Voittaja, Laurin_mulligan,
                                                Martin_mulligan, Laurin_pysyvyys_pct, Martin_pysyvyys_pct, Aloittaja, hinta_lauri, hinta_martti, laurin_kortti_lkm, martin_kortti_lkm)]
  
  subset_pelit[, ':=' (Mull_diff =  Martin_mulligan - Laurin_mulligan,
                       VS_peli_bool = ifelse(Laurin_pakka == LP & Martin_pakka == MP, 1, 0),
                       L_kerroin = ifelse(Laurin_pakka == LP, Laurin_pysyvyys_pct,1),
                       M_kerroin = ifelse(Martin_pakka == MP, Martin_pysyvyys_pct, 1),
                       laurin_keskihinta = hinta_lauri / laurin_kortti_lkm,
                       martin_keskihinta = hinta_martti / martin_kortti_lkm)]
  
  
  
  subset_pelit[, ':=' (weight = L_kerroin * M_kerroin,
                       keskiHintaEro = log((laurin_keskihinta / martin_keskihinta), base = exp(1)))]
  
  
  #analyysisarakkeet
  analyse_cols <- subset_pelit[, .(Laurin_pakka,
                                   Martin_pakka,
                                   Voittaja,
                                   Mull_diff,
                                   VS_peli_bool,
                                   weight,
                                   Aloittaja,
                                   keskiHintaEro)]
  count_vs_peli <- analyse_cols[, sum(VS_peli_bool)]
  if (count_vs_peli > 6) {
  model <- glm(data = analyse_cols, 
               formula = Voittaja ~ Aloittaja + Mull_diff  + VS_peli_bool + keskiHintaEro, 
               family = binomial(link = "logit")
               ,weights = weight)
  } else {
    model <- glm(data = analyse_cols, 
                 formula = Voittaja ~ Aloittaja + Mull_diff  + keskiHintaEro, 
                 family = binomial(link = "logit")
                 ,weights = weight)
  }

  
  model_list <- list()
  model_list[[1]]<-model
  pakkayhdistelmat[kierros, malli:=.(model_list)]
}
  pakkayhdistelmat [, ':=' (rivi = NULL)]
  #predict
  
 # aloittaja_for_dt <- Aloittaja
  
#  pred_data <- data.table(Mull_diff = 0 - 0, VS_peli_bool = 1, Aloittaja = 0,
                         # keskiHintaEro = log((1 / 1)/(1 / 1), base = exp(1)))
  #print(pred_data)
 # prediction <- predict(pakkayhdistelmat[1,model], pred_data, type = "response")
  return(pakkayhdistelmat)
  #analyse_cols[,fix_voit := ifelse(Voittaja==0, -1, 1)]
  #analyse_cols[, weight_fix := weight*fix_voit]
  #analyse_cols[, ':=' (cum_Voit = cumsum(fix_voit), cum_Voit_weight = cumsum(weight_fix))]
  #analyse_cols[, tulos := (predict(model, analyse_cols, type = "response"))]
  #analyse_cols[,.N,by=.(tulos, Mull_diff, Aloittaja, VS_peli_bool)]
}
