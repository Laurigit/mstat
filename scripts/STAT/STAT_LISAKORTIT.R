#STAT_LISAKORTIT
required_data(c("ADM_PELIT", "ADM_TURNAUSSAANTO"))
required_functions("Prepare_Pelit_for_stats")

BO_stats <- Prepare_Pelit_for_stats(ADM_PELIT,
                                    MA = "NO",
                                    PFI = FALSE,
                                    BO = TRUE)

#voittokerroin on 1, jos pelataan paras x tyylillä. JOs ei, niin sitten pelien lukumäärä
BO_stats[, Voittokerroin := ifelse(BO_mode == 1, 1, Peli_LKM)]
#BO_stats[, BO_mode := 1]
aggr_pelit <- BO_stats[, .(sum_lisakorttivoito = sum(Voittaja * Voittokerroin + Tasapeli / 2 * Voittokerroin, na.rm = TRUE),
                           Pakka_form_ID = max(Pakka_form_ID)),
                                           , by = .(Pakka_ID, Turnaus_NO, Divari)]
#aggr_pelit[, lisakorttivoitot := (sum_Voittaja + sum_Tasapeli / 2) * sum_Voittokerroin]

turnaussaanto<- ADM_TURNAUSSAANTO
#levita saannot
turnauksia<-data.table(TurnausNoSeq=1:1000)
#setwd("C:/Users/Lauri/Documents/R/mstat2/code/omawd")
levite<-data.table(expand.grid(1:10000,1:100))
setnames(levite,c("Var1","Var2"),c("Turnaus_NO","Divari"))
joinsaanto <- turnaussaanto[levite,on=c("Turnaus_NO","Divari")]
joinsaanto<-joinsaanto[order(Turnaus_NO,Divari)]
#korvaa NA:t seuraavalla

joinsaanto[, lisakortit_per_voitto := na.locf(lisakortit_per_voitto, na.rm = FALSE), by = Divari]


join_lisakortit <- joinsaanto[aggr_pelit , on = .(Turnaus_NO, Divari)]
join_lisakortit[, lisakortit := as.numeric(lisakortit_per_voitto) * sum_lisakorttivoito]
setorder(join_lisakortit, Pakka_ID, Turnaus_NO)
join_lisakortit[, lisakortit_cum := cumsum(lisakortit), by = Pakka_ID]
STAT_LISAKORTIT <- join_lisakortit[,.(Pakka_ID, Lisakortit_lkm = lisakortit_cum, Turnaus_NO, Pakka_form_ID)]

