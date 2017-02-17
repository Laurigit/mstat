#joinaa pysyvyys_pct divariin
pysyvyys_pct<-pakkaUutuusProsentti(pakat)
pysyvyys_pct[,':=' (dt_alku=oma_timedate(pvm,kello),dt_loppu=oma_timedate(pvm_end,kello_end))]
laurin_pakat<-pysyvyys_pct[omistaja=="L",.(pakka_form_id=id,pakkanumero,dt_alku,dt_loppu)]
martin_pakat<-pysyvyys_pct[omistaja=="M",.(pakka_form_id=id,pakkanumero,dt_alku,dt_loppu)]
pelidata_dt<-pelidata_temp_all[,':=' (pelidt_alku=oma_timedate(Aloituspvm,Aloitusaika),pelitdt_loppu=oma_timedate(Lopetuspvm,Lopetusaika))]

joiniID_and_pct<-laurin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","pakkanumero==Laurin_pakka")]
#joinaa viela martti
joiniID_and_pct_both<-martin_pakat[pelidata_dt,on=c("dt_alku<pelidt_alku","dt_loppu>pelitdt_loppu","pakkanumero==Martin_pakka")]


