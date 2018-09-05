required_data(c("STG_PFI", "STG_PAKKA_COMPONENTS"))
#calc pysyvyys
temp <- STG_PFI
temp[,Pakka_form_pct := (calc_pysyvyys_pct(Pakka_form_ID, Current_Pakka_form_ID, STG_PAKKA_COMPONENTS)),
     by = Pakka_form_ID]
INT_PFI <- temp
