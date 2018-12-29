#ADM_PICKIT
#tän ja ADM_DICARI VOIS YHDISTÄÄ NIIN, ETTÄ ADM_DIVARISSA OIS MYÖS NE JOITA EI PICKATTU VIIMEKS
required_data(c("STG_DIVARI", "STG_PAKAT"))
  ssdiv <- STG_DIVARI[1 == 1]
  sspakat <- STG_PAKAT[Retired == 0 & Side == 0, .(Pakka_ID, Omistaja_ID, Pakka_NM)]
  joini <- ssdiv[sspakat, on = "Pakka_ID"]
ADM_PICKIT <- joini

