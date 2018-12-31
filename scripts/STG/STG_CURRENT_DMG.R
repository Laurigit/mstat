#STG_CURRENT_DMG
required_data("SRC_CURRENT_DMG")
STG_CURRENT_DMG <- SRC_CURRENT_DMG[, .(DID = as.numeric(DID),
                                       Amount = as.numeric(Amount),
                                       Target_player,
                                       Dmg_source,
                                       Combat_dmg = as.numeric(Combat_dmg),
                                       Input_Omistaja_NM,
                                       TSID = as.numeric(TSID),
                                       Peli_ID = as.numeric(Peli_ID))]
