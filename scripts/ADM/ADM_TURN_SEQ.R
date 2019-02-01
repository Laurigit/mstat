#ADM_TURN_SEQ

ADM_TURN_SEQ <- data.table(TSID = seq(1:1000),
                     Turn = ceiling(seq(1:1000) / 4),
                     End_phase = seq(1:1000) %% 2 == 0,
                     Starters_turn = ceiling(seq(1:1000) / 2) %% 2 == 1,
                     Next_turn_TSID = ceiling(seq(1:1000) / 2) * 2 + 1)
ADM_TURN_SEQ[, Turn_text := (paste0(Turn + ifelse(Starters_turn == FALSE,
                                                  0.5,
                                                  0),
                                    ifelse(End_phase == TRUE, " End", "")))]

