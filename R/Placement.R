##This function outputs a student's math placement from vectors containing the three components


uw_math_placement <- function(MFUND, AALG, TAG, output = "numeric"){
  
  
  
  
}















PLACEMENTData$TEST_TYPE <- NA
PLACEMENTData[which(PLACEMENTData$TEST_COMPONENT_ID %in% c("MBSC", "MFND")),]$TEST_TYPE <- "MATH_BASIC"
PLACEMENTData[which(PLACEMENTData$TEST_COMPONENT_ID %in% c("ALG", "AALG")),]$TEST_TYPE <- "MATH_ALG"
PLACEMENTData[which(PLACEMENTData$TEST_COMPONENT_ID %in% c("TRG", "TAG")),]$TEST_TYPE <- "MATH_TRIG"
PLACEMENTData[which(PLACEMENTData$TEST_COMPONENT_ID %in% c("ENGL", "UWEPT")),]$TEST_TYPE <- "ENGLISH"

PLACEMENTData <- PLACEMENTData[which((PLACEMENTData$TEST_SCORE > 149) & (PLACEMENTData$TEST_SCORE < 851)),]



#BEST_PLACEMENT <-  aggregate(PLACEMENTData$TEST_SCORE, by = list(PLACEMENTData$ID, PLACEMENTData$TestType), FUN = max)
#colnames(BEST_PLACEMENT) <- c("ID", "TestType", "TEST_SCORE")
#PLACEMENTData <- merge(x = PLACEMENTData, y = BEST_PLACEMENT, by = c("ID", "TestType", "TEST_SCORE"))


PLACEMENTDataSHORT <- PLACEMENTData[,c("ID", "TEST_SCORE", "TEST_TYPE")]
PLACEMENTDataSHORT <- dcast(PLACEMENTDataSHORT, ID ~ TEST_TYPE, value.var = "TEST_SCORE", fun.aggregate = max, na.rm = TRUE)


#colnames(PLACEMENTData) <- c("CAMPUS_ID", "ALG_MATH_TEST", "BASIC_MATH_TEST", "TRIG_MATH_TEST")
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$ENGLISH < 0),]$ENGLISH <- NA
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$MATH_ALG < 0),]$MATH_ALG <- NA
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$MATH_BASIC < 0),]$MATH_BASIC <- NA
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$MATH_TRIG < 0),]$MATH_TRIG <- NA
PLACEMENTDataSHORT$COMB <- PLACEMENTDataSHORT$ENGLISH + PLACEMENTDataSHORT$MATH_ALG



# PLACEMENTDataSHORT$MATH_COURSE <- "MATH 130 or MATH 211 or MATH 221 [QR-A satisfied]"
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$MATH_BASIC >= 150 & PLACEMENTDataSHORT$MATH_BASIC <= 355),]$MATH_COURSE <- "MATH 96"
# PLACEMENTDataSHORT[which(PLACEMENTDataSHORT$MATH_BASIC >= 356 & PLACEMENTDataSHORT$MATH_BASIC <= 465),]$MATH_COURSE <- "MATH 96 or MATH 141"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 150 & PLACEMENTDataSHORT$MATH_ALG <= 485)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 150 & PLACEMENTDataSHORT$MATH_TRIG <= 555))
#                    ,]$MATH_COURSE <-"MATH 112 (followed by MATH 113 for MATH 221) or MATH 130"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 150 & PLACEMENTDataSHORT$MATH_ALG <= 485)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 556 & PLACEMENTDataSHORT$MATH_TRIG <= 850))
#                    ,]$MATH_COURSE <- "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 486 & PLACEMENTDataSHORT$MATH_ALG <= 535)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 150 & PLACEMENTDataSHORT$MATH_TRIG <= 555))
#                    ,]$MATH_COURSE <- "MATH 112 or MATH 114 or MATH 130 or MATH 171/217 sequence [QR-A Satisfied]"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 486 & PLACEMENTDataSHORT$MATH_ALG <= 535)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 556 & PLACEMENTDataSHORT$MATH_TRIG <= 850))
#                    ,]$MATH_COURSE <- "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence [QR-A Satisfied]"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 536 & PLACEMENTDataSHORT$MATH_ALG <= 850)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 150 & PLACEMENTDataSHORT$MATH_TRIG <= 555))
#                    ,]$MATH_COURSE <- "MATH 113 or MATH 130 or MATH 211 or MATH 114 or MATH 171/217 sequence [QR-A Satisfied]"
# PLACEMENTDataSHORT[which((PLACEMENTDataSHORT$MATH_BASIC >= 486 & PLACEMENTDataSHORT$MATH_BASIC <= 850)
#                          & (PLACEMENTDataSHORT$MATH_ALG >= 536 & PLACEMENTDataSHORT$MATH_ALG <= 850)
#                          & (PLACEMENTDataSHORT$MATH_TRIG >= 556 & PLACEMENTDataSHORT$MATH_TRIG <= 850))
#                    ,]$MATH_COURSE <- "MATH 130 or MATH 211 or MATH 221 [QR-A satisfied]"
#

PLACEMENTCourse <- PLACEMENTDataSHORT %>%
  mutate(MATH_COURSE = case_when(MATH_BASIC >= 150 & MATH_BASIC <= 355 ~ "MATH 96",
                                 MATH_BASIC >= 356 & MATH_BASIC <= 465 ~ "MATH 96 or MATH 141",
                                 (MATH_BASIC >= 466 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 150 & MATH_ALG <= 485) &
                                   (MATH_TRIG >= 150 & MATH_TRIG <= 555) ~ "MATH 112 (followed by MATH 113 for MATH 221) or MATH 130",
                                 (MATH_BASIC >= 486 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 150 & MATH_ALG <= 485) &
                                   (MATH_TRIG >= 556 & MATH_TRIG <= 850) ~ "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence",
                                 (MATH_BASIC >= 466 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 486 & MATH_ALG <= 535) &
                                   (MATH_TRIG >= 150 & MATH_TRIG <= 555) ~ "MATH 112 or MATH 114 or MATH 130 or MATH 171/217 sequence [QR-A Satisfied]",
                                 (MATH_BASIC >= 466 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 486 & MATH_ALG <= 535) &
                                   (MATH_TRIG >= 556 & MATH_TRIG <= 850) ~ "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence [QR-A Satisfied]",
                                 (MATH_BASIC >= 466 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 536 & MATH_ALG <= 850) &
                                   (MATH_TRIG >= 150 & MATH_TRIG <= 555) ~ "MATH 113 or MATH 130 or MATH 211 or MATH 114 or MATH 171/217 sequence [QR-A Satisfied]",
                                 (MATH_BASIC >= 466 & MATH_BASIC <= 850) &
                                   (MATH_ALG >= 536 & MATH_ALG <= 850) &
                                   (MATH_TRIG >= 556 & MATH_TRIG <= 850) ~ "MATH 130 or MATH 211 or MATH 221 [QR-A satisfied]"
  ))
