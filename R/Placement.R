##This function outputs a student's math placement from vectors containing the three components



####CODE USED FOR TESTING





#####






uw.math.placement <- function(MFND, AALG, TAG, MFND_COMPONENT_ID = "MFND",  
                              output = "numeric"){
  
#Puts input data into a data frame  
dt <- data.frame(MFND, AALG, TAG, MFND_COMPONENT_ID, AALG_COMPONENT_ID, TAG_COMPONENT_ID)

#Sets whether the placement is new or pre-2017.  
#This assumes that only one type are input for every row.
dt$CURRENT <- MFND_COMPONENT_ID == "MFND"
dt$PRE_2017 <- MFND_COMPONENT_ID == "MBSC"

#This outputs a numerical representation of placement if "numeric" (default) and otherwise the full text.

ifelse(output == "numeric",{
  
  #This table represents the current math placement table (as of 2021-03-16) with numeric values.
  dt$output <- NA
  dt[which((dt$CURRENT & (dt$MFND >= 150 & dt$MFND <= 355)) | 
                      `(dt$PRE_2017 & (dt$MFND >= 150 & dt$MFND <= 355))),]$output <- 1 

  dt[which((dt$CURRENT & (dt$MFND >= 356 & dt$MFND <= 465)) | 
                      `(dt$PRE_2017 & ((dt$MFND >= 356 & dt$MFND <= 405) | (dt$ALLG >= 150 & dt$AALG <= 415)))),]$output <- 2 
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- 3
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- 4
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- 5
  
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- 6
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- 7
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- 8
  
},
{
  
  
  #This table represents the current math placement table (as of 2021-03-16) with text.
  #Text refers to the current placements.
  dt$output <- NA
  dt[which((dt$CURRENT & (dt$MFND >= 150 & dt$MFND <= 355)) | 
             `(dt$PRE_2017 & (dt$MFND >= 150 & dt$MFND <= 355))),]$output <- "MATH 96" 
  
  dt[which((dt$CURRENT & (dt$MFND >= 356 & dt$MFND <= 465)) | 
             `(dt$PRE_2017 & ((dt$MFND >= 356 & dt$MFND <= 405) | (dt$ALLG >= 150 & dt$AALG <= 415)))),]$output <- "MATH 96 or MATH 141." 
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- "MATH 112 (followed by MATH 113 for MATH 221) or MATH 130"
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221)"
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- "MATH 114 or MATH 112 (followed by MATH 113 for MATH 221) or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]"
  
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]"
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 565 ))))),]$output <- "MATH 113 (will not need MATH 112 for MATH 221) or MATH 130 or MATH 211 or MATH 114 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]"
  
  dt[which((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 566 & dt$TAG <= 850 ))))),]$output <- " 	MATH 130 or MATH 211 or MATH 221 [QR-A satisfied]"
  
  
  
  
  
})
  
  
  
}











