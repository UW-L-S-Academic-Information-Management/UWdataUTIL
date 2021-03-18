##This function outputs a student's math placement from vectors containing the three components



####CODE USED FOR TESTING

# names <- c("Mike", "Jen", "Joe")
# MFND <- c(450, 670, 555)
# AALG <- c(598, 469, 344)
# TAG <- c(722, 358, 658)
# 
# dt <- data.frame(names, MFND, AALG, TAG)
# 
# dt$placement <- uw.math.placement(dt$MFND, dt$AALG, dt$TAG)
# 
# uw.math.placement(MFND = c(450, 670, 555), 
#                   AALG = c(598, 469, 344), 
#                   TAG = c(722, 358, 658), 
#                   MFND_COMPONENT_ID = c("MFND", "MBSC", "MBSC"),
#                   output_type = "text")


#MFND <- c(450, 670, 555)
#AALG <- c(598, 469, 344)
#TAG <- c(722, 358, 658)
#MFND_COMPONENT_ID = c("MFND", "MBSC", "MBSC")
#output = "numeric"
#####






uw.math.placement <- function(MFND, AALG, TAG, MFND_COMPONENT_ID = "MFND",  
                              output_type = "numeric"){
  
#Puts input data into a data frame  
dt <- data.frame(MFND, AALG, TAG, MFND_COMPONENT_ID)

#Sets whether the placement is new or pre-2017.  
#This assumes that only one type are input for every row.
dt$CURRENT <- MFND_COMPONENT_ID == "MFND"
dt$PRE_2017 <- MFND_COMPONENT_ID == "MBSC"

dt$output <- NA


#This outputs a numerical representation of placement if "numeric" (default) and otherwise the full text.

ifelse(output_type == "numeric",{
  
  #This table represents the current math placement table (as of 2021-03-16) with numeric values.
dt$output <- ifelse(((dt$CURRENT & (dt$MFND >= 150 & dt$MFND <= 355)) | 
                      (dt$PRE_2017 & (dt$MFND >= 150 & dt$MFND <= 355))),1,dt$output)

dt$output <- ifelse((dt$CURRENT & (dt$MFND >= 356 & dt$MFND <= 465)) | 
                      (dt$PRE_2017 & ((dt$MFND >= 356 & dt$MFND <= 405) | (dt$AALG >= 150 & dt$AALG <= 415))), 2,dt$output)
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 150 & dt$TAG <= 565 )))),3,dt$output) 
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 566 & dt$TAG <= 850 )))),4,dt$output)
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 150 & dt$TAG <= 565 )))),5,dt$output)
  
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 566 & dt$TAG <= 850 )))),6,dt$output)
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 565 )))),7,dt$output)
  
dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
             (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 566 & dt$TAG <= 850 )))),8,dt$output)
  
},
{
  
  
  #This table represents the current math placement table (as of 2021-03-16) with text.
  #Text refers to the current placements.
 
  
  dt$output <- ifelse(((dt$CURRENT & (dt$MFND >= 150 & dt$MFND <= 355)) | 
                         (dt$PRE_2017 & (dt$MFND >= 150 & dt$MFND <= 355))),"MATH 96",dt$output)
  
  dt$output <- ifelse((dt$CURRENT & (dt$MFND >= 356 & dt$MFND <= 465)) | 
                        (dt$PRE_2017 & ((dt$MFND >= 356 & dt$MFND <= 405) | (dt$AALG >= 150 & dt$AALG <= 415))), "MATH 96 or MATH 141." , dt$output)
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 150 & dt$TAG <= 565 )))),"MATH 112 (followed by MATH 113 for MATH 221) or MATH 130", dt$output) 
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 150 & dt$AALG <= 485) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 416 & dt$AALG <= 495) & (dt$TAG >= 566 & dt$TAG <= 850 )))),"MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221)",dt$output)
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 150 & dt$TAG <= 565 )))),"MATH 114 or MATH 112 (followed by MATH 113 for MATH 221) or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]",dt$output)
  
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 486 & dt$AALG <= 535) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 496 & dt$AALG <= 565) & (dt$TAG >= 566 & dt$TAG <= 850 )))), "MATH 112 (will not need MATH 113 for MATH 221) or MATH 114 or MATH 130 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]",dt$output)
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 555 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 150 & dt$TAG <= 565 )))),"MATH 113 (will not need MATH 112 for MATH 221) or MATH 130 or MATH 211 or MATH 114 or MATH 171/217 sequence (must take both courses and is equivalent to MATH 114 and MATH 221).[QR-A satisfied]",dt$output)
  
  dt$output <- ifelse((dt$CURRENT & ((dt$MFND >= 466 & dt$MFND <= 850) & ((dt$AALG >= 536 & dt$AALG <= 850) & (dt$TAG >= 556 & dt$TAG <= 850 )))) | 
                        (dt$PRE_2017 & ((dt$MFND >= 406 & dt$MFND <= 850) & ((dt$AALG >= 566 & dt$AALG <= 850) & (dt$TAG >= 566 & dt$TAG <= 850 )))),"MATH 130 or MATH 211 or MATH 221 [QR-A satisfied]",dt$output)
  
})


return(dt$output)

}











