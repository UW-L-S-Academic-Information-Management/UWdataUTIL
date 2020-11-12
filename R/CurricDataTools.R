acad.year <- function(Term, TermType){
  Data <- data.frame(Term, stringsAsFactors = FALSE)

  if(TermType == "SIS"){
    Data$cent <- substr(Data$Term, 1, 1)
    Data$year <- substr(Data$Term, 2, 3)
    Data$semester <- substr(Data$Term, 4, 4)

    if (length(Data[which(Data$cent == "0"),]$cent) > 0)
      {Data[which(Data$cent == "0"),]$cent <- "19"}
    if (length(Data[which(Data$cent == "1"),]$cent) > 0)
      {Data[which(Data$cent == "1"),]$cent <- "20"}
    Data$acadYear <- paste(Data$cent, Data$year, sep = "")
    if (length(Data[which(Data$semester == "6"),]$cent) > 0)
      {Data[which(Data$semester == "6"),]$acadYear <- as.integer(Data[which(Data$semester == "6"),]$acadYear) + 1}

  }
  if(TermType == "DARS"){
    Data$acadYear <- substr(Data$Term, 1, 4)
    Data$semester <- substr(Data$Term, 5, 5)
    if (length(Data[which(Data$semester == "3"),]$cent) > 0)
      {Data[which(Data$semester == "3"),]$acadYear <- as.integer(Data[which(Data$semester == "3"),]$acadYear) + 1}

  }

  return(Data$acadYear)


}

dars.to.sis <- function(Term){

Data <- data.frame(Term, stringsAsFactors = FALSE)
Data$cent <- substr(Data$Term, 1, 2)
Data$year <- substr(Data$Term, 3, 4)
Data$semester <- substr(Data$Term, 5,5)
if (length(Data[which(Data$cent == "19"),]$cent) > 0)
{Data[which(Data$cent == "19"),]$cent <- "0"}
if (length(Data[which(Data$cent == "20"),]$cent) > 0)
{Data[which(Data$cent == "20"),]$cent <- "1"}
Data$semester <- as.character(as.integer(Data$semester)*2)
Data$SISterm <- paste(Data$cent, Data$year, Data$semester, sep = "")

return(Data$SISterm)
}

sis.to.dars <- function(Term){

  Data <- data.frame(Term, stringsAsFactors = FALSE)
  Data$cent <- substr(Data$Term, 1, 1)
  Data$year <- substr(Data$Term, 2, 3)
  Data$semester <- substr(Data$Term, 4,4)
  if (length(Data[which(Data$cent == "0"),]$cent) > 0)
  {Data[which(Data$cent == "0"),]$cent <- "19"}
  if (length(Data[which(Data$cent == "1"),]$cent) > 0)
  {Data[which(Data$cent == "1"),]$cent <- "20"}
  Data$semester <- as.character(as.integer(Data$semester) / 2)
  Data$DARSterm <- paste(Data$cent, Data$year, Data$semester, sep = "")


  return(Data$DARSterm)


}
