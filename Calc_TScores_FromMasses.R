


setwd("X:\\bhinton")




BlackzData <- read.table("BlackFmiLmi.txt", header=T, sep="\t")
HispzData <- read.table("HispFmiLmi.txt", header=T, sep="\t")
WhitezData <- read.table("WhiteFmiLmi.txt", header=T, sep="\t")


blackFemalezData <- BlackzData[BlackzData[, "Gender"] == "Female",] 
blackMalezData <- BlackzData[BlackzData[, "Gender"] == "Male",] 

hispFemalezData <- HispzData[HispzData[, "Gender"] == "Female",] 
hispMalezData <- HispzData[HispzData[, "Gender"] == "Male",] 

whiteFemalezData <- WhitezData[WhitezData[, "Gender"] == "Female",] 
whiteMalezData <- WhitezData[WhitezData[, "Gender"] == "Male",] 


refBlackFemalezData <- subset(blackFemalezData, 
                              blackFemalezData$RIDAGEYR <= 30 & blackFemalezData$RIDAGEYR >= 25)
refBlackMalezData <- subset(blackMalezData, 
                            blackMalezData$RIDAGEYR <= 30 & blackMalezData$RIDAGEYR >= 25)

refHispFemalezData <- subset(hispFemalezData, 
                             hispFemalezData$RIDAGEYR <= 30 & hispFemalezData$RIDAGEYR >= 25)
refHispMalezData <- subset(hispMalezData, 
                           hispMalezData$RIDAGEYR <= 30 & hispMalezData$RIDAGEYR >= 25)

refWhiteFemalezData <- subset(whiteFemalezData, 
                              whiteFemalezData$RIDAGEYR <= 30 & whiteFemalezData$RIDAGEYR >= 25)
refWhiteMalezData <- subset(whiteMalezData, 
                            whiteMalezData$RIDAGEYR <= 30 & whiteMalezData$RIDAGEYR >= 25)





library(fBasics)
#require('fBasics')

blackFemaleRefMean = colMeans(refBlackFemalezData[,c(4:22)])
blackMaleRefMean = colMeans(refBlackMalezData[,c(4:22)])

hispFemaleRefMean = colMeans(refHispFemalezData[,c(4:22)])
hispMaleRefMean = colMeans(refHispMalezData[,c(4:22)])

whiteFemaleRefMean = colMeans(refWhiteFemalezData[,c(4:22)])
whiteMaleRefMean = colMeans(refWhiteMalezData[,c(4:22)])


blackFemaleRefStdev = colStdevs(refBlackFemalezData[,c(4:22)])
blackMaleRefStdev = colStdevs(refBlackMalezData[,c(4:22)])

hispFemaleRefStdev = colStdevs(refHispFemalezData[,c(4:22)])
hispMaleRefStdev = colStdevs(refHispMalezData[,c(4:22)])

whiteFemaleRefStdev = colStdevs(refHispFemalezData[,c(4:22)])
whiteMaleRefStdev = colStdevs(refHispMalezData[,c(4:22)])



#Next need to calculate t scores. 

blackFemaleTScores <- transform(blackFemalezData,
                                T_FMI_TR = (Z_FMI_TR - blackFemaleRefMean['Z_FMI_TR']) / blackFemaleRefStdev['Z_FMI_TR'],
                                T_LMI_TR = (Z_LMI_TR - blackFemaleRefMean['Z_LMI_TR']) / blackFemaleRefStdev['Z_LMI_TR'],
                                T_FMI_LA = (Z_FMI_LA - blackFemaleRefMean['Z_FMI_LA']) / blackFemaleRefStdev['Z_FMI_LA'],
                                T_FMI_RA = (Z_FMI_RA - blackFemaleRefMean['Z_FMI_RA']) / blackFemaleRefStdev['Z_FMI_RA'],
                                T_LMI_LA = (Z_LMI_LA - blackFemaleRefMean['Z_LMI_LA']) / blackFemaleRefStdev['Z_LMI_LA'],
                                T_LMI_RA = (Z_LMI_RA - blackFemaleRefMean['Z_LMI_RA']) / blackFemaleRefStdev['Z_LMI_RA'],
                                T_FMI_LL = (Z_FMI_LL - blackFemaleRefMean['Z_FMI_LL']) / blackFemaleRefStdev['Z_FMI_LL'],
                                T_FMI_RL = (Z_FMI_RL - blackFemaleRefMean['Z_FMI_RL']) / blackFemaleRefStdev['Z_FMI_RL'],
                                T_LMI_LL = (Z_LMI_LL - blackFemaleRefMean['Z_LMI_LL']) / blackFemaleRefStdev['Z_LMI_LL'],
                                T_LMI_RL = (Z_LMI_RL - blackFemaleRefMean['Z_LMI_RL']) / blackFemaleRefStdev['Z_LMI_RL'],
                                T_FMI_AVG = (Z_FMI_AVG - blackFemaleRefMean['Z_FMI_AVG']) / blackFemaleRefStdev['Z_FMI_AVG'],
                                T_LMI_AVG = (Z_LMI_AVG - blackFemaleRefMean['Z_LMI_AVG']) / blackFemaleRefStdev['Z_LMI_AVG']) 


blackMaleTScores <- transform(blackMalezData,
                              T_FMI_TR = (Z_FMI_TR - blackMaleRefMean['Z_FMI_TR']) / blackMaleRefStdev['Z_FMI_TR'],
                              T_LMI_TR = (Z_LMI_TR - blackMaleRefMean['Z_LMI_TR']) / blackMaleRefStdev['Z_LMI_TR'],
                              T_FMI_LA = (Z_FMI_LA - blackMaleRefMean['Z_FMI_LA']) / blackMaleRefStdev['Z_FMI_LA'],
                              T_FMI_RA = (Z_FMI_RA - blackMaleRefMean['Z_FMI_RA']) / blackMaleRefStdev['Z_FMI_RA'],
                              T_LMI_LA = (Z_LMI_LA - blackMaleRefMean['Z_LMI_LA']) / blackMaleRefStdev['Z_LMI_LA'],
                              T_LMI_RA = (Z_LMI_RA - blackMaleRefMean['Z_LMI_RA']) / blackMaleRefStdev['Z_LMI_RA'],
                              T_FMI_LL = (Z_FMI_LL - blackMaleRefMean['Z_FMI_LL']) / blackMaleRefStdev['Z_FMI_LL'],
                              T_FMI_RL = (Z_FMI_RL - blackMaleRefMean['Z_FMI_RL']) / blackMaleRefStdev['Z_FMI_RL'],
                              T_LMI_LL = (Z_LMI_LL - blackMaleRefMean['Z_LMI_LL']) / blackMaleRefStdev['Z_LMI_LL'],
                              T_LMI_RL = (Z_LMI_RL - blackMaleRefMean['Z_LMI_RL']) / blackMaleRefStdev['Z_LMI_RL'],
                              T_FMI_AVG = (Z_FMI_AVG - blackMaleRefMean['Z_FMI_AVG']) / blackMaleRefStdev['Z_FMI_AVG'],
                              T_LMI_AVG = (Z_LMI_AVG - blackMaleRefMean['Z_LMI_AVG']) / blackMaleRefStdev['Z_LMI_AVG']) 


hispFemaleTScores <- transform(hispFemalezData,
                               T_FMI_TR = (Z_FMI_TR - hispFemaleRefMean['Z_FMI_TR']) / hispFemaleRefStdev['Z_FMI_TR'],
                               T_LMI_TR = (Z_LMI_TR - hispFemaleRefMean['Z_LMI_TR']) / hispFemaleRefStdev['Z_LMI_TR'],
                               T_FMI_LA = (Z_FMI_LA - hispFemaleRefMean['Z_FMI_LA']) / hispFemaleRefStdev['Z_FMI_LA'],
                               T_FMI_RA = (Z_FMI_RA - hispFemaleRefMean['Z_FMI_RA']) / hispFemaleRefStdev['Z_FMI_RA'],
                               T_LMI_LA = (Z_LMI_LA - hispFemaleRefMean['Z_LMI_LA']) / hispFemaleRefStdev['Z_LMI_LA'],
                               T_LMI_RA = (Z_LMI_RA - hispFemaleRefMean['Z_LMI_RA']) / hispFemaleRefStdev['Z_LMI_RA'],
                               T_FMI_LL = (Z_FMI_LL - hispFemaleRefMean['Z_FMI_LL']) / hispFemaleRefStdev['Z_FMI_LL'],
                               T_FMI_RL = (Z_FMI_RL - hispFemaleRefMean['Z_FMI_RL']) / hispFemaleRefStdev['Z_FMI_RL'],
                               T_LMI_LL = (Z_LMI_LL - hispFemaleRefMean['Z_LMI_LL']) / hispFemaleRefStdev['Z_LMI_LL'],
                               T_LMI_RL = (Z_LMI_RL - hispFemaleRefMean['Z_LMI_RL']) / hispFemaleRefStdev['Z_LMI_RL'],
                               T_FMI_AVG = (Z_FMI_AVG - hispFemaleRefMean['Z_FMI_AVG']) / hispFemaleRefStdev['Z_FMI_AVG'],
                               T_LMI_AVG = (Z_LMI_AVG - hispFemaleRefMean['Z_LMI_AVG']) / hispFemaleRefStdev['Z_LMI_AVG']) 

hispMaleTScores <- transform(hispMalezData,
                             T_FMI_TR = (Z_FMI_TR - hispMaleRefMean['Z_FMI_TR']) / hispMaleRefStdev['Z_FMI_TR'],
                             T_LMI_TR = (Z_LMI_TR - hispMaleRefMean['Z_LMI_TR']) / hispMaleRefStdev['Z_LMI_TR'],
                             T_FMI_LA = (Z_FMI_LA - hispMaleRefMean['Z_FMI_LA']) / hispMaleRefStdev['Z_FMI_LA'],
                             T_FMI_RA = (Z_FMI_RA - hispMaleRefMean['Z_FMI_RA']) / hispMaleRefStdev['Z_FMI_RA'],
                             T_LMI_LA = (Z_LMI_LA - hispMaleRefMean['Z_LMI_LA']) / hispMaleRefStdev['Z_LMI_LA'],
                             T_LMI_RA = (Z_LMI_RA - hispMaleRefMean['Z_LMI_RA']) / hispMaleRefStdev['Z_LMI_RA'],
                             T_FMI_LL = (Z_FMI_LL - hispMaleRefMean['Z_FMI_LL']) / hispMaleRefStdev['Z_FMI_LL'],
                             T_FMI_RL = (Z_FMI_RL - hispMaleRefMean['Z_FMI_RL']) / hispMaleRefStdev['Z_FMI_RL'],
                             T_LMI_LL = (Z_LMI_LL - hispMaleRefMean['Z_LMI_LL']) / hispMaleRefStdev['Z_LMI_LL'],
                             T_LMI_RL = (Z_LMI_RL - hispMaleRefMean['Z_LMI_RL']) / hispMaleRefStdev['Z_LMI_RL'],
                             T_FMI_AVG = (Z_FMI_AVG - hispMaleRefMean['Z_FMI_AVG']) / hispMaleRefStdev['Z_FMI_AVG'],
                             T_LMI_AVG = (Z_LMI_AVG - hispMaleRefMean['Z_LMI_AVG']) / hispMaleRefStdev['Z_LMI_AVG']) 


whiteFemaleTScores <- transform(whiteFemalezData,
                                T_FMI_TR = (Z_FMI_TR - whiteFemaleRefMean['Z_FMI_TR']) / whiteFemaleRefStdev['Z_FMI_TR'],
                                T_LMI_TR = (Z_LMI_TR - whiteFemaleRefMean['Z_LMI_TR']) / whiteFemaleRefStdev['Z_LMI_TR'],
                                T_FMI_LA = (Z_FMI_LA - whiteFemaleRefMean['Z_FMI_LA']) / whiteFemaleRefStdev['Z_FMI_LA'],
                                T_FMI_RA = (Z_FMI_RA - whiteFemaleRefMean['Z_FMI_RA']) / whiteFemaleRefStdev['Z_FMI_RA'],
                                T_LMI_LA = (Z_LMI_LA - whiteFemaleRefMean['Z_LMI_LA']) / whiteFemaleRefStdev['Z_LMI_LA'],
                                T_LMI_RA = (Z_LMI_RA - whiteFemaleRefMean['Z_LMI_RA']) / whiteFemaleRefStdev['Z_LMI_RA'],
                                T_FMI_LL = (Z_FMI_LL - whiteFemaleRefMean['Z_FMI_LL']) / whiteFemaleRefStdev['Z_FMI_LL'],
                                T_FMI_RL = (Z_FMI_RL - whiteFemaleRefMean['Z_FMI_RL']) / whiteFemaleRefStdev['Z_FMI_RL'],
                                T_LMI_LL = (Z_LMI_LL - whiteFemaleRefMean['Z_LMI_LL']) / whiteFemaleRefStdev['Z_LMI_LL'],
                                T_LMI_RL = (Z_LMI_RL - whiteFemaleRefMean['Z_LMI_RL']) / whiteFemaleRefStdev['Z_LMI_RL'],
                                T_FMI_AVG = (Z_FMI_AVG - whiteFemaleRefMean['Z_FMI_AVG']) / whiteFemaleRefStdev['Z_FMI_AVG'],
                                T_LMI_AVG = (Z_LMI_AVG - whiteFemaleRefMean['Z_LMI_AVG']) / whiteFemaleRefStdev['Z_LMI_AVG']) 



whiteMaleTScores <- transform(whiteMalezData,
                              T_FMI_TR = (Z_FMI_TR - whiteMaleRefMean['Z_FMI_TR']) / whiteMaleRefStdev['Z_FMI_TR'],
                              T_LMI_TR = (Z_LMI_TR - whiteMaleRefMean['Z_LMI_TR']) / whiteMaleRefStdev['Z_LMI_TR'],
                              T_FMI_LA = (Z_FMI_LA - whiteMaleRefMean['Z_FMI_LA']) / whiteMaleRefStdev['Z_FMI_LA'],
                              T_FMI_RA = (Z_FMI_RA - whiteMaleRefMean['Z_FMI_RA']) / whiteMaleRefStdev['Z_FMI_RA'],
                              T_LMI_LA = (Z_LMI_LA - whiteMaleRefMean['Z_LMI_LA']) / whiteMaleRefStdev['Z_LMI_LA'],
                              T_LMI_RA = (Z_LMI_RA - whiteMaleRefMean['Z_LMI_RA']) / whiteMaleRefStdev['Z_LMI_RA'],
                              T_FMI_LL = (Z_FMI_LL - whiteMaleRefMean['Z_FMI_LL']) / whiteMaleRefStdev['Z_FMI_LL'],
                              T_FMI_RL = (Z_FMI_RL - whiteMaleRefMean['Z_FMI_RL']) / whiteMaleRefStdev['Z_FMI_RL'],
                              T_LMI_LL = (Z_LMI_LL - whiteMaleRefMean['Z_LMI_LL']) / whiteMaleRefStdev['Z_LMI_LL'],
                              T_LMI_RL = (Z_LMI_RL - whiteMaleRefMean['Z_LMI_RL']) / whiteMaleRefStdev['Z_LMI_RL'],
                              T_FMI_AVG = (Z_FMI_AVG - whiteMaleRefMean['Z_FMI_AVG']) / whiteMaleRefStdev['Z_FMI_AVG'],
                              T_LMI_AVG = (Z_LMI_AVG - whiteMaleRefMean['Z_LMI_AVG']) / whiteMaleRefStdev['Z_LMI_AVG']) 






library(fmsb)



