
#####
#                                        #
#                                        #
#Part 0: Importing Sas data set and doing calcs
#                                        #
#                                        #

#Explanation:
#This program imports the full SAS data set, calculates Avg arm and leg fat/lean,
#and exports this data into separate files based on race

#Inputs:
setwd("X:\\bhinton") # Set the working directory where "dxa_bmx.sas7bdat" is located

#Imports the .sas7bdat file:
library(sas7bdat) #Loads the package that allows for sas data import
nhanesMortData = read.sas7bdat("dxa_bmx_lab_ques_mort.sas7bdat")


nhanesData = read.sas7bdat("dxa_bmx.sas7bdat")

Mort <- nhanesMortData[,c("SEQN","MORTSTAT")]


nhanesBodyComp <- nhanesMortData[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                                    "DXXTRFAT", "DXXTRLI", "DXDTRPF",
                                    "DXXLAFAT", "DXXLALI", "DXDLAPF",
                                    "DXXLLFAT", "DXXLLLI", "DXDLLPF",
                                    "DXXRLFAT", "DXXRLLI", "DXDRLPF",
                                    "DXXRAFAT", "DXXRALI", "DXDRAPF",
                                    "DXDTOFAT", "DXDTOLI", "DXDTOPF", "MORTSTAT",
                                    "LBXGH", "LBXTC")] 

dNhanesClean <- data.frame(nhanesBodyComp) 

dtest <- dNhanesClean[!is.na(dNhanesClean$BMXHT),]
dtest1 <- dtest[!is.na(dtest$BMXWT),]

dNhanesClean1 <- dtest1

#Calculates avg arm and leg fat and lean, and then FMI/LMI of each segment and avg arm/leg
dNhanesClean2 <- transform(dNhanesClean1, avgArmFat = (DXXLAFAT + DXXRAFAT) / 2, 
                           avgLegFat = (DXXLLFAT + DXXRLFAT) / 2,
                           avgArmLI = (DXXLALI + DXXRALI) / 2,
                           avgLegLI = (DXXLLLI + DXXRLLI) / 2
)
dNhanesClean3 <- transform(dNhanesClean2, avgArmFmi = (avgArmFat/1000) / ((BMXHT/100)^2), 
                           avgLegFmi = (avgLegFat/1000) / ((BMXHT/100)^2),
                           trunkFmi = (DXXTRFAT/1000) / ((BMXHT/100)^2),
                           leftArmFmi = (DXXLAFAT/1000) / ((BMXHT/100)^2),
                           leftLegFmi = (DXXLLFAT/1000) / ((BMXHT/100)^2),
                           rightLegFmi = (DXXRLFAT/1000) / ((BMXHT/100)^2),
                           rightArmFmi = (DXXRAFAT/1000) / ((BMXHT/100)^2),
                           totBodyFmi = (DXDTOFAT/1000) / ((BMXHT/100)^2))
dNhanesCleanFinal <- transform(dNhanesClean3, avgArmLmi = (avgArmLI/1000) / ((BMXHT/100)^2), 
                               avgLegLmi = (avgLegLI/1000) / ((BMXHT/100)^2),
                               trunkLmi = (DXXTRLI/1000) / ((BMXHT/100)^2),
                               leftArmLmi = (DXXLALI/1000) / ((BMXHT/100)^2),
                               leftLegLmi = (DXXLLLI/1000) / ((BMXHT/100)^2),
                               rightLegLmi = (DXXRLLI/1000) / ((BMXHT/100)^2),
                               rightArmLmi = (DXXRALI/1000) / ((BMXHT/100)^2),
                               totBodyLmi = (DXDTOLI/1000) / ((BMXHT/100)^2))







dNhanesFmiLmi <- dNhanesCleanFinal   #[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                                    #  "avgArmFmi", "avgLegFmi", "trunkFmi",
                                    #  "leftArmFmi", "leftLegFmi",
                                    #  "rightLegFmi", "rightArmFmi", "totBodyFmi",
                                    #  "avgArmLmi", "avgLegLmi", "trunkLmi",
                                    #  "leftArmLmi", "leftLegLmi",
                                    #  "rightLegLmi", "rightArmLmi", "totBodyLmi", "MORTSTAT")]


#Keeps all variables including percent fat, etc. (Not used for LMS, but could be useful later)
nhanesBlack <- dNhanesCleanFinal[dNhanesCleanFinal[, "Race"] == "Non-Hispanic Black",] 
nhanesWhite <- dNhanesCleanFinal[dNhanesCleanFinal[, "Race"] == "Non-Hispanic White",] 
nhanesHisp <- subset(dNhanesCleanFinal, dNhanesCleanFinal$Race=="Mexican American"
                     | dNhanesCleanFinal$Race=="Other Hispanic")
#Keeps only variables used for LMS tables (Will be useful for LMS chartmaker)
nhanesBlackFmiLmi <- dNhanesFmiLmi[dNhanesFmiLmi[, "Race"] == "Non-Hispanic Black",] 
nhanesWhiteFmiLmi <- dNhanesFmiLmi[dNhanesFmiLmi[, "Race"] == "Non-Hispanic White",] 
nhanesHispFmiLmi <- subset(dNhanesFmiLmi, dNhanesFmiLmi$Race=="Mexican American"
                           | dNhanesFmiLmi$Race=="Other Hispanic")

#Optional export to new datafile (Will export to current working directory)

#write.csv(nhanesBlackFmiLmi, file = "BlackFmiLmi.csv")
#write.csv(nhanesWhiteFmiLmi, file = "WhiteFmiLmi.csv")
#write.csv(nhanesHispFmiLmi, file = "HispFmiLmi.csv")




#Part 1: Importing Z scores from LMS tables
#####
#                                        #
#                                        #
#Part 1: Importing Individual Z scores
#                                        #
#                                        #


#Explanations: each section takes a different race and imports the male and female regional
#FMI and LMI z scores. Each column is roughly half empty (Filled with *) because the female 
#and male columns have spaces for both genders but only have values for that particular gender
#the program then combines these two gender-separated columns into one and saves it as a new
#Dataset

#Specifies which columns we will keep from the FMI/LMI data in part 0
#keep <- c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
#          "trunkFmi", "trunkLmi", "leftArmFmi", "leftArmLmi",
#          "rightArmFmi", "rightArmLmi", "leftLegFmi",
#          "leftLegLmi", "rightLegFmi", "rightLegLmi", "MORTSTAT")

#Inputs:
nhBlack <- nhanesBlackFmiLmi #Output from part 0
nhHisp <- nhanesHispFmiLmi#Output from part 0
nhWhite <- nhanesWhiteFmiLmi #Output from part 0
setwd("X:\\bhinton\\Data\\LMS Tables\\Zind") # Set the working directory to location
#of z scores for all demographic types

##Black Data set##

#Import of data of individual Z scores #bm = black male
#If you import new data, Make sure the numbers for the model are correct.
#Trunk Z scores
bfTrunkFmiZ <- read.table("BlackFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
bmTrunkFmiZ <- read.table("BlackFmiLmi_Male_TrunkFMI_Zind_020401t.txt", header=T, sep="\t")
bfTrunkLmiZ <- read.table("BlackFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
bmTrunkLmiZ <- read.table("BlackFmiLmi_Male_TrunkLMI_Zind_010601t.txt", header=T, sep="\t")
#Avg Arm Z scores
bfArmFmiZ <- read.table("BlackFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bmArmFmiZ <- read.table("BlackFmiLmi_Male_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bfArmLmiZ <- read.table("BlackFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
bmArmLmiZ <- read.table("BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, sep="\t")
#Avg Leg Z scores
bfLegFmiZ <- read.table("BlackFmiLmi_Female_AvgLegFMI_Zind_020302t.txt", header=T, sep="\t")
bmLegFmiZ <- read.table("BlackFmiLmi_Male_AvgLegFMI_Zind_020202t.txt", header=T, sep="\t")
bfLegLmiZ <- read.table("BlackFmiLmi_Female_AvgLegLMI_Zind_010401t.txt", header=T, sep="\t")
bmLegLmiZ <- read.table("BlackFmiLmi_Male_AvgLegLMI_Zind_010501t.txt", header=T, sep="\t")
#Combines all of the above
CombinedBlackZ <- cbind(bfTrunkFmiZ, bmTrunkFmiZ, bfTrunkLmiZ, bmTrunkLmiZ,
                        bfArmFmiZ, bmArmFmiZ, bfArmLmiZ, bmArmLmiZ, bfLegFmiZ,
                        bmLegFmiZ, bfLegLmiZ, bmLegLmiZ)
#Converts to dataframe
dCombinedBlackZ <- data.frame(CombinedBlackZ)
#Keeping only the columns for Age and the Z scores of each segment
keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedBlackZ2 <- dCombinedBlackZ[keep]
#Renaming the columns to be more Readable ZF=female Z ZM= male z
colnames(dCombinedBlackZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                                "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                                "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi",
                                "ZMLegLmi")
#Replaces all * as NA in data set 
dCombinedBlackZ2 <- as.data.frame(sapply(dCombinedBlackZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedBlackZ2[,unfactorize] <- 
  as.numeric(as.character(unlist(dCombinedBlackZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
dCombinedBlackZ2 <- transform(dCombinedBlackZ2,
                              ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                              ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                              ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                              ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                              ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                              ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
dCombinedBlackZ2$Gender1[is.na(dCombinedBlackZ2$ZMTrunkFmi)] <- "Female"
dCombinedBlackZ2$Gender1[!is.na(dCombinedBlackZ2$ZMTrunkFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
          "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedBlackZ3 <- dCombinedBlackZ2[keep]
#Contains black male and female trunk,avg arm,avg leg FMI and LMI z scores
blackLMSZScoreFmiLmi <- cbind(nhBlack, dCombinedBlackZ3)





##Hispanic Data Set##
#Import of data of individual Z scores #hm = hispanic male
#If you import new data, Make sure the numbers for the model are correct.
#Trunk Z scores
hfTrunkFmiZ <- read.table("HispFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
hmTrunkFmiZ <- read.table("HispFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
hfTrunkLmiZ <- read.table("HispFmiLmi_Female_TrunkLMI_Zind_020401t.txt", header=T, sep="\t")
hmTrunkLmiZ <- read.table("HispFmiLmi_Male_TrunkLMI_Zind_010702t.txt", header=T, sep="\t")
#Avg Arm Z scores
hfArmFmiZ <- read.table("HispFmiLmi_Female_AvgArmFMI_Zind_020302t.txt", header=T, sep="\t")
hmArmFmiZ <- read.table("HispFmiLmi_Male_AvgArmFMI_Zind_010403t.txt", header=T, sep="\t")
hfArmLmiZ <- read.table("HispFmiLmi_Female_AvgArmLMI_Zind_020401t.txt", header=T, sep="\t")
hmArmLmiZ <- read.table("HispFmiLmi_Male_AvgArmLMI_Zind_010702t.txt", header=T, sep="\t")
#Avg Leg Z scores
hfLegFmiZ <- read.table("HispFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
hmLegFmiZ <- read.table("HispFmiLmi_Male_AvglegFMI_Zind_010102t.txt", header=T, sep="\t")
hfLegLmiZ <- read.table("HispFmiLmi_Female_AvgLegLMI_Zind_020401t.txt", header=T, sep="\t")
hmLegLmiZ <- read.table("HispFmiLmi_Male_AvgLegLMI_Zind_010602t.txt", header=T, sep="\t")
#Combines all of the above
CombinedHispZ <- cbind(hfTrunkFmiZ, hmTrunkFmiZ, hfTrunkLmiZ, hmTrunkLmiZ,
                       hfArmFmiZ, hmArmFmiZ, hfArmLmiZ, hmArmLmiZ, 
                       hfLegFmiZ, hmLegFmiZ, hfLegLmiZ, hmLegLmiZ)
#Converts to dataframe
dCombinedHispZ <- data.frame(CombinedHispZ)

#Keeping only the columns for Age and the Z scores of each segment
keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedHispZ2 <- dCombinedHispZ[keep]
colnames(dCombinedHispZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                               "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                               "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
#Replaces all * as NA in data set 
dCombinedHispZ2 <- as.data.frame(sapply(dCombinedHispZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedHispZ2[,unfactorize] <-
  as.numeric(as.character(unlist(dCombinedHispZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
dCombinedHispZ2 <- transform(dCombinedHispZ2, 
                             ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                             ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                             ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                             ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                             ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                             ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
dCombinedHispZ2$Gender1[is.na(dCombinedHispZ2$ZMTrunkFmi)] <- "Female"
dCombinedHispZ2$Gender1[!is.na(dCombinedHispZ2$ZMTrunkFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
          "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedHispZ3 <- dCombinedHispZ2[keep]
#Contains hispanic male and female trunk,avg arm,avg leg FMI and LMI z scores
hispLMSZScoreFmiLmi <- cbind(nhHisp, dCombinedHispZ3)


##White Data Set##
#Import of data of individual Z scores #wm = white male
#If you import new data, Make sure the numbers for the model are correct.
#Trunk Z scores
wfTrunkFmiZ <- read.table("WhiteFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
wmTrunkFmiZ <- read.table("WhiteFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
wfTrunkLmiZ <- read.table("WhiteFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
wmTrunkLmiZ <- read.table("WhiteFmiLmi_Male_TrunkLMI_Zind_020702t.txt", header=T, sep="\t")
#Avg Arm Z scores
wfArmFmiZ <- read.table("WhiteFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
wmArmFmiZ <- read.table("WhiteFmiLmi_Male_AvgArmFMI_Zind_020402t.txt", header=T, sep="\t")
wfArmLmiZ <- read.table("WhiteFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
wmArmLmiZ <- read.table("WhiteFmiLmi_Male_AvgArmLMI_Zind_010801t.txt", header=T, sep="\t")
#Avg Leg Z scores
wfLegFmiZ <- read.table("WhiteFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
wmLegFmiZ <- read.table("WhiteFmiLmi_Male_AvgLegFMI_Zind_010202t.txt", header=T, sep="\t")
wfLegLmiZ <- read.table("WhiteFmiLmi_Female_AvgLegLMI_Zind_010601t.txt", header=T, sep="\t")
wmLegLmiZ <- read.table("WhiteFmiLmi_Male_AvgLegLMI_Zind_020702t.txt", header=T, sep="\t")
#Combines all of the above
CombinedWhiteZ <- cbind(wfTrunkFmiZ, wmTrunkFmiZ, wfTrunkLmiZ, wmTrunkLmiZ, wfArmFmiZ,
                        wmArmFmiZ, wfArmLmiZ, wmArmLmiZ, wfLegFmiZ, wmLegFmiZ,
                        wfLegLmiZ, wmLegLmiZ)
#Converts to dataframe
dCombinedWhiteZ <- data.frame(CombinedWhiteZ)

#Keeping only the columns for Age and the Z scores of each segment
keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedWhiteZ2 <- dCombinedWhiteZ[keep]
colnames(dCombinedWhiteZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                                "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                                "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
#Replaces all * as NA in data set 
dCombinedWhiteZ2 <- as.data.frame(sapply(dCombinedWhiteZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedWhiteZ2[,unfactorize] <- 
  as.numeric(as.character(unlist(dCombinedWhiteZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
dCombinedWhiteZ2 <- transform(dCombinedWhiteZ2, 
                              ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                              ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                              ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                              ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                              ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                              ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
dCombinedWhiteZ2$Gender1[is.na(dCombinedWhiteZ2$ZMTrunkFmi)] <- "Female"
dCombinedWhiteZ2$Gender1[!is.na(dCombinedWhiteZ2$ZMTrunkFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
          "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedWhiteZ3 <- dCombinedWhiteZ2[keep]
#Contains hispanic male and female trunk,avg arm,avg leg FMI and LMI z scores
whiteLMSZScoreFmiLmi <- cbind(nhWhite, dCombinedWhiteZ3)





##Part 2 get totals
setwd("X:\\bhinton\\Data\\LMS Tables\\Zind")
wfTotFmiZ <- read.table("WhiteFmiLmi_Female_TotFMI_Zind_020402t.txt", header=T, sep="\t")
wmTotFmiZ <- read.table("WhiteFmiLmi_Male_TotFMI_Zind_020402t.txt", header=T, sep="\t")
wfTotLmiZ <- read.table("WhiteFmiLmi_Female_TotLMI_Zind_010401t.txt", header=T, sep="\t")
wmTotLmiZ <- read.table("WhiteFmiLmi_Male_TotLMI_Zind_020702t.txt", header=T, sep="\t")

bfTotFmiZ <- read.table("BlackFmiLmi_Female_TotFMI_Zind_020302t.txt", header=T, sep="\t")
bmTotFmiZ <- read.table("BlackFmiLmi_Male_TotFMI_Zind_020401t.txt", header=T, sep="\t")
bfTotLmiZ <- read.table("BlackFmiLmi_Female_TotLMI_Zind_010401t.txt", header=T, sep="\t")
bmTotLmiZ <- read.table("BlackFmiLmi_Male_TotLMI_Zind_020601t.txt", header=T, sep="\t")

hfTotFmiZ <- read.table("HispFmiLmi_Female_TotFMI_Zind_020402t.txt", header=T, sep="\t")
hmTotFmiZ <- read.table("HispFmiLmi_Male_TotFMI_Zind_020402t.txt", header=T, sep="\t")
hfTotLmiZ <- read.table("HispFmiLmi_Female_TotLMI_Zind_020601t.txt", header=T, sep="\t")
hmTotLmiZ <- read.table("HispFmiLmi_Male_TotLMI_Zind_010702t.txt", header=T, sep="\t")

aCombinedWhiteZ <- cbind(wfTotFmiZ,wmTotFmiZ,wfTotLmiZ,wmTotLmiZ)

adCombinedWhiteZ <- data.frame(aCombinedWhiteZ)

keep <- c(1, 3, 6, 9, 12)
adCombinedWhiteZ2 <- adCombinedWhiteZ[keep]
colnames(adCombinedWhiteZ2) <- c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
                                "ZMTotLmi")
#Replaces all * as NA in data set 
adCombinedWhiteZ2 <- as.data.frame(sapply(adCombinedWhiteZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
               "ZMTotLmi")
adCombinedWhiteZ2[,unfactorize] <- 
  as.numeric(as.character(unlist(adCombinedWhiteZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
adCombinedWhiteZ2 <- transform(adCombinedWhiteZ2, 
                              ZFTotFmi= pmax(ZFTotFmi, ZMTotFmi, na.rm=TRUE), 
                              ZFTotLmi= pmax(ZFTotLmi, ZMTotLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
adCombinedWhiteZ2$Gender1[is.na(adCombinedWhiteZ2$ZMTotFmi)] <- "Female"
adCombinedWhiteZ2$Gender1[!is.na(adCombinedWhiteZ2$ZMTotFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTotFmi", "ZFTotLmi","Gender1")
adCombinedWhiteZ3 <- adCombinedWhiteZ2[keep]
#Contains hispanic male and female trunk,avg arm,avg leg FMI and LMI z scores
WhiteFinished <- cbind(whiteLMSZScoreFmiLmi, adCombinedWhiteZ3)


###


aCombinedHispZ <- cbind(hfTotFmiZ,hmTotFmiZ,hfTotLmiZ,hmTotLmiZ)

adCombinedHispZ <- data.frame(aCombinedHispZ)

keep <- c(1, 3, 6, 9, 12)
adCombinedHispZ2 <- adCombinedHispZ[keep]
colnames(adCombinedHispZ2) <- c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
                                 "ZMTotLmi")
#Replaces all * as NA in data set 
adCombinedHispZ2 <- as.data.frame(sapply(adCombinedHispZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
               "ZMTotLmi")
adCombinedHispZ2[,unfactorize] <- 
  as.numeric(as.character(unlist(adCombinedHispZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
adCombinedHispZ2 <- transform(adCombinedHispZ2, 
                               ZFTotFmi= pmax(ZFTotFmi, ZMTotFmi, na.rm=TRUE), 
                               ZFTotLmi= pmax(ZFTotLmi, ZMTotLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
adCombinedHispZ2$Gender1[is.na(adCombinedHispZ2$ZMTotFmi)] <- "Female"
adCombinedHispZ2$Gender1[!is.na(adCombinedHispZ2$ZMTotFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTotFmi", "ZFTotLmi","Gender1")
adCombinedHispZ3 <- adCombinedHispZ2[keep]
#Contains hispanic male and female trunk,avg arm,avg leg FMI and LMI z scores
HispFinished <- cbind(hispLMSZScoreFmiLmi, adCombinedHispZ3)




###


aCombinedBlackZ <- cbind(bfTotFmiZ,bmTotFmiZ,bfTotLmiZ,bmTotLmiZ)

adCombinedBlackZ <- data.frame(aCombinedBlackZ)

keep <- c(1, 3, 6, 9, 12)
adCombinedBlackZ2 <- adCombinedBlackZ[keep]
colnames(adCombinedBlackZ2) <- c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
                                "ZMTotLmi")
#Replaces all * as NA in data set 
adCombinedBlackZ2 <- as.data.frame(sapply(adCombinedBlackZ2,sub,pattern='\\*',replacement=NA))
#More data treating (need to change type of column as not a factor)
unfactorize<-c("Age", "ZFTotFmi", "ZMTotFmi", "ZFTotLmi",
               "ZMTotLmi")
adCombinedBlackZ2[,unfactorize] <- 
  as.numeric(as.character(unlist(adCombinedBlackZ2[,unfactorize])))
#Combines the columns of male region and female region fmi/lmi
adCombinedBlackZ2 <- transform(adCombinedBlackZ2, 
                              ZFTotFmi= pmax(ZFTotFmi, ZMTotFmi, na.rm=TRUE), 
                              ZFTotLmi= pmax(ZFTotLmi, ZMTotLmi, na.rm=TRUE))
#Labels male rows as males and female rows as female
adCombinedBlackZ2$Gender1[is.na(adCombinedBlackZ2$ZMTotFmi)] <- "Female"
adCombinedBlackZ2$Gender1[!is.na(adCombinedBlackZ2$ZMTotFmi)] <- "Male"
#keeps only the columns that combined the separate male/female columns into one
keep <- c("Age","ZFTotFmi", "ZFTotLmi","Gender1")
adCombinedBlackZ3 <- adCombinedBlackZ2[keep]
#Contains Blackanic male and female trunk,avg arm,avg leg FMI and LMI z scores
BlackFinished <- cbind(blackLMSZScoreFmiLmi, adCombinedBlackZ3)









#####
#                                        #
#                                        #
#Part 2: Importing LMS Z scores (And maybe calculating values?)
#                                        #
#                                        #

#Explanation:This section imports the L,M,S values from the LMS chartmaker modeling
#and calculates what the left leg/right leg and left arm/right arm individual z scores
#would be based on the Average leg and average arm L,M,S values. It then stores these
#values in new columns and gives an opportunity to export this new dataset in a new .txt 
#table separated by race
#  Formula to convert from value (y) to z score (z)
# z = ( y / m)^L - 1 / (L*S)

#Inputs:
setwd("X:\\bhinton\\Data\\LMS Tables\\LMS Values") #Set this to wd with LMS charts
#Specifies which columns to keep from LMS tables
keep <- c("Age","L", "M", "S")

bfArmFmiLms <- 
  read.table("BlackFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bfArmLmiLms <- 
  read.table("BlackFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
bfLegFmiLms <- 
  read.table("BlackFmiLmi_Female_AvgLegFMI_020302t.txt", header=T, skip=10, sep="\t")
bfLegLmiLms <- 
  read.table("BlackFmiLmi_Female_AvgLegLMI_010401t.txt", header=T, skip=10, sep="\t")
#Keeps only the relevant columns for the black females
bfLms <- cbind(bfArmFmiLms[keep], bfArmLmiLms[keep],
               bfLegFmiLms[keep], bfLegLmiLms[keep])

bmArmFmiLms <- 
  read.table("BlackFmiLmi_Male_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bmArmLmiLms <- 
  read.table("BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, skip=10, sep="\t")
bmLegFmiLms <- 
  read.table("BlackFmiLmi_Male_AvgLegFMI_020202t.txt", header=T, skip=10, sep="\t")
bmLegLmiLms <- 
  read.table("BlackFmiLmi_Male_AvgLegLMI_010501t.txt", header=T, skip=10, sep="\t")
#Keeps only the relevant columns for the black males
bmLms <- cbind(bmArmFmiLms[keep], bmArmLmiLms[keep], 
               bmLegFmiLms[keep], bmLegLmiLms[keep])

hfArmFmiLms <- 
  read.table("HispFmiLmi_Female_AvgArmFMI_020302t.txt", header=T, skip=10, sep="\t")
hfArmLmiLms <- 
  read.table("HispFmiLmi_Female_AvgArmLMI_020401t.txt", header=T, skip=10, sep="\t")
hfLegFmiLms <- 
  read.table("HispFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
hfLegLmiLms <- 
  read.table("HispFmiLmi_Female_AveLegLMI_020401t.txt", header=T, skip=10, sep="\t")
#Hispanic Females
hfLms <- cbind(hfArmFmiLms[keep], hfArmLmiLms[keep], 
               hfLegFmiLms[keep], hfLegLmiLms[keep])

hmArmFmiLms <- 
  read.table("HispFmiLmi_Male_AvgArmFMI_010403t.txt", header=T, skip=10, sep="\t")
hmArmLmiLms <- 
  read.table("HispFmiLmi_Male_AvgArmLMI_010702t.txt", header=T, skip=10, sep="\t")
hmLegFmiLms <- 
  read.table("HispFmiLmi_Male__AvgLegFMI_010102t.txt", header=T, skip=10, sep="\t")
hmLegLmiLms <- 
  read.table("HispFmiLmi_Male_AvgLegLMI_010602t.txt", header=T, skip=10, sep="\t")
#Hispanic Males
hmLms <- cbind(hmArmFmiLms[keep], hmArmLmiLms[keep], 
               hmLegFmiLms[keep], hmLegLmiLms[keep])

wfArmFmiLms <- 
  read.table("WhiteFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
wfArmLmiLms <- 
  read.table("WhiteFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
wfLegFmiLms <- 
  read.table("WhiteFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
wfLegLmiLms <- 
  read.table("WhiteFmiLmi_Female_AvgLegLMI_010601t.txt", header=T, skip=10, sep="\t")
#White Females
wfLms <- cbind(wfArmFmiLms[keep], wfArmLmiLms[keep], 
               wfLegFmiLms[keep], wfLegLmiLms[keep])

wmArmFmiLms <- 
  read.table("WhiteFmiLmi_Male_AvgArmFMI_020402t.txt", header=T, skip=10, sep="\t")
wmArmLmiLms <- 
  read.table("WhiteFmiLmi_Male_AvgArmLMI_010801t.txt", header=T, skip=10, sep="\t")
wmLegFmiLms <- 
  read.table("WhiteFmiLmi_Male_AvgLegFMI_010202t.txt", header=T, skip=10, sep="\t")
wmLegLmiLms <- 
  read.table("WhiteFmiLmi_Male_AvgLagLMI_020702t.txt", header=T, skip=10, sep="\t")
#White Males
wmLms <- cbind(wmArmFmiLms[keep], wmArmLmiLms[keep], 
               wmLegFmiLms[keep], wmLegLmiLms[keep])


#This for loop calculates the right/left arm/leg z scores based on gender/ethnicity
#Nested if statements select the appropriate lms tables based on which gender/race the 
#Loop is currently calculating for
for (i in 1:3){#Ethnicity, normally 1:3
  zScoreFinal = NULL
  for (j in 1:2){#Gender, normally 1:2
    if (i == 1) {zScore = BlackFinished #Black arm and leg FMI/LMI data
    race = "Black"
    if (j == 1) {lmsChart = bfLms
    gender = "Female" #Female
    }
    else if (j == 2) {lmsChart = bmLms
    gender = "Male"#Male
    }
    } else if (i == 2) {zScore = HispFinished #Hisp arm and leg FMI/LMI data
    race = "Hisp"
    if (j == 1) {lmsChart = hfLms
    gender = "Female"#Female
    }
    else if (j == 2) {lmsChart = hmLms
    gender = "Male"#Male
    }
    } else if (i == 3) {zScore = WhiteFinished #White arm and leg FMI/LMI data
    race = "White"
    if (j == 1) {lmsChart = wfLms
    gender = "Female"#Female
    }
    else if (j == 2) {lmsChart = wmLms
    gender = "Male"#Male
    }}
    for (k in  1:78){ #k is 1-78 because age goes from 8-85
      ageRow = k
      #If k = 1, the age being calculated is 8 year olds.
      age = ageRow + 7
      #selects only the section of the table that is appropriate
      #for the current age and gender
      zScore1 <- zScore[zScore[, "Age"] == age,]   
      zScore2 <- zScore1[zScore1[, "Gender"] == gender,] 
      lmsAge <- lmsChart[ageRow ,]                           
      #Converts all to data matrix (better for calculations)
      lArmFmi = data.matrix(lmsAge[2]) 
      mArmFmi = data.matrix(lmsAge[3]) 
      sArmFmi = data.matrix(lmsAge[4])
      lArmLmi = data.matrix(lmsAge[6])
      mArmLmi = data.matrix(lmsAge[7])
      sArmLmi = data.matrix(lmsAge[8])
      lLegFmi = data.matrix(lmsAge[10])
      mLegFmi = data.matrix(lmsAge[11])
      sLegFmi = data.matrix(lmsAge[12])
      lLegLmi = data.matrix(lmsAge[14])
      mLegLmi = data.matrix(lmsAge[15])
      sLegLmi = data.matrix(lmsAge[16])
      #Calculates Z scores for right/left arm/leg lmi/fmi
      zScore3 <- transform(zScore2, 
                           zLArmFmi= (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                           zRArmFmi= (((rightArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                           zLArmLmi= (((leftArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                           zRArmLmi= (((rightArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                           zLLegFmi= (((leftLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                           zRLegFmi= (((rightLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                           zLLegLmi= (((leftLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi),
                           zRLegLmi= (((rightLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi))
      #Calculates avg z score (useful in finding populations based on avg Z = +2, 0, -2, etc)
      zScore4 <- transform(zScore3,
                           zAvgFmi= (ZFTrunkFmi+zLArmFmi+zRArmFmi+zLLegFmi+zRLegFmi) / 5, 
                           zAvgLmi= (ZFTrunkLmi+zLArmLmi+zRArmLmi+zLLegLmi+zRLegLmi) / 5
      )
      
      #Keeps only th                   
      #keep <- c("Race","Gender", "BMXHT","BMXWT","RIDAGEYR",
      #          "ZFTrunkFmi","ZFTrunkLmi", "zLArmFmi","zRArmFmi",
       #         "zLArmLmi","zRArmLmi","zLLegFmi","zRLegFmi",
        #        "zLLegLmi", "zRLegLmi", "zAvgFmi", "zAvgLmi")
      zScore5 <- zScore4#[keep]
      #Changes column names to create the radar charts later on
      colnames(zScore5) <- c("Race", "Gender", "Height", "Weight", "Age",
                             "TrunkFat", "TrunkLean", "TrunkPfat",
                             "LarmFat", "LarmLean", "LarmPfat",
                             "LlegFat", "LlegLean", "LlegPfat",
                             "RlegFat", "RlegLean", "RlegPfat",
                             "RarmFat", "RarmLean", "RarmPfat",
                             "TotFat", "TotLean", "TotPfat",
                             "Mort", "GH_Ha1c", "TC_Cholest", #"20ftWalk",
                             "AvgArmFat", "AvgLegFat", "AvgArmLean", "AvgLegLean",
                             "AvgArmFmi", "AvgLegFmi", "TrunkFmi",
                             "LArmFmi","LLegFmi","RLegFmi","RArmFmi", "TotBodyFmi",
                             "AvgArmLmi", "AvgLegLmi", "TrunkLmi",
                             "LArmLmi","LLegLmi","RLegLmi","RArmLmi", "TotBodyLmi",
                             "Age1","Z_FMI_TR", "Z_LMI_TR",
                             "ZarmFMI", "ZArmLMI", "ZLegFMI", "ZLegLMI",
                             "Gender1", "Age2", "Z_TOT_FMI", "Z_TOT_LMI", "Gender3",
                              "Z_FMI_LA", "Z_FMI_RA",
                             "Z_LMI_LA","Z_LMI_RA", "Z_FMI_LL", "Z_FMI_RL",
                             "Z_LMI_LL", "Z_LMI_RL","Z_FMI_AVG", "Z_LMI_AVG")
      
      #stores this data in a dataset for a specific age, and will be added to for
      #each additional age
      #(will have 8 yr olds, then add in the 9 yr olds.... then 85 yr olds)
      zScoreFinal <- rbind(zScoreFinal,zScore5)
      #Once it cycles through all ages for one gender, 8-85 of the next gender is calculated
      #and added so this dataset has 8-85 of both genders for one race
      
    }#End of cycle through ages 8-85
  }#End of cycle through genders
  
  
  #calculates the standard deviation for the FMI and LMI for each individual
  fmiZSd <- data.frame(sd(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
  #fmiZSd <- data.frame(sd(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
  colnames(fmiZSd) <- "Z_FMI_SD"
  lmiZSd <- data.frame(sd(t(zScoreFinal[,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")])))
  #lmiZSd <- data.frame(sd(t(zScoreFinal[,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")])))
  colnames(lmiZSd) <- "Z_LMI_SD"
  #Adds this Sd value in with the right/left arm/leg lmi/fmi calculations
  zScoreFinal <- cbind(zScoreFinal, fmiZSd, lmiZSd)
  
  #Optionally:
  #the two commented out lines below this would write each of these tables to a .txt
  #by race
  setwd("X:\\bhinton")
  write.table(zScoreFinal, file=sprintf("%s.MortZScoreValues.txt",race))
  #Activate this to write to a new table (Will need to do this for averages)
  
}#End of cycle through races



##Part 2

setwd("X:\\bhinton") 

ZScoreFmiLmi <- rbind(WhiteFinished, BlackFinished, HispFinished)
Test <- data.frame(ZScoreFmiLmi) #Convert to data frame
ZScoresFinal <- na.omit(Test)

write.csv(ZScoresFinal, file = "ZMortFinal.csv")
