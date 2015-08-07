

#Readme
#####
#Required Packages: fmsb (Contains radar function)
#                   sas7bdat (Allows to import sas files)
#must load and install the packages in order for the code to run


#NEED TO DO:
#Clean up variables and table names and legends
  #change colors to be better for FMI/LMI
  #Comment Code more clearly
  #change way plotting works so it is function for 1-4 plots



#Instructions/README:

#Parts A, B, and C (4th, 5th and 6th section) are Parts
#that actually produce radar plots

#Parts 0-2 are not necessary to produce radar plots.
#These parts only existed to import LMS data and treat data
#so that I had all the data to produce radar plots

#Part A: produces separate Radar Charts of FMI and of LMI of 4 individuals
#Part B: Produces 4 radar charts of individuals with FMI/LMI overlayed together
#Part C: Produces 4 radar charts of individuals with FMI/LMI overlayed together
        # But you can constrain data to only subjects with an average Z score of 
        #your choosing plus/minus 0.1

#Each part has several inputs at the beginning that allow you to specify demographics

#Each can be run independently of one another as long as you have three files:
  # Black.ZScoreValues.txt
  # Hisp.ZScoreValues.txt
  # White.ZScoreValues.txt

#These are the essential files produced in parts 0-2

#END OF README
#Part 0: Import Sas Data Set
#####
#                                        #
#                                        #
#Part 0: Importing Sas data set and doing calcs
#                                        #
#                                        #

#Inputs:
setwd("X:\\bhinton") # Set the working directory where "dxa_bmx.sas7bdat" is located

#Imports the .sas7bdat file
library(sas7bdat)

nhanesFullData = read.sas7bdat("dxa_bmx.sas7bdat")
nhanesBodyComp <- nhanesFullData[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                               "DXXTRFAT", "DXXTRLI", "DXDTRPF",
                               "DXXLAFAT", "DXXLALI", "DXDLAPF",
                               "DXXLLFAT", "DXXLLLI", "DXDLLPF",
                               "DXXRLFAT", "DXXRLLI", "DXDRLPF",
                               "DXXRAFAT", "DXXRALI", "DXDRAPF",
                               "DXDTOFAT", "DXDTOLI", "DXDTOPF"
                              )]
dNhanesClean <- data.frame(nhanesBodyComp)
dNhanesClean1 <- na.omit(dNhanesClean)
dNhanesClean2 <- transform(dNhanesClean1, avgArmFat = (DXXLAFAT + DXXRAFAT) / 2, 
                           avgLegFat = (DXXLLFAT + DXXRLFAT) / 2,
                           avgArmLI = (DXXLALI + DXXRALI) / 2,
                           avgLegLI = (DXXLLLI + DXXRLLI) / 2
                           )
#fff
dNhanesClean3 <- transform(dNhanesClean2, avgArmFmi = (avgArmFat/1000) / ((BMXHT/100)^2), 
                           avgLegFmi = (avgLegFat/1000) / ((BMXHT/100)^2),
                           trunkFmi = (DXXTRFAT/1000) / ((BMXHT/100)^2),
                           leftArmFmi = (DXXLAFAT/1000) / ((BMXHT/100)^2),
                           leftLegFmi = (DXXLLFAT/1000) / ((BMXHT/100)^2),
                           rightLegFmi = (DXXRLFAT/1000) / ((BMXHT/100)^2),
                           rightArmFmi = (DXXRAFAT/1000) / ((BMXHT/100)^2))

dNhanesCleanFinal <- transform(dNhanesClean3, avgArmLmi = (avgArmLI/1000) / ((BMXHT/100)^2), 
                           avgLegLmi = (avgLegLI/1000) / ((BMXHT/100)^2),
                           trunkLmi = (DXXTRLI/1000) / ((BMXHT/100)^2),
                           leftArmLmi = (DXXLALI/1000) / ((BMXHT/100)^2),
                           leftLegLmi = (DXXLLLI/1000) / ((BMXHT/100)^2),
                           rightLegLmi = (DXXRLLI/1000) / ((BMXHT/100)^2),
                           rightArmLmi = (DXXRALI/1000) / ((BMXHT/100)^2))

dNhanesFmiLmi <- dNhanesCleanFinal[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                                      "avgArmFmi", "avgLegFmi", "trunkFmi",
                                      "leftArmFmi", "leftLegFmi",
                                      "rightLegFmi", "rightArmFmi",
                                      "avgArmLmi", "avgLegLmi", "trunkLmi",
                                      "leftArmLmi", "leftLegLmi",
                                      "rightLegLmi", "rightArmLmi")]

nhanesBlack <- dNhanesCleanFinal[dNhanesCleanFinal[, "Race"] == "Non-Hispanic Black",] 
nhanesWhite <- dNhanesCleanFinal[dNhanesCleanFinal[, "Race"] == "Non-Hispanic White",] 
nhanesHisp <- subset(dNhanesCleanFinal, dNhanesCleanFinal$Race=="Mexican American"
                     | dNhanesCleanFinal$Race=="Other Hispanic")

nhanesBlackFmiLmi <- dNhanesFmiLmi[dNhanesFmiLmi[, "Race"] == "Non-Hispanic Black",] 
nhanesWhiteFmiLmi <- dNhanesFmiLmi[dNhanesFmiLmi[, "Race"] == "Non-Hispanic White",] 
nhanesHispFmiLmi <- subset(dNhanesFmiLmi, dNhanesFmiLmi$Race=="Mexican American"
                           | dNhanesFmiLmi$Race=="Other Hispanic")
#This contains just the information needed to produce LMS charts  

#write.csv(nhanesBlackFmiLmi, file = "BlackFmiLmi.csv")
#write.csv(nhanesWhiteFmiLmi, file = "WhiteFmiLmi.csv")
#write.csv(nhanesHispFmiLmi, file = "HispFmiLmi.csv")
#write.csv(test, file = "dxa_bmx.csv")
#If I want to write the whole database
#Part 1: Importing Z scores from LMS tables
#####
#                                        #
#                                        #
#Part 1: Importing Individual Z scores
#                                        #
#                                        #

keep <- c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
           "trunkFmi", "trunkLmi", "leftArmFmi", "leftArmLmi",
           "rightArmFmi", "rightArmLmi", "leftLegFmi",
           "leftLegLmi", "rightLegFmi", "rightLegLmi")

#Inputs: 
nhBlack <- nhanesBlackFmiLmi[keep] #Output from part 0
nhHisp <- nhanesHispFmiLmi[keep] #Output from part 0
nhWhite <- nhanesWhiteFmiLmi[keep] #Output from part 0
setwd("X:\\bhinton\\Data\\LMS Tables\\Zind") # Set the working directory to location
#of z scores for all demographic types

##Black Data set##

#Import of data (Make sure the numbers for the model are correct.)
bfTrunkFmiZ <- read.table("BlackFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
bmTrunkFmiZ <- read.table("BlackFmiLmi_Male_TrunkFMI_Zind_020401t.txt", header=T, sep="\t")
bfTrunkLmiZ <- read.table("BlackFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
bmTrunkLmiZ <- read.table("BlackFmiLmi_Male_TrunkLMI_Zind_010601t.txt", header=T, sep="\t")

bfArmFmiZ <- read.table("BlackFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bmArmFmiZ <- read.table("BlackFmiLmi_Male_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bfArmLmiZ <- read.table("BlackFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
bmArmLmiZ <- read.table("BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, sep="\t")

bfLegFmiZ <- read.table("BlackFmiLmi_Female_AvgLegFMI_Zind_020302t.txt", header=T, sep="\t")
bmLegFmiZ <- read.table("BlackFmiLmi_Male_AvgLegFMI_Zind_020202t.txt", header=T, sep="\t")
bfLegLmiZ <- read.table("BlackFmiLmi_Female_AvgLegLMI_Zind_010401t.txt", header=T, sep="\t")
bmLegLmiZ <- read.table("BlackFmiLmi_Male_AvgLegLMI_Zind_010501t.txt", header=T, sep="\t")

CombinedBlackZ <- cbind(bfTrunkFmiZ, bmTrunkFmiZ, bfTrunkLmiZ, bmTrunkLmiZ,
                        bfArmFmiZ, bmArmFmiZ, bfArmLmiZ, bmArmLmiZ, bfLegFmiZ,
                        bmLegFmiZ, bfLegLmiZ, bmLegLmiZ)

dCombinedBlackZ <- data.frame(CombinedBlackZ)

keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedBlackZ2 <- dCombinedBlackZ[keep]

colnames(dCombinedBlackZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                                "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                                "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi",
                                "ZMLegLmi")

dCombinedBlackZ2 <- as.data.frame(sapply(dCombinedBlackZ2,sub,pattern='\\*',replacement=NA))

unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedBlackZ2[,unfactorize] <- 
        as.numeric(as.character(unlist(dCombinedBlackZ2[,unfactorize])))
dCombinedBlackZ2 <- transform(dCombinedBlackZ2,
                          ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombinedBlackZ2$Gender1[is.na(dCombinedBlackZ2$ZMTrunkFmi)] <- "Female"
dCombinedBlackZ2$Gender1[!is.na(dCombinedBlackZ2$ZMTrunkFmi)] <- "Male"

keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
           "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedBlackZ3 <- dCombinedBlackZ2[keep]

blackLMSZScoreFmiLmi <- cbind(nhBlack, dCombinedBlackZ3)

##Hispanic Data Set##

hfTrunkFmiZ <- read.table("HispFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
hmTrunkFmiZ <- read.table("HispFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
hfTrunkLmiZ <- read.table("HispFmiLmi_Female_TrunkLMI_Zind_020401t.txt", header=T, sep="\t")
hmTrunkLmiZ <- read.table("HispFmiLmi_Male_TrunkLMI_Zind_010702t.txt", header=T, sep="\t")

hfArmFmiZ <- read.table("HispFmiLmi_Female_AvgArmFMI_Zind_020302t.txt", header=T, sep="\t")
hmArmFmiZ <- read.table("HispFmiLmi_Male_AvgArmFMI_Zind_010403t.txt", header=T, sep="\t")
hfArmLmiZ <- read.table("HispFmiLmi_Female_AvgArmLMI_Zind_020401t.txt", header=T, sep="\t")
hmArmLmiZ <- read.table("HispFmiLmi_Male_AvgArmLMI_Zind_010702t.txt", header=T, sep="\t")

hfLegFmiZ <- read.table("HispFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
hmLegFmiZ <- read.table("HispFmiLmi_Male_AvglegFMI_Zind_010102t.txt", header=T, sep="\t")
hfLegLmiZ <- read.table("HispFmiLmi_Female_AvgLegLMI_Zind_020401t.txt", header=T, sep="\t")
hmLegLmiZ <- read.table("HispFmiLmi_Male_AvgLegLMI_Zind_010602t.txt", header=T, sep="\t")

CombinedHispZ <- cbind(hfTrunkFmiZ, hmTrunkFmiZ, hfTrunkLmiZ, hmTrunkLmiZ,
                       hfArmFmiZ, hmArmFmiZ, hfArmLmiZ, hmArmLmiZ, 
                       hfLegFmiZ, hmLegFmiZ, hfLegLmiZ, hmLegLmiZ)

dCombinedHispZ <- data.frame(CombinedHispZ)

keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedHispZ2 <- dCombinedHispZ[keep]

colnames(dCombinedHispZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                               "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                               "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")


dCombinedHispZ2 <- as.data.frame(sapply(dCombinedHispZ2,sub,pattern='\\*',replacement=NA))


unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedHispZ2[,unfactorize] <-
                as.numeric(as.character(unlist(dCombinedHispZ2[,unfactorize])))
dCombinedHispZ2 <- transform(dCombinedHispZ2, 
                          ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombinedHispZ2$Gender1[is.na(dCombinedHispZ2$ZMTrunkFmi)] <- "Female"
dCombinedHispZ2$Gender1[!is.na(dCombinedHispZ2$ZMTrunkFmi)] <- "Male"

keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
           "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedHispZ3 <- dCombinedHispZ2[keep]

hispLMSZScoreFmiLmi <- cbind(nhHisp, dCombinedHispZ3)


##White Data Set##

wfTrunkFmiZ <- read.table("WhiteFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
wmTrunkFmiZ <- read.table("WhiteFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
wfTrunkLmiZ <- read.table("WhiteFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
wmTrunkLmiZ <- read.table("WhiteFmiLmi_Male_TrunkLMI_Zind_020702t.txt", header=T, sep="\t")

wfArmFmiZ <- read.table("WhiteFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
wmArmFmiZ <- read.table("WhiteFmiLmi_Male_AvgArmFMI_Zind_020402t.txt", header=T, sep="\t")
wfArmLmiZ <- read.table("WhiteFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
wmArmLmiZ <- read.table("WhiteFmiLmi_Male_AvgArmLMI_Zind_010801t.txt", header=T, sep="\t")

wfLegFmiZ <- read.table("WhiteFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
wmLegFmiZ <- read.table("WhiteFmiLmi_Male_AvgLegFMI_Zind_010202t.txt", header=T, sep="\t")
wfLegLmiZ <- read.table("WhiteFmiLmi_Female_AvgLegLMI_Zind_010601t.txt", header=T, sep="\t")
wmLegLmiZ <- read.table("WhiteFmiLmi_Male_AvgLegLMI_Zind_020702t.txt", header=T, sep="\t")


CombinedWhiteZ <- cbind(wfTrunkFmiZ, wmTrunkFmiZ, wfTrunkLmiZ, wmTrunkLmiZ, wfArmFmiZ,
                        wmArmFmiZ, wfArmLmiZ, wmArmLmiZ, wfLegFmiZ, wmLegFmiZ,
                        wfLegLmiZ, wmLegLmiZ)

dCombinedWhiteZ <- data.frame(CombinedWhiteZ)

keep <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombinedWhiteZ2 <- dCombinedWhiteZ[keep]

colnames(dCombinedWhiteZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi",
                                "ZMTrunkLmi", "ZFArmFmi", "ZMArmFmi", "ZFArmLmi",
                                "ZMArmLmi", "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")


dCombinedWhiteZ2 <- as.data.frame(sapply(dCombinedWhiteZ2,sub,pattern='\\*',replacement=NA))


unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombinedWhiteZ2[,unfactorize] <- 
                as.numeric(as.character(unlist(dCombinedWhiteZ2[,unfactorize])))
dCombinedWhiteZ2 <- transform(dCombinedWhiteZ2, 
                          ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombinedWhiteZ2$Gender1[is.na(dCombinedWhiteZ2$ZMTrunkFmi)] <- "Female"
dCombinedWhiteZ2$Gender1[!is.na(dCombinedWhiteZ2$ZMTrunkFmi)] <- "Male"

keep <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi",
           "ZFLegFmi","ZFLegLmi","Gender1")
dCombinedWhiteZ3 <- dCombinedWhiteZ2[keep]

whiteLMSZScoreFmiLmi <- cbind(nhWhite, dCombinedWhiteZ3)
#Importing LMS values and calculating arms/legs
#####
#                                        #
#                                        #
#Part 2: Importing LMS Z scores (And maybe calculating values?)
#                                        #
#                                        #

#Inputs:
setwd("X:\\bhinton\\Data\\LMS Tables\\LMS Values") #Set this to wd with LMS charts

keep <- c("Age","L", "M", "S")

bfArmFmiLms <- 
  read.table("BlackFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bfArmLmiLms <- 
  read.table("BlackFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
bfLegFmiLms <- 
  read.table("BlackFmiLmi_Female_AvgLegFMI_020302t.txt", header=T, skip=10, sep="\t")
bfLegLmiLms <- 
  read.table("BlackFmiLmi_Female_AvgLegLMI_010401t.txt", header=T, skip=10, sep="\t")

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

wmLms <- cbind(wmArmFmiLms[keep], wmArmLmiLms[keep], 
               wmLegFmiLms[keep], wmLegLmiLms[keep])

#Add more comments here and within the loop
#Right here I calculate the LMS Z score for both arms and legs in all cases.

for (i in 1:3){#Ethnicity, normally 1:3
  zScoreFinal = NULL
  for (j in 1:2){#Gender, normally 1:2
    if (i == 1) {zScore = blackLMSZScoreFmiLmi #Black 
              race = "Black"
    if (j == 1) {lmsChart = bfLms
      gender = "Female" #Female
    }
    else if (j == 2) {lmsChart = bmLms
      gender = "Male"#Male
    }
    } else if (i == 2) {zScore = hispLMSZScoreFmiLmi #Hisp
                      race = "Hisp"
    if (j == 1) {lmsChart = hfLms
      gender = "Female"#Female
    }
    else if (j == 2) {lmsChart = hmLms
      gender = "Male"#Male
    }
    } else if (i == 3) {zScore = whiteLMSZScoreFmiLmi #White
                    race = "White"
    if (j == 1) {lmsChart = wfLms
     gender = "Female"#Female
    }
    else if (j == 2) {lmsChart = wmLms
     gender = "Male"#Male
    }}
    for (k in  1:78){
      ageRow = k
      age = ageRow + 7
      zScore1 <- zScore[zScore[, "Age"] == age,]   
      zScore2 <- zScore1[zScore1[, "Gender"] == gender,] 
      lmsAge <- lmsChart[ageRow ,]                           
      
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
      
      zScore3 <- transform(zScore2, 
                         zLArmFmi= (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                         zRArmFmi= (((rightArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                         zLArmLmi= (((leftArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                         zRArmLmi= (((rightArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                         zLLegFmi= (((leftLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                         zRLegFmi= (((rightLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                         zLLegLmi= (((leftLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi),
                         zRLegLmi= (((rightLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi))
      zScore4 <- transform(zScore3,
                         zAvgFmi= (ZFTrunkFmi+zLArmFmi+zRArmFmi+zLLegFmi+zRLegFmi) / 5, 
                         zAvgLmi= (ZFTrunkLmi+zLArmLmi+zRArmLmi+zLLegLmi+zRLegLmi) / 5
      )
                        
                         
      keep <- c("Race","Gender", "BMXHT","BMXWT","RIDAGEYR",
                 "ZFTrunkFmi","ZFTrunkLmi", "zLArmFmi","zRArmFmi",
                 "zLArmLmi","zRArmLmi","zLLegFmi","zRLegFmi",
                 "zLLegLmi", "zRLegLmi", "zAvgFmi", "zAvgLmi")
      zScore5 <- zScore4[keep]
      
      colnames(zScore5) <- c("Race", "Gender", "Height", "Weight", "Age",
                             "Z_FMI_TR", "Z_LMI_TR", "Z_FMI_LA", "Z_FMI_RA",
                             "Z_LMI_LA","Z_LMI_RA", "Z_FMI_LL", "Z_FMI_RL",
                             "Z_LMI_LL", "Z_LMI_RL","Z_FMI_AVG", "Z_LMI_AVG")
      
      zScoreFinal <- rbind(zScoreFinal,zScore5)
      
      
      
}#End of cycle through ages 8-85
}#End of cycle through genders
  
  
  
  fmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
  colnames(fmiZSd) <- "Z_FMI_SD"
  lmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")])))
  colnames(lmiZSd) <- "Z_LMI_SD"
  zScoreFinal <- cbind(zScoreFinal, fmiZSd, lmiZSd)
  
  #setwd("X:\\bhinton")
  #write.table(zScoreFinal, file=sprintf("%s.ZScoreValues.txt",race))
  #Activate this to write to a new table (Will need to do this for averages)
  
}#End of cycle through races

# z = ( y / m)^L - 1 / (L*S)
#Producing Radar Chart (Separate LMI/FMI)
#####
#                                        #
#                                        #
#Part A: Producing separate FMI/LMI Charts
#                                        #
#                                        #

#Inputs:
library(fmsb) #Required package for Radar Charts
race = "Hisp" # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Female" #Either "Male" or "Female" CASE SENSITIVE
selectAge = 27  # set to age of interest
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4  #Number of individuals in this group that you want to examine
                  #Will choose random individuals in that demographic set
zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))


#Currently code will only select the first selectNumber rows in the file
zData1 <- zData[zData[, "Gender"] == gender,] 
zData2 <- zData1[zData1[, "Age"] == selectAge,] 
#Selects patients of certain age

dzData <- data.frame(zData2)  #Converts to dataframe
dimension <- dim(dzData)
nRow <- floor(runif(selectNumber, 1,dimension[1]))

fmiData <- dzData[nRow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nRow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]

colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")

maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

fmiDataFinal <- rbind(maxmin,fmiData)
lmiDataFinal <- rbind(maxmin,lmiData)

op <- par(mar=c(1, 2, 2, 1),mfrow=c(1, 2))
#mfrow: 1st  number is no. rows, 2nd is no. columns.
radarchart(fmiDataFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
 title=sprintf("%1.0f %s %s FMI Charts", selectNumber, race, gender))
#Cex.main is for the title

radarchart(lmiDataFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
           title=sprintf("%1.0f %s %s FFMI Charts", selectNumber, race, gender))

legend('topright', c("Person1", "Person2", "Person3", "Person4") ,
       lty=1, col=c("Black", "Red","Green","Blue"), bty='n', cex=1)    
# gives the legend appropriate symbols (lines)
#Producing Radar Chart (LMI/FMI superimposed for each individual)
#####
#                                                       #
#                                                       #
#Part B: Superimposing FFMI/FMI chart for each individual
#                                                       #
#                                                       #

#Inputs:
library(fmsb) #Required package for Radar Charts
race = "Hisp" # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Female" #Either "Male" or "Female" CASE SENSITIVE
selectAge = 24 # set to age of interest
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4  #Choices: 1, 2, 4, 9
                  #Number of individuals in this group that you want to examine
                  #Will choose random individuals in that demographic set

zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))

if (selectNumber == 1) {
  chartDim <- c(1,1)
} else if (selectNumber == 2) {
  chartDim <- c(1,2)
}else if (selectNumber == 4) {
  chartDim <- c(2,2) 
}else if (selectNumber == 9) {
  chartDim <- c(3,3)
}

#Currently code will only select the first selectNumber rows in the file
zData1 <- zData[zData[, "Gender"] == gender,] 
zData2 <- zData1[zData1[, "Age"] == selectAge,] 
#Selects patients of certain age

dzData <- data.frame(zData2)  #Converts to dataframe
dimension <- dim(dzData)
nRow <- floor(runif(selectNumber, 1,dimension[1]))

fmiData <- dzData[nRow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nRow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]

colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")

maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))


op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
for (i in 1:selectNumber){#Ethnicity, normally 1:3
  
  
  ind1Data <- rbind(maxmin,fmiData[i,],lmiData[i,])
  radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
             pcol = c("goldenrod3", "firebrick4"),
             vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
             title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
  
}  

legend('topright', c("FMI", "FFMI") , lwd=2, 
       col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2)   


#Color scheme.... black first, then red?
#Black is FMI
#Red is FFMI
#Producing Radar Chart (Superimposed and restricted by avg Z score)
#####
#                                        #
#                                        #
#Part C: Producing separate FMI/LMI Charts
#                                        #
#                                        #

#Inputs:
library(fmsb) #Required package for Radar Charts
race = "Hisp" # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Male" #Either "Male" or "Female" CASE SENSITIVE
#selectAge = 24   Not currently functional, but could maybe change this to 
                  #select from certain age range
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4  #Choices: 1, 2, 4, 9
                  #Number of individuals in this group that you want to examine
                  #Will choose random individuals in that demographic set
zChoice = 2  #Choice of the Avg Z score of individuals you'd like to plot
fmiOrLmi = "FMI"  #Choice of if you want the Avg Z score to be of Avg FMI or LMI
                  #Either FMI or LMI (Case sensitive)

zMax = zChoice + 0.1
zMin = zChoice - 0.1
zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
zData1 <- zData[zData[, "Gender"] == gender,] 


if (fmiOrLmi == "FMI") {
  zData2 <- subset(zData1, zData1$Z_FMI_AVG <= zMax & zData1$Z_FMI_AVG >= zMin) 
} else if (fmiOrLmi == "LMI") {
  zData2 <- subset(zData1, zData1$Z_LMI_AVG <= zMax & zData1$Z_LMI_AVG >= zMin) 
}
if (selectNumber == 1) {
  chartDim <- c(1,1)
} else if (selectNumber == 2) {
  chartDim <- c(1,2)
}else if (selectNumber == 4) {
  chartDim <- c(2,2) 
}else if (selectNumber == 9) {
  chartDim <- c(3,3)
}


dzData <- data.frame(zData2)  #Converts to dataframe
dimension <- dim(dzData)
nrow <- floor(runif(selectNumber, 1,dimension[1]))

fmiData <- dzData[nrow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nrow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]

colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")

maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))


op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
for (i in 1:selectNumber){#Ethnicity, normally 1:3
  
  
  ind1Data <- rbind(maxmin,fmiData[i,],lmiData[i,])
  radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
             pcol = c("goldenrod3", "firebrick4"),
             vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
             title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
  
}  

legend('topright', c("FMI", "FFMI") , lwd=2, 
       col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2)  
#Produce Table of SD of Regional Z by different Avg Z scores
#####
#                                        #
#                                        #
#Part D: Examining SD regional z by avg z score
#                                        #
#                                        #


#Inputs:
library(fmsb) #Required package for Radar Charts

#select from certain age range
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2

cNames <- c("zRange")
zRange <- (c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2))
zTable <- NULL
zTable <- data.frame(zRange)
nTable <- NULL
nTable <- data.frame(zRange)
column <- 1
count = 0
for (h in 1:2){#FMI/LMI}  
  for (i in 1:2){#Gender, normally 1:3
    zScoreFinal <- NULL

    for (j in 1:3){#Ethnicity, normally 1:2
      column <- column + 1
      zFile <- 0
      zSd <- NULL
      for (k in zRange){#Z score ranges
        print(k)
        
        zFile <- zFile + 1
        if (h == 1) {leanOrFat = "FMI"
          if (i == 1) {gender = "Female"
            if (j==1) {race = "Black"
            }else if (j==2) {race = "Hisp"
            }else if (j==3) {race = "White"}
          }else if (i==2) {gender = "Male"
            if (j==1) {race = "Black"
            }else if (j==2) {race = "Hisp"
            }else if (j==3) {race = "White"}}
        }else if (h==2) {leanOrFat = "LMI"}
          if (i == 1) {gender = "Female"
            if (j==1) {race = "Black"
            }else if (j==2) {race = "Hisp"
            }else if (j==3) {race = "White"}
          }else if (i==2) {gender = "Male"
            if (j==1) {race = "Black"
            }else if (j==2) {race = "Hisp"
            }else if (j==3) {race = "White"}
          }
        
        
        zCurrent <- k
        zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
        zData1 <- zData[zData[, "Gender"] == gender,] 
        zMax = zCurrent + 0.1
        zMin = zCurrent - 0.1
        
        
        
        if (fmiOrLmi == "FMI") {
          zData2 <- subset(zData1, zData1$Z_FMI_AVG <= zMax & zData1$Z_FMI_AVG >= zMin) 
          zSd <- colMeans(data.frame(zData2[,"Z_FMI_SD"]))
        } else if (fmiOrLmi == "LMI") {
          zData2 <- subset(zData1, zData1$Z_LMI_AVG <= zMax & zData1$Z_LMI_AVG >= zMin)
          zSd <- colMeans(data.frame(zData2[,"Z_LMI_SD"]))
        }
        print(zSd)
        dimension <- dim(zData2)
        #zSd <- data.frame(SD(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
        print(dimension[1])
        zTable[zFile,column] <- zSd
        nTable[zFile,column] <- dimension[1]
        
        
        #NEed to calculate average z score and store in a string
        #need to calculate and include the n of this stuff
        
      }#End of cycling through z scores 
      cNames <- cbind(cNames,sprintf("%s.%s.%s.Z_SD.",race,gender,leanOrFat) )
      #colnames(zSd) <- sprintf("%s.%s.%s.Z_SD.",race,gender,leanOrFat)
      #zTable[column,] <- cbind(zTable,zSd)
    }#End of Ethnicity
  }#End of Gender Cycles
}#End of FMI/LMI Switch
colnames(zTable) <- cNames        
colnames(nTable) <- cNames           
          
          
       
        
        zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
        zData1 <- zData[zData[, "Gender"] == gender,] 
        
        if (fmiOrLmi == "FMI") {
          zData2 <- subset(zData1, zData1$Z_FMI_AVG <= zMax & zData1$Z_FMI_AVG >= zMin)
          aa1 <- colMeans(data.frame(zData2[,"Z_FMI_SD"]))
        } else if (fmiOrLmi == "LMI") {
          zData2 <- subset(zData1, zData1$Z_LMI_AVG <= zMax & zData1$Z_LMI_AVG >= zMin) 
          aa1 <- colMeans(data.frame(zData2[,"Z_LMI_SD"]))
        }
        
        
      }#End of cycle through genders
      
      
      
      fmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
      colnames(fmiZSd) <- "Z_FMI_SD"
      lmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")])))
      colnames(lmiZSd) <- "Z_LMI_SD"
      zScoreFinal <- cbind(zScoreFinal, fmiZSd, lmiZSd)
      
      #setwd("X:\\bhinton")
      #write.table(zScoreFinal, file=sprintf("%s.ZScoreValues.txt",race))
      #Activate this to write to a new table (Will need to do this for averages)
      
    }#End of cycle through races
  
# Perform Various tTests to test symmetry, ethnicity, age, ratios, etc.
#####
#                                        #
#                                        #
#Part E: T-test of different things
#                                        #
#                                        #

blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))


#To compare::::
#Young vs old   # Each region, 
#Black vs Hisp vs White   #Each Region, 
#z=-2 vs zavg = 0  --> eccentricity of the shapes? FMI shape vs LMI shape?

#options to test:  (Could have different section for each test)
#total LMI/FMI ratios
#Symmetry/Assymetry (right side divided by left side)
#Regional LMI to FMI ratio
#Symmetry of LMI curve vs FMI curve

#Look at email with John



FullData <- rbind(blackData, hispData, whiteData)
#Options to select by:
#Inputs:
library(fmsb) #Required package for Radar Charts
race = "Hisp" # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Male" #Either "Male" or "Female" CASE SENSITIVE
#selectAge = 24   Not currently functional, but could maybe change this to 
#select from certain age range
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4  #Choices: 1, 2, 4, 9
#Number of individuals in this group that you want to examine
#Will choose random individuals in that demographic set
 #Choice of the Avg Z score of individuals you'd like to plot
 #Choice of if you want the Avg Z score to be of Avg FMI or LMI
#Either FMI or LMI (Case sensitive)


zChoice = 2 
zMax = zChoice + 0.1
zMin = zChoice - 0.1
fmiOrLmi = "FMI" 


if (fmiOrLmi == "FMI") {
  zData1 <- subset(blackData, blackData$Z_FMI_AVG <= zMax & blackData$Z_FMI_AVG >= zMin) 
  zData2 <- subset(whiteData, whiteData$Z_LMI_AVG <= zMax & whiteData$Z_LMI_AVG >= zMin)
} else if (fmiOrLmi == "LMI") {
  zData1 <- subset(blackData, blackData$Z_FMI_AVG <= zMax & blackData$Z_FMI_AVG >= zMin) 
  zData2 <- subset(whiteData, whiteData$Z_LMI_AVG <= zMax & whiteData$Z_LMI_AVG >= zMin) 
}

age1 <- blackData[blackData[, "Age"] == 40,]
age2 <- whiteData[whiteData[, "Age"] == 40,]

gender1 <- blackData[blackData[, "Gender"] == "Male",]
gender2 <- whiteData[whiteData[, "Gender"] == "Male",]


data1 <- zData1["Z_FMI_SD"]
data2 <-zData2["Z_FMI_SD"]
  

t.test(data1, data2)      # P = .00001855
