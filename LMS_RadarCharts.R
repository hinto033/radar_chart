#This program will take a certain amount of 
#individuals that you select and create radar charts for them
#It requires a certain number of inputs at the beginning to function properly

#Required Packages: fmsb (Contains radar function)
#                   sas7bdat (Allows to import sas files)
#must load and install the packages in order for the code to run


#NEED TO DO:
#Clean up variables and table names and legends
  #change colors to be better for FMI/LMI
  #CDhange variabels to exclude FFMI
  #change way plotting works so it is function for 1-4 plots
#Collect average z score values to get groups of avg -2, -1, -.5, 0, .5, 1, 2

#Bigger longer goal: get one large data sheet and be able 
#to select race/gender/etc all at once without uploading different files.

#####
#                                        #
#                                        #
#Part 0: Importing Sas data set and doing calcs
#                                        #
#                                        #

setwd("X:\\bhinton") # Set the working directory
library(sas7bdat)
nhanesFull = read.sas7bdat("dxa_bmx.sas7bdat")

nhanes <- nhanesFull[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                               "DXXTRFAT", "DXXTRLI", "DXDTRPF",
                               "DXXLAFAT", "DXXLALI", "DXDLAPF",
                               "DXXLLFAT", "DXXLLLI", "DXDLLPF",
                               "DXXRLFAT", "DXXRLLI", "DXDRLPF",
                               "DXXRAFAT", "DXXRALI", "DXDRAPF",
                               "DXDTOFAT", "DXDTOLI", "DXDTOPF"
                              )]

dfNhanes <- data.frame(nhanes)


dfNhanesClean <- na.omit(dfNhanes)


dfNhanesClean <- transform(dfNhanesClean, avgArmFat = (DXXLAFAT + DXXRAFAT) / 2, 
                           avgLegFat = (DXXLLFAT + DXXRLFAT) / 2,
                           avgArmLI = (DXXLALI + DXXRALI) / 2,
                           avgLegLI = (DXXLLLI + DXXRLLI) / 2
                           )

#Fix these equations
dfNhanesClean <- transform(dfNhanesClean, avgArmFmi = (avgArmFat/1000) / ((BMXHT/100)^2), 
                           avgLegFmi = (avgLegFat/1000) / ((BMXHT/100)^2),
                           trunkFmi = (DXXTRFAT/1000) / ((BMXHT/100)^2),
                           leftArmFmi = (DXXLAFAT/1000) / ((BMXHT/100)^2),
                           leftLegFmi = (DXXLLFAT/1000) / ((BMXHT/100)^2),
                           rightLegFmi = (DXXRLFAT/1000) / ((BMXHT/100)^2),
                           rightArmFmi = (DXXRAFAT/1000) / ((BMXHT/100)^2))

dfNhanesClean <- transform(dfNhanesClean, avgArmLmi = (avgArmLI/1000) / ((BMXHT/100)^2), 
                           avgLegLmi = (avgLegLI/1000) / ((BMXHT/100)^2),
                           trunkLmi = (DXXTRLI/1000) / ((BMXHT/100)^2),
                           leftArmLmi = (DXXLALI/1000) / ((BMXHT/100)^2),
                           leftLegLmi = (DXXLLLI/1000) / ((BMXHT/100)^2),
                           rightLegLmi = (DXXRLLI/1000) / ((BMXHT/100)^2),
                           rightArmLmi = (DXXRALI/1000) / ((BMXHT/100)^2))

dfNhanesCleanFmiLmi <- dfNhanesClean[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                                      "avgArmFmi", "avgLegFmi", "trunkFmi",
                                      "leftArmFmi", "leftLegFmi",
                                      "rightLegFmi", "rightArmFmi",
                                      "avgArmLmi", "avgLegLmi", "trunkLmi",
                                      "leftArmLmi", "leftLegLmi",
                                      "rightLegLmi", "rightArmLmi")]

nhanesBlack <- dfNhanesClean[dfNhanesClean[, "Race"] == "Non-Hispanic Black",] 
nhanesWhite <- dfNhanesClean[dfNhanesClean[, "Race"] == "Non-Hispanic White",] 
nhanesHisp <- subset(dfNhanesClean, dfNhanesClean$Race=="Mexican American" | dfNhanesClean$Race=="Other Hispanic")

nhanesBlackFmiLmi <- dfNhanesCleanFmiLmi[dfNhanesCleanFmiLmi[, "Race"] == "Non-Hispanic Black",] 
nhanesWhiteFmiLmi <- dfNhanesCleanFmiLmi[dfNhanesCleanFmiLmi[, "Race"] == "Non-Hispanic White",] 
nhanesHispFmiLmi <- subset(dfNhanesCleanFmiLmi, dfNhanesCleanFmiLmi$Race=="Mexican American" | dfNhanesCleanFmiLmi$Race=="Other Hispanic")
#This contains just the information needed to produce LMS charts  
#Tomorrow export this to CSV

#write.csv(nhanesBlackFmiLmi, file = "BlackFmiLmi.csv")
#write.csv(nhanesWhiteFmiLmi, file = "WhiteFmiLmi.csv")
#write.csv(nhanesHispFmiLmi, file = "HispFmiLmi.csv")

#write.csv(test, file = "dxa_bmx.csv")
#If I want to write the whole database

#####
#                                        #
#                                        #
#Part 1: Importing Individual Z scores (And maybe calculating values?)
#                                        #
#                                        #

keeps <- c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR", "trunkFmi", "trunkLmi", "leftArmFmi",
           "leftArmLmi", "rightArmFmi", "rightArmLmi", "leftLegFmi", "leftLegLmi",
           "rightLegFmi", "rightLegLmi")
nhBlack <- nhanesBlackFmiLmi[keeps]
nhHisp <- nhanesHispFmiLmi[keeps]
nhWhite <- nhanesWhiteFmiLmi[keeps]
setwd("X:\\bhinton\\Data\\LMS Tables\\Zind") # Set the working directory

bfTrunkFmiz <- read.table("BlackFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
bmTrunkFmiz <- read.table("BlackFmiLmi_Male_TrunkFMI_Zind_020401t.txt", header=T, sep="\t")
bfTrunkLmiz <- read.table("BlackFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
bmTrunkLmiz <- read.table("BlackFmiLmi_Male_TrunkLMI_Zind_010601t.txt", header=T, sep="\t")

bfArmFmiz <- read.table("BlackFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bmArmFmiz <- read.table("BlackFmiLmi_Male_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
bfArmLmiz <- read.table("BlackFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
bmArmLmiz <- read.table("BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, sep="\t")

bfLegFmiz <- read.table("BlackFmiLmi_Female_AvgLegFMI_Zind_020302t.txt", header=T, sep="\t")
bmLegFmiz <- read.table("BlackFmiLmi_Male_AvgLegFMI_Zind_020202t.txt", header=T, sep="\t")
bfLegLmiz <- read.table("BlackFmiLmi_Female_AvgLegLMI_Zind_010401t.txt", header=T, sep="\t")
bmLegLmiz <- read.table("BlackFmiLmi_Male_AvgLegLMI_Zind_010501t.txt", header=T, sep="\t")

CombBlackZ <- cbind(bfTrunkFmiz, bmTrunkFmiz, bfTrunkLmiz, bmTrunkLmiz, bfArmFmiz, bmArmFmiz, 
                    bfArmLmiz, bmArmLmiz, bfLegFmiz, bmLegFmiz, bfLegLmiz, bmLegLmiz)

dCombBlackZ <- data.frame(CombBlackZ)

keeps <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombBlackZ2 <- dCombBlackZ[keeps]

colnames(dCombBlackZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
                            "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
                            "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")

dCombBlackZ2 <- as.data.frame(sapply(dCombBlackZ2,sub,pattern='\\*',replacement=NA))

unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombBlackZ2[,unfactorize] <- as.numeric(as.character(unlist(dCombBlackZ2[,unfactorize])))
dCombBlackZ2 <- transform(dCombBlackZ2, ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombBlackZ2$Gender1[is.na(dCombBlackZ2$ZMTrunkFmi)] <- "Female"
dCombBlackZ2$Gender1[!is.na(dCombBlackZ2$ZMTrunkFmi)] <- "Male"

keeps <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi","ZFLegFmi","ZFLegLmi","Gender1")
dCombBlackZ3 <- dCombBlackZ2[keeps]

BlackEnd <- cbind(nhBlack, dCombBlackZ3)

##Hispanic##

hfTrunkFmiz <- read.table("HispFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
hmTrunkFmiz <- read.table("HispFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
hfTrunkLmiz <- read.table("HispFmiLmi_Female_TrunkLMI_Zind_020401t.txt", header=T, sep="\t")
hmTrunkLmiz <- read.table("HispFmiLmi_Male_TrunkLMI_Zind_010702t.txt", header=T, sep="\t")

hfArmFmiz <- read.table("HispFmiLmi_Female_AvgArmFMI_Zind_020302t.txt", header=T, sep="\t")
hmArmFmiz <- read.table("HispFmiLmi_Male_AvgArmFMI_Zind_010403t.txt", header=T, sep="\t")
hfArmLmiz <- read.table("HispFmiLmi_Female_AvgArmLMI_Zind_020401t.txt", header=T, sep="\t")
hmArmLmiz <- read.table("HispFmiLmi_Male_AvgArmLMI_Zind_010702t.txt", header=T, sep="\t")

hfLegFmiz <- read.table("HispFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
hmLegFmiz <- read.table("HispFmiLmi_Male_AvglegFMI_Zind_010102t.txt", header=T, sep="\t")
hfLegLmiz <- read.table("HispFmiLmi_Female_AvgLegLMI_Zind_020401t.txt", header=T, sep="\t")
hmLegLmiz <- read.table("HispFmiLmi_Male_AvgLegLMI_Zind_010602t.txt", header=T, sep="\t")

CombHispZ <- cbind(hfTrunkFmiz, hmTrunkFmiz, hfTrunkLmiz, hmTrunkLmiz, hfArmFmiz, hmArmFmiz, 
                    hfArmLmiz, hmArmLmiz, hfLegFmiz, hmLegFmiz, hfLegLmiz, hmLegLmiz)

dCombHispZ <- data.frame(CombHispZ)

keeps <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombHispZ2 <- dCombHispZ[keeps]

colnames(dCombHispZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
                            "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
                            "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")


dCombHispZ2 <- as.data.frame(sapply(dCombHispZ2,sub,pattern='\\*',replacement=NA))


unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombHispZ2[,unfactorize] <- as.numeric(as.character(unlist(dCombHispZ2[,unfactorize])))
dCombHispZ2 <- transform(dCombHispZ2, ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombHispZ2$Gender1[is.na(dCombHispZ2$ZMTrunkFmi)] <- "Female"
dCombHispZ2$Gender1[!is.na(dCombHispZ2$ZMTrunkFmi)] <- "Male"

keeps <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi","ZFLegFmi","ZFLegLmi","Gender1")
dCombHispZ3 <- dCombHispZ2[keeps]

HispEnd <- cbind(nhHisp, dCombHispZ3)


##White##
wfTrunkFmiz <- read.table("WhiteFmiLmi_Female_TrunkFMI_Zind_020402t.txt", header=T, sep="\t")
wmTrunkFmiz <- read.table("WhiteFmiLmi_Male_TrunkFMI_Zind_020502t.txt", header=T, sep="\t")
wfTrunkLmiz <- read.table("WhiteFmiLmi_Female_TrunkLMI_Zind_010401t.txt", header=T, sep="\t")
wmTrunkLmiz <- read.table("WhiteFmiLmi_Male_TrunkLMI_Zind_020702t.txt", header=T, sep="\t")

wfArmFmiz <- read.table("WhiteFmiLmi_Female_AvgArmFMI_Zind_020202t.txt", header=T, sep="\t")
wmArmFmiz <- read.table("WhiteFmiLmi_Male_AvgArmFMI_Zind_020402t.txt", header=T, sep="\t")
wfArmLmiz <- read.table("WhiteFmiLmi_Female_AvgArmLMI_Zind_010401t.txt", header=T, sep="\t")
wmArmLmiz <- read.table("WhiteFmiLmi_Male_AvgArmLMI_Zind_010801t.txt", header=T, sep="\t")

wfLegFmiz <- read.table("WhiteFmiLmi_Female_AvgLegFMI_Zind_020301t.txt", header=T, sep="\t")
wmLegFmiz <- read.table("WhiteFmiLmi_Male_AvgLegFMI_Zind_010202t.txt", header=T, sep="\t")
wfLegLmiz <- read.table("WhiteFmiLmi_Female_AvgLegLMI_Zind_010601t.txt", header=T, sep="\t")
wmLegLmiz <- read.table("WhiteFmiLmi_Male_AvgLegLMI_Zind_020702t.txt", header=T, sep="\t")


CombWhiteZ <- cbind(wfTrunkFmiz, wmTrunkFmiz, wfTrunkLmiz, wmTrunkLmiz, wfArmFmiz, wmArmFmiz, 
                    wfArmLmiz, wmArmLmiz, wfLegFmiz, wmLegFmiz, wfLegLmiz, wmLegLmiz)

dCombWhiteZ <- data.frame(CombWhiteZ)

keeps <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
dCombWhiteZ2 <- dCombWhiteZ[keeps]

colnames(dCombWhiteZ2) <- c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
                            "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
                            "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")


dCombWhiteZ2 <- as.data.frame(sapply(dCombWhiteZ2,sub,pattern='\\*',replacement=NA))


unfactorize<-c("Age", "ZFTrunkFmi", "ZMTrunkFmi", "ZFTrunkLmi", "ZMTrunkLmi",
               "ZFArmFmi", "ZMArmFmi", "ZFArmLmi", "ZMArmLmi",
               "ZFLegFmi", "ZMLegFmi", "ZFLegLmi", "ZMLegLmi")
dCombWhiteZ2[,unfactorize] <- as.numeric(as.character(unlist(dCombWhiteZ2[,unfactorize])))
dCombWhiteZ2 <- transform(dCombWhiteZ2, ZFTrunkFmi= pmax(ZFTrunkFmi, ZMTrunkFmi, na.rm=TRUE), 
                          ZFTrunkLmi= pmax(ZFTrunkLmi, ZMTrunkLmi, na.rm=TRUE), 
                          ZFArmFmi= pmax(ZFArmFmi, ZMArmFmi, na.rm=TRUE), 
                          ZFArmLmi= pmax(ZFArmLmi, ZMArmLmi, na.rm=TRUE),
                          ZFLegFmi= pmax(ZFLegFmi, ZMLegFmi, na.rm=TRUE),
                          ZFLegLmi= pmax(ZFLegLmi, ZMLegLmi, na.rm=TRUE))

dCombWhiteZ2$Gender1[is.na(dCombWhiteZ2$ZMTrunkFmi)] <- "Female"
dCombWhiteZ2$Gender1[!is.na(dCombWhiteZ2$ZMTrunkFmi)] <- "Male"

keeps <- c("Age","ZFTrunkFmi", "ZFTrunkLmi","ZFArmFmi","ZFArmLmi","ZFLegFmi","ZFLegLmi","Gender1")
dCombWhiteZ3 <- dCombWhiteZ2[keeps]

WhiteEnd <- cbind(nhWhite, dCombWhiteZ3)

#####
#                                        #
#                                        #
#Part 2: Importing LMS Z scores (And maybe calculating values?)
#                                        #
#                                        #
setwd("X:\\bhinton\\Data\\LMS Tables\\LMS Values")

dat2 <- read.table("BlackFmiLmi_Female_AvgArmFMI_020202t.txt",  skip=10, header =TRUE, sep ="\t")

keeps <- c("Age","L", "M", "S")
nhBlack <- nhanesBlackFmiLmi[keeps]
nhHisp <- nhanesHispFmiLmi[keeps]
nhWhite <- nhanesWhiteFmiLmi[keeps]

bfArmFmiLms <- read.table("BlackFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bfArmLmiLms <- read.table("BlackFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
bfLegFmiLms <- read.table("BlackFmiLmi_Female_AvgLegFMI_020302t.txt", header=T, skip=10, sep="\t")
bfLegLmiLms <- read.table("BlackFmiLmi_Female_AvgLegLMI_010401t.txt", header=T, skip=10, sep="\t")

bfLms <- cbind(bfArmFmiLms[keeps], bfArmLmiLms[keeps], bfLegFmiLms[keeps], bfLegLmiLms[keeps])

bmArmFmiLms <- read.table("BlackFmiLmi_Male_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bmArmLmiLms <- read.table("BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, skip=10, sep="\t")
bmLegFmiLms <- read.table("BlackFmiLmi_Male_AvgLegFMI_020202t.txt", header=T, skip=10, sep="\t")
bmLegLmiLms <- read.table("BlackFmiLmi_Male_AvgLegLMI_010501t.txt", header=T, skip=10, sep="\t")

bmLms <- cbind(bmArmFmiLms[keeps], bmArmLmiLms[keeps], bmLegFmiLms[keeps], bmLegLmiLms[keeps])

hfArmFmiLms <- read.table("HispFmiLmi_Female_AvgArmFMI_020302t.txt", header=T, skip=10, sep="\t")
hfArmLmiLms <- read.table("HispFmiLmi_Female_AvgArmLMI_020401t.txt", header=T, skip=10, sep="\t")
hfLegFmiLms <- read.table("HispFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
hfLegLmiLms <- read.table("HispFmiLmi_Female_AveLegLMI_020401t.txt", header=T, skip=10, sep="\t")

hfLms <- cbind(hfArmFmiLms[keeps], hfArmLmiLms[keeps], hfLegFmiLms[keeps], hfLegLmiLms[keeps])

hmArmFmiLms <- read.table("HispFmiLmi_Male_AvgArmFMI_010403t.txt", header=T, skip=10, sep="\t")
hmArmLmiLms <- read.table("HispFmiLmi_Male_AvgArmLMI_010702t.txt", header=T, skip=10, sep="\t")
hmLegFmiLms <- read.table("HispFmiLmi_Male__AvgLegFMI_010102t.txt", header=T, skip=10, sep="\t")
hmLegLmiLms <- read.table("HispFmiLmi_Male_AvgLegLMI_010602t.txt", header=T, skip=10, sep="\t")

hmLms <- cbind(hmArmFmiLms[keeps], hmArmLmiLms[keeps], hmLegFmiLms[keeps], hmLegLmiLms[keeps])

wfArmFmiLms <- read.table("WhiteFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
wfArmLmiLms <- read.table("WhiteFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
wfLegFmiLms <- read.table("WhiteFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
wfLegLmiLms <- read.table("WhiteFmiLmi_Female_AvgLegLMI_010601t.txt", header=T, skip=10, sep="\t")

wfLms <- cbind(wfArmFmiLms[keeps], wfArmLmiLms[keeps], wfLegFmiLms[keeps], wfLegLmiLms[keeps])

wmArmFmiLms <- read.table("WhiteFmiLmi_Male_AvgArmFMI_020402t.txt", header=T, skip=10, sep="\t")
wmArmLmiLms <- read.table("WhiteFmiLmi_Male_AvgArmLMI_010801t.txt", header=T, skip=10, sep="\t")
wmLegFmiLms <- read.table("WhiteFmiLmi_Male_AvgLegFMI_010202t.txt", header=T, skip=10, sep="\t")
wmLegLmiLms <- read.table("WhiteFmiLmi_Male_AvgLagLMI_020702t.txt", header=T, skip=10, sep="\t")

wmLms <- cbind(wmArmFmiLms[keeps], wmArmLmiLms[keeps], wmLegFmiLms[keeps], wmLegLmiLms[keeps])


#Right here I calculate the LMS Z score for both arms and legs in all cases.
for (i in 1:3){#Ethnicity, normally 1:3
  valsfinals = NULL
for (j in 1:2){#Gender, normally 1:2

  if (i == 1) {vals = BlackEnd #Black 
              race = "Black"
  if (j == 1) {LMSChart = bfLms
    gender = "Female" #Female
  }
  else if (j == 2) {LMSChart = bmLms
    gender = "Male"#Male
  }
  } else if (i == 2) {vals = HispEnd #Hisp
                      race = "Hisp"
  if (j == 1) {LMSChart = hfLms
    gender = "Female"#Female
  }
  else if (j == 2) {LMSChart = hmLms
    gender = "Male"#Male
  }
  } else if (i == 3) {vals = WhiteEnd #White
                    race = "White"
  if (j == 1) {LMSChart = wfLms
    gender = "Female"#Female
  }
  else if (j == 2) {LMSChart = wmLms
    gender = "Male"#Male
  }}
  
for (k in  1:78){
  print(k)
  agerow = k
  age = agerow + 7
  
  vals1 <- vals[vals[, "Age"] == age,]   
  vals2 <- vals1[vals1[, "Gender"] == gender,] 
  LMSage <- LMSChart[agerow ,]                           
  
  LArmFmi = data.matrix(LMSage[2]) 
  MArmFmi = data.matrix(LMSage[3]) 
  SArmFmi = data.matrix(LMSage[4])
  LArmLmi = data.matrix(LMSage[6])
  MArmLmi = data.matrix(LMSage[7])
  SArmLmi = data.matrix(LMSage[8])
  LLegFmi = data.matrix(LMSage[10])
  MLegFmi = data.matrix(LMSage[11])
  SLegFmi = data.matrix(LMSage[12])
  LLegLmi = data.matrix(LMSage[14])
  MLegLmi = data.matrix(LMSage[15])
  SLegLmi = data.matrix(LMSage[16])
  
  vals3 <- transform(vals2, ZLArmFmi= (((leftArmFmi/MArmFmi)^LArmFmi)-1)/(LArmFmi*SArmFmi),
                     ZRArmFmi= (((rightArmFmi/MArmFmi)^LArmFmi)-1)/(LArmFmi*SArmFmi),
                     ZLArmLmi= (((leftArmLmi/MArmLmi)^LArmLmi)-1)/(LArmLmi*SArmLmi),
                     ZRArmLmi= (((rightArmLmi/MArmLmi)^LArmLmi)-1)/(LArmLmi*SArmLmi),
                     ZLLegFmi= (((leftLegFmi/MLegFmi)^LLegFmi)-1)/(LLegFmi*SLegFmi),
                     ZRLegFmi= (((rightLegFmi/MLegFmi)^LLegFmi)-1)/(LLegFmi*SLegFmi),
                     ZLLegLmi= (((leftLegLmi/MLegLmi)^LLegLmi)-1)/(LLegLmi*SLegLmi),
                     ZRLegLmi= (((rightLegLmi/MLegLmi)^LLegLmi)-1)/(LLegLmi*SLegLmi))
  
  keeps <- c("Race","Gender", "BMXHT","BMXWT","RIDAGEYR","ZFTrunkFmi","ZFTrunkLmi",
             "ZLArmFmi","ZRArmFmi", "ZLArmLmi","ZRArmLmi","ZLLegFmi","ZRLegFmi","ZLLegLmi", "ZRLegLmi")
  vals4 <- vals3[keeps]
  
  colnames(vals4) <- c("Race", "Gender", "Height", "Weight", "Age", "Z_FMI_Tr", "Z_FFMI_Tr", "Z_FMI_LA",
                       "Z_FMI_RA", "Z_FFMI_LA","Z_FFMI_RA", "Z_FMI_LL", "Z_FMI_RL", "Z_FFMI_LL", "Z_FFMI_RL")
  
  valsfinals <- rbind(valsfinals,vals4)
  
}#End of cycle through ages 8-85
}#End of cycle through genders
  setwd("X:\\bhinton")
  write.table(valsfinals, file=sprintf("%s.ZScoreValues.txt",race))
}#End of cycle through races

# z = ( y / m)^L - 1 / (L*S)

#####
#                                        #
#                                        #
#Part A: Producing separate FMI/LMI Charts
#                                        #
#                                        #
#Parameter input:
#setwd("X:\\bhinton\\Ind Z Scores") # Set the working directory
#Must use forward slash or double blackslash. a single backslash will not work in R
#Zind = read.csv("BF_Comb_Zind.csv")  # read csv file  BF = Black Female
#Other options are: BF_Comb_Zind.csv  BM_Comb_Zind.csv   (Black male/female)
#                   WF_Comb_Zind.csv  WM_Comb_Zind.csv  (White male/female)
#                   HF_Comb_Zind.csv  HF_Comb_Zind.csv  (Hispanic/Mexican Male/female)

  race = "Hisp"
  gender = "Female"
  selectAge = 24
  setwd("X:\\bhinton")
  Zind <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))

#selectAge = 15
selectNumber = 4
#Currently code will only select the first selectNumber rows in the file
ZindAge <- Zind[Zind[, "Gender"] == gender,] 
ZindAges <- ZindAge[ZindAge[, "Age"] == selectAge,] 
#Selects patients of certain age

dfZindAges <- data.frame(ZindAges)  #Converts to dataframe
test <- dim(dfZindAges)
numb <- floor(runif(selectNumber, 1,test[1]))

dfZindAgesFmi <- dfZindAges[numb,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
dfZindAgesFfmi <- dfZindAges[numb,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]

#dfZindAgesFmi <- dfZindAges[1:selectNumber,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
#dfZindAgesFfmi <- dfZindAges[1:selectNumber,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]
#1:4 selects the first 4 rows, the c and quotes selects certain columns
fmiDat <- dfZindAgesFmi
ffmiDat <- dfZindAgesFfmi

colnames(fmiDat) <- c("Z_Tr", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(ffmiDat) <- c("Z_Tr", "Z_LA", "Z_LL", "Z_RL", "Z_RA")


maxmin <- data.frame(
  Z_Tr=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

fmiDatFinal <- rbind(maxmin,fmiDat)
ffmiDatFinal <- rbind(maxmin,ffmiDat)

op <- par(mar=c(1, 2, 2, 1),mfrow=c(1, 2))
#mfrow: 1st  number is no. rows, 2nd is no. columns.
radarchart(fmiDatFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%1.0f %s %s FMI Charts", selectNumber, race, gender))
#cex.lab doesnt do anything
#Cex.main is for the title

radarchart(ffmiDatFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%1.0f %s %s FFMI Charts", selectNumber, race, gender))
#cex.lab doesnt do anything



legend('topright', c("Person1", "Person2", "Person3", "Person4") , lty=1, col=c("Black", "Red","Green","Blue"), bty='n', cex=1)    
# gives the legend appropriate symbols (lines)
#####
#                                                       #
#                                                       #
#Part B: Superimposing FFMI/FMI chart for each individual
#                                                       #
#                                                       #

#Parameter input:
#setwd("X:\\bhinton\\Ind Z Scores") # Set the working directory
#Must use forward slash or double blackslash. a single backslash will not work in R
#Zind = read.csv("BF_Comb_Zind.csv")  # read csv file  BF = Black Female
#Other options are: BF_Comb_Zind.csv  BM_Comb_Zind.csv   (Black male/female)
#                   WF_Comb_Zind.csv  WM_Comb_Zind.csv  (White male/female)
#                   HF_Comb_Zind.csv  HF_Comb_Zind.csv  (Hispanic/Mexican Male/female)


race = "Hisp"
gender = "Male"
selectAge = 24
setwd("X:\\bhinton")
Zind <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))

#selectAge = 15
selectNumber = 4


#Currently code will only select the first selectNumber rows in the file
ZindAge <- Zind[Zind[, "Gender"] == gender,] 
ZindAges <- ZindAge[ZindAge[, "Age"] == selectAge,] 
#Selects patients of certain age

dfZindAges <- data.frame(ZindAges)  #Converts to dataframe
test <- dim(dfZindAges)
numb <- floor(runif(selectNumber, 1,test[1]))

dfZindAgesFmi <- dfZindAges[numb,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
dfZindAgesFfmi <- dfZindAges[numb,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]
#1:4 selects the first 4 rows, the c and quotes selects certain columns
fmiDat <- dfZindAgesFmi
ffmiDat <- dfZindAgesFfmi


colnames(fmiDat) <- c("Z_Tr", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(ffmiDat) <- c("Z_Tr", "Z_LA", "Z_LL", "Z_RL", "Z_RA")


maxmin <- data.frame(
  Z_Tr=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

ind1Dat <- rbind(maxmin,fmiDat[1,],ffmiDat[1,])
ind2Dat <- rbind(maxmin,fmiDat[2,],ffmiDat[2,])
ind3Dat <- rbind(maxmin,fmiDat[3,],ffmiDat[3,])
ind4Dat <- rbind(maxmin,fmiDat[4,],ffmiDat[4,])

op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))

radarchart(ind1Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
#cex.lab doesnt do anything
#Cex.main is for the title

radarchart(ind2Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
#cex.lab doesnt do anything
#Cex.main is for the title
radarchart(ind3Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
#cex.lab doesnt do anything
#Cex.main is for the title
radarchart(ind4Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
#cex.lab doesnt do anything
#Cex.main is for the title


legend('topright', c("FMI", "FFMI") , lty=1, col=c("Black", "Red"), bty='n', cex=1)    
        # gives the legend appropriate symbols (lines)
       
      # gives the legend lines the correct color and width


#Color scheme.... black first, then red?
#Black is FMI
#Red is FFMI
