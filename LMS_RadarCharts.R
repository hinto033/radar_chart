

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
nhanesFullData = read.sas7bdat("dxa_bmx.sas7bdat")
#keeps only certain columns in new data set
nhanesBodyComp <- nhanesFullData[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                               "DXXTRFAT", "DXXTRLI", "DXDTRPF",
                               "DXXLAFAT", "DXXLALI", "DXDLAPF",
                               "DXXLLFAT", "DXXLLLI", "DXDLLPF",
                               "DXXRLFAT", "DXXRLLI", "DXDRLPF",
                               "DXXRAFAT", "DXXRALI", "DXDRAPF",
                               "DXDTOFAT", "DXDTOLI", "DXDTOPF"
                              )] #Fat = fat mass, #LI = lean + bone PF = percent fat
                                #TR = Trunk LA = Left Arm LL = Left Leg RA = right arm
                                #RL = right leg

dNhanesClean <- data.frame(nhanesBodyComp) #Convert to data frame
dNhanesClean1 <- na.omit(dNhanesClean) #Clears any rows that have NA entries (Small number)

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
                           rightArmFmi = (DXXRAFAT/1000) / ((BMXHT/100)^2))
dNhanesCleanFinal <- transform(dNhanesClean3, avgArmLmi = (avgArmLI/1000) / ((BMXHT/100)^2), 
                           avgLegLmi = (avgLegLI/1000) / ((BMXHT/100)^2),
                           trunkLmi = (DXXTRLI/1000) / ((BMXHT/100)^2),
                           leftArmLmi = (DXXLALI/1000) / ((BMXHT/100)^2),
                           leftLegLmi = (DXXLLLI/1000) / ((BMXHT/100)^2),
                           rightLegLmi = (DXXRLLI/1000) / ((BMXHT/100)^2),
                           rightArmLmi = (DXXRALI/1000) / ((BMXHT/100)^2))
#Keeps only variables we are interested in for producing LMS tables with:
dNhanesFmiLmi <- dNhanesCleanFinal[,c("Race","Gender", "BMXHT", "BMXWT", "RIDAGEYR",
                                      "avgArmFmi", "avgLegFmi", "trunkFmi",
                                      "leftArmFmi", "leftLegFmi",
                                      "rightLegFmi", "rightArmFmi",
                                      "avgArmLmi", "avgLegLmi", "trunkLmi",
                                      "leftArmLmi", "leftLegLmi",
                                      "rightLegLmi", "rightArmLmi")]
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
#write.csv(test, file = "dxa_bmx.csv")   #to rewrite the entire NHANES database as .csv

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
#Importing LMS values and calculating arms/legs
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
    if (i == 1) {zScore = blackLMSZScoreFmiLmi #Black arm and leg FMI/LMI data
              race = "Black"
    if (j == 1) {lmsChart = bfLms
      gender = "Female" #Female
    }
    else if (j == 2) {lmsChart = bmLms
      gender = "Male"#Male
    }
    } else if (i == 2) {zScore = hispLMSZScoreFmiLmi #Hisp arm and leg FMI/LMI data
                      race = "Hisp"
    if (j == 1) {lmsChart = hfLms
      gender = "Female"#Female
    }
    else if (j == 2) {lmsChart = hmLms
      gender = "Male"#Male
    }
    } else if (i == 3) {zScore = whiteLMSZScoreFmiLmi #White arm and leg FMI/LMI data
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
      keep <- c("Race","Gender", "BMXHT","BMXWT","RIDAGEYR",
                 "ZFTrunkFmi","ZFTrunkLmi", "zLArmFmi","zRArmFmi",
                 "zLArmLmi","zRArmLmi","zLLegFmi","zRLegFmi",
                 "zLLegLmi", "zRLegLmi", "zAvgFmi", "zAvgLmi")
      zScore5 <- zScore4[keep]
      #Changes column names to create the radar charts later on
      colnames(zScore5) <- c("Race", "Gender", "Height", "Weight", "Age",
                             "Z_FMI_TR", "Z_LMI_TR", "Z_FMI_LA", "Z_FMI_RA",
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
  fmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")])))
  colnames(fmiZSd) <- "Z_FMI_SD"
  lmiZSd <- data.frame(SD(t(zScoreFinal[,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")])))
  colnames(lmiZSd) <- "Z_LMI_SD"
  #Adds this Sd value in with the right/left arm/leg lmi/fmi calculations
  zScoreFinal <- cbind(zScoreFinal, fmiZSd, lmiZSd)
  
  #Optionally:
  #the two commented out lines below this would write each of these tables to a .txt
  #by race
  #setwd("X:\\bhinton")
  #write.table(zScoreFinal, file=sprintf("%s.ZScoreValues.txt",race))
  #Activate this to write to a new table (Will need to do this for averages)
  
}#End of cycle through races


#Producing Radar Chart (Separate LMI/FMI)
#####
#                                        #
#                                        #
#Part A: Producing separate FMI/LMI Charts
#                                        #
#                                        #

#Explanation: This produces radar charts that will produce 2 radar charts: one of FMI
#Pentagons of random individuals in a certain age/gender/race demographic that you choose
#and one LMI pentagons of those same individuals. You can select the number of individuals
#To plot at any one time.

#Inputs:
library(fmsb)         #Required package for Radar Charts
race = "Hisp"         # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Female"     #Either "Male" or "Female" CASE SENSITIVE
selectAge = 27        # set to age of interest
setwd("X:\\bhinton")  #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4      #Number of individuals in this group that you want to examine
                      #Will choose random individuals in that demographic set
zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t")) #imports the data
                                          #set from part 2


#Selects patients of certain age and gender
zData1 <- zData[zData[, "Gender"] == gender,] 
zData2 <- zData1[zData1[, "Age"] == selectAge,] 


dzData <- data.frame(zData2)  #Converts that set to dataframe
#finds dimensions of that table and takes selectNumber random rows from that data set
dimension <- dim(dzData)
nRow <- floor(runif(selectNumber, 1,dimension[1]))
#selects out only those random rows and their FMI/LMI data
fmiData <- dzData[nRow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nRow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]
#renames the columns because column names in fmiData/lmiData must match maxmin
colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
#maxmin sets the maximum and minimum pentagon values
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))
#radarchart only works with two rows (maxmin) up on the top, 
#and then the data you are plotting in rows below
fmiDataFinal <- rbind(maxmin,fmiData)
lmiDataFinal <- rbind(maxmin,lmiData)

# op = Graphing parameters: #mfrow: 1st  number is no. rows, 2nd is no. columns.
#Cex.main is for the title size, seg=number of segments between lines
#plwd = line width
op <- par(mar=c(1, 2, 2, 1),mfrow=c(1, 2))
radarchart(fmiDataFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
 title=sprintf("%1.0f %s %s FMI Charts", selectNumber, race, gender))
radarchart(lmiDataFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
           title=sprintf("%1.0f %s %s FFMI Charts", selectNumber, race, gender))
#Sets legend
legend('topright', c("Person1", "Person2", "Person3", "Person4") ,
       lty=1, col=c("Black", "Red","Green","Blue"), bty='n', cex=1)    
#Producing Radar Chart (LMI/FMI superimposed for each individual)
#####
#                                                       #
#                                                       #
#Part B: Superimposing FFMI/FMI chart for each individual
#                                                       #
#                                                       #

#Explanation: This produces radar charts that will produce selectNumber radar charts:
#Each chart is an individuals overlayed FMI/LMI radar chart. You can select the number
#of individuals and the age/gender/race to plot at any one time.


#Inputs:
library(fmsb) #Required package for Radar Charts
race = "Hisp" # Either "Black", "Hisp", or "White" Case Sensitive
gender = "Female" #Either "Male" or "Female" CASE SENSITIVE
selectAge = 24 # set to age of interest
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2
selectNumber = 4  #Choices: 1, 2, 4, 9
                  #Number of individuals in this group that you want to examine
                  #Will choose random individuals in that demographic set
#imports dataset of the race that you chose
zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
#Changes chart dimensions based on number of plots you want to make.
if (selectNumber == 1) {
  chartDim <- c(1,1)
} else if (selectNumber == 2) {
  chartDim <- c(1,2)
}else if (selectNumber == 4) {
  chartDim <- c(2,2) 
}else if (selectNumber == 9) {
  chartDim <- c(3,3)
}

#Selects patients of certain age and gender
zData1 <- zData[zData[, "Gender"] == gender,] 
zData2 <- zData1[zData1[, "Age"] == selectAge,] 


dzData <- data.frame(zData2)  #Converts that set to dataframe
#finds dimensions of that table and takes selectNumber random rows from that data set
dimension <- dim(dzData)
nRow <- floor(runif(selectNumber, 1,dimension[1]))
#selects out only those random rows and their FMI/LMI data
fmiData <- dzData[nRow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nRow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]
#renames the columns because column names in fmiData/lmiData must match maxmin
colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
#maxmin sets the maximum and minimum pentagon values
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))
#radarchart only works with two rows (maxmin) up on the top, 
#and then the data you are plotting in rows below

# op = Graphing parameters: #mfrow: 1st  number is no. rows, 2nd is no. columns.
op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
for (i in 1:selectNumber){#Each time loop runs, it plots another individual's lmi/fmi chart
  ind1Data <- rbind(maxmin,fmiData[i,],lmiData[i,])
 
  #Cex.main is for the title size, seg=number of segments between lines
  #plwd = line width
  radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
             pcol = c("goldenrod3", "firebrick4"),
             vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
             title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
  
}  
#Sets legend
legend('topright', c("FMI", "FFMI") , lwd=2, 
       col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2)   

#goldenrod3 is FMI
#firebrick4 is FFMI
#Producing Radar Chart (Superimposed and restricted by avg Z score)
#####
#                                        #
#                                        #
#Part C: Producing separate FMI/LMI Charts for a limited average Z score
#                                        #
#                                        #

#Explanation: This produces radar charts that will produce selectNumber radar charts:
#Each chart is an individuals overlayed FMI/LMI radar chart. You can select the number
#of individuals and the age/gender/race to plot at any one time. In addition,
#This program allows you to choose only individuals with a certain average FMI or LMI Z
#score that is averages from their five regional Z scores


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

#sets the max and min tolerance for average Z scores
zMax = zChoice + 0.1
zMin = zChoice - 0.1
#imports data
zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
#selects only people of certain gender of your choice
zData1 <- zData[zData[, "Gender"] == gender,] 

#chooses only individuals with an avg FMI or LMI z score based on your choice.
if (fmiOrLmi == "FMI") {
  zData2 <- subset(zData1, zData1$Z_FMI_AVG <= zMax & zData1$Z_FMI_AVG >= zMin) 
} else if (fmiOrLmi == "LMI") {
  zData2 <- subset(zData1, zData1$Z_LMI_AVG <= zMax & zData1$Z_LMI_AVG >= zMin) 
}
#sets chart dimensions based on number of plots.
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
#finds dimensions of that table and takes selectNumber random rows from that data set
dimension <- dim(dzData)
nrow <- floor(runif(selectNumber, 1,dimension[1]))
#selects out only those random rows and their FMI/LMI data
fmiData <- dzData[nrow,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
lmiData <- dzData[nrow,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]
#renames the columns because column names in fmiData/lmiData must match maxmin
colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
#maxmin sets the maximum and minimum pentagon values
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))
#radarchart only works with two rows (maxmin) up on the top, 
#and then the data you are plotting in rows below

# op = Graphing parameters: #mfrow: 1st  number is no. rows, 2nd is no. columns.
op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
for (i in 1:selectNumber){#Ethnicity, normally 1:3
  
  #Cex.main is for the title size, seg=number of segments between lines
  #plwd = line width
  ind1Data <- rbind(maxmin,fmiData[i,],lmiData[i,])
  radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
             pcol = c("goldenrod3", "firebrick4"),
             vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
             title=sprintf("%s %s Individual FMI/LMI Chart", race, gender))
  
}  
#Sets legend
legend('topright', c("FMI", "FFMI") , lwd=2, 
       col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2)  
#Produce Table of SD of Regional Z by different Avg Z scores
#####
#                                        #
#                                        #
#Part D: Examining SD regional z by avg z score
#                                        #
#                                        #

#Explanation: This produces a table comparing the standard deviation of regional
#FMI/LMI z scores separated by race, gender, and average Z score


#Inputs:
library(fmsb) #Required package for Radar Charts

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
        #Selects certain FMI/LMI, gender, and race group
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
        
        #Sets Z range
        zCurrent <- k
        zData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
        zData1 <- zData[zData[, "Gender"] == gender,] 
        zMax = zCurrent + 0.1
        zMin = zCurrent - 0.1
        
        
        #Finds the average standard deviation of that group within the certain z range
        if (fmiOrLmi == "FMI") {
          zData2 <- subset(zData1, zData1$Z_FMI_AVG <= zMax & zData1$Z_FMI_AVG >= zMin) 
          zSd <- colMeans(data.frame(zData2[,"Z_FMI_SD"]))
        } else if (fmiOrLmi == "LMI") {
          zData2 <- subset(zData1, zData1$Z_LMI_AVG <= zMax & zData1$Z_LMI_AVG >= zMin)
          zSd <- colMeans(data.frame(zData2[,"Z_LMI_SD"]))
        }
        
        
        dimension <- dim(zData2)
        print(dimension[1])
        #Puts this information into a data table.
        #Produces table of average standard deviation of each group
        zTable[zFile,column] <- zSd
        #produces table of number of individuals in each group
        nTable[zFile,column] <- dimension[1]
        
        
        #NEed to calculate average z score and store in a string
        #need to calculate and include the n of this stuff
        
      }#End of cycling through z scores 
      #Labels the column names to read them. 
      cNames <- cbind(cNames,sprintf("%s.%s.%s.Z_SD.",race,gender,leanOrFat) )
      #colnames(zSd) <- sprintf("%s.%s.%s.Z_SD.",race,gender,leanOrFat)
      #zTable[column,] <- cbind(zTable,zSd)
    }#End of Ethnicity
  }#End of Gender Cycles
}#End of FMI/LMI Switch
colnames(zTable) <- cNames        
colnames(nTable) <- cNames           
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


##Separate by young vs old##

blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))

fullData <- rbind(blackData, hispData, whiteData)

ageChoice1 <-24 #Maybe include a range?
ageChoice2 <-80 #Maybe include a range?
ageMax1 = ageChoice1 + 3
ageMin1 = ageChoice1 - 3
ageMax2 = ageChoice2 + 3
ageMin2 = ageChoice2 - 3  
  
set1 <- blackData
set2 <- whiteData

ageData1 <- subset(set1, set1$Age <= ageMax1 & set1$Age >= ageMin1) 
ageData2 <- subset(set2, set2$Age <= ageMax2 & set2$Age >= ageMin2)

genderData1 <- ageData1[ageData1[, "Gender"] == "Male",]
genderData2 <- ageData2[ageData2[, "Gender"] == "Male",]


data1 <- genderData1["Z_FMI_SD"]
data2 <-genderData2["Z_FMI_SD"]

t.test(data1, data2)      # P = .00001855




##Z score -2 vs 0 avg##

blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))

fmiOrLmi = "FMI" 
zChoice1 <- 2
zChoice2 <- 0
zMax1 = zChoice1 + 0.1
zMin1 = zChoice1 - 0.1
zMax2 = zChoice2 + 0.1
zMin2 = zChoice2 - 0.1

set1 <- blackData
set2 <- whiteData


if (fmiOrLmi == "FMI") {
  zData1 <- subset(set1, set1$Z_FMI_AVG <= zMax1 & set1$Z_FMI_AVG >= zMin1) 
  zData2 <- subset(set2, set2$Z_FMI_AVG <= zMax2 & set2$Z_FMI_AVG >= zMin2)
} else if (fmiOrLmi == "LMI") {
  zData1 <- subset(set1, set1$Z_LMI_AVG <= zMax1 & set1$Z_LMI_AVG >= zMin1) 
  zData2 <- subset(set2, set2$Z_LMI_AVG <= zMax2 & set2$Z_LMI_AVG >= zMin2)
}


data1 <- zData1["Z_FMI_SD"]
data2 <-zData2["Z_FMI_SD"]

t.test(data1, data2)      # P = .00001855


##Race Vs Race##

blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))

#Choose age and gender as well if you want.
set1 <- blackData
set2 <- whiteData


age1 <- set1[set1[, "Age"] == 40,]
age2 <- set2[set2[, "Age"] == 40,]

gender1 <- age1[age1[, "Gender"] == "Male",]
gender2 <- age2[age2[, "Gender"] == "Male",]

zData1 <- gender1
zdata2 <- gender2

data1 <- zData1["Z_FMI_SD"]
data2 <-zData2["Z_FMI_SD"]


t.test(data1, data2)      # P = .00001855















#To compare::::
#Young vs old   # Each region, 
#Black vs Hisp vs White   #Each Region, 
#z=-2 vs zavg = 0  --> eccentricity of the shapes? FMI shape vs LMI shape?

#options to test:  (Could have different section for each test)
#Each Region
#total LMI/FMI ratios
#Symmetry/Assymetry (right side divided by left side)
#Regional LMI to FMI ratio
#Symmetry of LMI curve vs FMI curve


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

#####
#                                        #
#                                        #
#Part F: Tracking of BMDCS data points
#                                        #
#                                        #


#Explanation: This will take a random individual from BMDCS data set and track
#their pentagon over a time of several years to see how thier pentagon changes as they age.


#Inputs:
setwd("X:\\bhinton\\BMDCS") 
    # Set the working directory where "dxa_bmx.sas7bdat" is located

#Imports the .sas7bdat file:
#library(sas7bdat) #Loads the package that allows for sas data import

form01 <- 
  read.table("form01.txt", header=T, skip=0, sep="\t")
form03 <- 
  read.table("form03.txt", header=T, skip=0, sep="\t")
form03a <- 
  read.table("form03a.txt", header=T, skip=0, sep="\t")
form03b <- 
  read.table("form03b.txt", header=T, skip=0, sep="\t")
dxafull <- 
  read.table("dxafull.txt", header=T, skip=0, sep="\t")


#Cannot use sas7bdat package in r to convert these files because these files are
#'big endian' and the package only supports little endian files

#Next: Need to merge the data sets all into one

#Next: need to find way to random/semi random choose a certain girl/boy and get
        #their data fro 6 yrs long

#Next: calculate Z scores for all thos years (Will need appropriate LMS tables)

#Next: store z score data and plot it during those 6 years
      #Experiment with different methods to do that.


#'\\win\bbdg\aastudies\Bone Studies\zzClosedstudies\CTASC (BMCDS)\Source Data\Public\DatasetsSAS'

#The data sets you need to use are form1 (for the gender and race), 
#form3 (for weight, height, bmi, bmiZ) and dxafull. The RANDID variable
#is the link between these files.




