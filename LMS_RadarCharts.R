#This program will take a certain amount of 
#individuals that you select and create radar charts for them
#It requires a certain number of inputs at the beginning to function properly

#Required Packages: fmsb (Contains radar function)
#                   sas7bdat (Allows to import sas files)
#must load and install the packages in order for the code to run




#NEED TO DO:
#Include age of individuals in title and adjust for different race/gender
#Work on including the ability for the second program to 
#adjust for number of users (Right now only works with 4)
#Merge Maxmins from 2 dataframes


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

write.csv(nhanesBlackFmiLmi, file = "BlackFmiLmi.csv")
write.csv(nhanesWhiteFmiLmi, file = "WhiteFmiLmi.csv")
write.csv(nhanesHispFmiLmi, file = "HispFmiLmi.csv")




#write.csv(test, file = "dxa_bmx.csv")
#If I want to write the whole database

#####
#                                        #
#                                        #
#Part B: Importing Individual Z scores (And maybe calculating values?)
#                                        #
#                                        #


#Need to think through how this will work with each arm and each leg.
#Will probably need to calculate z scores from the LMS values provided for the average.
#... trunk should be fairly straightforward though.

setwd("X:\\bhinton\\Data\\LMS Tables\\Zind") # Set the working directory
bfLegFMI <- read.table("BlackFmiLmi_Female_AvgLegFMI_Zind_020302t.txt", header=T, sep="\t")
bmLegFMI <- read.table("BlackFmiLmi_Male_AvgLegFMI_Zind_020202t.txt", header=T, sep="\t")

dbfLegFMI <- data.frame(bfLegFMI)
dbfLegFMI <- transform(dbfLegFMI, zTot = (DXXLAFAT + DXXRAFAT) / 2, 
                           avgLegFat = (DXXLLFAT + DXXRLFAT) / 2,
                           avgArmLI = (DXXLALI + DXXRALI) / 2,
                           avgLegLI = (DXXLLLI + DXXRLLI) / 2
)

setwd("X:\bhinton\Data\LMS Tables\LMS Values")
#Various Files to work with.....
BlackFmiLmi_Female_AvgArmFMI_Zind_020202t.txt
BlackFmiLmi_Female_AvgArmLMI_Zind_010401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Female_AvgLegFMI_Zind_020302t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Female_AvgLegLMI_Zind_010401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Female_TrunkFMI_Zind_020402t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_TrunkLMI_Zind_020702t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Female_TrunkLMI_Zind_010401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_AvgArmFMI_Zind_020202t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_AvgArmLMI_020601t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_AvgLegFMI_Zind_020202t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_AvgLegLMI_Zind_010501t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_TrunkFMI_Zind_020401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/BlackFmiLmi_Male_TrunkLMI_Zind_010601t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_AvgArmFMI_Zind_020302t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_AvgArmLMI_Zind_020401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_AvgLegFMI_Zind_020301t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_AvgLegLMI_Zind_020401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_TrunkFMI_Zind_020402t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Female_TrunkLMI_Zind_020401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_AvgArmFMI_Zind_010403t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_AvgArmLMI_Zind_010702t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_AvglegFMI_Zind_010102t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_AvgLegLMI_Zind_010602t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_TrunkFMI_Zind_020502t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/HispFmiLmi_Male_TrunkLMI_Zind_010702t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_AvgArmFMI_Zind_020202t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_AvgArmLMI_Zind_010401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_AvgLegFMI_Zind_020301t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_AvgLegLMI_Zind_010601t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_TrunkFMI_Zind_020402t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Female_TrunkLMI_Zind_010401t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_AvgArmFMI_Zind_020402t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_AvgArmLMI_Zind_010801t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_AvgLegFMI_Zind_010202t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_AvgLegLMI_Zind_020702t.txt
file:///X:/bhinton/Data/LMS Tables/Zind/WhiteFmiLmi_Male_TrunkFMI_Zind_020502t.txt



file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_TrunkLMI_020702t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_AvgArmFMI_020202t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_AvgArmLMI_010401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_AvgLegFMI_020302t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_AvgLegLMI_010401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_TrunkFMI_020402t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Female_TrunkLMI_010401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_AvgArmFMI_020202t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_AvgArmLMI_020601t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_AvgLegFMI_020202t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_AvgLegLMI_010501t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_TrunkFMI_020401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/BlackFmiLmi_Male_TrunkLMI_010601t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_AveLegLMI_020401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_AvgArmFMI_020302t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_AvgArmLMI_020401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_AvgLegFMI_020301t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_TrunkFMI_020402t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Female_TrunkLMI_020401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male__AvgLegFMI_010102t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_AvgArmFMI_010403t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_AvgArmLMI_010702t.ept
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_AvgArmLMI_010702t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_AvgLegLMI_010602t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_TrunkFMI_020502t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/HispFmiLmi_Male_TrunkLMI_010702t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_AvgArmFMI_020202t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_AvgArmLMI_010401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_AvgLegFMI_020301t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_AvgLegLMI_010601t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_TrunkFMI_020402t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Female_TrunkLMI_010401t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_AvgArmFMI_020402t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_AvgArmLMI_010801t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_AvgLagLMI_020702t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_AvgLegFMI_010202t.txt
file:///X:/bhinton/Data/LMS Tables/LMS Values/WhiteFmiLmi_Male_TrunkFMI_020502t.txt









#####
#                                        #
#                                        #
#Part A: Producing separate FMI/LMI Charts
#                                        #
#                                        #
#Parameter input:
setwd("X:\\bhinton\\Ind Z Scores") # Set the working directory
#Must use forward slash or double blackslash. a single backslash will not work in R
Zind = read.csv("BF_Comb_Zind.csv")  # read csv file  BF = Black Female
#Other options are: BF_Comb_Zind.csv  BM_Comb_Zind.csv   (Black male/female)
#                   WF_Comb_Zind.csv  WM_Comb_Zind.csv  (White male/female)
#                   HF_Comb_Zind.csv  HF_Comb_Zind.csv  (Hispanic/Mexican Male/female)


selectAge = 15
selectNumber = 4
#Currently code will only select the first selectNumber rows in the file

ZindAges <- Zind[Zind[, "Age"] == selectAge,] 
#Selects patients of certain age

dfZindAges <- data.frame(ZindAges)  #Converts to dataframe
dfZindAgesFmi <- dfZindAges[1:selectNumber,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
dfZindAgesFfmi <- dfZindAges[1:selectNumber,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]
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
 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title

radarchart(ffmiDatFinal, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FFMI Charts")
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
setwd("X:\\bhinton\\Ind Z Scores") # Set the working directory
#Must use forward slash or double blackslash. a single backslash will not work in R
Zind = read.csv("BF_Comb_Zind.csv")  # read csv file  BF = Black Female
#Other options are: BF_Comb_Zind.csv  BM_Comb_Zind.csv   (Black male/female)
#                   WF_Comb_Zind.csv  WM_Comb_Zind.csv  (White male/female)
#                   HF_Comb_Zind.csv  HF_Comb_Zind.csv  (Hispanic/Mexican Male/female)


selectAge = 15
selectNumber = 4 #Don't change this currently, it won't work with ~=4
#Currently code will only select the first selectNumber rows in the file

ZindAges <- Zind[Zind[, "Age"] == selectAge,] 
#Selects patients of certain age

dfZindAges <- data.frame(ZindAges)  #Converts to dataframe
dfZindAgesFmi <- dfZindAges[1:selectNumber,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
dfZindAgesFfmi <- dfZindAges[1:selectNumber,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]
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
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title

radarchart(ind2Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title
radarchart(ind3Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title
radarchart(ind4Dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title


legend('topright', c("FMI", "FFMI") , lty=1, col=c("Black", "Red"), bty='n', cex=1)    
        # gives the legend appropriate symbols (lines)
       
      # gives the legend lines the correct color and width


#Color scheme.... black first, then red?
#Black is FMI
#Red is FFMI











