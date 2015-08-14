
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



#####


setwd("C:\\Users\\bhinton\\Documents\\radar_chart") 
library(shiny)
runApp("Test")


system.file("examples", package="shiny")

runExample("01_hello") # a histogram
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer


