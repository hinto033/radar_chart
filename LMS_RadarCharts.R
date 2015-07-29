#This program will take a certain amount of 
#individuals that you select and create radar charts for them
#It requires a certain number of inputs at the beginning to function properly

#Required Packages: fmsb (Contains radar function)
#must load and install the package in order for the code to run




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

maxmin1 <- data.frame(
  Z_Tr=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

fmiDatFinal <- rbind(maxmin,fmiDat)
ffmiDatFinal <- rbind(maxmin1,ffmiDat)

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

maxmin1 <- data.frame(
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











