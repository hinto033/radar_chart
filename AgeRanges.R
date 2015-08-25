#Finding numbers by age


blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))



bMaleData <- blackData[blackData[, "Gender"] == "Male",]
bFemaleData <- blackData[blackData[, "Gender"] == "Female",]

wMaleData <- whiteData[whiteData[, "Gender"] == "Male",]
wFemaleData <- whiteData[whiteData[, "Gender"] == "Female",]

hMaleData <- hispData[hispData[, "Gender"] == "Male",]
hFemaleData <- hispData[hispData[, "Gender"] == "Female",]

Data <- hFemaleData

Male8to9 <- subset(Data, Data$Age <= 9 & Data$Age >= 8)
Male10to11 <- subset(Data, Data$Age <= 11 & Data$Age >= 10)
Male12to13 <- subset(Data, Data$Age <= 13 & Data$Age >= 12)
Male14to15 <- subset(Data, Data$Age <= 15 & Data$Age >= 14)
Male16to17 <- subset(Data, Data$Age <= 17 & Data$Age >= 16)
Male18to19 <- subset(Data, Data$Age <= 19 & Data$Age >= 18)
Male20to24 <- subset(Data, Data$Age <= 24 & Data$Age >= 20)
Male25to29 <- subset(Data, Data$Age <= 29 & Data$Age >= 25)
Male30to34 <- subset(Data, Data$Age <= 34 & Data$Age >= 30)
Male35to39 <- subset(Data, Data$Age <= 39 & Data$Age >= 35)
Male40to44 <- subset(Data, Data$Age <= 44 & Data$Age >= 40)
Male45to49 <- subset(Data, Data$Age <= 49 & Data$Age >= 45)
Male50to54 <- subset(Data, Data$Age <= 54 & Data$Age >= 50)
Male55to59 <- subset(Data, Data$Age <= 59 & Data$Age >= 55)
Male60to64 <- subset(Data, Data$Age <= 64 & Data$Age >= 60)
Male65to69 <- subset(Data, Data$Age <= 69 & Data$Age >= 65)
Male70to74 <- subset(Data, Data$Age <= 74 & Data$Age >= 70)
Male75to79 <- subset(Data, Data$Age <= 79 & Data$Age >= 75)
Male80to84 <- subset(Data, Data$Age <= 84 & Data$Age >= 80)
Male85plus <- subset(Data, Data$Age >= 85)

ageData1 <- subset(set1, set1$Age <= ageMax1 & set1$Age >= ageMin1) 
ageData2 <- subset(set2, set2$Age <= ageMax2 & set2$Age >= ageMin2)

