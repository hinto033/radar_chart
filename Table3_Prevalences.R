

blackData <- read.table(file=sprintf("Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("White.ZScoreValues.txt", sep="\t"))

setwd("X:\\bhinton")
blackData <- read.table(file = "BlackFmiLmi.txt", header=T, sep="\t")
whiteData <- read.table(file = "WhiteFmiLmi.txt", header=T, sep="\t")
hispData <- read.table(file = "HispFmiLmi.txt", header=T, sep="\t")


bMaleData <- blackData[blackData[, "Gender"] == "Male",]
bFemaleData <- blackData[blackData[, "Gender"] == "Female",]

wMaleData <- whiteData[whiteData[, "Gender"] == "Male",]
wFemaleData <- whiteData[whiteData[, "Gender"] == "Female",]

hMaleData <- hispData[hispData[, "Gender"] == "Male",]
hFemaleData <- hispData[hispData[, "Gender"] == "Female",]

Data <- hFemaleData

total1 <- transform(Data,
                Bmi = totBodyFmi + totBodyLmi )

total <- subset(total1, total1$RIDAGEYR <= 25 & total1$RIDAGEYR >= 25)
total <- subset(total1, total1$RIDAGEYR <= 29 & total1$RIDAGEYR >= 20)


ntotal <- nrow(total)
nunder16 <- nrow(subset(total, total$Bmi <= 16))
nunder17 <- nrow(subset(total, total$Bmi <= 17))
nunder185 <- nrow(subset(total, total$Bmi <= 18.5))
nover25 <- nrow(subset(total, total$Bmi >= 25))
nover30 <- nrow(subset(total, total$Bmi >= 30))
nover35 <- nrow(subset(total, total$Bmi >= 35))
nover40 <- nrow(subset(total, total$Bmi >= 40))


punder16 <- nunder16/ntotal
punder17 <- nunder17/ntotal
punder185 <- nunder185/ntotal
pover25 <- nover25/ntotal
pover30 <- nover30/ntotal
pover35 <- nover35/ntotal
pover40 <- nover40/ntotal


#TotalFmi


test <- nrow(subset(total, total$totBodyFmi >= 18.8))



#Trunk

test <- nrow(subset(total, total$trunkFmi <= 1.8))

test <- nrow(subset(total, total$trunkFmi >= 9.5))

#Arm

test <- nrow(subset(total, total$avgArmFmi <= .25))

test <- nrow(subset(total, total$avgArmFmi >= 1.20))

#Leg

test <- nrow(subset(total, total$avgLegFmi <= .99))

test <- nrow(subset(total, total$avgLegFmi >= 3.35))


