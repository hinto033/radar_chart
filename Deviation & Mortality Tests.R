
setwd("X:\\bhinton")


race = "Black"
BlackzData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
race = "Hisp"
HispzData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))
race = "White"
WhitezData <- read.table(file=sprintf("%s.ZScoreValues.txt",race, sep="\t"))


zData <- rbind(BlackzData, HispzData, WhitezData)




fmiSd <- zData[,c("Z_FMI_SD")]
lmiSd <- zData[,c("Z_LMI_SD")]
 

# paired t-test
t.test(fmiSd,lmiSd,paired=TRUE) # where y1 & y2 are numeric



t.test(fmiSd,lmiSd) # where y1 and y2 are numeric
#Mean of x (FMI_SD) = 0.289    -> Shows that Fmi is less variable across different compartments
#Mean of y (LMI_SD) = 0.411    -> Shows that Lmi is more variable across different compartments
#P_Value: 2.2 e-16

#95% confidence interval:  -0.1261388 -0.1180590


regionalFmi <- zData[,c("Z_FMI_TR", "Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA", "Z_FMI_AVG")]
regionalLmi <- zData[,c("Z_LMI_TR", "Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA", "Z_LMI_AVG")]

#Now to test correlations:
cor(regionalFmi, regionalFmi)


cor(regionalLmi, regionalLmi)

cor(regionalFmi, regionalLmi)





setwd("X:\\bhinton")


race = "Black"
BlackzData <- read.table(file=sprintf("%s.MortZScoreValues2.txt",race, sep="\t"))
race = "Hisp"
HispzData <- read.table(file=sprintf("%s.MortZScoreValues2.txt",race, sep="\t"))
race = "White"
WhitezData <- read.table(file=sprintf("%s.MortZScoreValues2.txt",race, sep="\t"))


zData <- rbind(BlackzData, HispzData, WhitezData)


LMISDS <- data.frame(apply(zData[,c("Z_LMI_LL","Z_LMI_RL", "Z_LMI_RA", "Z_LMI_LA", "Z_LMI_TR")], 1, sd ) )
FMISDS <- data.frame(apply(zData[,c("Z_FMI_LL","Z_FMI_RL", "Z_FMI_RA", "Z_FMI_LA", "Z_FMI_TR")], 1, sd ) )

#Add fmi and lmi to get BMI
BMI <- zData[,c("TotBodyFmi")] + zData[,c("TotBodyLmi")]



allData <- cbind(zData, LMISDS, FMISDS, BMI)


colnames(allData)[72] <- "ZLMI_SD"
colnames(allData)[73] <- "ZFMI_SD"


#Start analyzing the data


t.test(fmiSd,lmiSd,paired=TRUE) # where y1 & y2 are numeric
t.test(fmiSd,lmiSd) # where y1 and y2 are numeric

regionalFmi <- zData[,c("Z_FMI_TR", "Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA", "Z_FMI_AVG")]
regionalLmi <- zData[,c("Z_LMI_TR", "Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA", "Z_LMI_AVG")]

cor(regionalLmi, regionalLmi)
LmiRegions = c("Z_LMI_TR", "Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA", "Z_LMI_AVG")
FmiRegions = c("Z_FMI_TR", "Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA", "Z_FMI_AVG")


#Split into adult (21-85)/non adult (8-20)

allYouth <- subset(allData, allData$Age <= 20 & allData$Age >= 8)
allAdult <- subset(allData, allData$Age <= 85 & allData$Age >= 21)

#Split into obesity ranges






allYouthUnder <- subset(allYouth, allYouth$BMI <= 18.5)
allYouthHealthy <- subset(allYouth, allYouth$BMI < 25 & allYouth$BMI > 18.5)
allYouthOverweight <- subset(allYouth, allYouth$BMI < 30 & allYouth$BMI > 25)
allYouthObese1 <- subset(allYouth, allYouth$BMI < 35 & allYouth$BMI > 30)
allYouthObese2 <- subset(allYouth, allYouth$BMI < 40 & allYouth$BMI > 35)
allYouthObese3 <- subset(allYouth, allYouth$BMI > 40)
allYouthAllObese <- subset(allYouth, allYouth$BMI > 30)
allYouthAllHealthy <- subset(allYouth, allYouth$BMI < 30)

t.test(allYouthUnder[,c("ZFMI_SD")],allYouthUnder[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthUnder[,FmiRegions],allYouthUnder[,FmiRegions])
cor(allYouthUnder[,LmiRegions],allYouthUnder[,LmiRegions])
t.test(allYouthHealthy[,c("ZFMI_SD")],allYouthHealthy[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthHealthy[,FmiRegions],allYouthHealthy[,FmiRegions])
cor(allYouthHealthy[,LmiRegions],allYouthHealthy[,LmiRegions])
t.test(allYouthOverweight[,c("ZFMI_SD")],allYouthOverweight[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthOverweight[,FmiRegions],allYouthOverweight[,FmiRegions])
cor(allYouthOverweight[,LmiRegions],allYouthOverweight[,LmiRegions])
t.test(allYouthObese1[,c("ZFMI_SD")],allYouthObese1[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthObese1[,FmiRegions],allYouthObese1[,FmiRegions])
cor(allYouthObese1[,LmiRegions],allYouthObese1[,LmiRegions])
t.test(allYouthObese2[,c("ZFMI_SD")],allYouthObese2[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthObese2[,FmiRegions],allYouthObese2[,FmiRegions])
cor(allYouthObese2[,LmiRegions],allYouthObese2[,LmiRegions])
t.test(allYouthObese3[,c("ZFMI_SD")],allYouthObese3[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthObese3[,FmiRegions],allYouthObese3[,FmiRegions])
cor(allYouthObese3[,LmiRegions],allYouthObese3[,LmiRegions])
t.test(allYouthAllObese[,c("ZFMI_SD")],allYouthAllObese[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthAllObese[,FmiRegions],allYouthAllObese[,FmiRegions])
cor(allYouthAllObese[,LmiRegions],allYouthAllObese[,LmiRegions])
t.test(allYouthAllHealthy[,c("ZFMI_SD")],allYouthAllHealthy[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthAllHealthy[,FmiRegions],allYouthAllHealthy[,FmiRegions])
cor(allYouthAllHealthy[,LmiRegions],allYouthAllHealthy[,LmiRegions])

allAdultUnder <- subset(allAdult, allAdult$BMI <= 18.5)
allAdultHealthy <- subset(allAdult, allAdult$BMI < 25 & allAdult$BMI > 18.5)
allAdultOverweight <- subset(allAdult, allAdult$BMI < 30 & allAdult$BMI > 25)
allAdultObese1 <- subset(allAdult, allAdult$BMI < 35 & allAdult$BMI > 30)
allAdultObese2 <- subset(allAdult, allAdult$BMI < 40 & allAdult$BMI > 35)
allAdultObese3 <- subset(allAdult, allAdult$BMI > 40)
allAdultAllObese <- subset(allAdult, allAdult$BMI > 30)
allAdultAllHealthy <- subset(allAdult, allAdult$BMI < 30)


t.test(allAdultUnder[,c("ZFMI_SD")],allAdultUnder[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultUnder[,FmiRegions],allAdultUnder[,FmiRegions])
cor(allAdultUnder[,LmiRegions],allAdultUnder[,LmiRegions])
t.test(allAdultHealthy[,c("ZFMI_SD")],allAdultHealthy[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultHealthy[,FmiRegions],allAdultHealthy[,FmiRegions])
cor(allAdultHealthy[,LmiRegions],allAdultHealthy[,LmiRegions])
t.test(allAdultOverweight[,c("ZFMI_SD")],allAdultOverweight[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultOverweight[,FmiRegions],allAdultOverweight[,FmiRegions])
cor(allAdultOverweight[,LmiRegions],allAdultOverweight[,LmiRegions])
t.test(allAdultObese1[,c("ZFMI_SD")],allAdultObese1[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultObese1[,FmiRegions],allAdultObese1[,FmiRegions])
cor(allAdultObese1[,LmiRegions],allAdultObese1[,LmiRegions])
t.test(allAdultObese2[,c("ZFMI_SD")],allAdultObese2[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultObese2[,FmiRegions],allAdultObese2[,FmiRegions])
cor(allAdultObese2[,LmiRegions],allAdultObese2[,LmiRegions])
t.test(allAdultObese3[,c("ZFMI_SD")],allAdultObese3[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultObese3[,FmiRegions],allAdultObese3[,FmiRegions])
cor(allAdultObese3[,LmiRegions],allAdultObese3[,LmiRegions])
t.test(allAdultAllObese[,c("ZFMI_SD")],allAdultAllObese[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultAllObese[,FmiRegions],allAdultAllObese[,FmiRegions])
cor(allAdultAllObese[,LmiRegions],allAdultAllObese[,LmiRegions])
t.test(allAdultAllHealthy[,c("ZFMI_SD")],allAdultAllHealthy[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultAllHealthy[,FmiRegions],allAdultAllHealthy[,FmiRegions])
cor(allAdultAllHealthy[,LmiRegions],allAdultAllHealthy[,LmiRegions])





#Split into Ethnicity/Gender ranges - youth


allYouthMale <- subset(allYouth, allYouth$Gender == "Male")
allYouthFemale <- subset(allYouth, allYouth$Gender == "Female")

allYouthWhite <- subset(allYouth, allYouth$Race == "White")
allYouthBlack <- subset(allYouth, allYouth$Race == "Black")
allYouthHisp <- subset(allYouth, allYouth$Race == "American" | 
                         allYouth$Race == "Hispanic")


allYouthMaleWhite <- subset(allYouthMale, allYouthMale$Race == "White")
allYouthMaleBlack <- subset(allYouthMale, allYouthMale$Race == "Black")
allYouthMaleHisp <- subset(allYouthMale, allYouthMale$Race == "American" | 
                         allYouthMale$Race == "Hispanic")


allYouthFemaleWhite <- subset(allYouthFemale, allYouthFemale$Race == "White")
allYouthFemaleBlack <- subset(allYouthFemale, allYouthFemale$Race == "Black")
allYouthFemaleHisp <- subset(allYouthFemale, allYouthFemale$Race == "American" | 
                         allYouthFemale$Race == "Hispanic")



t.test(allYouthMaleWhite[,c("ZFMI_SD")],allYouthMaleWhite[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthMaleWhite[,FmiRegions],allYouthMaleWhite[,FmiRegions])
cor(allYouthMaleWhite[,LmiRegions],allYouthMaleWhite[,LmiRegions])
t.test(allYouthMaleBlack[,c("ZFMI_SD")],allYouthMaleBlack[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthMaleBlack[,FmiRegions],allYouthMaleBlack[,FmiRegions])
cor(allYouthMaleBlack[,LmiRegions],allYouthMaleBlack[,LmiRegions])
t.test(allYouthMaleHisp[,c("ZFMI_SD")],allYouthMaleHisp[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthMaleHisp[,FmiRegions],allYouthMaleHisp[,FmiRegions])
cor(allYouthMaleHisp[,LmiRegions],allYouthMaleHisp[,LmiRegions])

t.test(allYouthFemaleWhite[,c("ZFMI_SD")],allYouthFemaleWhite[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthFemaleWhite[,FmiRegions],allYouthFemaleWhite[,FmiRegions])
cor(allYouthFemaleWhite[,LmiRegions],allYouthFemaleWhite[,LmiRegions])
t.test(allYouthFemaleBlack[,c("ZFMI_SD")],allYouthFemaleBlack[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthFemaleBlack[,FmiRegions],allYouthFemaleBlack[,FmiRegions])
cor(allYouthFemaleBlack[,LmiRegions],allYouthFemaleBlack[,LmiRegions])
t.test(allYouthFemaleHisp[,c("ZFMI_SD")],allYouthFemaleHisp[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthFemaleHisp[,FmiRegions],allYouthFemaleHisp[,FmiRegions])
cor(allYouthFemaleHisp[,LmiRegions],allYouthFemaleHisp[,LmiRegions])


t.test(allYouthMale[,c("ZFMI_SD")],allYouthMale[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthMale[,FmiRegions],allYouthMale[,FmiRegions])
cor(allYouthMale[,LmiRegions],allYouthMale[,LmiRegions])
t.test(allYouthFemale[,c("ZFMI_SD")],allYouthFemale[,c("ZLMI_SD")],paired=TRUE)
cor(allYouthFemale[,FmiRegions],allYouthFemale[,FmiRegions])
cor(allYouthFemale[,LmiRegions],allYouthFemale[,LmiRegions])








#Split into Ethnicity/Gender ranges - adult


allAdultMale <- subset(allAdult, allAdult$Gender == "Male")
allAdultFemale <- subset(allAdult, allAdult$Gender == "Female")

allAdultWhite <- subset(allAdult, allAdult$Race == "White")
allAdultBlack <- subset(allAdult, allAdult$Race == "Black")
allAdultHisp <- subset(allAdult, allAdult$Race == "American" | 
                         allAdult$Race == "Hispanic")


allAdultMaleWhite <- subset(allAdultMale, allAdultMale$Race == "White")
allAdultMaleBlack <- subset(allAdultMale, allAdultMale$Race == "Black")
allAdultMaleHisp <- subset(allAdultMale, allAdultMale$Race == "American" | 
                             allAdultMale$Race == "Hispanic")


allAdultFemaleWhite <- subset(allAdultFemale, allAdultFemale$Race == "White")
allAdultFemaleBlack <- subset(allAdultFemale, allAdultFemale$Race == "Black")
allAdultFemaleHisp <- subset(allAdultFemale, allAdultFemale$Race == "American" | 
                               allAdultFemale$Race == "Hispanic")



t.test(allAdultMaleWhite[,c("ZFMI_SD")],allAdultMaleWhite[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultMaleWhite[,FmiRegions],allAdultMaleWhite[,FmiRegions])
cor(allAdultMaleWhite[,LmiRegions],allAdultMaleWhite[,LmiRegions])
t.test(allAdultMaleBlack[,c("ZFMI_SD")],allAdultMaleBlack[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultMaleBlack[,FmiRegions],allAdultMaleBlack[,FmiRegions])
cor(allAdultMaleBlack[,LmiRegions],allAdultMaleBlack[,LmiRegions])
t.test(allAdultMaleHisp[,c("ZFMI_SD")],allAdultMaleHisp[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultMaleHisp[,FmiRegions],allAdultMaleHisp[,FmiRegions])
cor(allAdultMaleHisp[,LmiRegions],allAdultMaleHisp[,LmiRegions])

t.test(allAdultFemaleWhite[,c("ZFMI_SD")],allAdultFemaleWhite[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultFemaleWhite[,FmiRegions],allAdultFemaleWhite[,FmiRegions])
cor(allAdultFemaleWhite[,LmiRegions],allAdultFemaleWhite[,LmiRegions])
t.test(allAdultFemaleBlack[,c("ZFMI_SD")],allAdultFemaleBlack[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultFemaleBlack[,FmiRegions],allAdultFemaleBlack[,FmiRegions])
cor(allAdultFemaleBlack[,LmiRegions],allAdultFemaleBlack[,LmiRegions])
t.test(allAdultFemaleHisp[,c("ZFMI_SD")],allAdultFemaleHisp[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultFemaleHisp[,FmiRegions],allAdultFemaleHisp[,FmiRegions])
cor(allAdultFemaleHisp[,LmiRegions],allAdultFemaleHisp[,LmiRegions])


t.test(allAdultMale[,c("ZFMI_SD")],allAdultMale[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultMale[,FmiRegions],allAdultMale[,FmiRegions])
cor(allAdultMale[,LmiRegions],allAdultMale[,LmiRegions])
t.test(allAdultFemale[,c("ZFMI_SD")],allAdultFemale[,c("ZLMI_SD")],paired=TRUE)
cor(allAdultFemale[,FmiRegions],allAdultFemale[,FmiRegions])
cor(allAdultFemale[,LmiRegions],allAdultFemale[,LmiRegions])












#Now to test Mortality data::

#Need logistf package

setwd("X:\\bhinton") 

#write.table(zScoreFinal, file=sprintf("%s.MortZScoreValues.txt",race))

race = "Black"
BlackzMortData <- read.table(file=sprintf("%s.MortZScoreValues.txt",race, sep="\t"))
race = "Hisp"
HispzMortData <- read.table(file=sprintf("%s.MortZScoreValues.txt",race, sep="\t"))
race = "White"
WhitezMortData <- read.table(file=sprintf("%s.MortZScoreValues.txt",race, sep="\t"))

MortData <- rbind(BlackzMortData,HispzMortData,WhitezMortData)



#colnames(zScore5) <- c("Race", "Gender", "Height", "Weight", "Age",
 #                      "TrunkFat", "TrunkLean", "TrunkPfat",
  #                     "LarmFat", "LarmLean", "LarmPfat",
   #                    "LlegFat", "LlegLean", "LlegPfat",
    #                   "RlegFat", "RlegLean", "RlegPfat",
     #                  "RarmFat", "RarmLean", "RarmPfat",
      #                 "TotFat", "TotLean", "TotPfat",
       #                "Mort", "GH_Ha1c", "TC_Cholest", #"20ftWalk",
        #               "AvgArmFat", "AvgLegFat", "AvgArmLean", "AvgLegLean",
         #              "AvgArmFmi", "AvgLegFmi", "TrunkFmi",
          #             "LArmFmi","LLegFmi","RLegFmi","RArmFmi", "TotBodyFmi",
           #            "AvgArmLmi", "AvgLegLmi", "TrunkLmi",
            #           "LArmLmi","LLegLmi","RLegLmi","RArmLmi", "TotBodyLmi",
             #          "Age1","Z_FMI_TR", "Z_LMI_TR",
              #         "ZarmFMI", "ZArmLMI", "ZLegFMI", "ZLegLMI",
               #        "Gender1", "Age2", "Z_TOT_FMI", "Z_TOT_LMI", "Gender3",
                #       "Z_FMI_LA", "Z_FMI_RA",
                 #      "Z_LMI_LA","Z_LMI_RA", "Z_FMI_LL", "Z_FMI_RL",
                  #     "Z_LMI_LL", "Z_LMI_RL","Z_FMI_AVG", "Z_LMI_AVG")






age <- MortData[,c("Age")]
mort <- MortData[,c("Mort")]
Ha1c <- MortData[,c("GH_Ha1c")]
cholesterol <- MortData[,c("TC_Cholest")]
gender <- MortData[,c("Gender")]
race <- MortData[,c("Race")]
weight <- MortData[,c("Weight")]
height <- MortData[,c("Height")]
fmi <- MortData[,c("Z_TOT_FMI")]
fmiTrunk <- MortData[,c("Z_FMI_TR")]
fmiLarm <- MortData[,c("Z_FMI_LA")]
fmiLleg <- MortData[,c("Z_FMI_LL")]
fmiRarm <- MortData[,c("Z_FMI_RA")]
fmiRleg <- MortData[,c("Z_FMI_RL")]
lmi <- MortData[,c("Z_TOT_LMI")]
fmiTrunk <- MortData[,c("Z_LMI_TR")]
fmiLarm <- MortData[,c("Z_LMI_LA")]
fmiLleg <- MortData[,c("Z_LMI_LL")]
fmiRarm <- MortData[,c("Z_LMI_RA")]
fmiRleg <- MortData[,c("Z_LMI_RL")]

lm(mort~fmi)
lm(mort~fmi + lmi)


lm(mort~fmiTrunk)
lm(mort~fmiTrunk + fmi)


lm(cholesterol~fmiTrunk)
lm(cholesterol~fmiTrunk + fmi)

covariate <- sample(0:1, 100, replace=TRUE)

exposure <- runif(100,0,1)+(0.3*covariate)

outcome <- 2.0+(0.5*exposure)+(0.25*covariate)

lm(outcome~exposure+covariate)


library("logistf")

proc logistic data = testfirth;
class outcome pred (param=ref ref='0');
model outcome(event='1') = pred / cl firth;
weight weight;
run;


>library(logistf)
>lr2 = logistf(outcome ~ pred)
>summary(lr2)

logistf(formula = outcome ~ pred)


proc logistic data=nhanes_shape_odds_total plots(only)=(roc); class gender race;
model diabetes(ref='0') = age gender race bmi;
units age=sd bmi=sd;
title "LR w/ Age, Gender, Race, BMI";
run; quit;

proc logistic data=nhanes_shape_odds_total plots(only)=(roc); class gender race;
model diabetes(ref='0') = age gender race bmi bmxwaist tlr;
units age=sd bmi=sd bmxwaist=sd tlr=sd;
title "LR w/ Age, Gender, Race, BMI, WC, TLR";
run; quit;


lr2 = logistf(formula = mort ~ age)
summary(lr2)

lr2 = logistf(formula = mort ~ fmiTrunk)
summary(lr2)

lr2 = logistf(formula = mort ~ fmiTrunk + fmi)
summary(lr2)

lr2 = logistf(formula = mort ~ fmiLarm + fmiRarm + fmi)
summary(lr2)

lr2 = logistf(formula = mort ~ fmiLleg + fmiRleg + fmi)
summary(lr2)

lr2 = logistf(formula = mort ~ ((fmiLleg+fmiRleg+fmiRarm+fmiLarm)/fmiTrunk) + fmi)
summary(lr2)




pred = c(1,0,0)
outcome = c(1,1,0)
weight=c(20,20,200)
lr1 = glm(outcome ~ pred, binomial, weights=weight)
lr2 = logistf(outcome ~ pred,  weights=weight)


