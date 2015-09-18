
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


