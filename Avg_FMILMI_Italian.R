
setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2

blackFmiLmi <- read.table("BlackFmiLmi.txt", header=T, sep="\t")
hispFmiLmi <- read.table("HispFmiLmi.txt",header=T, sep="\t")
whiteFmiLmi <- read.table("WhiteFmiLmi.txt",header=T, sep="\t")

keep = c("Race", "Gender", "RIDAGEYR", "totBodyFmi", "totBodyLmi")

blackFmiLmi2<- blackFmiLmi[keep]
hispFmiLmi2 <- hispFmiLmi[keep]
whiteFmiLmi2 <- whiteFmiLmi[keep]

fullFmiLmi <- rbind(blackFmiLmi2, hispFmiLmi2, whiteFmiLmi2)


data <- whiteFmiLmi2

d20AvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])

d30AvggFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])

d40AvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])


d50AvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])

d60AvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])

d70AvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70SdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70AvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70SdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])

dAllAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])











d20AvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])
d20SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 29 & data$RIDAGEYR >= 20])

d30AvggFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])
d30SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 39 & data$RIDAGEYR >= 30])

d40AvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])
d40SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 49 & data$RIDAGEYR >= 40])


d50AvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])
d50SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 59 & data$RIDAGEYR >= 50])

d60AvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])
d60SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 69 & data$RIDAGEYR >= 60])

d70AvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70SdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70AvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                    data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])
d70SdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                 data$RIDAGEYR <= 80 & data$RIDAGEYR >= 70])

dAllAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                  data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])
dAllSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                  data$RIDAGEYR <= 80 & data$RIDAGEYR >= 20])



