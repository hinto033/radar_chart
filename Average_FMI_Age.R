
###GOOOD
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


setwd("X:\\bhinton") #Set this to wd with (Black/White/Hisp)ZScoreValues.txt from part 2

blackFmiLmi <- read.table("BlackFmiLmi.txt", header=T, sep="\t")
hispFmiLmi <- read.table("HispFmiLmi.txt",header=T, sep="\t")
whiteFmiLmi <- read.table("WhiteFmiLmi.txt",header=T, sep="\t")

keep = c("Race", "Gender", "RIDAGEYR", "totBodyFmi", "totBodyLmi")

blackFmiLmi2<- blackFmiLmi[keep]
hispFmiLmi2 <- hispFmiLmi[keep]
whiteFmiLmi2 <- whiteFmiLmi[keep]

fullFmiLmi <- rbind(blackFmiLmi2, hispFmiLmi2, whiteFmiLmi2)


yMin <- 18
yMax <- 34
mMin <- 35
mMax <- 54
oMin <- 55
oMax <- 74


#Separate by Gender!

data <- blackFmiLmi2

bmYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                           data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bmYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bmYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                           data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bmYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

bmMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                         data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bmMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bmMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                         data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bmMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

bmOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                         data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bmOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bmOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                         data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bmOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])


bfYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bfYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bfYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
bfYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

bfMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bfMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bfMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
bfMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

bfOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bfOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bfOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
bfOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])


bfAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" ])
bfSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" ])
bfAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female"])
bfSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" ])

bmAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" ])
bmSdFmi <- sd(data$totBodyFmi[data$Gender == "Male"])
bmAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male"])
bmSdLmi <- sd(data$totBodyLmi[data$Gender == "Male"])



ranges <- c("18-34", "35-54", "55-74", "All Ages")

bmvaluesFmi <- c(bmYoungAvgFmi, bmMidAvgFmi, bmOldAvgFmi, bmAvgFmi)
bmSdFmi <- c(bmYoungSdFmi, bmMidSdFmi, bmOldSdFmi, bmSdFmi)
bmvaluesLmi <- c(bmYoungAvgLmi, bmMidAvgLmi, bmOldAvgLmi,bmAvgLmi)
bmSdLmi <- c(bmYoungSdLmi, bmMidSdLmi, bmOldSdLmi, bmSdLmi)

bfvaluesFmi <- c(bfYoungAvgFmi, bfMidAvgFmi, bfOldAvgFmi, bfAvgFmi)
bfSdFmi <- c(bfYoungSdFmi, bfMidSdFmi, bfOldSdFmi, bfSdFmi)
bfvaluesLmi <- c(bfYoungAvgLmi, bfMidAvgLmi, bfOldAvgLmi, bfAvgLmi)
bfSdLmi <- c(bfYoungSdLmi, bfMidSdLmi, bfOldSdLmi, bfSdLmi)


op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 1))

y.means <- bmvaluesFmi
y.sd <- bmSdFmi
y1.means <- bfvaluesFmi
y1.sd <- bfSdFmi

yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="Black FMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 15))
text(barx, yy, labels=round(yy, digits=2), pos=3)
error.bar(barx,yy,ee)

legend('topleft', c("Male", "Female") ,col=c("blue","magenta"),
       lty=1, bty='n', cex=.75)  

### GOOOOD
y.means <- bmvaluesLmi
y.sd <- bmSdLmi
y1.means <- bfvaluesLmi
y1.sd <- bfSdLmi


yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="Black LMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 30))
error.bar(barx,yy,ee)
text(barx, yy, round(yy, digits=2), pos=3)






###Hispanics


data <- hispFmiLmi2

hmYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hmYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hmYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hmYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

hmMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hmMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hmMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hmMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

hmOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hmOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hmOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hmOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])


hfYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hfYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hfYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
hfYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

hfMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hfMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hfMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
hfMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

hfOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hfOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hfOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
hfOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])


hfAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" ])
hfSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" ])
hfAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female"])
hfSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" ])

hmAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" ])
hmSdFmi <- sd(data$totBodyFmi[data$Gender == "Male"])
hmAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male"])
hmSdLmi <- sd(data$totBodyLmi[data$Gender == "Male"])



ranges <- c("young", "mid", "old", "All Ages")

hmvaluesFmi <- c(hmYoungAvgFmi, hmMidAvgFmi, hmOldAvgFmi, hmAvgFmi)
hmSdFmi <- c(hmYoungSdFmi, hmMidSdFmi, hmOldSdFmi, hmSdFmi)
hmvaluesLmi <- c(hmYoungAvgLmi, hmMidAvgLmi, hmOldAvgLmi, hmAvgLmi)
hmSdLmi <- c(hmYoungSdLmi, hmMidSdLmi, hmOldSdLmi, hmSdLmi)

hfvaluesFmi <- c(hfYoungAvgFmi, hfMidAvgFmi, hfOldAvgFmi, hfAvgFmi)
hfSdFmi <- c(hfYoungSdFmi, hfMidSdFmi, hfOldSdFmi, hfSdFmi)
hfvaluesLmi <- c(hfYoungAvgLmi, hfMidAvgLmi, hfOldAvgLmi, hfAvgLmi)
hfSdLmi <- c(hfYoungSdLmi, hfMidSdLmi, hfOldSdLmi, hfSdLmi)


op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 1))

y.means <- hmvaluesFmi
y.sd <- hmSdFmi
y1.means <- hfvaluesFmi
y1.sd <- hfSdFmi

yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="Hispanic FMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 15))
text(barx, yy, labels=round(yy, digits=2), pos=3)
error.bar(barx,yy,ee)

legend('topleft', c("Male", "Female") ,col=c("blue","magenta"),
       lty=1, bty='n', cex=.75)  

### GOOOOD
y.means <- hmvaluesLmi
y.sd <- hmSdLmi
y1.means <- hfvaluesLmi
y1.sd <- hfSdLmi


yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="Hispanic LMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 30))
error.bar(barx,yy,ee)
text(barx, yy, round(yy, digits=2), pos=3)















###Whites


data <- whiteFmiLmi2

wmYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wmYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wmYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wmYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

wmMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wmMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wmMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wmMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

wmOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wmOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wmOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wmOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Male" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])


wfYoungAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wfYoungSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wfYoungAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                        data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])
wfYoungSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                     data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])

wfMidAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wfMidSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wfMidAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])
wfMidSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= mMax & data$RIDAGEYR >= mMin])

wfOldAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wfOldSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wfOldAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female" & 
                                      data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])
wfOldSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" & 
                                   data$RIDAGEYR <= oMax & data$RIDAGEYR >= oMin])



wfAvgFmi <- mean(data$totBodyFmi[data$Gender == "Female" ])
wfSdFmi <- sd(data$totBodyFmi[data$Gender == "Female" ])
wfAvgLmi <- mean(data$totBodyLmi[data$Gender == "Female"])
wfSdLmi <- sd(data$totBodyLmi[data$Gender == "Female" ])

wmAvgFmi <- mean(data$totBodyFmi[data$Gender == "Male" ])
wmSdFmi <- sd(data$totBodyFmi[data$Gender == "Male"])
wmAvgLmi <- mean(data$totBodyLmi[data$Gender == "Male"])
wmSdLmi <- sd(data$totBodyLmi[data$Gender == "Male"])




ranges <- c("young", "mid", "old", "All Ages")

wmvaluesFmi <- c(wmYoungAvgFmi, wmMidAvgFmi, wmOldAvgFmi, wmAvgFmi)
wmSdFmi <- c(wmYoungSdFmi, wmMidSdFmi, wmOldSdFmi, wmSdFmi)
wmvaluesLmi <- c(wmYoungAvgLmi, wmMidAvgLmi, wmOldAvgLmi, wmAvgLmi)
wmSdLmi <- c(wmYoungSdLmi, wmMidSdLmi, wmOldSdLmi, wmSdLmi)

wfvaluesFmi <- c(wfYoungAvgFmi, wfMidAvgFmi, wfOldAvgFmi, wfAvgFmi)
wfSdFmi <- c(wfYoungSdFmi, wfMidSdFmi, wfOldSdFmi, wfSdFmi)
wfvaluesLmi <- c(wfYoungAvgLmi, wfMidAvgLmi, wfOldAvgLmi, wfAvgLmi)
wfSdLmi <- c(wfYoungSdLmi, wfMidSdLmi, wfOldSdLmi, wfSdLmi)


op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 1))

y.means <- wmvaluesFmi
y.sd <- wmSdFmi
y1.means <- wfvaluesFmi
y1.sd <- wfSdFmi

yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="White FMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 15))
text(barx, yy, labels=round(yy, digits=2), pos=3)
error.bar(barx,yy,ee)

legend('topleft', c("Male", "Female") ,col=c("blue","magenta"),
       lty=1, bty='n', cex=.75)

### GOOOOD
y.means <- wmvaluesLmi
y.sd <- wmSdLmi
y1.means <- wfvaluesLmi
y1.sd <- wfSdLmi

yy <- matrix(c(y.means,y1.means),2,4,byrow=TRUE)
ee <- matrix(c(y.sd,y1.sd),2,4,byrow=TRUE)*1.96/10
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), main="White LMI Values",
                names.arg=ranges, axis.lty=1, xlab="Replicates",
                ylab="Value (arbitrary units)", ylim=c(0, 30))
text(barx, yy, round(yy, digits=2), pos=3)
error.bar(barx,yy,ee)









vals <- 1:5
names(vals) <- LETTERS[1:5]
mp <- barplot(vals, ylim = c(0, 6))
text(mp, vals, labels = vals, pos = 3)









aa2 <- sd(data$totBodyFmi[data$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])







aa1 <- mean(dataYoung$totBodyFmi[dataYoung$RIDAGEYR <= yMax & data$RIDAGEYR >= yMin])


yFmiAvg <- colMeans(as.numeric(dataYoung[totBodyFmi]))












op <- par(mar=c(1, 2, 2, 1),mfrow=c(1, 1))
barplot(rbind(bmvaluesFmi, bfvaluesFmi), main="Avg FMI by Age", ylab="LMI Value",
        xlab="Age Ranges", col=c("darkblue","red"), names.arg=ranges,
        beside=TRUE)
legend('topleft', c("Male", "Female") ,col=c("darkblue","red"),
       lty=1, bty='n', cex=.5)  
barplot(rbind(bmvaluesLmi, bfvaluesLmi), main="Avg LMI by Age", ylab="LMI Value",
        xlab="Age Ranges", col=c("darkblue","red"), names.arg=ranges,
        beside=TRUE)

legend('topleft', c("Male", "Female") ,col=c("darkblue","red"),
       lty=1, bty='n', cex=.5)  

# Grouped Bar Plot

barplot(rbind(valuesFmi, valuesLmi), main="MY NEW BARPLOT", ylab="FMI/LMI Value",
        xlab="Age Ranges", col=c("darkblue","red"), names.arg=ranges,
        legend = c("FMI","LMI"), beside=TRUE)


#rm(list = ls(all = TRUE))  #Clears all variables in the environemnt