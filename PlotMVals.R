
#Inputs:
setwd("X:\\bhinton\\Data\\LMS Tables\\LMS Values") #Set this to wd with LMS charts
#Specifies which columns to keep from LMS tables
keep <- c("Age","L", "M", "S")


bfTrunkFmiLms <- 
  read.table("BlackFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
bfTrunkLmiLms <- 
  read.table("BlackFmiLmi_Female_TrunkLMI_010401t.txt", header=T, skip=10, sep="\t")
bmTrunkFmiLms <- 
  read.table("BlackFmiLmi_Male_TrunkFMI_020401t.txt", header=T, skip=10, sep="\t")
bmTrunkLmiLms <- 
  read.table("BlackFmiLmi_Male_TrunkLMI_010601t.txt", header=T, skip=10, sep="\t")
hfTrunkFmiLms <- 
  read.table("HispFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
hfTrunkLmiLms <- 
  read.table("HispFmiLmi_Female_TrunkLMI_020401t.txt", header=T, skip=10, sep="\t")
hmTrunkFmiLms <- 
  read.table("HispFmiLmi_Male_TrunkFMI_020502t.txt", header=T, skip=10, sep="\t")
hmTrunkLmiLms <- 
  read.table("HispFmiLmi_Male_TrunkLMI_010702t.txt", header=T, skip=10, sep="\t")
wfTrunkFmiLms <- 
  read.table("WhiteFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
wfTrunkLmiLms <- 
  read.table("WhiteFmiLmi_Female_TrunkLMI_010401t.txt", header=T, skip=10, sep="\t")
wmTrunkFmiLms <- 
  read.table("WhiteFmiLmi_Male_TrunkFMI_020502t.txt", header=T, skip=10, sep="\t")
wmTrunkLmiLms <- 
  read.table("WhiteFmiLmi_Male_TrunkLMI_020702t.txt", header=T, skip=10, sep="\t")

keep= c("Age", "M")
trunkFmiM = cbind(bfTrunkFmiLms[keep], hfTrunkFmiLms["M"],wfTrunkFmiLms["M"], 
                bmTrunkFmiLms["M"], hmTrunkFmiLms["M"],wmTrunkFmiLms["M"])
trunkLmiM = cbind(bfTrunkLmiLms[keep], hfTrunkLmiLms["M"],wfTrunkLmiLms["M"], 
                bmTrunkLmiLms["M"], hmTrunkLmiLms["M"],wmTrunkLmiLms["M"])

plot(trunkFmiM$Age, trunkFmiM[,2], ylim=c(0,10), col="darkgoldenrod1")
points(trunkFmiM$Age, trunkFmiM[,3], col="dodgerblue1")
points(trunkFmiM$Age, trunkFmiM[,4], col="mediumorchid1")
points(trunkFmiM$Age, trunkFmiM[,5], col="darkgoldenrod4")
points(trunkFmiM$Age, trunkFmiM[,6], col="dodgerblue4")
points(trunkFmiM$Age, trunkFmiM[,7], col="mediumorchid4")

plot(trunkLmiM$Age, trunkLmiM[,2], ylim=c(0,10), col="darkgoldenrod1")
points(trunkLmiM$Age, trunkLmiM[,3], col="dodgerblue1")
points(trunkLmiM$Age, trunkLmiM[,4], col="mediumorchid1")
points(trunkLmiM$Age, trunkLmiM[,5], col="darkgoldenrod4")
points(trunkLmiM$Age, trunkLmiM[,6], col="dodgerblue4")
points(trunkLmiM$Age, trunkLmiM[,7], col="mediumorchid4")


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


keep= c("Age", "M")
armFmiM = cbind(bfArmFmiLms[keep], hfArmFmiLms["M"],wfArmFmiLms["M"], 
             bmArmFmiLms["M"], hmArmFmiLms["M"],wmArmFmiLms["M"])
armLmiM = cbind(bfArmLmiLms[keep], hfArmLmiLms["M"],wfArmLmiLms["M"], 
                bmArmLmiLms["M"], hmArmLmiLms["M"],wmArmLmiLms["M"])
legFmiM = cbind(bfLegFmiLms[keep], hfLegFmiLms["M"],wfLegFmiLms["M"], 
                bmLegFmiLms["M"], hmLegFmiLms["M"],wmLegFmiLms["M"])
legLmiM = cbind(bfLegLmiLms[keep], hfLegLmiLms["M"],wfLegLmiLms["M"], 
                bmLegLmiLms["M"], hmLegLmiLms["M"],wmLegLmiLms["M"])

plot(legLmiM$Age, legLmiM[,2], ylim=c(0,4), col="darkgoldenrod1")
points(legLmiM$Age, legLmiM[,3], col="dodgerblue1")
points(legLmiM$Age, legLmiM[,4], col="mediumorchid1")
points(legLmiM$Age, legLmiM[,5], col="darkgoldenrod4")
points(legLmiM$Age, legLmiM[,6], col="dodgerblue4")
points(legLmiM$Age, legLmiM[,7], col="mediumorchid4")

plot(legFmiM$Age, legFmiM[,2], ylim=c(0,4), col="darkgoldenrod1")
points(legFmiM$Age, legFmiM[,3], col="dodgerblue1")
points(legFmiM$Age, legFmiM[,4], col="mediumorchid1")
points(legFmiM$Age, legFmiM[,5], col="darkgoldenrod4")
points(legFmiM$Age, legFmiM[,6], col="dodgerblue4")
points(legFmiM$Age, legFmiM[,7], col="mediumorchid4")

plot(armLmiM$Age, armLmiM[,2], ylim=c(0,4), col="darkgoldenrod1")
points(armLmiM$Age, armLmiM[,3], col="dodgerblue1")
points(armLmiM$Age, armLmiM[,4], col="mediumorchid1")
points(armLmiM$Age, armLmiM[,5], col="darkgoldenrod4")
points(armLmiM$Age, armLmiM[,6], col="dodgerblue4")
points(armLmiM$Age, armLmiM[,7], col="mediumorchid4")

plot(armFmiM$Age, armFmiM[,2], ylim=c(0,4), col="darkgoldenrod1")
points(armFmiM$Age, armFmiM[,3], col="dodgerblue1")
points(armFmiM$Age, armFmiM[,4], col="mediumorchid1")
points(armFmiM$Age, armFmiM[,5], col="darkgoldenrod4")
points(armFmiM$Age, armFmiM[,6], col="dodgerblue4")
points(armFmiM$Age, armFmiM[,7], col="mediumorchid4")


keep= c("Age", "L")
trunkFmiL = cbind(bfTrunkFmiLms[keep], hfTrunkFmiLms["L"],wfTrunkFmiLms["L"], 
                  bmTrunkFmiLms["L"], hmTrunkFmiLms["L"],wmTrunkFmiLms["L"])
trunkLmiL = cbind(bfTrunkLmiLms[keep], hfTrunkLmiLms["L"],wfTrunkLmiLms["L"], 
                  bmTrunkLmiLms["L"], hmTrunkLmiLms["L"],wmTrunkLmiLms["L"])
armFmiL = cbind(bfArmFmiLms[keep], hfArmFmiLms["L"],wfArmFmiLms["L"], 
                bmArmFmiLms["L"], hmArmFmiLms["L"],wmArmFmiLms["L"])
armLmiL = cbind(bfArmLmiLms[keep], hfArmLmiLms["L"],wfArmLmiLms["L"], 
                bmArmLmiLms["L"], hmArmLmiLms["L"],wmArmLmiLms["L"])
legFmiL = cbind(bfLegFmiLms[keep], hfLegFmiLms["L"],wfLegFmiLms["L"], 
                bmLegFmiLms["L"], hmLegFmiLms["L"],wmLegFmiLms["L"])
legLmiL = cbind(bfLegLmiLms[keep], hfLegLmiLms["L"],wfLegLmiLms["L"], 
                bmLegLmiLms["L"], hmLegLmiLms["L"],wmLegLmiLms["L"])


keep= c("Age", "S")
trunkFmiS = cbind(bfTrunkFmiLms[keep], hfTrunkFmiLms["S"],wfTrunkFmiLms["S"], 
                  bmTrunkFmiLms["S"], hmTrunkFmiLms["S"],wmTrunkFmiLms["S"])
trunkLmiS = cbind(bfTrunkLmiLms[keep], hfTrunkLmiLms["S"],wfTrunkLmiLms["S"], 
                  bmTrunkLmiLms["S"], hmTrunkLmiLms["S"],wmTrunkLmiLms["S"])
armFmiS = cbind(bfArmFmiLms[keep], hfArmFmiLms["S"],wfArmFmiLms["S"], 
                bmArmFmiLms["S"], hmArmFmiLms["S"],wmArmFmiLms["S"])
armLmiS = cbind(bfArmLmiLms[keep], hfArmLmiLms["S"],wfArmLmiLms["S"], 
                bmArmLmiLms["S"], hmArmLmiLms["S"],wmArmLmiLms["S"])
legFmiS = cbind(bfLegFmiLms[keep], hfLegFmiLms["S"],wfLegFmiLms["S"], 
                bmLegFmiLms["S"], hmLegFmiLms["S"],wmLegFmiLms["S"])
legLmiS = cbind(bfLegLmiLms[keep], hfLegLmiLms["S"],wfLegLmiLms["S"], 
                bmLegLmiLms["S"], hmLegLmiLms["S"],wmLegLmiLms["S"])


TrunkFmiHighPercentile = (((1.8808*trunkFmiL*trunkFmiS) +1)^(1/trunkFmiL))*trunkFmiM
TrunkLmiHighPercentile = (((1.8808*trunkLmiL*trunkLmiS) +1)^(1/trunkLmiL))*trunkLmiM
ArmFmiHighPercentile = (((1.8808*armFmiL*armFmiS) +1)^(1/armFmiL))*armFmiM
ArmLmiHighPercentile = (((1.8808*armLmiL*armLmiS) +1)^(1/armLmiL))*armLmiM
LegFmiHighPercentile = (((1.8808*legFmiL*legFmiS) +1)^(1/legFmiL))*legFmiM
LegLmiHighPercentile = (((1.8808*legLmiL*legLmiS) +1)^(1/legLmiL))*legLmiM

TrunkFmiZ = (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi)
  
  
  transform(zScore2, 
                     zLArmFmi= (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                     zRArmFmi= (((rightArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                     zLArmLmi= (((leftArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                     zRArmLmi= (((rightArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                     zLLegFmi= (((leftLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                     zRLegFmi= (((rightLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                     zLLegLmi= (((leftLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi),
                     zRLegLmi= (((rightLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi))
