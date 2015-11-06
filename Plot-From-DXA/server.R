# server.R

###Need to:
##Convert to LMI and FMI
#finish the final calculations
#Produce the radar charts.



library(fmsb)
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

chartDim <- c(1,1)

setwd('X:\\bhinton\\radar_chart\\Plot-From-DXA')

blackData <- read.table(file=sprintf("data/Black.ZScoreValues.txt", sep="\t"))
hispData <- read.table(file=sprintf("data/Hisp.ZScoreValues.txt", sep="\t"))
whiteData <- read.table(file=sprintf("data/White.ZScoreValues.txt", sep="\t"))
fullData <- rbind(blackData, hispData, whiteData)

#Import the Fit 3D Group
fit3dBase <- read.table(file="data/DXA.Fit3d.Export.txt", sep="\t", header = TRUE)
dfit3dBase <- data.frame(transform(fit3dBase,
                                   ageYr= age,
                                   Gender= SEX,
                                   Race= ethnicity,
                                   avgArmFat = (LARM_FAT + RARM_FAT) / 2, 
                                   avgLegFat = (L_LEG_FAT + R_LEG_FAT) / 2,
                                   avgArmLI = (LARM_LEAN + RARM_LEAN) / 2,
                                   avgLegLI = (L_LEG_LEAN + R_LEG_LEAN) / 2,
                                   BMI = (WBTOT_MASS/1000) / ((height_cm/100)^2),
                                   FMI = (WBTOT_FAT/1000) / ((height_cm/100)^2),
                                   LMI = (WBTOT_LEAN/1000) / ((height_cm/100)^2)
                                   ))
dfit3dBase$ageYr= floor(dfit3dBase$age)
dfit3dBase <- transform(dfit3dBase, avgArmFmi = (avgArmFat/1000) / ((height_cm/100)^2), 
                           avgLegFmi = (avgLegFat/1000) / ((height_cm/100)^2),
                           trunkFmi = (TRUNK_FAT/1000) / ((height_cm/100)^2),
                           leftArmFmi = (LARM_FAT/1000) / ((height_cm/100)^2),
                           leftLegFmi = (L_LEG_FAT/1000) / ((height_cm/100)^2),
                           rightLegFmi = (R_LEG_FAT/1000) / ((height_cm/100)^2),
                           rightArmFmi = (RARM_FAT/1000) / ((height_cm/100)^2),
                              avgArmLmi = (avgArmLI/1000) / ((height_cm/100)^2), 
                                 avgLegLmi = (avgLegLI/1000) / ((height_cm/100)^2),
                                 trunkLmi = (TRUNK_LEAN/1000) / ((height_cm/100)^2),
                                 leftArmLmi = (LARM_LEAN/1000) / ((height_cm/100)^2),
                                 leftLegLmi = (L_LEG_LEAN/1000) / ((height_cm/100)^2),
                                 rightLegLmi = (R_LEG_LEAN/1000) / ((height_cm/100)^2),
                                 rightArmLmi = (RARM_LEAN/1000) / ((height_cm/100)^2)
                         ) 
                              







genderFix <- function(x) { 
  if(x == 'M') y <- "Male"
  if(x == 'F') y <- "Female"
  return(y)
}

dfit3dBase$Gender <- sapply(dfit3dBase$SEX,genderFix)

RaceFix <- function(x) { 
  if(x == 'black') y <- 'Non-Hispanic Black'
  else if(x == 'white') y <- 'Non-Hispanic White'
  else if(x == 'hispanic') y <- 'Hispanic'
  else y <- 'Other'
  return(y)
}

dfit3dBase$Race <- sapply(dfit3dBase$ethnicity,RaceFix)




#if (selectNumber == 1) {
#  chartDim <- c(1,1)
#} else if (selectNumber == 2) {
#  chartDim <- c(1,2)
#}else if (selectNumber == 4) {
#  chartDim <- c(2,2) 
#}else if (selectNumber == 9) {
#  chartDim <- c(3,3)
#}




#Calculate Z Scores for all these people. 
#Find way to just target the age in that row. 

#Take just the eligible people (Hisp, White, Black)
fit3dEligible <- subset(dfit3dBase, dfit3dBase$Race=="Hispanic"
                     | dfit3dBase$Race=="Non-Hispanic Black"
                     | dfit3dBase$Race == "Non-Hispanic White")



####Works to here####

#####
#                                        #
#                                        #
#Part 2: Importing LMS Z scores (And maybe calculating values?)
#                                        #
#                                        #

#Explanation:This section imports the L,M,S values from the LMS chartmaker modeling
#and calculates what the left leg/right leg and left arm/right arm individual z scores
#would be based on the Average leg and average arm L,M,S values. It then stores these
#values in new columns and gives an opportunity to export this new dataset in a new .txt 
#table separated by race
#  Formula to convert from value (y) to z score (z)
# z = ( y / m)^L - 1 / (L*S)

#Inputs:

#Specifies which columns to keep from LMS tables
keep <- c("Age","L", "M", "S")

bfArmFmiLms <- 
  read.table("data/BlackFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bfArmLmiLms <- 
  read.table("data/BlackFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
bfLegFmiLms <- 
  read.table("data/BlackFmiLmi_Female_AvgLegFMI_020302t.txt", header=T, skip=10, sep="\t")
bfLegLmiLms <- 
  read.table("data/BlackFmiLmi_Female_AvgLegLMI_010401t.txt", header=T, skip=10, sep="\t")
bfTrunkFmiLms <- 
  read.table("data/BlackFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
bfTrunkLmiLms <- 
  read.table("data/BlackFmiLmi_Female_TrunkLMI_010401t.txt", header=T, skip=10, sep="\t")
#Keeps only the relevant columns for the black females
bfLms <- cbind(bfArmFmiLms[keep], bfArmLmiLms[keep],
               bfLegFmiLms[keep], bfLegLmiLms[keep],
               bfTrunkFmiLms[keep], bfTrunkLmiLms[keep])

bmArmFmiLms <- 
  read.table("data/BlackFmiLmi_Male_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
bmArmLmiLms <- 
  read.table("data/BlackFmiLmi_Male_AvgArmLMI_020601t.txt", header=T, skip=10, sep="\t")
bmLegFmiLms <- 
  read.table("data/BlackFmiLmi_Male_AvgLegFMI_020202t.txt", header=T, skip=10, sep="\t")
bmLegLmiLms <- 
  read.table("data/BlackFmiLmi_Male_AvgLegLMI_010501t.txt", header=T, skip=10, sep="\t")
bmTrunkFmiLms <- 
  read.table("data/BlackFmiLmi_Male_TrunkFMI_020401tt.txt", header=T, skip=10, sep="\t")
#This blew up at 8 yr old and didn't display a number so I put a junk variable in. 
bmTrunkLmiLms <- 
  read.table("data/BlackFmiLmi_Male_TrunkLMI_010601t.txt", header=T, skip=10, sep="\t")
#Keeps only the relevant columns for the black males
bmLms <- cbind(bmArmFmiLms[keep], bmArmLmiLms[keep], 
               bmLegFmiLms[keep], bmLegLmiLms[keep],
               bmTrunkFmiLms[keep], bmTrunkLmiLms[keep])

hfArmFmiLms <- 
  read.table("data/HispFmiLmi_Female_AvgArmFMI_020302t.txt", header=T, skip=10, sep="\t")
hfArmLmiLms <- 
  read.table("data/HispFmiLmi_Female_AvgArmLMI_020401t.txt", header=T, skip=10, sep="\t")
hfLegFmiLms <- 
  read.table("data/HispFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
hfLegLmiLms <- 
  read.table("data/HispFmiLmi_Female_AveLegLMI_020401t.txt", header=T, skip=10, sep="\t")
hfTrunkFmiLms <- 
  read.table("data/HispFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
hfTrunkLmiLms <- 
  read.table("data/HispFmiLmi_Female_TrunkLMI_020401t.txt", header=T, skip=10, sep="\t")
#Hispanic Females
hfLms <- cbind(hfArmFmiLms[keep], hfArmLmiLms[keep], 
               hfLegFmiLms[keep], hfLegLmiLms[keep],
               hfTrunkFmiLms[keep], hfTrunkLmiLms[keep])


hmArmFmiLms <- 
  read.table("data/HispFmiLmi_Male_AvgArmFMI_010403t.txt", header=T, skip=10, sep="\t")
hmArmLmiLms <- 
  read.table("data/HispFmiLmi_Male_AvgArmLMI_010702t.txt", header=T, skip=10, sep="\t")
hmLegFmiLms <- 
  read.table("data/HispFmiLmi_Male__AvgLegFMI_010102t.txt", header=T, skip=10, sep="\t")
hmLegLmiLms <- 
  read.table("data/HispFmiLmi_Male_AvgLegLMI_010602t.txt", header=T, skip=10, sep="\t")
hmTrunkFmiLms <- 
  read.table("data/HispFmiLmi_Male_TrunkFMI_020502t.txt", header=T, skip=10, sep="\t")
hmTrunkLmiLms <- 
  read.table("data/HispFmiLmi_Male_TrunkLMI_010702t.txt", header=T, skip=10, sep="\t")
#Hispanic Males
hmLms <- cbind(hmArmFmiLms[keep], hmArmLmiLms[keep], 
               hmLegFmiLms[keep], hmLegLmiLms[keep],
               hmTrunkFmiLms[keep], hmTrunkLmiLms[keep])

wfArmFmiLms <- 
  read.table("data/WhiteFmiLmi_Female_AvgArmFMI_020202t.txt", header=T, skip=10, sep="\t")
wfArmLmiLms <- 
  read.table("data/WhiteFmiLmi_Female_AvgArmLMI_010401t.txt", header=T, skip=10, sep="\t")
wfLegFmiLms <- 
  read.table("data/WhiteFmiLmi_Female_AvgLegFMI_020301t.txt", header=T, skip=10, sep="\t")
wfLegLmiLms <- 
  read.table("data/WhiteFmiLmi_Female_AvgLegLMI_010601t.txt", header=T, skip=10, sep="\t")
wfTrunkFmiLms <- 
  read.table("data/WhiteFmiLmi_Female_TrunkFMI_020402t.txt", header=T, skip=10, sep="\t")
wfTrunkLmiLms <- 
  read.table("data/WhiteFmiLmi_Female_TrunkLMI_010401t.txt", header=T, skip=10, sep="\t")
#White Females
wfLms <- cbind(wfArmFmiLms[keep], wfArmLmiLms[keep], 
               wfLegFmiLms[keep], wfLegLmiLms[keep],
               wfTrunkFmiLms[keep], wfTrunkLmiLms[keep])

wmArmFmiLms <- 
  read.table("data/WhiteFmiLmi_Male_AvgArmFMI_020402t.txt", header=T, skip=10, sep="\t")
wmArmLmiLms <- 
  read.table("data/WhiteFmiLmi_Male_AvgArmLMI_010801t.txt", header=T, skip=10, sep="\t")
wmLegFmiLms <- 
  read.table("data/WhiteFmiLmi_Male_AvgLegFMI_010202t.txt", header=T, skip=10, sep="\t")
wmLegLmiLms <- 
  read.table("data/WhiteFmiLmi_Male_AvgLagLMI_020702t.txt", header=T, skip=10, sep="\t")
wmTrunkFmiLms <- 
  read.table("data/WhiteFmiLmi_Male_TrunkFMI_020502t.txt", header=T, skip=10, sep="\t")
wmTrunkLmiLms <- 
  read.table("data/WhiteFmiLmi_Male_TrunkLMI_020702t.txt", header=T, skip=10, sep="\t")
#White Males
wmLms <- cbind(wmArmFmiLms[keep], wmArmLmiLms[keep], 
               wmLegFmiLms[keep], wmLegLmiLms[keep],
               wmTrunkFmiLms[keep], wmTrunkLmiLms[keep])


rows = nrow(fit3dEligible)
FullZSet = NULL

for (j in 1:rows){
  race = fit3dEligible$Race[j]
  gender = fit3dEligible$Gender[j]
  age = fit3dEligible$ageYr[j]

  zScore <- fit3dEligible[j ,] 
  
  if (race == 'Non-Hispanic Black'){
    racePrefix = 'b'
  }else if (race == 'Non-Hispanic White'){
    racePrefix = 'w'
  }else if (race == 'Hispanic'){
    racePrefix = 'h'
  }
  if (gender == 'Male'){
    genderPrefix = 'm'
  }else if (gender == 'Female'){
    genderPrefix = 'f'
  }


  frames <- c(sprintf("%s%sLms", racePrefix, genderPrefix))
 
    df <- get(frames)

    lmsChart <- assign(as.character(frames), df, envir= .GlobalEnv)
  
    
    
    
    
    agerow = age - 7
    lmsAge <- lmsChart[agerow ,]                           
    #Converts all to data matrix (better for calculations)
    lArmFmi = data.matrix(lmsAge[2]) 
    mArmFmi = data.matrix(lmsAge[3]) 
    sArmFmi = data.matrix(lmsAge[4])
    lArmLmi = data.matrix(lmsAge[6])
    mArmLmi = data.matrix(lmsAge[7])
    sArmLmi = data.matrix(lmsAge[8])
    lLegFmi = data.matrix(lmsAge[10])
    mLegFmi = data.matrix(lmsAge[11])
    sLegFmi = data.matrix(lmsAge[12])
    lLegLmi = data.matrix(lmsAge[14])
    mLegLmi = data.matrix(lmsAge[15])
    sLegLmi = data.matrix(lmsAge[16])
    lTrunkFmi = data.matrix(lmsAge[18])
    mTrunkFmi = data.matrix(lmsAge[19])
    sTrunkFmi = data.matrix(lmsAge[20])
    lTrunkLmi = data.matrix(lmsAge[22])
    mTrunkLmi = data.matrix(lmsAge[23])
    sTrunkLmi = data.matrix(lmsAge[24])
    
    #Select just a row
    
    
    zScore1 <- transform(zScore, 
                         zLArmFmi= (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                         zRArmFmi= (((rightArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi),
                         zLArmLmi= (((leftArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                         zRArmLmi= (((rightArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi),
                         zLLegFmi= (((leftLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                         zRLegFmi= (((rightLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi),
                         zLLegLmi= (((leftLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi),
                         zRLegLmi= (((rightLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi),
                         zTrunkFmi= (((trunkFmi/mTrunkFmi)^lTrunkFmi)-1)/(lTrunkFmi*sTrunkFmi),
                         zTrunkLmi= (((trunkLmi/mTrunkLmi)^lTrunkLmi)-1)/(lTrunkLmi*sTrunkLmi))
    
    
    colnames(zScore1)[c(49:58)] <- 
      c('zLArmFmi', 'zRArmFmi', 'zLArmLmi', 'zRArmLmi', 'zLLegFmi', 'zRLegFmi', 
        'zLLegLmi',  'zRLegLmi', 'zTrunkFmi', 'zTrunkLmi')
    
    #Calculates avg z score (useful in finding populations based on avg Z = +2, 0, -2, etc)
    zScore2 <- data.frame(transform(zScore1,
                         zAvgFmi= (zTrunkFmi+zLArmFmi+zRArmFmi+zLLegFmi+zRLegFmi) / 5, 
                         zAvgLmi= (zTrunkLmi+zLArmLmi+zRArmLmi+zLLegLmi+zRLegLmi) / 5,
                         ZSDFMI = sd(c(zLArmFmi, zRArmFmi,zLLegFmi,zRLegFmi, zTrunkFmi)),
                         ZSDLMI = sd(c(zLArmLmi, zRArmLmi,zLLegLmi,zRLegLmi, zTrunkLmi)))
    )
    
    
    
    #Keeps only th                   
    keep <- c('BMI','FMI','LMI',  "height_cm","scan_package_id",
              "ageYr",'Gender','Race','zLArmFmi', 'zRArmFmi', 
              'zLArmLmi', 'zRArmLmi', 'zLLegFmi', 'zRLegFmi', 
              'zLLegLmi',  'zRLegLmi', 'zTrunkFmi', 'zTrunkLmi','zAvgFmi', 'zAvgLmi', 'ZSDFMI',
              'ZSDLMI')
              
              

    zScore3 <- zScore2[keep]
    
    #These are the n-1 versions of the SDs
    FullZSet = rbind(FullZSet, zScore3)
    

    #Changes column names to create the radar charts later on


}#End of For statment
  
colnames(FullZSet) <- c('BMI','FMI','LMI',  "height_cm","scan_package_id",
                        "ageYr",'Gender','Race','Z_FMI_LA', 'Z_FMI_RA', 
                        'Z_LMI_LA', 'Z_LMI_RA', 'Z_FMI_LL', 'Z_FMI_RL', 
                        'Z_LMI_LL',  'Z_LMI_RL', 'Z_FMI_TR', 'Z_LMI_TR','zAvgFmi',
                        'zAvgLmi', 'ZSDFMI','ZSDLMI') 
 


shinyServer(
  function(input, output) {

    output$map <- renderPlot({

      zData2 <- FullZSet[input$Person , ] 
      
      
      dzData <- data.frame(zData2) 
      #print(dzData)
      #print(2)
      #Converts that set to dataframe
      #finds dimensions of that table and takes selectNumber random rows from that data set
      #dimension <- dim(dzData)
      #nRow <- floor(runif(1, 1,dimension[1]))  #Normally floor(runif(selectNumber, 1,dimension[1]))
      #selects out only those random rows and their FMI/LMI data
      fmiData <- dzData[1,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
      lmiData <- dzData[1,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]
      #renames the columns because column names in fmiData/lmiData must match maxmin
      colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
      colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
      ind1Data <- rbind(maxmin,fmiData[1,],lmiData[1,])   #normally in a loop and i instead of 1
     
      op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
      radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
                 pcol = c("goldenrod3", "firebrick4"),
                 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
                 title=sprintf("%s %s Individual FMI/LMI Chart", input$race, input$gender))
      legend('topright', c("FMI", "FFMI") , lwd=2, 
             col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2) 
      
      
       })
    
    
    output$text1 <- renderText({ 
      #total <- total1[ which(total1$Gender==input$gender 
       #                      & total1$Race==input$race) , ]
  
      zData2 <- FullZSet[input$Person , ] 
      paste("Age/Gender/Race: ", zData2$ageYr, zData2$Gender, zData2$Race)


    })
    output$text2 <- renderText({ 
      #total <- total1[ which(total1$Gender==input$gender 
      #                      & total1$Race==input$race) , ]
      
      zData2 <- FullZSet[input$Person , ] 

      paste("BMI/FMI/LMI: ", zData2$BMI, zData2$FMI, zData2$LMI)

    })
    output$text3 <- renderText({ 
      #total <- total1[ which(total1$Gender==input$gender 
      #                      & total1$Race==input$race) , ]
      
      zData2 <- FullZSet[input$Person , ] 

      paste("Package ID Number: ", zData2$scan_package_id)
    })
      
  }
    )





#      race <- switch(input$race, 
#                    "Percent White" = counties$white,
#                   "Percent Black" = counties$black,
#                  "Percent Hispanic" = counties$hispanic,
#                 "Percent Asian" = counties$asian)

#      gender <- switch(input$gender, 
#                     "Percent White" = "darkgreen",
#                    "Percent Black" = "black",
#                   "Percent Hispanic" = "darkorange",
#                  "Percent Asian" = "darkviolet")

#      age <- switch(input$age, 
#                      "Percent White" = "% White",
#                     "Percent Black" = "% Black",
#                    "Percent Hispanic" = "% Hispanic",
#                   "Percent Asian" = "% Asian")

#percent_map(var = data, 
#           color = color, 
#          legend.title = legend,
#         max = input$range[2], 
#        min = input$range[1])