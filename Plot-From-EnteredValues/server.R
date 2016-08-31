# server.R

##Need to:
#Make subfnctions and call them
#Clean up code
#Clean up variable Names
#Mess with ordering
#Fix tabbing
#Overlay image behind the radar chart
#Work on the ui.r
#Think about R Shiny widget types to use.


# blackData <- read.table(file=sprintf("data/Black.ZScoreValues.txt", sep="\t"))
# hispData <- read.table(file=sprintf("data/Hisp.ZScoreValues.txt", sep="\t"))
# whiteData <- read.table(file=sprintf("data/White.ZScoreValues.txt", sep="\t"))
# fullData <- rbind(blackData, hispData, whiteData)
#Maybe have 2 sidebar panels

#  Formula to convert from FMI/LMI value (y) to FMI/LMI z score (z)
# z = ( y / m)^L - 1 / (L*S)

#Inputs: The LMS Tables for {White,Black,Hispanic}{Male,Female}{Arm,Leg,Trunk}{FMI,LMI}
#Specifies which columns to keep from LMS tables
keep <- c("Age","L", "M", "S")
#Keeps only the relevant columns for the black females
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
bfLms <- cbind(bfArmFmiLms[keep], bfArmLmiLms[keep],
               bfLegFmiLms[keep], bfLegLmiLms[keep],
               bfTrunkFmiLms[keep], bfTrunkLmiLms[keep])
#Keeps only the relevant columns for the black males
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
bmTrunkLmiLms <- 
  read.table("data/BlackFmiLmi_Male_TrunkLMI_010601t.txt", header=T, skip=10, sep="\t")
bmLms <- cbind(bmArmFmiLms[keep], bmArmLmiLms[keep], 
               bmLegFmiLms[keep], bmLegLmiLms[keep],
               bmTrunkFmiLms[keep], bmTrunkLmiLms[keep])
#Hispanic Females
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
hfLms <- cbind(hfArmFmiLms[keep], hfArmLmiLms[keep], 
               hfLegFmiLms[keep], hfLegLmiLms[keep],
               hfTrunkFmiLms[keep], hfTrunkLmiLms[keep])
#Hispanic Males
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
hmLms <- cbind(hmArmFmiLms[keep], hmArmLmiLms[keep], 
               hmLegFmiLms[keep], hmLegLmiLms[keep],
               hmTrunkFmiLms[keep], hmTrunkLmiLms[keep])
#White Females
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
wfLms <- cbind(wfArmFmiLms[keep], wfArmLmiLms[keep], 
               wfLegFmiLms[keep], wfLegLmiLms[keep],
               wfTrunkFmiLms[keep], wfTrunkLmiLms[keep])
#White Males
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
wmLms <- cbind(wmArmFmiLms[keep], wmArmLmiLms[keep], 
               wmLegFmiLms[keep], wmLegLmiLms[keep],
               wmTrunkFmiLms[keep], wmTrunkLmiLms[keep])

#Set up initial variables for radar chart generation
library(fmsb)
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_RA=c(2, -2),
  Z_RL=c(2, -2),
  Z_LL=c(2, -2),
  Z_LA=c(2, -2))
chartDim <- c(1,1)

#Beginning of Graphing
shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      hfLms <- cbind(hfArmFmiLms[keep], hfArmLmiLms[keep], 
                     hfLegFmiLms[keep], hfLegLmiLms[keep],
                     hfTrunkFmiLms[keep], hfTrunkLmiLms[keep])
      hmLms <- cbind(hmArmFmiLms[keep], hmArmLmiLms[keep], 
                     hmLegFmiLms[keep], hmLegLmiLms[keep],
                     hmTrunkFmiLms[keep], hmTrunkLmiLms[keep])
      bmLms <- cbind(bmArmFmiLms[keep], bmArmLmiLms[keep], 
                     bmLegFmiLms[keep], bmLegLmiLms[keep],
                     bmTrunkFmiLms[keep], bmTrunkLmiLms[keep])
      bfLms <- cbind(bfArmFmiLms[keep], bfArmLmiLms[keep],
                     bfLegFmiLms[keep], bfLegLmiLms[keep],
                     bfTrunkFmiLms[keep], bfTrunkLmiLms[keep])
      wmLms <- cbind(wmArmFmiLms[keep], wmArmLmiLms[keep], 
                     wmLegFmiLms[keep], wmLegLmiLms[keep],
                     wmTrunkFmiLms[keep], wmTrunkLmiLms[keep])
      wfLms <- cbind(wfArmFmiLms[keep], wfArmLmiLms[keep], 
                     wfLegFmiLms[keep], wfLegLmiLms[keep],
                     wfTrunkFmiLms[keep], wfTrunkLmiLms[keep])
      dfit3dBase <- NULL
      
      #Set the relevant variables as your inputs
      ageYr = input$AgeYr
      height_cm = input$height_cm
      Race = input$Race
      Gender = input$Gender 
      RARM_FAT = input$RARM_FAT
      RARM_LEAN = input$RARM_LEAN
      LARM_FAT = input$LARM_FAT
      LARM_LEAN = input$LARM_LEAN
      L_LEG_FAT = input$L_LEG_FAT
      L_LEG_LEAN = input$L_LEG_LEAN
      R_LEG_FAT = input$R_LEG_FAT
      R_LEG_LEAN = input$R_LEG_LEAN
      TRUNK_FAT = input$TRUNK_FAT
      TRUNK_LEAN = input$TRUNK_LEAN
      race = input$Race
      gender = input$Gender
      age = input$AgeYr
      
      #Calculate FMI/LMI from that.
      trunkFmi = (TRUNK_FAT/1000) / ((height_cm/100)^2)
      leftArmFmi = (LARM_FAT/1000) / ((height_cm/100)^2)
      leftLegFmi = (L_LEG_FAT/1000) / ((height_cm/100)^2)
      rightLegFmi = (R_LEG_FAT/1000) / ((height_cm/100)^2)
      rightArmFmi = (RARM_FAT/1000) / ((height_cm/100)^2)
      trunkLmi = (TRUNK_LEAN/1000) / ((height_cm/100)^2)
      leftArmLmi = (LARM_LEAN/1000) / ((height_cm/100)^2)
      leftLegLmi = (L_LEG_LEAN/1000) / ((height_cm/100)^2)
      rightLegLmi = (R_LEG_LEAN/1000) / ((height_cm/100)^2)
      rightArmLmi = (RARM_LEAN/1000) / ((height_cm/100)^2)
      
      if (race == 3){
        racePrefix = 'b'
      }else if (race == 2){
        racePrefix = 'w'
      }else if (race == 1){
        racePrefix = 'h'
      }
      if (gender == 2){
        genderPrefix = 'm'
      }else if (gender == 1){
        genderPrefix = 'f'
      }
      #Find the appropriate LMS Table absed on the gender/ethnicity choice
      frames <- c(sprintf("%s%sLms", racePrefix, genderPrefix))
      df <- get(frames)
      lmsChart <- assign(as.character(frames), df, envir= .GlobalEnv)
      #Select the right age based on the age selection
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
      #Calculate the z scores of every region
      zLArmFmi= (((leftArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi)
      zRArmFmi= (((rightArmFmi/mArmFmi)^lArmFmi)-1)/(lArmFmi*sArmFmi)
      zLArmLmi= (((leftArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi)
      zRArmLmi= (((rightArmLmi/mArmLmi)^lArmLmi)-1)/(lArmLmi*sArmLmi)
      zLLegFmi= (((leftLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi)
      zRLegFmi= (((rightLegFmi/mLegFmi)^lLegFmi)-1)/(lLegFmi*sLegFmi)
      zLLegLmi= (((leftLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi)
      zRLegLmi= (((rightLegLmi/mLegLmi)^lLegLmi)-1)/(lLegLmi*sLegLmi)
      zTrunkFmi= (((trunkFmi/mTrunkFmi)^lTrunkFmi)-1)/(lTrunkFmi*sTrunkFmi)
      zTrunkLmi= (((trunkLmi/mTrunkLmi)^lTrunkLmi)-1)/(lTrunkLmi*sTrunkLmi)
      
      #Keep only the necessary columns for radarchart plotting
      zData2 <- data.frame(zLArmFmi,  zRArmFmi,     zLArmLmi, zRArmLmi,  zLLegFmi,  zRLegFmi,
                           zLLegLmi, zRLegLmi, zTrunkFmi, zTrunkLmi)
      colnames(zData2) <- c( 'Z_FMI_LA', 'Z_FMI_RA', 
                              'Z_LMI_LA', 'Z_LMI_RA', 'Z_FMI_LL', 'Z_FMI_RL', 
                              'Z_LMI_LL',  'Z_LMI_RL', 'Z_FMI_TR', 'Z_LMI_TR') 
      dzData <- data.frame(zData2)
      #Organizes the FMI/LMI data in format required for radar chart.
      fmiData <- dzData[1,c("Z_FMI_TR","Z_FMI_RA", "Z_FMI_RL", "Z_FMI_LL", "Z_FMI_LA")]
      lmiData <- dzData[1,c("Z_LMI_TR","Z_LMI_RA", "Z_LMI_RL", "Z_LMI_LL", "Z_LMI_LA")]
      #renames the columns because column names in fmiData/lmiData must match maxmin
      colnames(fmiData) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      colnames(lmiData) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      ind1Data <- rbind(maxmin,fmiData[1,],lmiData[1,])   #normally in a loop and i instead of 1
      #Plots the radar chart data
      op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
      radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
                 pcol = c("goldenrod3", "firebrick4"),
                 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
                 title="Individual FMI/LMI Chart")
      legend('topright', c("FMI", "LMI") , lwd=2, 
             col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2) 
       })
  })