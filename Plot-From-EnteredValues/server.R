# server.R

library(fmsb)
library(shiny)


##Need to:
#Make subfnctions and call them
#Clean up code
#Clean up variable Names
#Mess with ordering
#Fix tabbing
#Overlay image behind the radar chart
#Work on the ui.r
#Think about R Shiny widget types to use.

#  Formula to convert from FMI/LMI value (y) to FMI/LMI z score (z)
# z = ( y / m)^L - 1 / (L*S)

# Pre-calculations
##### 
# Beginning of Each variable -> bf = black female, wm = white male, hm = hispanic male, etc.
# Keeps only the relevant LMS columns for each table

#Inputs: The LMS Tables for {White,Black,Hispanic}{Male,Female}{Arm,Leg,Trunk}{FMI,LMI}



#Uploads all LMS tables for the black females
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

#Uploads all LMS tables for Black Males 
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

#Set up initial variables for radar chart generation
#####
#Max and min for the radar chart Z scores eventually
maxmin <- data.frame(
              Z_TR=c(2, -2),
              Z_RA=c(2, -2),
              Z_RL=c(2, -2),
              Z_LL=c(2, -2),
              Z_LA=c(2, -2))
chartDim <- c(1,1)
# Server/Graphing
#####
shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      #Specifies which columns to keep from LMS tables
      keepCols <- c("Age","L", "M", "S")
      # Keeps only the relevant columns for each demographic group
      # bmLMS = LMS tables for black males, Each 4-column set is a region
      # each row is an age (from 8-85)
      hfLms <- cbind(hfArmFmiLms[keepCols], hfArmLmiLms[keepCols], 
                     hfLegFmiLms[keepCols], hfLegLmiLms[keepCols],
                     hfTrunkFmiLms[keepCols], hfTrunkLmiLms[keepCols])
      hmLms <- cbind(hmArmFmiLms[keepCols], hmArmLmiLms[keepCols], 
                     hmLegFmiLms[keepCols], hmLegLmiLms[keepCols],
                     hmTrunkFmiLms[keepCols], hmTrunkLmiLms[keepCols])
      bmLms <- cbind(bmArmFmiLms[keepCols], bmArmLmiLms[keepCols], 
                     bmLegFmiLms[keepCols], bmLegLmiLms[keepCols],
                     bmTrunkFmiLms[keepCols], bmTrunkLmiLms[keepCols])
      bfLms <- cbind(bfArmFmiLms[keepCols], bfArmLmiLms[keepCols],
                     bfLegFmiLms[keepCols], bfLegLmiLms[keepCols],
                     bfTrunkFmiLms[keepCols], bfTrunkLmiLms[keepCols])
      wmLms <- cbind(wmArmFmiLms[keepCols], wmArmLmiLms[keepCols], 
                     wmLegFmiLms[keepCols], wmLegLmiLms[keepCols],
                     wmTrunkFmiLms[keepCols], wmTrunkLmiLms[keepCols])
      wfLms <- cbind(wfArmFmiLms[keepCols], wfArmLmiLms[keepCols], 
                     wfLegFmiLms[keepCols], wfLegLmiLms[keepCols],
                     wfTrunkFmiLms[keepCols], wfTrunkLmiLms[keepCols])
      
      #Set the relevant variables as your inputs from ui.R
      height_cm = input$height_cm
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
      
      #Calculate FMI/LMI from the mass and height inputs
      #Divide mass values by 1000 if you have a grams input
      trunkFmi = (TRUNK_FAT) / ((height_cm/100)^2)  
      leftArmFmi = (LARM_FAT) / ((height_cm/100)^2)
      leftLegFmi = (L_LEG_FAT) / ((height_cm/100)^2)
      rightLegFmi = (R_LEG_FAT) / ((height_cm/100)^2)
      rightArmFmi = (RARM_FAT) / ((height_cm/100)^2)
      trunkLmi = (TRUNK_LEAN) / ((height_cm/100)^2)
      leftArmLmi = (LARM_LEAN) / ((height_cm/100)^2)
      leftLegLmi = (L_LEG_LEAN) / ((height_cm/100)^2)
      rightLegLmi = (R_LEG_LEAN) / ((height_cm/100)^2)
      rightArmLmi = (RARM_LEAN) / ((height_cm/100)^2)
      
      #Sets short string based on ui.R race/gender selections to match with the LMS tables above
      #Will make string saying 'hfLMS' if you selected hispanic females.
      if (race == 3){
        racePrefix = 'b'
        raceFull = 'black'
      }else if (race == 2){
        racePrefix = 'w'
        raceFull = 'white'
      }else if (race == 1){
        racePrefix = 'h'
        raceFull = 'Hispanic'
      }
      if (gender == 2){
        genderPrefix = 'm'
        genderFull = 'male'
      }else if (gender == 1){
        genderPrefix = 'f'
        genderFull = 'female'
      }
      #Find the appropriate LMS Table absed on the gender/ethnicity choice
      frames <- c(sprintf("%s%sLms", racePrefix, genderPrefix))
      df <- get(frames)
      lmsChart <- assign(as.character(frames), df, envir= .GlobalEnv)
      
      #Select the right age (Row) based on the age selection
      agerow = age - 7 #(-7 because age range is 8-85)
      lmsAge <- lmsChart[agerow ,]   #Every column of the correct row
      
      #each column of that variable is assigned to the correct L,M,S values for the right region.
      # mArmFmi -> M value of the FMI of the Arm (for the correct, Age, ethnicity, and gender)
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
      
      #Uses the LMS values and FMI/LMI of each region to get Z scores
      #Based on the equation at top of server.R
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
      zScoresIndividual <- data.frame(zLArmFmi, zRArmFmi, zLArmLmi, zRArmLmi, zLLegFmi, zRLegFmi,
                           zLLegLmi, zRLegLmi, zTrunkFmi, zTrunkLmi)
      #renames columns of the variable for easier plotting
      colnames(zScoresIndividual) <- c( 'Z_FMI_LA', 'Z_FMI_RA', 
                              'Z_LMI_LA', 'Z_LMI_RA', 'Z_FMI_LL', 'Z_FMI_RL', 
                              'Z_LMI_LL',  'Z_LMI_RL', 'Z_FMI_TR', 'Z_LMI_TR') 
      
      #Organizes the FMI/LMI data in format required for radar chart.
      zDataFmi <- zScoresIndividual[1,c("Z_FMI_TR","Z_FMI_RA", "Z_FMI_RL", "Z_FMI_LL", "Z_FMI_LA")]
      zDataLmi <- zScoresIndividual[1,c("Z_LMI_TR","Z_LMI_RA", "Z_LMI_RL", "Z_LMI_LL", "Z_LMI_LA")]
      #renames the columns because column names in zDataFmi/zDataLmi must match column names in maxmin
      colnames(zDataFmi) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      colnames(zDataLmi) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      ind1Data <- rbind(maxmin,zDataFmi[1,],zDataLmi[1,])   #Formatting in structure radarchart requires
      #Plots the radar chart data
      radarPlot <- radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
                 pcol = c("goldenrod3", "firebrick4"),
                 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
                 title="Individual FMI/LMI Chart")
      legend('topright', c("FMI", "LMI") , lwd=2, 
             col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2) 
      
      
      #Waits for person to click the 'save button'
      observeEvent(input$saveAction, {
        
        #select save folder
          #TODO
        #Arrange save data
          #TODO
        #Opens ability to write to PDF
        pdf(file = "myPlot.pdf", height = 11, width = 8.5)
        
        #Puts the radar chart in the PDF
        test1 <- radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2,
                            pcol = c("goldenrod3", "firebrick4"),
                            vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
                            title="Individual FMI/LMI Chart")
        legend('topright', c("FMI", "LMI") , lwd=2, 
               col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2) 
        
        # Adds descriptive text to PDF
        str <- sprintf("Radar chart of a %1.0f years old %s %s that is %1.0f cm tall.",input$AgeYr, raceFull, genderFull, input$height_cm)
        fileTxt <- text(0,-1.1,str) #Demog
        
        str <- sprintf("Trunk Fat Mass/FMI: %1.1f/%1.1f.   Trunk Lean Mass/LMI: %1.1f/%1.1f.",input$TRUNK_FAT,trunkFmi, input$TRUNK_LEAN, trunkLmi)
        fileTxt <- text(0,-1.2,str) #Trunk Info
        
        str <- sprintf("Right/Left Arm FMI: %1.1f/%1.1f   Right/Left Arm LMI: %1.1f/%1.1f.",rightArmFmi, leftArmFmi, rightArmLmi, leftArmLmi)
        fileTxt <- text(0,-1.3,str) #Arm Info
        
        str <- sprintf("Right/Left Arm Fat Mass: %1.1f/%1.1f   Right/Left Arm Lean Mass: %1.1f/%1.1f.",input$RARM_FAT, input$LARM_FAT, input$RARM_LEAN, input$LARM_LEAN)
        fileTxt <- text(0,-1.4,str) #Arm Info
        
        str <- sprintf("Right/Left Leg FMI: %1.1f/%1.1f   Right/Left Leg LMI: %1.1f/%1.1f.",rightLegFmi, leftLegFmi, rightLegLmi, leftLegLmi)
        fileTxt <- text(0,-1.5,str) #Leg Info
        
        str <- sprintf("Right/Left Leg Fat Mass: %1.1f/%1.1f   Right/Left Leg Lean Mass: %1.1f/%1.1f.",input$R_LEG_FAT, input$L_LEG_FAT, input$R_LEG_LEAN, input$L_LEG_LEAN)
        fileTxt <- text(0,-1.6,str) #Arm Info
        dev.off()  #Ends writing
    
        test0 <- print('File saved to folder with server.R')
        }) #Ends Observe Event
    }) #Ends outputMap / RenderPlot
  }#Ends Function
)#Ends ShinyServer