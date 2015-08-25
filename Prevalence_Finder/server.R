# server.R
setwd("X:\\bhinton\\radar_chart\\Prevalence_Finder")
blackData <- read.table(file="data/BlackFmiLmi.txt",header=T, sep="\t")
hispData <- read.table(file="data/HispFmiLmi.txt",header=T, sep="\t")
whiteData <- read.table(file="data/WhiteFmiLmi.txt",header=T, sep="\t")
fullData <- rbind(blackData, hispData, whiteData)
fullData2 <- transform(fullData,
                       Bmi = totBodyFmi + totBodyLmi)

total1 <- subset(fullData2, fullData2$RIDAGEYR <= 29 & fullData2$RIDAGEYR >= 20)

shinyServer(
  function(input, output) {

# 

#       
#       ntotal <- nrow(total)
#       nunder16 <- nrow(subset(total, total$Bmi <= 16))
#       nunder17 <- nrow(subset(total, total$Bmi <= 17))
#       nunder185 <- nrow(subset(total, total$Bmi <= 18.5))
#       nover25 <- nrow(subset(total, total$Bmi >= 25))
#       nover30 <- nrow(subset(total, total$Bmi >= 30))
#       nover35 <- nrow(subset(total, total$Bmi >= 35))
#       nover40 <- nrow(subset(total, total$Bmi >= 40))
#       
#       
#       
#       
#       #nrow(subset(total, total$totBodyFmi >= 18.8))
#       
#       
#       #nTotUnder <- nrow(subset(total, total$totBodyFmi <= input$totFmi))
#       #TotOver  <- nrow(subset(total, total$totBodyFmi >= input$totFmi))
#       
#       nTrunkUnder <- nrow(subset(total, total$trunkFmi <= input$trunkFmi))
#       nTrunkOver  <- nrow(subset(total, total$trunkFmi >= input$trunkFmi))
#       
#       nArmUnder <- nrow(subset(total, total$avgArmFmi <= input$ArmFmi))
#       nArmOver  <- nrow(subset(total, total$avgArmFmi >= input$ArmFmi))
#       
#       nLegUnder <- nrow(subset(total, total$avgLegFmi <= input$LegFmi))
#       nLegOver  <- nrow(subset(total, total$avgLegFmi >= input$LegFmi))
#       
      
      
      
      output$text1 <- renderText({ 
        total <- total1[ which(total1$Gender==input$gender 
                               & total1$Race==input$race) , ]
        
              ntotal <- nrow(total)
              nunder16 <- nrow(subset(total, total$Bmi <= 16))
              nunder17 <- nrow(subset(total, total$Bmi <= 17))
              nunder185 <- nrow(subset(total, total$Bmi <= 18.5))
              nover25 <- nrow(subset(total, total$Bmi >= 25))
              nover30 <- nrow(subset(total, total$Bmi >= 30))
              nover35 <- nrow(subset(total, total$Bmi >= 35))
              nover40 <- nrow(subset(total, total$Bmi >= 40))
        
        paste("Under 16:", nunder16, nunder17, nunder185, nover25,
              nover30, nover35, nover40)
        paste("Under 17": nunder17)
        paste("Under 18.5:", nunder185)
        paste("Over 25:", nover25)
        paste("Over 35:",nover35)
        paste("Over 40:", nover40)
        
        
      })
      #input$race
      output$text2 <- renderText({ 
        total <- total1[ which(total1$Gender==input$gender 
                               & total1$Race==input$race) , ]
        
              nTotUnder <- nrow(subset(total, total$totBodyFmi <= input$totFmi))
              TotOver  <- nrow(subset(total, total$totBodyFmi >= input$totFmi))
              
              nTrunkUnder <- nrow(subset(total, total$trunkFmi <= input$trunkFmi))
              nTrunkOver  <- nrow(subset(total, total$trunkFmi >= input$trunkFmi))
              
              nArmUnder <- nrow(subset(total, total$avgArmFmi <= input$armFmi))
              nArmOver  <- nrow(subset(total, total$avgArmFmi >= input$armFmi))
              
              nLegUnder <- nrow(subset(total, total$avgLegFmi <= input$legFmi))
              nLegOver  <- nrow(subset(total, total$avgLegFmi >= input$legFmi))
        
        
        
        paste("Total Over/Under:", nTotUnder, TotOver)
        paste("Trunk Over/Under:", nTrunkUnder, nTrunkOver)
        paste("Arm Over/Under:", nArmUnder, nArmOver)
        paste("Leg Over/Under:",  nLegUnder, nLegOver)
       
      })
      

      
      
      #dzData <- data.frame(zData2) 
      #print(dzData)
      #print(2)
      #Converts that set to dataframe
      #finds dimensions of that table and takes selectNumber random rows from that data set
      #dimension <- dim(dzData)
      #nRow <- floor(runif(1, 1,dimension[1]))  #Normally floor(runif(selectNumber, 1,dimension[1]))
      #selects out only those random rows and their FMI/LMI data
      #fmiData <- dzData[1,c("Z_FMI_TR","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
      #lmiData <- dzData[1,c("Z_LMI_TR","Z_LMI_LA", "Z_LMI_LL", "Z_LMI_RL", "Z_LMI_RA")]
      #renames the columns because column names in fmiData/lmiData must match maxmin
      #colnames(fmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
      #colnames(lmiData) <- c("Z_TR", "Z_LA", "Z_LL", "Z_RL", "Z_RA")
      #ind1Data <- rbind(maxmin,fmiData[1,],lmiData[1,])   #normally in a loop and i instead of 1
     
      #op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
      #radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
        #         pcol = c("goldenrod3", "firebrick4"),
         #        vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
          #       title=sprintf("%s %s Individual FMI/LMI Chart", input$race, input$gender))
      #legend('topright', c("FMI", "FFMI") , lwd=2, 
       #      col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2) 
      
      
       
      
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