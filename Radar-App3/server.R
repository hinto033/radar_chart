# server.R
#Should Produce radar charts of people ina range of FMI LMI Values
library(fmsb)
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_RA=c(2, -2),
  Z_RL=c(2, -2),
  Z_LL=c(2, -2),
  Z_LA=c(2, -2))

chartDim <- c(1,1)

setwd("C:\\Users\\bhinton\\Documents\\radar_chart\\Radar-App3")

# blackData1 <- read.table(file="data/BlackFmiLmi.txt",header=T, sep="\t")
# hispData1 <- read.table(file="data/HispFmiLmi.txt",header=T, sep="\t")
# whiteData1 <- read.table(file="data/WhiteFmiLmi.txt",header=T, sep="\t")
# 
# blackData1 <- data.frame(transform(blackData1,
#                                  BMI= totBodyFmi+totBodyLmi))
# hispData1 <- data.frame(transform(hispData1,
#                                  BMI= totBodyFmi+totBodyLmi))
# whiteData1 <- data.frame(transform(whiteData1,
#                                  BMI= totBodyFmi+totBodyLmi))
# keep <- c("Age","L", "M", "S")
# bfFmiLms <- 
#   read.table("data/BlackFmiLmi_Female_TotFMI_020302t.txt", header=T, skip=10, sep="\t")
# bfLmiLms <- 
#   read.table("data/BlackFmiLmi_Female_TotLMI_010401t.txt", header=T, skip=10, sep="\t")
# bmFmiLms <- 
#   read.table("data/BlackFmiLmi_Male_TotFMI_020401t.txt", header=T, skip=10, sep="\t")
# bmLmiLms <- 
#   read.table("data/BlackFmiLmi_Male_TotLMI_020601t.txt", header=T, skip=10, sep="\t")
# hfFmiLms <- 
#   read.table("data/HispFmiLmi_Female_TotFMI_020402t.txt", header=T, skip=10, sep="\t")
# hfLmiLms <- 
#   read.table("data/HispFmiLmi_Female_TotLMI_020601t.txt", header=T, skip=10, sep="\t")
# hmFmiLms <- 
#   read.table("data/HispFmiLmi_Male_TotFMI_020402t.txt", header=T, skip=10, sep="\t")
# hmLmiLms <- 
#   read.table("data/HispFmiLmi_Male_TotLMI_010702t.txt", header=T, skip=10, sep="\t")
# wfFmiLms <- 
#   read.table("data/WhiteFmiLmi_Female_TotFMI_020402t.txt", header=T, skip=10, sep="\t")
# wfLmiLms <- 
#   read.table("data/WhiteFmiLmi_Female_TotLMI_010401t.txt", header=T, skip=10, sep="\t")
# wmFmiLms <- 
#   read.table("data/WhiteFmiLmi_Male_TotFMI_020402t.txt", header=T, skip=10, sep="\t")
# wmLmiLms <- 
#   read.table("data/WhiteFmiLmi_Male_TotLMI_020702t.txt", header=T, skip=10, sep="\t")
# bfLms <- cbind(bfFmiLms[keep], bfLmiLms[keep])
# bmLms <- cbind(bmFmiLms[keep], bmLmiLms[keep])
# wfLms <- cbind(wfFmiLms[keep], wfLmiLms[keep])
# wmLms <- cbind(wmFmiLms[keep], wmLmiLms[keep])
# hfLms <- cbind(hfFmiLms[keep], hfLmiLms[keep])
# hmLms <- cbind(hmFmiLms[keep], hmLmiLms[keep])
# 
# bfData <-  blackData1[ which(blackData1$Gender=='Female' ) , ]
# bmData <-  blackData1[ which(blackData1$Gender=='Male' ) , ]
# hfData <-  hispData1[ which(hispData1$Gender=='Female' ) , ]
# hmData <-  hispData1[ which(hispData1$Gender=='Male' ) , ]
# wfData <-  whiteData1[ which(whiteData1$Gender=='Female' ) , ]
# wmData <-  whiteData1[ which(whiteData1$Gender=='Male' ) , ]
# 
# 
# 
# 
# blackData <- read.table(file=sprintf("data/Black.ZScoreValues.txt", sep="\t"))
# hispData <- read.table(file=sprintf("data/Hisp.ZScoreValues.txt", sep="\t"))
# whiteData <- read.table(file=sprintf("data/White.ZScoreValues.txt", sep="\t"))
# 
# 
# 
# whiteData1 <- whiteData1[order(whiteData1$Gender),]
# whiteData1 <- whiteData1[order(whiteData1$RIDAGEYR),]
# hispData1 <- hispData1[order(hispData1$Gender),]
# hispData1 <- hispData1[order(hispData1$RIDAGEYR),]
# blackData1 <- blackData1[order(blackData1$Gender),]
# blackData1 <- blackData1[order(blackData1$RIDAGEYR),]
# 
# blackFull <- cbind(blackData1, blackData)
# hispFull <- cbind(hispData1, hispData)
# whiteFull <- cbind(whiteData1, whiteData)
# 
# fullData <- rbind(blackFull, hispFull, whiteFull)
# 
# 

# FullZSet = NULL
# 
# rows = nrow(fullData)
# for (j in 1:rows){
#   race = fullData$Race[j]
#   gender = fullData$Gender[j]
#   age = fullData$RIDAGEYR[j]
#   
#   zScore <- fullData[j ,] 
#   
#   if (race == 'Non-Hispanic Black'){
#     racePrefix = 'b'
#   }else if (race == 'Non-Hispanic White'){
#     racePrefix = 'w'
#   }else if (race == 'Hispanic'){
#     racePrefix = 'h'
#   }
#   if (gender == 'Male'){
#     genderPrefix = 'm'
#   }else if (gender == 'Female'){
#     genderPrefix = 'f'
#   }
#   
#   frames <- c(sprintf("%s%sLms", racePrefix, genderPrefix))
#   
#   df <- get(frames)
#   
#   lmsChart <- assign(as.character(frames), df, envir= .GlobalEnv)
#   
#   agerow = age - 7
#   lmsAge <- lmsChart[agerow ,]                           
#   #Converts all to data matrix (better for calculations)
#   lTotFmi = data.matrix(lmsAge[2]) 
#   mTotFmi = data.matrix(lmsAge[3]) 
#   sTotFmi = data.matrix(lmsAge[4])
#   lTotLmi = data.matrix(lmsAge[6])
#   mTotLmi = data.matrix(lmsAge[7])
#   sTotLmi = data.matrix(lmsAge[8])
#   
#   zScore1 <- transform(zScore, 
#                        zTotFmi= (((totBodyFmi/mTotFmi)^lTotFmi)-1)/(lTotFmi*sTotFmi),
#                        zTotLmi= (((totBodyLmi/mTotLmi)^lTotLmi)-1)/(lTotLmi*sTotLmi))
# 
#   
#   FullZSet = rbind(FullZSet, zScore1)
# 
#   
# }#End of For statment
# colnames(FullZSet)[c(43:44)] <- 
#   c('zTotalFmi', 'zTotalLmi')
# # 
# write.table(FullZSet, file=("All.ZScoreValuesFinal.txt"))


# 
# blackData <- read.table(file=sprintf("data/Black.ZScoreValues.txt", sep="\t"))
# hispData <- read.table(file=sprintf("data/Hisp.ZScoreValues.txt", sep="\t"))
# whiteData <- read.table(file=sprintf("data/White.ZScoreValues.txt", sep="\t"))
# 
# 
# 
# whiteData1 <- whiteData1[order(whiteData1$Gender),]
# whiteData1 <- whiteData1[order(whiteData1$RIDAGEYR),]
# hispData1 <- hispData1[order(hispData1$Gender),]
# hispData1 <- hispData1[order(hispData1$RIDAGEYR),]
# blackData1 <- blackData1[order(blackData1$Gender),]
# blackData1 <- blackData1[order(blackData1$RIDAGEYR),]
# 
# blackFull <- cbind(blackData1, blackData)
# hispFull <- cbind(hispData1, hispData)
# whiteFull <- cbind(whiteData1, whiteData)
# 
# fullData <- rbind(blackFull, hispFull, whiteFull)

fullData <- read.table(file="data/All.ZScoreValuesFinal.txt",header=T, sep=" ")

shinyServer(
  function(input, output) {

    output$map <- renderPlot({

      
      zData2 <- fullData[ which(fullData$BMI>=input$BMIMIN 
                                & fullData$zTotalFmi >= input$FMIMIN 
                                & fullData$zTotalLmi>=input$LMIMIN
                                & fullData$BMI<=input$BMIMAX 
                                & fullData$zTotalFmi <= input$FMIMAX 
                                & fullData$zTotalLmi<=input$LMIMAX), ]
      

#       
#       zData2 <- fullData[ which(fullData$BMI>=15 
#                                 & fullData$totBodyFmi >= 5
#                                 & fullData$totBodyLmi>=2) , ]
      
      zData2 <- zData2[input$nInd,]
      print(zData2$BMI)
      print(zData2$totBodyFmi)
      print(zData2$totBodyLmi)
#       if (input$race == "Hispanic"){
#       preTreat = fullData[ which(fullData$Gender==input$gender 
#                                  & fullData$Age == input$age) , ]
#         
#         zData2 <- subset(preTreat, preTreat$Race=="Other Hispanic" | preTreat$Race=="Mexican American")
#                                                    
#                   
# #      subset(dfit3dBase, dfit3dBase$Race=="Hispanic"
# #             | dfit3dBase$Race=="Non-Hispanic Black"
# #             | dfit3dBase$Race == "Non-Hispanic White")
#       }
      
      dzData <- data.frame(zData2) 
      #print(dzData)
      #print(2)
      #Converts that set to dataframe
      #finds dimensions of that table and takes selectNumber random rows from that data set
      #dimension <- dim(dzData)
      #nRow <- floor(runif(1, 1,dimension[1]))  #Normally floor(runif(selectNumber, 1,dimension[1]))
      #selects out only those random rows and their FMI/LMI data
      fmiData <- dzData[1,c("Z_FMI_TR","Z_FMI_RA", "Z_FMI_RL", "Z_FMI_LL", "Z_FMI_LA")]
      lmiData <- dzData[1,c("Z_LMI_TR","Z_LMI_RA", "Z_LMI_RL", "Z_LMI_LL", "Z_LMI_LA")]
      #renames the columns because column names in fmiData/lmiData must match maxmin
      colnames(fmiData) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      colnames(lmiData) <- c("Z_TR", "Z_RA", "Z_RL", "Z_LL", "Z_LA")
      ind1Data <- rbind(maxmin,fmiData[1,],lmiData[1,])   #normally in a loop and i instead of 1
     
      op <- par(mar=c(1, 2, 2, 1),mfrow=chartDim)
      radarchart(ind1Data, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
                 pcol = c("goldenrod3", "firebrick4"),
                 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),
                 title=sprintf("%s %s Individual FMI/LMI Chart", input$race, input$gender))
      legend('topright', c("FMI", "FFMI") , lwd=2, 
             col=c("goldenrod3", "firebrick4"), bty='n', cex=1.2)
      
      renderText({ 
        
        paste("BMI/FMI/LMI: ", zData2$BMI, zData2$totBodyFmi, zData2$totBodyLmi)
        
      })
      
       })
    
    output$text1 <- renderText({ 
      zData2 <- fullData[ which(fullData$BMI>=input$BMIMIN 
                                & fullData$zTotalFmi >= input$FMIMIN 
                                & fullData$zTotalLmi>=input$LMIMIN
                                & fullData$BMI<=input$BMIMAX 
                                & fullData$zTotalFmi <= input$FMIMAX 
                                & fullData$zTotalLmi<=input$LMIMAX) , ]
      
      
      
      zData2 <- zData2[input$nInd,]
      paste("BMI/FMI/LMI/%FMI/%LMI: ", zData2$BMI, zData2$totBodyFmi, zData2$totBodyLmi,zData2$zTotalFmi, zData2$zTotalLmi)
      
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