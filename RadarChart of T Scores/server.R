# server.R
library(fmsb)
maxmin <- data.frame(
  Z_TR=c(2, -2),
  Z_LA=c(2, -2),
  Z_LL=c(2, -2),
  Z_RL=c(2, -2),
  Z_RA=c(2, -2))

chartDim <- c(1,1)


# setwd("C:\\Users\\bhinton\\Documents\\radar_chart\\Radar-App\\data")
# blackData <- read.table(file=sprintf("Black.MortZScoreValues4.txt", sep="\t"))
# hispData <- read.table(file=sprintf("Hisp.MortZScoreValues4.txt", sep="\t"))
# whiteData <- read.table(file=sprintf("White.MortZScoreValues4.txt", sep="\t"))



blackData <- read.table(file=sprintf("data/Black.MortZScoreValues4.txt", sep="\t"))
hispData <- read.table(file=sprintf("data/Hisp.MortZScoreValues4.txt", sep="\t"))
whiteData <- read.table(file=sprintf("data/White.MortZScoreValues4.txt", sep="\t"))
fullData <- rbind(blackData, hispData, whiteData)
fullData2 <- transform(fullData,
                       Bmi = TotBodyFmi + TotBodyLmi)


hisptotal1 <- transform(hispData,
                        Bmi = TotBodyFmi + TotBodyLmi)

shinyServer(
  function(input, output) {

    output$map <- renderPlot({

      zData2 <- fullData2[ which(fullData2$Gender==input$gender 
                                & fullData2$Age == input$age 
                                & fullData2$Race==input$race) , ]
      if (input$race=="Hispanic") {
        zData2 <- hisptotal1[ which(hisptotal1$Gender==input$gender
                              & hisptotal1$Age == input$age), ]
        }
      
      
     
      
      ntotal <- nrow(zData2)
      
      
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
      
      
      output$text1 <- renderText({ 
        paste("BMI of this individual:", dzData[1,"Bmi" ]) })
      output$text2 <- renderText({ 
        paste("Number in this category:", ntotal) })
        
      
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