
# Set the working directory
setwd("X:\\bhinton\\Ind Z Scores")
#Must use forward slash or double blackslash.... a single slash will not work





#TEST CHANGES
#TESAT CHANGES










BF_Zind = read.csv("BF_Comb_Zind.csv")  # read csv file


BF_Zind_15 <- BF_Zind[BF_Zind[, "Age"] == 15,]  #Selects only 15 yr olds?

df_BF_Zind_15 <- data.frame(BF_Zind_15)
df_BF_FMI_Zind_15 <- df_BF_Zind_15[1:4,c("Z_FMI_Tr","Z_FMI_LA", "Z_FMI_LL", "Z_FMI_RL", "Z_FMI_RA")]
df_BF_FFMI_Zind_15 <- df_BF_Zind_15[1:4,c("Z_FFMI_Tr","Z_FFMI_LA", "Z_FFMI_LL", "Z_FFMI_RL", "Z_FFMI_RA")]
#1:4 selects the first 4 rows, the c and quotes selects certain columns
Test_Dat <- df_BF_FMI_Zind_15
Test_Dat1 <- df_BF_FFMI_Zind_15

maxmin <- data.frame(
 Z_FMI_Tr=c(2, -2),
 Z_FMI_LA=c(2, -2),
 Z_FMI_LL=c(2, -2),
 Z_FMI_RL=c(2, -2),
 Z_FMI_RA=c(2, -2))

maxmin1 <- data.frame(
  Z_FFMI_Tr=c(2, -2),
  Z_FFMI_LA=c(2, -2),
  Z_FFMI_LL=c(2, -2),
  Z_FFMI_RL=c(2, -2),
  Z_FFMI_RA=c(2, -2))

dat <- rbind(maxmin,Test_Dat)
dat1 <- rbind(maxmin1,Test_Dat1)

op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))

radarchart(dat, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
 vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FMI Charts")
#cex.lab doesnt do anything
#Cex.main is for the title

radarchart(dat1, axistype=3, seg=4, cex.main=1, plty=1, plwd=2, 
           vlabels=c("TR", "RA", "RL", "LL", "LA"), caxislabels=c("-2","-1","0","1","2"),  title="4 Black Female FFMI Charts")
#cex.lab doesnt do anything








#caxislabels=("-2","-1","0","1","2"),


radarchart(dat, axistype=2, pcol=topo.colors(3), plty=1, pdensity=30, pfcol=topo.colors(3),
 title="(topo.colors, fill, axis=2)")









# Data must be given as the data frame, where the first cases show maximum.
maxmin <- data.frame(
 total=c(5, 1),
 phys=c(15, 3),
 psycho=c(3, 0),
 social=c(5, 1),
 env=c(5, 1))
# data for radarchart function version 1 series, minimum value must be omitted from above.
RNGkind("Mersenne-Twister")
set.seed(123)
dat <- data.frame(
 total=runif(3, 1, 5),
 phys=rnorm(3, 10, 2),
 psycho=c(0.5, NA, 3),
 social=runif(3, 1, 5),
 env=c(5, 2.5, 4))
dat <- rbind(maxmin,dat)
op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))
radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
 "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
 title="(axis=1, 5 segments, with specified vlabels)")
radarchart(dat, axistype=2, pcol=topo.colors(3), plty=1, pdensity=30, pfcol=topo.colors(3),
 title="(topo.colors, fill, axis=2)")
radarchart(dat, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
 title="(no points, axis=3, na.itp=FALSE)")
radarchart(dat, axistype=1, plwd=1:5, pcol=1, centerzero=TRUE, 
 seg=4, caxislabels=c("worst", "", "", "", "best"),
 title="(use lty and lwd but b/w, axis=1,\n centerzero=TRUE, with centerlabels)")
par(op)

Test



