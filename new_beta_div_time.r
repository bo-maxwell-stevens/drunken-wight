library(vegan)
library(betapart) #sudo apt-get install libgmp3-dev
#Set wd to source
data <- as.matrix(sapply(read.table("~/Documents/Monsoon/gitwork/otu_table_elevation.txt", sep = "\t",row.names = 1, skip = 1,as.is=T), as.numeric))


colnames(data) <- c('PJset','GLCH4','GLH1','GLset','PJCH3','GLCH2','GLCH3','PJCH4','PPCH4','PPCH2','PPH1','PPset','PJH1','PJCH2')

#Changing abundance into presence/absence
dataGL <- data[,c('GLCH4','GLH1','GLset','GLCH2','GLCH3')]
dataGL <- dataGL[,c(3,2,4,5,1)]
dataGL <- t(dataGL)
<<<<<<< HEAD
dataPJ <- data[,c('PJset','PJCH3','PJCH4','PJH1','PJCH2')]
dataPJ <- dataPJ[,c(1,4,5,2,3)]
dataPJ <- t(dataPJ)
dataPP <- data[,c('PPCH4','PPCH2','PPH1','PPset')]
dataPP <- dataPP[,c(4,3,2,1)]
dataPP <- t(dataPP)
dataPP <- rbind('PPCH3' = NA, dataPP)
dataPP <- dataPP[c(2,3,4,1,5),]

dataGLp <- dataGL
dataGLp[dataGLp > 0] <- 1
dataGLp.beta <- beta.multi(dataGLp, index.family="sor")
dataGLp.beta

dataPJp <- dataPJ
dataPJp[dataPJp > 0] <- 1

dataPPp <- dataPP
dataPPp[dataPPp > 0] <- 1

bray.part(dataGL[c(1,2),])

=======
dataGL[dataGL > 0] <- 1

dataPJ <- data[,c('PJset','PJCH3','PJCH4','PJH1','PJCH2')]
dataPJ <- dataPJ[,c(1,4,5,2,3)]
dataPJ <- t(dataPJ)
dataPJ[dataPJ > 0] <- 1

dataPP <- data[,c('PPCH4','PPCH2','PPH1','PPset')]
dataPP <- dataPP[,c(4,3,2,1)]
dataPP <- t(dataPP)
dataPP[dataPP !=  0] <- 1
dataPP <- rbind('PPCH3' = NA, dataPP)
dataPP <- dataPP[c(2,3,4,1,5),]

#Creating function to loop over all possible comparisons
>>>>>>> fa72dbd4002d00ee86055c0c48be6788252d61da
betaSIM <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[1,]), t(df[i,]), index.family="sor")$beta.sim
<<<<<<< HEAD
  }
  return(df.turnover)
}
betaBRAY <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- bray.part(df[c(1,i),])$bray.gra
  }
  return(df.turnover)
}
betaSIM_1 <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[i-1,]), t(df[i,]), index.family="sor")$beta.sim
  }
  return(df.turnover)
}
#?beta.temp
#Plot betaSIM over time
=======
  }
  return(df.turnover)
}

#Plotting Beta turnover in relation to first time point
>>>>>>> fa72dbd4002d00ee86055c0c48be6788252d61da
#Big change from 0 to 1, very little change from 0-2,3,4

betaSIM(dataGL)
#x = days since setup
x <- c(36,72,110,152)
plot(x,betaSIM(dataGL), ylim = c(0,1), col = 'red')
points(x, betaSIM(dataPJ), col = 'blue')
points(x[c(1,2,4)], betaSIM(dataPP[c(1,2,3,5),]), col = 'green')
?beta.temp
#Plotting Beta turnover in relation to previous time point
betaSIM <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[i-1,]), t(df[i,]), index.family="sor")$beta.sim
  }
  return(df.turnover)
}
x <- c(36,72-36,110-72,152-110)
betaSIM(dataGL)
plot(x,betaSIM(dataGL), ylim = c(0,1), col = 'red')
points(x, betaSIM(dataPJ), col = 'blue')
points(x[c(1,2,4)], betaSIM(dataPP[c(1,2,3,5),]), col = 'green')

#Plot BetaSIM at each elevation

plot(c(1750,1750,1750,1750),betaSIM(dataGL)/x, ylim = c(-0.001, 0.025), xlim = c(1700,2400), col = 'red', pch = c('1','2','3','4'))
points(c(2020,2020,2020,2020), betaSIM(dataPJ)/x, col = 'blue', pch = c('1','2','3','4'))
points(c(2344,2344,2344), betaSIM(dataPP[c(1,2,3,5),])/x[c(1,2,4)], col = 'green', pch = c('1','2','4') )

#Plot BetaSIM over temperature maximum
#THIS SECTION NOT DONE
airTemp3 <- c(22.57,34.5533333333,28.7033333333,24.2933333333,20.5276190476,20.3666666667,32.1433333333,27.0066666667,22.3366666667,19.32,16.7833333333,28.4066666667,24.0233333333,19.29,16.6374285714)
delta.airTemp3 <- vector()
for (i in 2:(length(airTemp3)/3)){
  delta.airTemp3[i-1] <- airTemp3[i] - airTemp3[1]
  delta.airTemp3[i-1+4] <- airTemp3[i+5] - airTemp3[6]
  delta.airTemp3[i-1+8] <- airTemp3[i+10] - airTemp3[11]
}
x <- c(36,72,110,152)
<<<<<<< HEAD
plot(delta.airTemp3[1:4],betaSIM(dataGLp), col = 'red', pch = c('1','2','3','4'), xlim = c(-3,13), ylim = c(0,1))
points(delta.airTemp3[5:8], betaSIM(dataPJp), col = 'blue', pch = c('1','2','3','4'))
points(delta.airTemp3[c(9,10,12)], betaSIM(dataPPp[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )

points(delta.airTemp3[1:4],betaSIM_1(dataGLp), col = 'red')
points(delta.airTemp3[5:8], betaSIM_1(dataPJp), col = 'blue')
points(delta.airTemp3[c(9,10,12)], betaSIM_1(dataPPp[c(1,2,3,5),]), col = 'green')

#Bray
delta.time <- c(36,72-36,110-72,152-110)
plot(delta.time,betaBRAY(dataGL), col = 'red', pch = c('1','2','3','4'), xlim = c(30,45), ylim = c(0,0.4))
points(delta.time, betaBRAY(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(delta.time[c(1,2,4)], betaBRAY(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )


####Making iterable plot for each variable

------------
G <- as.data.frame(betaSIM(dataGLp))
PJ <- as.data.frame(betaSIM(dataPJp))
PP <- as.data.frame(betaSIM(dataPPp[c(1,2,3,5),]))
---------
  
plot(c(1, 2, 3, 4), c(0.5029851,  0.4832,     0.6301616,  0.4622951))
summary(lm(c(1, 2, 3, 4)~ c(0.5029851,  0.4832,     0.6301616,  0.4622951)))

=======
plot(airTemp3[c(2,3,4,5)],betaSIM(dataGL), col = 'red', pch = c('1','2','3','4'))
points(airTemp3[c(7,8,9,10)], betaSIM(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(airTemp3[c(12,13,15)], betaSIM(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )
>>>>>>> fa72dbd4002d00ee86055c0c48be6788252d61da
