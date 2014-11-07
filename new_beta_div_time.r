library(vegan)
library(betapart) #sudo apt-get install libgmp3-dev
#Set wd to source
data <- as.matrix(sapply(read.table("~/Documents/Monsoon/gitwork/otu_table_elevation.txt", sep = "\t",row.names = 1, skip = 1,as.is=T), as.numeric))
data <- as.matrix(sapply(read.table("~/Documents/Monsoon/gitwork/otu_table_elevation_new.txt", sep = "\t",row.names = 1, skip = 1,as.is=T), as.numeric))
head(data)
colnames(data) <- c('PJset','GLCH4','GLH1','GLset','PJCH3','GLCH2','GLCH3','PJCH4','PPCH4','PPCH2','PPH1','PPset','PJH1','PJCH2')
colnames(data) <- c('PJCH3','PJset','PPCH2','PPCH4','GLCH4','PJCH4','PPset','PPH1','GLCH2','PJCH2','GLset','PJH1','GLH1','GLCH3')
#Changing abundance into presence/absence
dataGL <- data[,c('GLCH4','GLH1','GLset','GLCH2','GLCH3')]
dataGL <- dataGL[,c(3,2,4,5,1)]
dataGL <- t(dataGL)

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

#Creating function to loop over all possible comparisons

betaSIM <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[1,]), t(df[i,]), index.family="sor")$beta.sim
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
betaBRAY <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- bray.part(df[c(1,i),])$bray
  }
  return(df.turnover)
}
betaBRAY_1 <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- bray.part(df[c(i-1,i),])$bray
  }
  return(df.turnover)
}

#Plot betaSIM over time
#Plotting Beta turnover in relation to first time point

#?Big change from 0 to 1, very little change from 0-2,3,4

#x = days since setup
data.frame(cbind())
x <- c(36,72,110,152)
plot(x,betaSIM(dataGLp), ylim = c(0,1), col = 'red')
points(x, betaSIM(dataPJp), col = 'blue')
points(x[c(1,2,4)], betaSIM(dataPPp[c(1,2,3,5),]), col = 'green')

#Plotting Beta turnover in relation to previous time point
x <- c(36,72-36,110-72,152-110)
plot(x,betaSIM(dataGLp), ylim = c(0,1), col = 'red')
points(x, betaSIM(dataPJp), col = 'blue')
points(x[c(1,2,4)], betaSIM(dataPPp[c(1,2,3,5),]), col = 'green')

#Plot BetaSIM at each elevation

plot(c(1750,1750,1750,1750),betaSIM(dataGLp)/x, ylim = c(-0.001, 0.025), xlim = c(1700,2400), col = 'red', pch = c('1','2','3','4'))
points(c(2020,2020,2020,2020), betaSIM(dataPJp)/x, col = 'blue', pch = c('1','2','3','4'))
points(c(2344,2344,2344), betaSIM(dataPPp[c(1,2,3,5),])/x[c(1,2,4)], col = 'green', pch = c('1','2','4') )

#Plot BetaSIM over temperature maximum

airTemp3 <- c(22.57,34.5533333333,28.7033333333,24.2933333333,20.5276190476,20.3666666667,32.1433333333,27.0066666667,22.3366666667,19.32,16.7833333333,28.4066666667,24.0233333333,19.29,16.6374285714)
delta.airTemp3 <- vector()
for (i in 2:(length(airTemp3)/3)){
  delta.airTemp3[i-1] <- airTemp3[i] - airTemp3[1]
  delta.airTemp3[i-1+4] <- airTemp3[i+5] - airTemp3[6]
  delta.airTemp3[i-1+8] <- airTemp3[i+10] - airTemp3[11]
}
x <- c(36,72,110,152)

plot(delta.airTemp3[1:4],betaSIM(dataGLp), col = 'red', pch = c('1','2','3','4'), xlim = c(-3,13), ylim = c(0,1))
points(delta.airTemp3[5:8], betaSIM(dataPJp), col = 'blue', pch = c('1','2','3','4'))
points(delta.airTemp3[c(9,10,12)], betaSIM(dataPPp[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )

# points(delta.airTemp3[1:4],betaSIM_1(dataGLp), col = 'red')
# points(delta.airTemp3[5:8], betaSIM_1(dataPJp), col = 'blue')
# points(delta.airTemp3[c(9,10,12)], betaSIM_1(dataPPp[c(1,2,3,5),]), col = 'green')

#Bray from time 0
time <- c(36,72,110,152)
plot(time,betaBRAY(dataGL), col = 'red', pch = c('1','2','3','4'), xlim = c(30,160), ylim = c(0,1))
points(time, betaBRAY(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(time[c(1,2,4)], betaBRAY(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )
#Bray from prtime
delta.time <- c(36,72-32,110-72,152-110)
plot(delta.time,betaBRAY_1(dataGL), col = 'red', pch = c('1','2','3','4'), xlim = c(35,45), ylim = c(0,1))
points(delta.time, betaBRAY_1(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(delta.time[c(1,2,4)], betaBRAY_1(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )
reg1 <- lm(betaBRAY_1(dataGL) ~ delta.time)
abline(reg1)
summary(reg1)
reg2 <- lm(betaBRAY_1(dataPJ) ~ delta.time)
abline(reg2)
summary(reg2)
reg3 <- lm( betaBRAY_1(dataPP[c(1,2,3,5),]) ~ delta.time[c(1,2,4)])
abline(reg3)
summary(reg3)
## Delta Bray_1 vs delta air temp
delta.time <- c(36,72-32,110-72,152-110)
plot(delta.airTemp3[1:4],betaBRAY_1(dataGL), col = 'red', pch = c('1','2','3','4'), xlim = c(35,45), ylim = c(0,1))
points(delta.time, betaBRAY_1(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(delta.time[c(1,2,4)], betaBRAY_1(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )
reg1 <- lm(betaBRAY_1(dataGL) ~ delta.airTemp3[1:4])
abline(reg1)
summary(reg1)
reg2 <- lm(betaBRAY_1(dataPJ) ~ delta.airTemp3[1:4])
abline(reg2)
summary(reg2)
reg3 <- lm( betaBRAY_1(dataPP[c(1,2,3,5),]) ~ delta.time[c(1,2,4)])
abline(reg3)
summary(reg3)
####Making iterable plot for each variable