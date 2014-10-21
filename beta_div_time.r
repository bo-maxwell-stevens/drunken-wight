library(vegan)
library(betapart) #sudo apt-get install libgmp3-dev
#Set wd to source
data <- as.matrix(sapply(read.table("otu_table_elevation.txt", sep = "\t",row.names = 1, skip = 1,as.is=T), as.numeric))


colnames(data) <- c('PJset','GLCH4','GLH1','GLset','PJCH3','GLCH2','GLCH3','PJCH4','PPCH4','PPCH2','PPH1','PPset','PJH1','PJCH2')

#Changing abundance into presence/absence
dataGL <- data[,c('GLCH4','GLH1','GLset','GLCH2','GLCH3')]
dataGL <- dataGL[,c(3,2,4,5,1)]
dataGL <- t(dataGL)
dataGL[dataGLp > 0] <- 1

dataPJ <- data[,c('PJset','PJCH3','PJCH4','PJH1','PJCH2')]
dataPJ <- dataPJ[,c(1,4,5,2,3)]
dataPJ <- t(dataPJ)
dataPJ[dataPJp > 0] <- 1

dataPP <- data[,c('PPCH4','PPCH2','PPH1','PPset')]
dataPP <- dataPP[,c(4,3,2,1)]
dataPP <- t(dataPP)
dataPP[dataPPp !=  0] <- 1
dataPP <- rbind('PPCH3' = NA, dataPP)
dataPP <- dataPP[c(2,3,4,1,5),]

#Creating function to loop over all possible comparisons
betaSIM <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[1,]), t(df[i,]), index.family="sor")$beta.sim
  }
  return(df.turnover)
}

#Plotting Beta turnover in relation to first time point
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
x <- c(36,72,110,152)
plot(airTemp3[c(2,3,4,5)],betaSIM(dataGL), col = 'red', pch = c('1','2','3','4'))
points(airTemp3[c(7,8,9,10)], betaSIM(dataPJ), col = 'blue', pch = c('1','2','3','4'))
points(airTemp3[c(12,13,15)], betaSIM(dataPP[c(1,2,3,5),]), col = 'green', pch = c('1','2','4') )
