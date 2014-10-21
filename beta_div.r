library(vegan)
library(betapart) #sudo apt-get install libgmp3-dev
#Set wd to source
data <- as.matrix(sapply(read.table("otu_table_elevation.txt", sep = "\t",row.names = 1, skip = 1,as.is=T), as.numeric))




colnames(data) <- c('PJset','GLCH4','GLH1','GLset','PJCH3','GLCH2','GLCH3','PJCH4','PPCH4','PPCH2','PPH1','PPset','PJH1','PJCH2')

dataGL <- data[,c('GLCH4','GLH1','GLset','GLCH2','GLCH3')]
dataGL <- dataGL[,c(3,2,4,5,1)]
dataGL <- t(dataGL[rowSums(dataGL) != 0,])
dataPJ <- data[,c('PJset','PJCH3','PJCH4','PJH1','PJCH2')]
dataPJ <- dataPJ[,c(1,4,5,2,3)]
dataPJ <- t(dataPJ[rowSums(dataPJ) != 0,])
dataPP <- data[,c('PPCH4','PPCH2','PPH1','PPset')]
dataPP <- dataPP[,c(4,3,2,1)]
dataPP <- t(dataPP[rowSums(dataPP) != 0,])
dataPP <- rbind('PPCH3' = NA, dataPP)
dataPP <- dataPP[c(2,3,4,1,5),]


# abc<-function(x){
#   gam<-ncol(x)
#   print(gam)
#   alph<-mean(specnumber(x))
#   whit<-gam/alph-1
#   lande<-gam-alph
#   out<-c(gam,alph,whit,lande)
#   names(out)<-c("Gamma", "Mean Alpha", "Whittaker's Beta", "Lande's Beta")
#   return(out)
# }

#Bt [Beta diversity turnover] = {g(H) [species gained, gamma - alpha at time 0] + l(H) [species lost, gamma - alpha at time n]}/2a {a = alpha diversity}
# plot one is time 0, plot n in time n (1,2,3,4)
#

# betaTurnover <- function(dfA){
#   alpha <- vector()
#   l <- vector()
#   for (i in 1:nrow(dfA)){
#     df <- dfA[i,][dfA[i,] != 0]
#     alpha[i] <- mean(specnumber(df))
#     if (i > 1){l[i] <- speciesDelta(ncol(dataPP), alpha[i])}
#   }
#   g <- ncol(dfA) - alpha[1]
#   print(g)
#   bt <- (l+rep(g, length(l)))/alpha
#   print(l)
#   print(alpha)
#   print(bt)
#   return(bt/c(0,36,72,110,152))
# }
# btGL <- betaTurnover(dataGL)
# plot(c(1750,1750,1750,1750,1750), btGL, pch = c('1','2','3','4'), col = 'red', ylim = c(0,1))
# 
# btPJ <- betaTurnover(dataPJ)
# points(c(2020,2020,2020,2020,2020), btPJ, pch = c('1','2','3','4'), col = 'blue')
# 
# btPP <- betaTurnover(dataPP)
# points(c(2344,2344,2344,2344,2344), btPP, pch = c('1','2','4'), col = 'green')
# 


dataGLp <- dataGL
dataGLp[dataGLp > 0] <- 1
dataGLp.beta <- beta.multi(dataGLp, index.family="sor")
dataGLp.beta

dataPJp <- dataPJ
dataPJp[dataPJp > 0] <- 1

dataPPp <- dataPP
dataPPp[dataPPp > 0] <- 1

betaSIM <- function(df){
  df.turnover <- vector()
  for (i in 2:nrow(df)){
    df.turnover[i-1] <- beta.temp(t(df[1,]), t(df[i,]), index.family="sor")$beta.sim
  }
  return(df.turnover)
}
x <- c(36,72,110,152)
plot(c(1750,1750,1750,1750),betaSIM(dataGLp)/x, ylim = c(-0.001, 0.025), xlim = c(1700,2400), col = 'red', pch = c('1','2','3','4'))
points(c(2020,2020,2020,2020), betaSIM(dataPJp)/x, col = 'blue', pch = c('1','2','3','4'))
points(c(2344,2344,2344), betaSIM(dataPPp[c(1,2,3,5),])/x[c(1,2,4)], col = 'green', pch = c('1','2','4') )
G <- as.data.frame(betaSIM(dataGLp))
PJ <- as.data.frame(betaSIM(dataPJp))
PP <- as.data.frame(betaSIM(dataPPp[c(1,2,3,5),]))
---------
  
# plot(c(1, 2, 3, 4), c(0.5029851,  0.4832,     0.6301616,  0.4622951))
# summary(lm(c(1, 2, 3, 4)~ c(0.5029851,  0.4832,     0.6301616,  0.4622951)))

  
---------------------
# Plot Similarity over time
