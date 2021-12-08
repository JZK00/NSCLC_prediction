library(Boruta)
library(ranger)

mydata <- read.csv("./ki-67.csv",header=T)
mydata <- mydata[complete.cases(mydata),]
predictors <- data.frame(mydata[1:83])
decision <- data.frame(mydata[,84])

mydata <- data.frame(predictors[1:83], decision = factor(decision[, 1]))

set.seed(1)
Boruta.mydata <- Boruta(decision ~., data = mydata,doTrace = 2, maxRuns=200, ntree = 500)
plot(Boruta.mydata)
plotImpHistory(Boruta.mydata)
plot(Boruta.mydata, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(Boruta.mydata$ImpHistory),function(i)
  Boruta.mydata$ImpHistory[is.finite(Boruta.mydata$ImpHistory[,i]),i])
names(lz) <- colnames(Boruta.mydata$ImpHistory)  
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), 
     at = 1:ncol(Boruta.mydata$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(Boruta.mydata)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <-  attStats(final.boruta)
boruta.df
