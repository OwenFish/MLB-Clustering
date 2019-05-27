
setwd("/Users/owen/Documents/School/Math400/BTS")
library("ggplot2")

origData <- read.csv("BattingData.csv")

#Remove 0 rows & people w/ less than 50 plate appearances
origData <- origData[origData$PA >= 500,]
origData <- na.omit(origData)

#Remove unnecessary rows
#final: R, H, 2B, HR, RBI, SB, SO, PCTAB
#Data 2: SB, BB, SO, BA, OBP, SLG, TB
data <- origData[,c(19,11,13:15,17,18)]
#data <- origData[,c(15,17:21, 24)]
data$pctAB <- origData$AB / origData$PA

cor(data)

#Scale Data
data <- scale(data)
data <- data.frame(data)

#Find appropriate K
wss.out <- 0
for (i in 1:10){
  model <- kmeans(data, centers=i, nstart=20)
  wss.out[i] <- model$tot.withinss
}
plot(wss.out, type='b', ylab="Total WithinSS")
k = 3

#Cluster & Combine with our data
clust1 <- kmeans(data, centers=3, nstart=20)
#clust2 <- kmeans(data2, centers=3, nstart=20)
mds.out <- data.frame(cmdscale(dist(data)))
colnames(mds.out) <- c("x", "y")
Cluster <- as.factor(clust1$cluster)
PA <- origData$PA

ggplot(data=mds.out, aes(x=mds.out$x, y=mds.out$y)) +
  geom_point(aes(colour=Cluster, size=PA)) +
   ylab("y") + xlab("x") +ggtitle("MDS Plot with Clusters and Size")


#Add in C1 to OrigData
origData <- cbind(origData, clust1$cluster)
colnames(origData)[31] <- "C1"


#Summary Statistics on C1
diff <- data.frame()
for (i in 1:3){
  df <- origData[origData$C1==i,]
  diff[i, "R"] <- median(df[,"R"])
  diff[i, "H"] <- median(df[,'H'])
  diff[i, "HR"] <- median(df[,"HR"])
  diff[i, "RBI"] <- median(df[,'RBI'])
  diff[i, "SB"] <- median(df[,"SB"])
  diff[i, "BB"] <- median(df[,'BB'])
  diff[i, "SO"] <- median(df[,"SO"])
  diff[i, "TB"] <- median(df[,'TB'])  
  diff[i, "BA"] <- median(df[,'BA'])
  diff[i, "OBP"] <- median(df[,"OBP"])
  diff[i, "SLG"] <- median(df[,"SLG"])
  diff[i, "PA"] <- median(df[,"PA"])
}
diff <- t(diff)
diff
