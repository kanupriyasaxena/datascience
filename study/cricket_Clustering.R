#Import file
setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Clustering")
cricket <- read.csv("Cricket.csv", stringsAsFactors=FALSE)
na.omit(cricket)

cricket$SR <- scale(cricket$SR)
cricket$Ave<-scale(cricket$Ave)


RFM_hclust2<- hclust(dist(cricket), method="complete")
plot(RFM_hclust2)

clusterCut <- cutree(RFM_hclust2, k=4)
RFM_hc <-cbind(cricket,clusterCut)

r_sq<- rnorm(20)
cricket_Cluster<-cricket[,c(1,8,10)]

for (number in 1:20){clus <- kmeans(cricket_Cluster, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

cric <- cricket[,c(8,10)] 
cric <- as.data.frame(scale(cric, center = TRUE, scale = TRUE)) 
for (i in 1:10)
{cric.km <- kmeans(cric, center = i, iter.max = 50, nstart = 50) 
  r_sq[i] <- cric.km$betweenss/cric.km$totss}
plot(r_sq)

clus4 <- kmeans(cric, centers = 4, iter.max = 50, nstart = 50)

cric_km <-cbind(cricket,clus4$cluster)