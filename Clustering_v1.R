
# Step1:- Reading the Original Data

r = data.frame

# Step2:- Removing irrelevant parameters
data_scrape_com<-r[,c(5:8)]

summary(data_scrape_com)
str(data_scrape_com)
c = data_scrape_com

# Step3:- Scaling the data    

library(clusterSim)

b=data.Normalization(c,type="n4")
summary(b)
str(b)
data_scrape_com$Average.Time.Spent.on.Site=b$Average.Time.Spent.on.Site

# Step4:- Using K-Means to determine Optimal Number of Clusters

# Empty Object To Store Total Within Sum Of Squares 
ss<-NULL

#Generating a Scree Plot
for (j in 1:6)
{
  fit <- kmeans(b, j)
  ss[j]<-fit$tot.withinss
}

plot(ss, xlab="# Clusters",ylab="Total Within SS")
lines(ss, col="maroon")

# Step5:- Applying  the optimal number  of  Clusters

best_fit<-kmeans(b,2)

r$cluster01<-best_fit$cluster
r[order(r$cluster01),]

cluster_1<-r[r$cluster01==1,]
cluster_2<-r[r$cluster01==2,]
cluster_3<-r[r$cluster01==3,]

summary(cluster_1)[,c(4:10)] #My Understanding:
summary(cluster_2)[,c(4:10)] #My Understanding:
summary(cluster_3)[,c(2:10)] #My Understanding: 

#scatter plot
plot(c(cluster_1$wc,cluster_2$wc,cluster_3$wc),col = c("green","red","blue"),main = "wordcount")

#boxplot
boxplot(cluster_1$atop,cluster_2$atop,cluster_3$atop,names = c("Cl1","Cl2","Cl3"),main = "Atop")

#clusterplots
library(fpc)
plotcluster(x = r,best_fit$cluster)

clusplot(r, best_fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0,main = "cluster visualization")
