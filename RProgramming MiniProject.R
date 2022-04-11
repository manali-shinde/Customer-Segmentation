#Extracting data from large data set
setwd("D:/Manali/Sem 8/R programming/Miniproject")
data <- read.csv("Mall_Customers.csv")
str(data)
head(data)
data$Gender<-factor(data$Gender)

#EDA
min(data$Age)
min(data$Annual.Income..k..)
min(data$Spending.Score..1.100.)
max(data$Age)
max(data$Spending.Score..1.100.)
max(data$Annual.Income..k..)
range(data$Age)
range(data$Annual.Income..k..)
range(data$Spending.Score..1.100.)
mean(data$Age)
mean(data$Annual.Income..k..)
mean(data$Spending.Score..1.100.)
median(data$Age)
median(data$Annual.Income..k..)
median(data$Spending.Score..1.100.)
tab <- table(data$Age)
tab1 <- table(data$Annual.Income..k..)
tab2 <- table(data$Spending.Score..1.100.)
quantile(data$Age, 0.25)
quantile(data$Annual.Income..k..,0.25)
quantile(data$Spending.Score..1.100.,0.25)
quantile(data$Age, 0.75)
quantile(data$Annual.Income..k..,0.75)
quantile(data$Spending.Score..1.100.,0.75)
IQR(data$Age)
IQR(data$Annual.Income..k..)
IQR(data$Spending.Score..1.100.)
sd(data$Age)
sd(data$Annual.Income..k..)
sd(data$Spending.Score..1.100.)
var(data$Age)
var(data$Annual.Income..k..)
var(data$Spending.Score..1.100.)
summary(data$Age)
summary(data$Annual.Income..k..)
summary(data$Spending.Score..1.100.)
summary(data)


#Data Visulizations and Interpretation of results
#Customer Gender Visualization
a=table(data$Gender)
#barplot
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=terrain.colors(2),
        legend=rownames(a))

#piechart
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male",col=cm.colors(2))

#Visualization of Age Distribution
#Histogram
hist(data$Age,
     col="purple",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)
#boxplot
boxplot(data$Age,
        col="pink",
        main="Boxplot for Descriptive Analysis of Age")

#Analysis of the Annual Income of the Customers
#Histogram
hist(data$Annual.Income..k..,
     col="light green",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

#Density Plot
plot(density(data$Annual.Income..k..),
     col="yellow",
     main="Annual Income Analysis of Customers",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(data$Annual.Income..k..),
        col="brown")

#Analyzing Spending Score of the Customers
#Boxplot
boxplot(data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="light blue",
        main="BoxPlot for Descriptive Analysis of Spending Score")

#Histogram
hist(data$Spending.Score..1.100.,
     main="Histogram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="violet",
     labels=TRUE)
#line graph
plot(data$Spending.Score..1.100.,
     main="Line Graph for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",xlim=c(0,50),
     col="brown",type="o",
     labels=TRUE)
#scatter plot of Annual Income VS Spending
plot(x = data$Annual.Income..k..,y = data$Spending.Score..1.100.,
     main="Annual Income VS Spending",
     xlab = "Annual Income",
     ylab = "Spending",
     xlim =c(20,100),
     ylim=c(20,100),
)
#Boxplot of All Data
boxplot(data,col=heat.colors(5))

#Age vs Annual Income Analysis
ggplot(data, aes(Age, Annual.Income..k..)) + 
  geom_point(stat = "identity", aes(color = as.factor(Gender))) + 
  labs(y = "Annual Income", x = "Age") +
  ggtitle("Age vs Annual Income") +
  scale_color_discrete(name="Gender")

#Age vs Spending Score Analysis
ggplot(data, aes(Age, Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(Gender))) + 
  labs(y = "Spending Score", x = "Age") +
  ggtitle("Age vs Spending Score") +
  scale_color_discrete(name="Gender")


#Mining Algorithm (K Means)
#K-Means Clustering
#Taking Elbow estimates and finding the bend
mall <- read.csv("Mall_Customers.csv")
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(ggplot2)
library(tidyverse)
library(maptools)
library(rgdal)
library(dplyr)
library(e1071)
library(NbClust)


dim(mall)  # checking dimensions
colnames(mall) # names of variables
any(is.na(mall)) # checking if there are any missing values

head(mall) # let's see how the dataset looks like
summary(mall) # investigating basic statistics

# Also, let's change the names of two columns regarding annual income and spending score
colnames(mall)[4] <- "AnnualIncome"
colnames(mall)[5] <- "SpendingScore"

set.seed(123)
k.max <- 12
data <- mall[,c(4,5)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,  lwd=3,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     cex.lab=1.5, 
     col = "#9C9CEE")

km1 <- eclust(data, "kmeans", hc_metric="euclidean", k=5) # five clusters is an optimal number
# km1$cluster

# Alternative visualisation results using fviz_cluster 
fviz_cluster(km1, ellipse.type="convex", geom="point",
             stand=FALSE, ggtheme=theme_classic())

fviz_silhouette(km1)


#Partitioning Around Medoid
set.seed(123)

pam1 <- eclust(data, "pam", hc_metric="euclidean", k=5) # five clusters is an optimal number
fviz_cluster(pam1, ellipse.type="convex", geom="point",
             stand=FALSE, ggtheme=theme_classic())
# Silhouette plot
fviz_silhouette(pam1)

#Comparing Techniques
library("clusterSim")
index.DB(data, km1$cluster, centrotypes = "centroids")$DB

index.DB(data, pam1$cluster, centrotypes = "medoids", dist(data))$DB

#Cluster 1. Customers with medium annual income and medium annual spend
#Cluster 2. Customers with low annual income and low annual spend
#Cluster 3. Customers with high annual income and high annual spend
#Cluster 4. Customers with high annual income but low annual spend
#Cluster 5. Customers low annual income but high annual spend
