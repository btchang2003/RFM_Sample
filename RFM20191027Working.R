

getwd()
setwd("D:/Ben/Ben/OneDrive/BI/Projects/R Insert")

# Skip the following installation for already existed packages

install.packages("cluster")
install.packages("colorspace")

# Import final RFM file

RFM <- read.csv("RFM_Report.csv",header=TRUE,
                fileEncoding = "UTF-8-BOM")

# Nomralize numeric columns

rfm <- RFM[,c(2,3,4)]
nz<-scale(rfm,center=TRUE,scale=TRUE)

# Scree Chart
set.seed(42)
wss <- sum(kmeans(nz,1)$withinss)
# wss <- kmeans(nz.1)$tot.withinss
for (i in 2:10){
  wss[i] <- sum(kmeans(nz,centers=i)$withinss)
} 

plot(1:10,wss,type="b",
     xlab="Number of Clusters",ylab="Within Group SS",
     main="Scree Chart",pch=19)  

# Execute Kmeans

ck=3
km <- kmeans(nz,ck)
Group <- km$cluster

# Silhouette Examination

library(cluster) # plot silhouette chart

dt.nz <- dist(nz, method = "euclidean")
plot(silhouette(Group,dt.nz),border=NA,col=1:ck)

## Improve appearance

library(colorspace) # get nice colors

Nice.color <- rainbow_hcl(ck)[1:ck]
plot(silhouette(Group,dt.nz),border=NA,col=Nice.color)

# Merge the grouped list to the original table

RFM.res <- cbind(RFM,Group)

# Connect to Business Decision

Pair.data <- RFM.res[,c(2,3,4)]
Group.color <- rainbow_hcl(3)[as.numeric(RFM.res$Group)]

# dev.set(1)

pairs(Pair.data, col = Group.color,
      lower.panel = NULL,
      cex.labels=2, pch=19, cex = 0.8)

par(xpd = NA)
legend(x = 0.05, y = 0.6, cex = 1,
       legend = as.character(levels(factor(RFM.res$Group))),
       fill = unique(Group.color))

# try ggplot2
install.packages("ggplot2")
library(ggplot2)
#data(mtcars)
library(GGally)
ggpairs(RFM.res[,2:4], aes(colour = Group.color, alpha = 0.4))

ggpairs(RFM.res[,2:4], aes(colour = as.character(RFM.res$Group), alpha = 0.4))

