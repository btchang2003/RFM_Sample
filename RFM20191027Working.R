# A sample of RFM Analysis by Kmeans with R 
# Step 0 - Data and Environment Preparation

## Check the current directory
getwd()

## Set the working directory to the current one
setwd("D:/Ben/Ben/OneDrive/BI/Projects/R Insert")

## Preinstall packages

install.packages("dplyr") ## for Data manipulate in Step 1
## "dplyr" is a member of  "tidyverse" package set

install.packages("ggplot2") ## for visualization in Step 3 and 4
## all needed packages "colorspace","cluster","GGALLY" are included

## Read then check the raw data
Sales.rpt <- read.csv("SalesReport.csv",header=TRUE,fileEncoding = "UTF-8-BOM")
head(Sales.rpt,5)

## Change the column names
names(Sales.rpt)[1] <- "Date"
names(Sales.rpt)[3] <- "Amount"

## Check the result
head(Sales.rpt,5)

## Check the Data tpye
sapply(Sales.rpt, typeof) ## the type of data in storage
sapply(Sales.rpt, class) ## the category of data in the Dataframe

## Modify the data class of the column "Date"
Sales.rpt$Date <- as.Date(Sales.rpt$Date,format = "%Y/%m/%d")

## Recheck the data type
sapply(Sales.rpt, class)

# Step 1 - Create the Query "RFM"

## Active package "dplyr"
library(dplyr)

## Group raw data by ID
RFM <- Sales.rpt %>%
  group_by(ID) %>%
  summarize(
    Date.latest=max(Date),
    F=length(Amount),
    M=sum(Amount)
  ) %>%
  mutate(R=as.integer(max(Date.latest)-Date.latest))

## Rearrange the content
RFM <- RFM[,c(1,5,3,4)]

# Step 2 - Create the Visual 1 "Elbow Chart"

## Normalization before clustering
rfm <- RFM[,c(2,3,4)]
nz<-scale(rfm,center=TRUE,scale=TRUE)

## Create Elbow Chart
set.seed(42)
wss <- vector()
for (i in 1:10){
  wss[i] <- kmeans(nz,centers=i)$tot.withinss
} 

plot(1:10,wss,type="b",
     xlab="Number of Clusters",ylab="Within Group SS",
     main="Scree Chart",pch=19)  

# Step 3 - Evaluation by Silhouette in Visual 2

ck=3
km <- kmeans(nz,ck)
Group <- km$cluster

## Silhouette Examination

library(cluster) # plot silhouette chart
dt.nz <- dist(nz, method = "euclidean") ## Calculate Euclidean Distance

plot(silhouette(Group,dt.nz),border=NA,col=1:ck)
## to make Better color with palette
## by replacint  the above line with the following 3 lines

# library(colorspace) # get nice colors
# Nice.color <- rainbow_hcl(ck)[1:ck]
# plot(silhouette(Group,dt.nz),border=NA,col=Nice.color)

# Step 4 - Make busines decission by Visual 3

## Create the result file
RFM.res <- cbind(RFM,Group)

library(ggplot2)
library(GGally)

ggpairs(RFM.res[,2:4], aes(colour = as.character(RFM.res$Group),alpha = 0.4))
# End of the Snippet
