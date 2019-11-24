

getwd()
# setwd("D:/Ben/Ben/OneDrive/BI/Projects/R Insert")

install.packages("dplyr")
# install.packages("tidyverse")

# Read the raw data
# Change the column names and data type
# Check the result

Sales.rpt <- read.csv("SalesReport.csv",header=TRUE,fileEncoding = "UTF-8-BOM")

names(Sales.rpt)[1] <- "Date"
names(Sales.rpt)[3] <- "Amount"
head(Sales.rpt,5)

# Check the Data tpye

sapply(Sales.rpt, typeof) # the type of data in storage
sapply(Sales.rpt, class) # the category of data in the Dataframe

# Modify the class of Date column
Sales.rpt$Date <- as.Date(Sales.rpt$Date,format = "%Y/%m/%d")


library(dplyr)

RFM <- Sales.rpt %>%
  group_by(ID) %>%
  summarize(
    Date.latest=max(Date),
    F=length(Amount),
    M=sum(Amount)
  ) %>%
  mutate(R=as.integer(max(Date.latest)-Date.latest))

RFM <- RFM[,c(1,5,3,4)]

write.csv(RFM,'RFM_Report.csv',row.names=FALSE)
