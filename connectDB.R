library(RODBC)
dbconnection <- odbcDriverConnect(
                    "Driver=ODBC Driver 17 for SQL Server;
                     Server=; 
                    Database=;
                    Uid=; 
                    Pwd=; 
                    trusted_connection=yes")
initdata <- sqlQuery(dbconnection,paste("select Postcode, TyreWidth, TyreProfile, TyreRim, TyresRequired, DateAdded from QuoteRequests;"))
odbcClose(channel)

#load libraries
library(dplyr)
library(lubridate)
library(odbc)
#make connection to server
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "",
                 Database = "",
                 UID = "",
                 PWD = "")
#get data using query
data<-dbGetQuery(con, "select Postcode, TyreWidth, TyreProfile, TyreRim, TyresRequired, DateAdded from QuoteRequests;")

#add the year column
data$year<-year(data$DateAdded)
data$month<-month(data$DateAdded)

#set up factors to group the data
data$Postcode<-as.factor(data$Postcode)
data$TyreWidth<-as.factor(as.numeric(data$TyreWidth))
data$TyreProfile<-as.factor(data$TyreProfile)
data$TyreRim<-as.factor(data$TyreRim)

#save the data
save(data,file="tyreData.rda")

#load the data
load("tyreData.rda")

#load libraries
library(dplyr)
library(ggplot2)


#check for complete cases
dataC<-data[which(complete.cases(data)),]


#find number of incomplete entries
incompleteEntries<-dim(data)[1]-dim(dataC)[1]
nrows1<-dim(dataC)[1]

#find invalid tyre profiles
dataC<-dataC[which(dataC$TyreProfile!="0"),]

#find number of invalid tyre profiles
invalidTyreProfile<-nrows1-dim(dataC)[1]
nrows2<-dim(dataC)[1]

#find invalid tyre rim data
dataC<-dataC[which(dataC$TyreRim!="0"),]

#find number of invalid tyre rims
invalidTyreRim<-nrows2-dim(dataC)[1]
nrows3<-dim(dataC)[1]

#find number of invalid postcodes
dataC<-dataC[which((!((dataC$Postcode=="0")|(dataC$Postcode=="")))),]
invalidPostcode<-nrows3-dim(dataC)[1]

#queries by year analysis
data2015<-dataC[which(dataC$year==2015),]
data2016<-dataC[which(dataC$year==2016),]
data2017<-dataC[which(dataC$year==2017),]
data2018<-dataC[which(dataC$year==2018),]

#save the data
save(data2015,file="tyreData2015.rda")
save(data2016,file="tyreData2016.rda")
save(data2017,file="tyreData2017.rda")
save(data2018,file="tyreData2018.rda")

#percentage by year
perc2015<-round((dim(data2015)[1]/dim(dataC)[1])*100,2)
perc2016<-round((dim(data2016)[1]/dim(dataC)[1])*100,2)
perc2017<-round((dim(data2017)[1]/dim(dataC)[1])*100,2)
perc2018<-round((dim(data2018)[1]/dim(dataC)[1])*100,2)

percentage<-c(perc2015,perc2016,perc2017,perc2018)
years<-c("2015","2016","2017","2018")

yearDf<-data.frame(years,percentage)

#do a graph for the year data here
p<-ggplot(data=yearDf, aes(x=years, y=percentage, fill=years)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percentage), vjust=-0.3, size=3.5)+
  theme_minimal()
p<-p+labs(x="Year",y="Percentage of total queries")
p

#month analysis

monthData2015<-data2015 %>% group_by(month) %>% summarise(n=n())
monthData2016<-data2016 %>% group_by(month) %>% summarise(n=n())
monthData2017<-data2017 %>% group_by(month) %>% summarise(n=n())
monthData2018<-data2018 %>% group_by(month) %>% summarise(n=n())



monthlyAvg2015<-mean(monthData2015$n)
monthlyAvg2016<-mean(monthData2016$n)
monthlyAvg2017<-mean(monthData2017$n)
monthlyAvg2018<-mean(monthData2018$n)

show(monthlyAvg2015)
show(monthlyAvg2016)
show(monthlyAvg2017)
show(monthlyAvg2018)

#postcode analysis
#group data by postcode, sum up all by postcode and arrange by number 
sortedPostcode<-dataC %>% group_by(Postcode) %>% summarise(n=n()) %>% arrange(desc(n))

#sanity check this should equal to dim(dataC)[1] 
totalQueries<-sum(sortedPostcode$n)

#check how many entried in sorted postcode make up 90% of total queries 
flag1<-TRUE
ctr1<-1
cumSum1<-0
while(flag1){
  cumSum1<-cumSum1+sortedPostcode[ctr1,]$n
  if(round((cumSum1/totalQueries),digits = 2)>.90){
    flag1<-FALSE
  }
  else{
    ctr1<-ctr1+1
  }
}

#subset the postcodes that make up 90% of the queries
sortedSubsetPostCode<-sortedPostcode[seq(1,ctr1),]

#what is the percentage of postcodes that make up 90% of the queries
percPostCodes<-dim(sortedSubsetPostCode)[1]/dim(sortedPostcode)

#display the postcodes result here

#old code
#sortedSubsetPostCode<-sortedPostcode[seq(1,800),2]
#sum1<-sum(sortedPostcode[seq(1,800),]$n)

#sort by tyre width - not required
#sortedTyreWidth<-dataC %>% group_by(TyreWidth) %>% summarise(n=n()) %>% arrange(desc(n))

#sort by all three, width, profile, rim
sortedTyres<-dataC %>% group_by(TyreWidth,TyreProfile,TyreRim) %>% summarise(n=n()) %>% arrange(desc(n))

#number of different sizes available 
noTyreSizes<-dim(sortedTyres)[1]

#display no of tyre sizes here

#check how many rows of sortedTyres make up 90% of the queries
flag2<-TRUE
ctr2<-1
cumtotalQueries<-0
while(flag2){
  cumtotalQueries<-cumtotalQueries+sortedTyres[ctr2,]$n
  if(round((cumtotalQueries/totalQueries),digits = 2)>.90){
    flag2<-FALSE
  }
  else{
    ctr2<-ctr2+1
  }
}

#% of tyre sizes that account for 90% of the queries
percTyreSizes<-ctr2/noTyreSizes

#display number and % of tyre sizes that make up 90% of the queries 

#subset the number of rows that make up 90% of the data 
subsetSortedTyres<-sortedTyres[seq(1,ctr2),]

#genrate a column that describes % of total queries
subsetSortedTyres$Percentage<-round((subsetSortedTyres$n/totalQueries)*100,digits = 2)
subsetSortedTyres$CumalativePercentage<-cumsum(subsetSortedTyres$Percentage)

#further subset top 20 rows of the subseted tyres
subset2SortedTyres<-subsetSortedTyres[seq(1,20),]

#what % of queries do the top 20 tyre sizes make up
percTop20Sizes<-sum(subset2SortedTyres$Percentage)

#further subset top 40 rows of the subseted tyres
subset3SortedTyres<-subsetSortedTyres[seq(1,40),]

#what % of queries do the top 40 tyre sizes make up
percTop40Sizes<-sum(subset3SortedTyres$Percentage)

#further subset top 60 rows of the subseted tyres
subset4SortedTyres<-subsetSortedTyres[seq(1,60),]

#what % of queries do the top 60 tyre sizes make up
percTop60Sizes<-sum(subset4SortedTyres$Percentage)

#further subset top 80 rows of the subseted tyres
subset5SortedTyres<-subsetSortedTyres[seq(1,80),]

#what % of queries do the top 80 tyre sizes make up
percTop80Sizes<-sum(subset5SortedTyres$Percentage)

# display a graph for 20, 40 , 60 , 80
top<-c("20","40","60","80")
percentages<-c(percTop20Sizes,percTop40Sizes,percTop60Sizes,percTop80Sizes)
topDf<-data.frame(top,percentages)

p<-ggplot(data=topDf, aes(x=top, y=percentages, fill=top)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percentages), vjust=-0.3, size=3.5)+
  theme_minimal()
p<-p+labs(x="Top tyre sizes",y="Percentage of queries")
p


# appendix - display the top tyre sizes and postcodes. 

#include a package
library(varhandle)
#copy the data to a different frame
dataC1<-dataC
#unfactor the postcode to label the area
dataC1$Postcode<-unfactor(dataC1$Postcode)
#add a new column for area
dataC1$PostArea<-""
#sort each row into different areas according to postcode
dataC1[between(dataC1$Postcode,2600,2620),"PostArea"]<-"CANBERRA"
dataC1[between(dataC1$Postcode,2000,2234),"PostArea"]<-"NSW-SYDNEY-METRO"
dataC1[between(dataC1$Postcode,2235,2999),"PostArea"]<-"NSW-REST"
dataC1[between(dataC1$Postcode,3000,3207),"PostArea"]<-"VIC-MELBOURNE-METRO"
dataC1[between(dataC1$Postcode,3208,3999),"PostArea"]<-"VIC-REST"
dataC1[between(dataC1$Postcode,4000,4207),"PostArea"]<-"QLD-BRISBANE-METRO"
dataC1[between(dataC1$Postcode,4300,4305),"PostArea"]<-"QLD-BRISBANE-METRO"
dataC1[between(dataC1$Postcode,4500,4519),"PostArea"]<-"QLD-BRISBANE-METRO"
dataC1[between(dataC1$Postcode,4208,4299),"PostArea"]<-"QLD-REST"
dataC1[between(dataC1$Postcode,4306,4499),"PostArea"]<-"QLD-REST"
dataC1[between(dataC1$Postcode,4520,4999),"PostArea"]<-"QLD-REST"
dataC1[between(dataC1$Postcode,5000,5199),"PostArea"]<-"SA-ADELAIDE-METRO"
dataC1[which(dataC1$Postcode==5950),"PostArea"]<-"SA-ADELAIDE-METRO"
dataC1[between(dataC1$Postcode,5200,5749),"PostArea"]<-"SA-REST"
dataC1[between(dataC1$Postcode,5825,5854),"PostArea"]<-"SA-REST"
dataC1[between(dataC1$Postcode,6000,6199),"PostArea"]<-"WA-PERTH-METRO"
dataC1[between(dataC1$Postcode,6200,6999),"PostArea"]<-"WA-REST"
dataC1[between(dataC1$Postcode,7000,7099),"PostArea"]<-"TAS-HOBART-METRO"
dataC1[between(dataC1$Postcode,7100,7999),"PostArea"]<-"TAS-REST"
dataC1[between(dataC1$Postcode,0800,0832),"PostArea"]<-"NT-DARWIN-METRO"
dataC1[between(dataC1$Postcode,0833,0899),"PostArea"]<-"NT-REST"
#convert the area into a factor variable
dataC1$PostArea<-as.factor(dataC1$PostArea)
#group entries by area and count them up 
sortedPostArea<-dataC1 %>% group_by(PostArea) %>% summarise(n=n()) %>% arrange(desc(n))
#generate the percentage of queries by number of queries from each area
sortedPostArea$Perc<-round((sortedPostArea$n/dim(dataC1)[1])*100,2)
#generate the cummalative percentage
sortedPostArea$CumPerc<-cumsum(sortedPostArea$Perc)

#generate a plot for displaying results
p<-ggplot(data=sortedPostArea, aes(x=PostArea, y=Perc, fill=PostArea)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Perc), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p<-p+labs(x="Area",y="Percentage of queries")
p
