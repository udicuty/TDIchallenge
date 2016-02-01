require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
#require(gtools)


#Reading central park temperatures and filtering for dates that intersect with NYPD accident records
w<-fread("Data/central_park_temp.csv",data.table=FALSE)
w$DATE<-ymd(w$DATE)
w<-filter(w,PRCP<1000) # Removing outliers

#Reading NYPD data for Manhattan only
NYPD<-fread("Data/NYPD_Motor_Vehicle_Collisions.csv",data.table=FALSE)
colnames(NYPD)<-make.names(colnames(NYPD))
NYPD$DATE<-mdy(NYPD$DATE)
M<-filter(NYPD,BOROUGH=="MANHATTAN") %>% arrange(DATE)
M<-filter(NYPD,NUMBER.OF.PERSONS.INJURED<100) %>% arrange(DATE) #Removing outliers
#M$MONTH<-substring(M$DATE,1,7)

#Summarizing information from NYPD file, "rat" stands for injured/incidents ratio
i<-group_by(M,DATE) %>% summarize(ped_injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),non_ped_injured=sum(NUMBER.OF.PERSONS.INJURED)-ped_injured)
head(i)

#Considering only dates to which we have information in both databases
common_dates<-intersect(w$DATE,i$DATE)
w<-filter(w,DATE %in% common_dates)
i<-filter(i,DATE %in% common_dates)
M<-filter(M,DATE %in% common_dates)


#Merging NYPD with weather data
d<-cbind(i,PRCP=w$PRCP)
head(d)
d$DATE<-as.character(d$DATE)


#Calculating corellation and fitting linear model between injuries and percipitation levels
cor(d$non_ped_injured,d$PRCP)
nonped<-coef(lm(d$non_ped_injured~d$PRCP))
nonped_intercept<-as.numeric(nonped[1])
nonped_slope<-as.numeric(nonped[2])
nonped_intercept
nonped_slope

cor(d$ped_injured,d$PRCP)
ped<-coef(lm(d$ped_injured~d$PRCP))
ped_intercept<-as.numeric(ped[1])
ped_slope<-as.numeric(ped[2])
ped_slope
#d$cyclist<-as.factor(ifelse(d$cyclist>0,1,0))


dd<-melt(d,measure.vars=c("ped_injured","non_ped_injured"))
head(dd)

ggplot(dd,aes(PRCP,value,col=variable)) + geom_point()

ggplot(dd,aes(PRCP,value,col=variable)) + geom_point() + geom_abline(intercept= nonped_intercept,slope=nonped_slope,col="cyan",size=1.5) + geom_abline(intercept= ped_intercept,slope=ped_slope,col="Orange",size=1.5)





#Setting categorical value for rainy days
rainy_days<-w$DATE[w$PRCP>0]
M$rainy_days<-M$DATE %in% rainy_days


x<-select(M,VEHICLE.TYPE.CODE.1,NUMBER.OF.PERSONS.INJURED,NUMBER.OF.PEDESTRIANS.INJURED,BOROUGH,NUMBER.OF.PERSONS.KILLED,rainy_days)
x[x==""]<-"UNKNOWN"



by_rain<-group_by(M,rainy_days) %>% summarize(incidents=n(),rain_d=n_distinct(DATE),ped_inj==sum(VEHICLE.TYPE.CODE.1=="TAXI")/rain_d
            taxi=sum(VEHICLE.TYPE.CODE.1=="TAXI")/rain_d,
            twowheels=sum(VEHICLE.TYPE.CODE.1 %in% c("BICYCLE","MOTORCYCLE","SCOOTER"))/rain_d,
            pas_vehicle=sum(VEHICLE.TYPE.CODE.1=="PASSENGER VEHICLE")/rain_d,
            bus=sum(VEHICLE.TYPE.CODE.1=="BUS")/rain_d)

x<-as.data.frame(by_rain)
x<-select(x,taxi:bus)
x<-apply(x,2,function(x) x/sum(x))
x<-melt(x)
x
x$rain<-c(rep(c(0,1),4))
x


xx<-melt(x)
xx
melt(x)
head(xx)
xx<-x[,8:10]

ggplot(x,aes(X2,value,fill=as.factor(rain))) + geom_bar(stat="identity") 

xx$rain<-c(0,1,0,1,0,1)

qplot(x)
qplot()
melt(x)

require(reshape)
x$per<-x$ped_inj_rat/sum(x$ped_inj_rat)
x
pie (x$per,labels = c(rain))

ggplot()

ggplot(by_rain)



#i$q<-quantcut(i$injured, q=seq(0,1,by=0.25), na.rm=TRUE,c(1:4))





#ggplot(x,aes(x==BOROUGH)) + geom_histogram()
#ggplot(x,aes(x=count,y=MONTH)) + geom_bar()
#qplot(factor(MONTH),datax$count,geom_bar())

#qplot(x$count,fill=x$VEHICLE.TYPE.CODE.1)
