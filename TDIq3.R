require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(gtools)


#Reading central park temperatures and filtering for dates that intersect with NYPD accident records
w<-fread("Data/central_park_temp.csv",data.table=FALSE)
w$DATE<-ymd(w$DATE)
w<-filter(w,PRCP<1000) # Removing outliers

#Reading NYPD data for Manhattan only
NYPD<-fread("Data/NYPD_Motor_Vehicle_Collisions.csv",data.table=FALSE)
colnames(NYPD)<-make.names(colnames(NYPD))
NYPD$DATE<-mdy(NYPD$DATE)
M<-filter(NYPD,BOROUGH=="MANHATTAN") %>% arrange(DATE)
#M<-filter(NYPD,NUMBER.OF.PERSONS.INJURED<100) %>% arrange(DATE) #Removing outliers
#M$MONTH<-substring(M$DATE,1,7)

#Summarizing information from NYPD file, "rat" stands for injured/incidents ratio
i<-group_by(M,DATE) %>% summarize(incidents=n(),injured=sum(NUMBER.OF.PERSONS.INJURED),ped_injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),death=sum(NUMBER.OF.PERSONS.KILLED),rat=injured/incidents)


#Considering only dates to which we have information in both databases
common_dates<-intersect(w$DATE,i$DATE)
w<-filter(w,DATE %in% common_dates)
i<-filter(i,DATE %in% common_dates)
M<-filter(M,DATE %in% common_dates)


#Merging NYPD with weather data
d<-cbind(i,PRCP=w$PRCP)

#Calculating corellation and fitting linear model between injuries and percipitation levels
cor(d$injured,d$PRCP)
x<-coef(lm(d$injured~d$PRCP))
xintercept<-as.numeric(x[1])
xslope<-as.numeric(x[2])
xintercept
xslope

cor(d$ped_injured,d$PRCP)
y<-coef(lm(d$ped_injured~d$PRCP))
yintercept<-as.numeric(y[1])
yslope<-as.numeric(y[2])
yslope
#d$cyclist<-as.factor(ifelse(d$cyclist>0,1,0))
ggplot(d,aes(PRCP,ped_injured)) + geom_point() + geom_abline(intercept= xintercept,slope=xslope,col="Red") + geom_abline(intercept= yintercept,slope=yslope,col="Red")






#Setting categorical value for rainy days
rainy_days<-w$DATE[w$PRCP>0]
M$rainy_days<-M$DATE %in% rainy_days


x<-select(M,VEHICLE.TYPE.CODE.1,NUMBER.OF.PERSONS.INJURED,NUMBER.OF.PEDESTRIANS.INJURED,BOROUGH,NUMBER.OF.PERSONS.KILLED,rainy_days)
x[x==""]<-"UNKNOWN"



by_rain<-group_by(M,rainy_days) %>% summarize(incidents=n(),injured=sum(NUMBER.OF.PERSONS.INJURED),
            ped_injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),rain_d=n_distinct(DATE),inj_rat=injured/rain_d/incidents,
            ped_inj_rat=ped_injured/rain_d/incidents,taxi=sum(VEHICLE.TYPE.CODE.1=="TAXI"),
            twowheels=sum(VEHICLE.TYPE.CODE.1 %in% c("BICYCLE","MOTORCYCLE","SCOOTER")),
                          pas_vehicle=sum(VEHICLE.TYPE.CODE.1=="PASSENGER VEHICLE"))
by_rain
x<-as.data.frame(by_rain)
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
