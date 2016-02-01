require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(reshape)
#require(gtools)


#Reading central park temperatures and filtering for dates that intersect with NYPD accident records
w<-fread("GIT/central_park_temp.csv",data.table=FALSE)
w$DATE<-ymd(w$DATE)
w<-filter(w,PRCP<1000) # Removing outliers

#Reading NYPD data for Manhattan only
NYPD<-fread("GIT/NYPD_Motor_Vehicle_Collisions.csv",data.table=FALSE)
colnames(NYPD)<-make.names(colnames(NYPD))
NYPD$DATE<-mdy(NYPD$DATE)
M<-filter(NYPD,BOROUGH=="MANHATTAN") %>% arrange(DATE)
#M<-filter(NYPD) %>% arrange(DATE) 
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


dd<-melt(d,measure.vars=c("ped_injured","non_ped_injured"))
colnames(dd)<-c("DATE","PRCP","Type","Injuries")
head(dd)

ggplot(dd,aes(PRCP,Injuries,col=Type)) + geom_point() + geom_abline(intercept= nonped_intercept,slope=nonped_slope,col="cyan",size=1.5) + geom_abline(intercept= ped_intercept,slope=ped_slope,col="Orange",size=1.5) +xlab("rain(mm/10) / day") +ylab("injuries / day") + 
  scale_color_discrete(labels=c("pedestrian","not_pedestrian")) + ggtitle( label="Rain vs Injuries, corellation \n Manhattan 07-2012 - 01-2016") + ggsave("Percipitation_injury.svg")





#Setting categorical value for rainy days
rainy_days<-w$DATE[w$PRCP>3]
M$rainy_days<-M$DATE %in% rainy_days
sum(M$rainy_days)


#Summarizing info
by_rain<-group_by(M,rainy_days) %>% summarize(incidents=n(),rain_d=n_distinct(DATE),taxi=sum(VEHICLE.TYPE.CODE.1=="TAXI")/rain_d,
            taxi=sum(VEHICLE.TYPE.CODE.1=="TAXI")/rain_d,
            twowheels=sum(VEHICLE.TYPE.CODE.2 %in% c("BICYCLE","MOTORCYCLE","SCOOTER"))/rain_d,
            pas_vehicle=sum(VEHICLE.TYPE.CODE.1=="PASSENGER VEHICLE")/rain_d,
            bus=sum(VEHICLE.TYPE.CODE.1=="BUS")/rain_d,
            pedestrian=sum(NUMBER.OF.PEDESTRIANS.INJURED>0)/rain_d)



#Reformating data
x<-as.data.frame(by_rain)
x<-select(x,c(rainy_days,taxi:pedestrian))
x<-apply(x,2,function(x) x/sum(x))
x<-as.data.frame(x)
colnames(x)[1]<-"rain"
x$rain<-as.factor(x$rain)
x<-melt(x,measure.vars = colnames(x[,2:6]))

#plotting
ggplot(x,aes(variable,value,fill=rain)) +scale_fill_discrete(labels=c("NO","YES")) + geom_bar(stat="identity") + ylab("normalized accidents ratio") +xlab ("participant") + ggtitle ("Acidents involvment ratio by rain status \n Manhattan 07-2012 - 01-2016") + ggsave("boxplot_participant_type.svg")

