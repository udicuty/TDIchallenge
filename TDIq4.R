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
NYPD<-fread("DATA/NYPD_Motor_Vehicle_Collisions.csv",data.table=FALSE)
colnames(NYPD)<-make.names(colnames(NYPD))
NYPD$DATE<-mdy(NYPD$DATE)

borough<-"ALL BOROUGHS"
M<-filter(NYPD,BOROUGH=="MANHATTAN") %>% arrange(DATE)
M<-filter(NYPD,BOROUGH=="BROOKLYN") %>% arrange(DATE)
M<-filter(NYPD,BOROUGH=="QUEENS") %>% arrange(DATE)
M<-filter(NYPD,BOROUGH==borough) %>% arrange(DATE)
if (borough=="ALL BOROUGHS") {
  M<-filter(NYPD) %>% arrange(DATE)
}

#M<-filter(NYPD) %>% arrange(DATE) 
#M$MONTH<-substring(M$DATE,1,7)

#Summarizing information from NYPD file, "rat" stands for injured/incidents ratio
i<-group_by(M,DATE) %>% summarize(ped_injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),cyclist_injured=(sum(NUMBER.OF.CYCLIST.INJURED)),other=sum(NUMBER.OF.PERSONS.INJURED)-ped_injured-cyclist_injured)
#i<-group_by(M,DATE) %>% summarize(ped_injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),non_ped_injured=sum(NUMBER.OF.PERSONS.INJURED)-ped_injured)
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
cor(d$other,d$PRCP)
other<-coef(lm(d$other~d$PRCP))
other_intercept<-as.numeric(other[1])
other_slope<-as.numeric(other[2])
other_intercept
other_slope

cor(d$ped_injured,d$PRCP)
ped<-coef(lm(d$ped_injured~d$PRCP))
ped_intercept<-as.numeric(ped[1])
ped_slope<-as.numeric(ped[2])
ped_slope

cor(d$cyclist_injured,d$PRCP)
cyclist<-coef(lm(d$cyclist_injured~d$PRCP))
cyclist_intercept<-as.numeric(cyclist[1])
cyclist_slope<-as.numeric(cyclist[2])
cyclist_intercept
cyclist_slope



dd<-melt(d,measure.vars=c("ped_injured","other","cyclist_injured"))
colnames(dd)<-c("DATE","PRCP","Type","Injuries")
head(dd)

ggplot(dd,aes(PRCP,Injuries,col=Type)) + geom_point() + geom_abline(intercept= other_intercept,slope=other_slope,col="green",size=1.5) + geom_abline(intercept= ped_intercept,slope=ped_slope,col="Orange",size=1.5) + geom_abline(intercept= cyclist_intercept,slope=cyclist_slope,col="blue",size=1.5) + xlab("rain(0.1mm) / day") +ylab("injuries / day") + 
  scale_color_discrete(labels=c("pedestrian","other","cyclist")) + ggtitle( label=paste0("Rain vs Injuries, corellation 07-2012 - 01-2016 ",borough)) + ggsave(paste0("Percipitation_injury_",borough,".jpg"))





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
ggplot(x,aes(variable,value,fill=rain)) +scale_fill_discrete(labels=c("NO","YES")) + geom_bar(stat="identity") + ylab("normalized accidents ratio") +xlab ("participant") + ggtitle (paste0("Acidents involvment ratio by rain status 07-2012 - 01-2016 ",borough)) + ggsave(paste0("boxplot_participant_type_",borough,".jpg"))





M<-filter(M,CONTRIBUTING.FACTOR.VEHICLE.1!="")
subM<-colnames(select(M,NUMBER.OF.PERSONS.INJURED:NUMBER.OF.MOTORIST.KILLED))

xx<-sapply(subM, function (indication) {
  fisher<-sapply(unique(M$CONTRIBUTING.FACTOR.VEHICLE.1), function (cfactor) {
    a<-M$CONTRIBUTING.FACTOR.VEHICLE.1==cfactor
    b<-(M[,indication]>0)
    fisher.test(a,b)$p.value
    
  })
})
xx_corrected<-apply(xx,2,function (indication) p.adjust(indication,"bonferroni"))

xx_corrected<-xx_corrected<=0.05

g<-melt(xx_corrected)
head(g)
colnames(g)[3]<-"dependency"
g$X2<-gsub("NUMBER.OF.",replacement = "",g$X2)
g$X1<-substr(g$X1,1,25)
ggplot(g,aes(X2,X1,fill=dependency)) + geom_tile(col="black") + theme(axis.text.x = element_text(angle =90,vjust = 0.8,hjust=1,size=10), axis.text.y = element_text(size=8)) +
  ggtitle (paste0("Dependency Outcome/factor 07-2012 - 01-2016 ",borough)) + xlab("Outcome") + ylab("Contributing factor vehicle 1") + ggsave(paste0("factor_dependency_",borough,".jpg"))

k<-filter(M,CONTRIBUTING.FACTOR.VEHICLE.1=="Pavement Defective") %>% summarize(sum(NUMBER.OF.PERSONS.INJURED))

