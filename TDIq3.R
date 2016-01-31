require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(gtools)

NYPD<-fread("Data/NYPD_Motor_Vehicle_Collisions.csv",data.table=FALSE)
colnames(NYPD)<-make.names(colnames(NYPD))
#colnames(NYPD)
#dim(NYPD)

NYPD$DATE<-mdy(NYPD$DATE)

M<-filter(NYPD,BOROUGH=="MANHATTAN") %>% arrange(DATE)
#M<-filter(NYPD) %>% arrange(DATE)

#M<-filter(NYPD) %>% arrange(DATE)
#M1<-select(M,DATE,TIME,BOROUGH,NUMBER.OF.PEDESTRIANS.INJURED)

i<-group_by(M,DATE) %>% summarize(count=n(),injured=sum(NUMBER.OF.PEDESTRIANS.INJURED),death=sum(NUMBER.OF.PERSONS.KILLED)) #%>% filter(injured<80)


w<-fread("Data/central_park_temp.csv",data.table=FALSE)
w$DATE<-ymd(w$DATE)
w<-filter(w,DATE>="2012-6-30" & DATE<="2016-01-26",PRCP<=2000)

common_dates<-intersect(w$DATE,i$DATE)
w<-filter(w,DATE %in% common_dates)
i<-filter(i,DATE %in% common_dates)


i$q<-quantcut(i$injured, q=seq(0,1,by=0.25), na.rm=TRUE,c(1:4))



#hist(i$injured)

#w$PRCP<-w$PRCP
#plot(w$PRCP,i$injured)
#cor(w$PRCP,i$injured)

#abline(lm(i$injured~w$PRCP),col="red")

d<-cbind(i,PRCP=w$PRCP)
d$death<-d$death>0
#group_by(d,q) %>% summarize(intercept=as.numeric(coef(lm(injured~PRCP))[1]),slope=as.numeric(coef(lm(injured~PRCP))[1]))


q1<-filter(d,q==1)
q1l<-coef(lm(q1$injured~q1$PRCP))
q1l

q2<-filter(d,q==2)
q2l<-coef(lm(q2$injured~q2$PRCP))
q2l

q3<-filter(d,q==3)
q3l<-coef(lm(q3$injured~q3$PRCP))
q3l


q4<-filter(d,q==4)
q4l<-coef(lm(q4$injured~q4$PRCP))
q4l



injuredcoef<-lm(d$injured~d$PRCP)

ggplot(d,aes(PRCP,injured)) + geom_point(aes(col=death)) + geom_abline(intercept=6.76,slope=0.009)

#w$PRCP<-w$PRCP*100
#plot(w$PRCP,i$count)
#cor(w$PRCP,i$count)
#abline(lm(i$count~w$PRCP),col="red")
